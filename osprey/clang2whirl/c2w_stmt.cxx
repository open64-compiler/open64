/*
  Copyright (C) 2019-2020 XC5 Limited, Inc.  All Rights Reserved.

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

#include "c2w_stmt.h"
#include "c2w_builder.h"
#include "c2w_expr.h"
#include "c2w_tracer.h"
#include "c2w_lambda.h"
#include "c2w_utils.h"
#include "c2w_target.h"

// clang header files
#include "clanginc.h"

using namespace clang;

// open64 header files
#include "open64inc.h"


static const CompoundStmt *current_stmt;
typedef std::pair<const clang::CompoundStmt *, DTOR_ST_PAIR> DTOR_CALL_ITEM;
typedef std::stack<DTOR_CALL_ITEM>  DTOR_CALL_STACK;
DTOR_CALL_STACK dtor_call_stack;
static const CompoundStmt *try_stmt;
BOOL in_catch_stmt = FALSE;
#define END_CATCH_CLEANUP ((ST_IDX) 1)

HANDLER_INFO current_handler; // formed from handler_info_stack in Do_handlers

#define ENLARGE(x) (x + (x >> 1))

typedef struct case_info_t {
  INT64     case_lower_bound_value;
  INT64     case_upper_bound_value;
  LABEL_IDX case_label_idx;
} CASE_INFO;

static INT32        case_info_i = -1;
static INT32        case_info_max = 32;
static CASE_INFO   *case_info_stack =
  (CASE_INFO *) malloc (sizeof (CASE_INFO) * case_info_max);

typedef struct switch_info_t {
  WN        *index;
  INT32      start_case_index;
  LABEL_IDX  default_label_idx;
  SRCPOS     default_spos;
} SWITCH_INFO;

static INT32        switch_info_i = -1;
static INT32        switch_info_max = 32;
static SWITCH_INFO *switch_info_stack =
  (SWITCH_INFO *) malloc (sizeof (SWITCH_INFO) * switch_info_max);

typedef struct label_info_t {
  LABEL_IDX         label_idx;
  unsigned char     symtab_idx;
  unsigned char     defined;
} LABEL_INFO;

static INT32        undefined_labels_i = -1;
static INT32        undefined_labels_max = 32;
static LABEL_INFO  *undefined_labels_stack =
  (LABEL_INFO *) malloc (sizeof (LABEL_INFO) * undefined_labels_max);

bool processing_handler = false;

bool try_block_seen;
typedef struct eh_cleanup_entry {
  const Stmt        *tryhandler;  // just for comparison, at present
  DTOR_ST_VECTOR    *cleanups;    // emit (all cleanups)
  LABEL_IDX          pad;         // emit (landing pad WN_Set_Label_Is_Handler_Begin 2)
  LABEL_IDX          start;       // emit after pad and before cleanups
  LABEL_IDX          goto_idx;    // emit a goto
} EH_CLEANUP_ENTRY;

static std::list<EH_CLEANUP_ENTRY> cleanup_list_for_eh;

// Iterator for C++ exception handlers
class HANDLER_ITER {
  const CXXTryStmt *handler;
  INT index;
  INT num_handlers;

  public:
  HANDLER_ITER (const CXXTryStmt *h) : handler(h)
  {
    num_handlers = h->getNumHandlers();
  }

  void First (void) { index = 0; }
  void Next (void) { index++; }
  const CXXCatchStmt *Current (void) const
  {
    // list of handlers
    if (index < num_handlers)
    {
      const CXXCatchStmt *h = handler->getHandler(index);
      return h;
    }
    else
      return NULL;
  }
  BOOL Not_Empty (void) const { return Current() != NULL; }
};

class TYPE_FILTER_ENTRY {
  public:
  ST_IDX  st;      // typeinfo
  int     filter;        // action record filter
  friend bool operator== (const TYPE_FILTER_ENTRY&, const TYPE_FILTER_ENTRY&);
};

inline bool operator==(const TYPE_FILTER_ENTRY& x, const TYPE_FILTER_ENTRY& y) {
        return x.st == y.st;
}

// the list to store exception type symbol/filter
static vector<TYPE_FILTER_ENTRY>        type_filter_vector;

typedef std::stack<DTOR_ST_PAIR> DTOR_COPY_STACK;
DTOR_COPY_STACK dtor_for_copy_ctor_stack;

void Push_dtor_for_copy_ctor(QualType ty, ST_IDX st) {
  dtor_for_copy_ctor_stack.push(std::make_pair(ty, st));
}

void Push_dtor_call_stack(QualType ty, ST_IDX st) {
  dtor_call_stack.push(std::make_pair(current_stmt,
                         std::make_pair(ty, st)));
}

BOOL Is_dtor_call_stack_empty() {
  return dtor_call_stack.empty();
}

// return true if dtor_call_stack has obj may need cleanup
bool Check_dtor_call() {
  DTOR_CALL_STACK may_cleanup = dtor_call_stack;
  while (!may_cleanup.empty()) {
    if (may_cleanup.top().second.second != ST_IDX_ZERO)
      return true;
    may_cleanup.pop();
  }
  return false;
}

// return true if dtor_for_copy_ctor_stack has obj may need cleanup
bool Check_dtor_for_copy_ctor() {
  DTOR_COPY_STACK maybe_cleanup = dtor_for_copy_ctor_stack;
  while (!maybe_cleanup.empty()) {
    if (maybe_cleanup.top().second != ST_IDX_ZERO)
      return true;
    maybe_cleanup.pop();
  }
  return false;
}

// check if cleanups may be called
bool Maybe_do_eh_cleanups() {
  if (try_stmt || in_catch_stmt ||
      Check_dtor_call() || Check_dtor_for_copy_ctor())
    return true;
  return false;
}

namespace wgen {

HANDLER_LABEL_MAP handler_label_map;
LABEL_IDX& HANDLER_LABEL(const CXXCatchStmt *t)    { return handler_label_map[t]; }

WhirlStmtBuilder::WhirlStmtBuilder(WhirlBuilder *builder)
  : _builder(builder) {
}

WhirlStmtBuilder::~WhirlStmtBuilder() {
}


// Build EH type info
// create type info symbol (__TYPEINFO_TABLE__ ), type, inito
void WhirlStmtBuilder::Build_eh_type_info() {
  INITV_IDX blk, start;
  INITO_IDX id;

  for (int i=0; i<type_filter_vector.size(); ++i)
  {
    // generate INITV for filter symbol, filter value
    INITV_IDX st = New_INITV();
    if (type_filter_vector[i].st)
      INITV_Set_VAL(Initv_Table[st],
                     Enter_tcon(Host_To_Targ(MTYPE_U4,type_filter_vector[i].st)), 1);
    else
      INITV_Set_ZERO(Initv_Table[st], MTYPE_U4, 1);

    INITV_IDX filter = New_INITV();
    INITV_Set_VAL(Initv_Table[filter],
                   Enter_tcon(Host_To_Targ(MTYPE_U4, type_filter_vector[i].filter)), 1);
    Set_INITV_next(st, filter);

    if (i == 0)
    {
      blk = start = New_INITV();
      INITV_Init_Block(blk, st);
    }
    else
    {
      INITV_IDX next_blk = New_INITV();
      INITV_Init_Block(next_blk, st);
      Set_INITV_next(blk, next_blk);
      blk = next_blk;
    }

    // generate type info table symbol, and its inito/initv info
    if (i == (type_filter_vector.size()-1))
    {
      // generate type info symbol
      ARB_HANDLE arb = New_ARB();
      ARB_Init(arb, 0, type_filter_vector.size()-1, sizeof(TYPE_FILTER_ENTRY));
      Set_ARB_flags(arb, ARB_flags(arb) | ARB_FIRST_DIMEN | ARB_LAST_DIMEN);
      STR_IDX str = Save_Str("__TYPEINFO_TABLE__");
      FLD_HANDLE fld1 = New_FLD();
      FLD_Init(fld1, Save_Str("st"),
                              MTYPE_TO_TY_array[MTYPE_U4], 0);
      FLD_HANDLE fld2 = New_FLD();
      FLD_Init(fld2, Save_Str("filter"),
                              MTYPE_TO_TY_array[MTYPE_U4], 4);
      Set_FLD_flags(fld2, FLD_LAST_FIELD);

      TY_IDX struct_ty;
      TY_Init(New_TY(struct_ty), sizeof(TYPE_FILTER_ENTRY), KIND_STRUCT,
                              MTYPE_M, Save_Str("__TYPEINFO_ENTRY__"));
      Set_TY_fld(struct_ty, fld1);
      TY_IDX ty;
      TY_Init(New_TY(ty), type_filter_vector.size()*sizeof(TYPE_FILTER_ENTRY),
               KIND_ARRAY, MTYPE_M, str);
      Set_TY_arb(ty, arb);
      Set_TY_etype(ty, struct_ty);
      ST * typeinfo = New_ST(CURRENT_SYMTAB);
      ST_Init(typeinfo, str, CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, ty);
      Set_ST_is_initialized(*typeinfo);
      Set_ST_one_per_pu(typeinfo);
      Set_ST_Srcpos(*typeinfo, GetSrcPos());

      id = New_INITO(ST_st_idx(typeinfo), start);
      // Store the inito_idx in the PU
      // 1. exc_ptr 2. filter : Set 3rd entry with inito_idx
      INITV_IDX index = INITV_next(INITV_next(INITO_val(
                                    PU_misc_info(Get_Current_PU()))));
      // INITV_Set_VAL resets the next field, so back it up
      // and set it again.
      INITV_IDX bkup = INITV_next(index);
      INITV_Set_VAL(Initv_Table[index],
      Enter_tcon(Host_To_Targ(MTYPE_U4, id)), 1);
      Set_INITV_next(index, bkup);
    }
  }
  // eh spec function end init deleted
  type_filter_vector.clear();
}

static void
Push_scope_cleanup(const Stmt *t)
{
  if (++scope_cleanup_i == scope_cleanup_max) {
    scope_cleanup_max = ENLARGE(scope_cleanup_max);
    scope_cleanup_stack =
      (SCOPE_CLEANUP_INFO *) realloc(scope_cleanup_stack,
                        scope_cleanup_max * sizeof(SCOPE_CLEANUP_INFO));
  }

  scope_cleanup_stack [scope_cleanup_i].stmt = t;
  scope_cleanup_stack[scope_cleanup_i].label_idx = 0;
  if (isa<CXXTryStmt>(t))
    New_LABEL(CURRENT_SYMTAB,
              scope_cleanup_stack [scope_cleanup_i].cmp_idx);
  else
    scope_cleanup_stack [scope_cleanup_i].cmp_idx = 0;

  scope_cleanup_stack [scope_cleanup_i].vla.has_alloca = FALSE;
  scope_cleanup_stack [scope_cleanup_i].vla.alloca_st = NULL;
  scope_cleanup_stack [scope_cleanup_i].vla.alloca_sts_vector =
                                                new vector<ST*>();
}

static WN *
CreateIdnameFromReg(INT reg) {
  if (reg < 0)
    return NULL;

  PREG_NUM preg = Map_Reg_To_Preg[reg];
  if (preg < 0)
    return NULL;

  ST* st = NULL;
  if (Preg_Offset_Is_Int(preg))
    st = Int_Preg;
  else if (Preg_Offset_Is_Float(preg))
    st = Float_Preg;
#ifdef TARG_X8664
  else if (Preg_Offset_Is_X87(preg))
    st = X87_Preg;
#endif
  else
    Is_True(FALSE, ("unsupported preg %d", preg));

  return WN_CreateIdname((WN_OFFSET)preg, st);
}

static void
Record_Loop_Switch(const Stmt::StmtClass stmt, LABEL_IDX break_label_idx, LABEL_IDX continue_label_idx) {
  INT32 i;
  Is_True(stmt == Stmt::CXXForRangeStmtClass ||
          stmt == Stmt::DoStmtClass ||
          stmt == Stmt::ForStmtClass ||
          stmt == Stmt::SwitchStmtClass ||
          stmt == Stmt::WhileStmtClass,
          ("unexpected stmt class"));

  // realloc break_continue_info_stack if needed
  if (++break_continue_info_i == break_continue_info_max) {
    break_continue_info_max = ENLARGE(break_continue_info_max);
    break_continue_info_stack =
      (BREAK_CONTINUE_INFO *) realloc (break_continue_info_stack,
                                     break_continue_info_max *
                                       sizeof (BREAK_CONTINUE_INFO));
  }

  break_continue_info_stack
    [break_continue_info_i].tree_code          = stmt;
  break_continue_info_stack
    [break_continue_info_i].break_label_idx    = break_label_idx;
  break_continue_info_stack
    [break_continue_info_i].continue_label_idx = continue_label_idx;
}

static void
Pop_Loop_Switch(const Stmt::StmtClass stmt, LABEL_IDX &break_label, LABEL_IDX &cont_label) {
  Is_True(break_continue_info_i >= 0 &&
          break_continue_info_stack[break_continue_info_i].tree_code == stmt,
          ("bad loop index"));
  break_label = break_continue_info_stack[break_continue_info_i].break_label_idx;
  cont_label = break_continue_info_stack[break_continue_info_i].continue_label_idx;
  --break_continue_info_i;
}

static void
Append_label(LABEL_IDX label_idx, SRCPOS spos) {
  Is_True(label_idx != 0, ("invalid label idx"));
  WN *wn = WN_CreateLabel((ST_IDX)0, label_idx, 0, NULL);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);
}

WN *WhirlStmtBuilder::ConvertGCCAsmStmt(const GCCAsmStmt *stmt) {
  TRACE_FUNC();
  SRCPOS spos = SetSrcPos(getLocation(stmt));

  // track opnds and their constraints
  INT opnd_map[MAX_ASM_OPERANDS];
  memset(opnd_map, 0, sizeof(opnd_map));
  std::string opnd_constraint[MAX_ASM_OPERANDS];
  INT ninputs = stmt->getNumInputs();

  INT i = 0;
  // check output list and track output constraints
  Is_True(stmt->getNumOutputs() < MAX_ASM_OPERANDS, ("TODO: incr limit"));
  for (i = 0; i < stmt->getNumOutputs(); ++i) {
    opnd_map[i] = i;
    Is_True(!stmt->getOutputConstraint(i).empty(),
            ("output constraint string is empty"));
    opnd_constraint[i] = stmt->getOutputConstraint(i).str();
  }
  opnd_map[i] = -1;

  // check if output constraints contains '+'
  for (i = 0; i < stmt->getNumOutputs(); ++i) {
    std::string str = stmt->getOutputConstraint(i).str();
    if (strchr(str.c_str(), '+') ||
        AsmConstraintsByAddress(str.c_str(), opnd_constraint))
      ++ ninputs;
  }

  // get asm string
  std::string asm_str = stmt->getAsmString()->getString().str();
  // generate asm stmt
  WN *asm_block = WN_CreateBlock();
  WN *asm_wn =  WN_CreateAsm_Stmt(ninputs + 2, (char*)asm_str.c_str());
  WN_Set_Asm_Volatile(asm_wn);
  WN_INSERT_BlockLast(asm_block, asm_wn);
  WN_Set_Linenum(asm_wn, spos);

  // handle clobber list
  WN *clobber_block = WN_CreateBlock();
  WN_kid0(asm_wn) = clobber_block;
  // handle clobber list
  for (i = 0; i < stmt->getNumClobbers(); ++i) {
    std::string str = stmt->getClobber(i).str();
    INT reg = AsmDecodeRegisterName(str.c_str());
    if (reg == ASM_CLOBBER_CC) {
      WN_Set_Asm_Clobbers_Cc(asm_wn);
    }
    else if (reg == ASM_CLOBBER_MEMORY) {
      WN_Set_Asm_Clobbers_Mem(asm_wn);
    }
    else {
      // create a st for this clobber register
      ST *st = New_ST(CURRENT_SYMTAB);
      ST_Init(st, Str_To_Index(Save_Str(str.c_str()), Current_Strtab),
              CLASS_NAME, SCLASS_UNKNOWN, EXPORT_LOCAL, TY_IDX_ZERO);
      Set_ST_Srcpos(*st, spos);

      WN *pragma = NULL;
      WN *idname = CreateIdnameFromReg(reg);
      if (idname) {
        // map to a dedicated whirl preg, create xpragma
        // and let the preg to be the kid
        pragma = WN_CreateXpragma(WN_PRAGMA_ASM_CLOBBER,
                                  ST_st_idx(st), 1);
        WN_kid0(pragma) = idname;
      }
      else {
        // map to a "unknown" st, create pragma with
        // the st_idx and mark asm "volatile"
        pragma = WN_CreatePragma(WN_PRAGMA_ASM_CLOBBER,
                                 ST_st_idx(st), 0, 0);
        WN_Set_Asm_Volatile(asm_wn);
      }
      WN_INSERT_BlockLast(clobber_block, pragma);
      WN_Set_Linenum(pragma, spos);
    }
  }

  WhirlExprBuilder expr_bldr(_builder);
  INT input_index = 2;   // start from kid2
  INT param_index = 0;
  // handle output list
  for (INT i = 0; i < stmt->getNumOutputs(); ++i) {
    std::string str = stmt->getOutputConstraint(i).str();
    if (AsmConstraintsByAddress(str.c_str(), opnd_constraint)) {
      WN *wn = expr_bldr.ConvertToNode(stmt->getOutputExpr(i));
      Is_True(wn && OPERATOR_is_expression(WN_operator(wn)),
              ("bad wn"));
      WN_kid(asm_wn, input_index) = WN_CreateAsm_Input((char*)str.c_str(),
                                                       param_index, wn);
      ++input_index;
    }
    ++param_index;
  }
  // handle input list
  for (INT i = 0; i < stmt->getNumInputs(); ++i) {
    std::string str = stmt->getInputConstraint(i).str();
    WN *wn = expr_bldr.ConvertToNode(stmt->getInputExpr(i));
    Is_True(wn && OPERATOR_is_expression(WN_operator(wn)),
            ("bad expr"));
    WN_kid(asm_wn, input_index) = WN_CreateAsm_Input((char*)str.c_str(),
                                                     param_index, wn);
    ++input_index;
    ++param_index;
  }
  // handle constraint list
  param_index = 0;
  INT nonmem_opnd_index = 0;
  static PREG_NUM asm_neg_preg = -2;
  WN *constraint_block = WN_CreateBlock();
  WN_kid1(asm_wn) = constraint_block;
  for (INT i = 0; i < stmt->getNumOutputs(); ++i) {
    std::string str = stmt->getOutputConstraint(i).str();
    if (!AsmConstraintsByAddress(str.c_str(), opnd_constraint)) {
      // convert output expr
      TY_IDX ty = _builder->TB().ConvertType(stmt->getOutputExpr(i)->getType());
      Result out = expr_bldr.ConvertExpr(stmt->getOutputExpr(i));
      ST *preg_st = MTYPE_To_PREG(TY_mtype(ty));
      // generate a store to output expr at first
      WN *ldid = WN_Ldid(TY_mtype(ty), asm_neg_preg, preg_st, ty);
      WN *store = WN_Istore(TY_mtype(ty), 0, Make_Pointer_Type(ty),
                            out.GetLValue(), ldid);
      WN_INSERT_BlockLast(asm_block, store);
      WN_Set_Linenum(store, spos);
      // handle '+'
      const char *c_str = str.c_str();
      if (strchr(c_str, '+') != NULL) {
        // remove leading '+'
        while (*c_str == '+')
          ++c_str;
        char input_cons[16];
        snprintf(input_cons, 16, "%d", nonmem_opnd_index);
        WN *wn = out.GetRValue();
        Is_True(wn && OPERATOR_is_expression(WN_operator(wn)),
                ("bad expr"));
        WN_kid(asm_wn, input_index) = WN_CreateAsm_Input(input_cons, param_index, wn);
        ++input_index;
        ++param_index;
      }
      ++ nonmem_opnd_index;
      // create asm constraint for ouput
      ST *cons_st = New_ST(CURRENT_SYMTAB);
      ST_Init(cons_st, Str_To_Index(Save_Str(c_str), Current_Strtab),
              CLASS_NAME, SCLASS_UNKNOWN, EXPORT_LOCAL, TY_IDX_ZERO);
      Set_ST_Srcpos(*cons_st, spos);
      WN *pragma = WN_CreatePragma(WN_PRAGMA_ASM_CONSTRAINT,
                                   ST_st_idx(preg_st),
                                   ST_st_idx(cons_st),
                                   asm_neg_preg,
                                   param_index);
      WN_INSERT_BlockLast(constraint_block, pragma);
      WN_Set_Linenum(pragma, spos);
      --asm_neg_preg;
    }
  }

  return asm_block;
}

WN *WhirlStmtBuilder::ConvertAttributedStmt(const AttributedStmt *stmt) {
  TRACE_FUNC();
  return NULL;
}

WN *WhirlStmtBuilder::ConvertBreakStmt(const BreakStmt *stmt) {
  TRACE_FUNC();
  Is_True(break_continue_info_i >= 0, ("No break/continue info"));
  INT32 i = break_continue_info_i;
  LABEL_IDX label_idx = break_continue_info_stack[i].break_label_idx;

  if (label_idx == 0) {
    // Control can reach here even while processing an
    // exception handler.
    New_LABEL(CURRENT_SYMTAB, label_idx);
    break_continue_info_stack[i].break_label_idx = label_idx;
  }

  // TODO: cleanup

  WN *wn = WN_CreateGoto((ST_IDX)NULL, label_idx);
  WN_Set_Linenum(wn, SetSrcPos(getLocation(stmt)));
  return wn;
}

WN *WhirlStmtBuilder::ConvertCallExpr(const CallExpr *stmt) {
  TRACE_FUNC();
  // no return value here
  WhirlExprBuilder expr_bldr(_builder);
  Result r = expr_bldr.ConvertExpr(stmt, Result::nwNone(), FALSE);
  if (r.isNone())
    return NULL;
  Is_True(r.isNode(), ("call expr not return whirl node"));
  Is_True(OPERATOR_is_stmt(WN_operator(r.Node())), ("node is not stmt"));
  return r.Node();
}

WN *WhirlStmtBuilder::ConvertCXXMemberCallExpr(const CXXMemberCallExpr *stmt) {
  TRACE_FUNC();
  // no return value here
  WhirlExprBuilder expr_bldr(_builder);
  Result r = expr_bldr.ConvertExpr(stmt, Result::nwNone(), FALSE);
  if (r.isNone())
    return NULL;
  Is_True(r.isNode(), ("call expr not return whirl node"));
  Is_True(OPERATOR_is_stmt(WN_operator(r.Node())), ("node is not stmt"));
  return r.Node();
}

WN *WhirlStmtBuilder::ConvertCXXOperatorCallExpr(const CXXOperatorCallExpr *stmt) {
  TRACE_FUNC();
  // no return value here
  WhirlExprBuilder expr_bldr(_builder);
  Result r = expr_bldr.ConvertExpr(stmt, Result::nwNone(), FALSE);
  if (r.isNone())
    return NULL;
  if (r.isNode()) {
    Is_True(OPERATOR_is_stmt(WN_operator(r.Node())), ("node is not stmt"));
    return r.Node();
  }
  else {
    Is_True(r.isSym(), ("Non-void call expr not return tmp symbol."));
    // call expr was inserted into block
    return NULL;
  }
}

WN *WhirlStmtBuilder::ConvertCapturedStmt(const CapturedStmt *stmt) {
  TRACE_FUNC();
  Is_True(false, ("unsupported ConvertCapturedStmt"));
  return NULL;
}

// find OPR_CALL/OPR_ICALL from wn
WN *FindCallWn(WN *wn) {
  Is_True(wn != NULL, ("invalid wn"));

  if (WN_operator(wn) == OPR_REGION) {
    return NULL;
  }

  if ((WN_operator(wn) == OPR_CALL
       && strcmp(ST_name(WN_st(wn)), "__cxa_allocate_exception") != 0)
      || WN_operator(wn) == OPR_ICALL)
     return wn;

  if (WN_operator(wn) == OPR_BLOCK) {
     WN *node = WN_first(wn);
     while (node != NULL) {
       WN *tmp = NULL;
       if (tmp = FindCallWn(node))
         return tmp;
       node = WN_next(node);
     }
  } else {
   INT i;
   for (i = 0; i < WN_kid_count(wn); i++) {
     WN *kid = NULL;
     if (kid = FindCallWn(WN_kid(wn, i)))
       return kid;
   }
  }
  return NULL;
}

WN *WhirlStmtBuilder::Emit_cxx_constructor_call(const VarDecl *decl) {
  ST_IDX arg_st = _builder->Get_var_st(decl);

  Result target = Result::nwSym(arg_st, ST_type(arg_st));
  WhirlExprBuilder expr_bldr(_builder);
  const Expr *init = decl->getInit();
  Mark_call_region(CALL_REGION_STACK_MARK);
  WN *ctor_block = WhirlBlockUtil::nwBlock();
  WN *call_wn = expr_bldr.ConvertToNode(init, target, FALSE);
  WhirlBlockUtil::popCurrentBlock();

  if (ExprWithCleanups::classof(init))
    init = ((ExprWithCleanups *)(init))->getSubExpr();

  const CXXConstructExpr *expr = dyn_cast<CXXConstructExpr>(init);
  Is_True(expr, ("should be CXXConstructExpr"));

  const ConstantArrayType *cat =
    _builder->Context()->getAsConstantArrayType(expr->getType());

  if (!decl->isStaticLocal() &&
      !expr->getConstructor()->getParent()->hasTrivialDestructor()) {  
    
    if (!cat && call_wn &&
        WN_operator(call_wn) == OPR_LDID && WN_st_idx(call_wn) != arg_st) {
      Push_dtor_for_copy_ctor(expr->getType(), WN_st_idx(call_wn));
    } else {
      // push decl to dtor_call_stack
      Push_dtor_call_stack(expr->getType(), arg_st);
    }
  }

  if (emit_exceptions &&
      WN_first(ctor_block) && FindCallWn(ctor_block)) {
    ctor_block = Setup_eh_region(ctor_block, GetSrcPos(),
                                 false /* for_unwinding */,
                                 true /* is_dtor_or_ctor_call */);
  }
  Unmark_call_region();
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ctor_block);

  if (!call_wn)
    return NULL;

  if (WN_operator(call_wn) == OPR_LDID) {
    if (!cat)
      Is_True(ST_st_idx(WN_st(call_wn)) == arg_st, ("st mismatch"));
    return NULL;
  }
  Is_True(call_wn != NULL &&
          (WN_operator(call_wn) == OPR_CALL ||
           WN_operator(call_wn) == OPR_ISTORE ||
           WN_operator(call_wn) == OPR_STID) , ("shoule return whirl node"));
  return call_wn;
}

WN *WhirlStmtBuilder::Emit_cxx_destructor_call(QualType type, ST_IDX arg_st) {
  WN *decl_wn, *call_wn = NULL;
  const CXXRecordDecl *record_decl = type->getAsCXXRecordDecl();

  // check for struct array
  bool is_constant_array = false;
  int num_element = 0;
  if (const ConstantArrayType *cat =
        _builder->Context()->getAsConstantArrayType(type)) {
    if (record_decl =
          _builder->DeclBuilder().ConvertConstantArrayType(type,
                                                             num_element)) {
      Is_True(num_element != 0,
              ("num_element should not be zero for constant array type"));
      is_constant_array = true;
    }
  }
  CXXDestructorDecl *dtor_decl = record_decl->getDestructor();
  Is_True(dtor_decl && !dtor_decl->isTrivial(), ("bad dtor decl"));

  // defer destructor if not exist
  GlobalDecl gd(dtor_decl, CXXDtorType::Dtor_Complete);
  ST_IDX st_idx = _builder->Get_func_st(gd);
  Is_True(st_idx, ("bad dtor st"));

  // create destructor call wn
  call_wn = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 1);
  WN_st_idx(call_wn) = st_idx;
  WN_Set_Call_Default_Flags(call_wn);
  WN_Set_Linenum(call_wn, GetSrcPos());

  TY_IDX arg_ty = _builder->TB().ConvertType(type);
  ST *st = ST_ptr(arg_st);
  Set_ST_addr_passed(st);
  Set_ST_addr_saved(st);
  TY_IDX arg_ptr_ty = Make_Pointer_Type(arg_ty);
  TYPE_ID arg_mtype = TY_mtype(arg_ptr_ty);
  WN_kid0(call_wn) =
    WGEN_CreateParm(Mtype_comparison(arg_mtype),
                    WN_Lda(Pointer_Mtype, ST_ofst(st), st),
                    arg_ptr_ty);

  if (is_constant_array) {
    // destructors for elements of an array are called
    // in reverse order of their construction
    WN *wn = NULL;
    WN *blk = WN_CreateBlock();
    ST *tmp_st = MTYPE_To_PREG(Pointer_Mtype);
    PREG_NUM tmp_ofst = Create_Preg(Pointer_Mtype, ".ptr.");
    WN *adjust_wn = WN_Add(Pointer_Mtype,
                           WN_Lda(Pointer_Mtype, ST_ofst(st), st),
                           WN_Intconst(Pointer_Mtype, TY_size(arg_ty)));

    wn = WN_Stid(Pointer_Mtype, tmp_ofst, tmp_st, arg_ptr_ty, adjust_wn);
    WN_INSERT_BlockLast(blk, wn);
    WN_Set_Linenum(wn, GetSrcPos());

    WN *body_blk = WN_CreateBlock();
    wn = WN_CreateWhileDo(WN_Intconst(Boolean_type, 1), body_blk);
    WN_INSERT_BlockLast(blk, wn);

    WN *cond_wn = WN_EQ(Pointer_Mtype,
                        WN_Ldid(Pointer_Mtype, tmp_ofst, tmp_st, arg_ptr_ty),
                        WN_Lda(Pointer_Mtype, ST_ofst(st), st));
    LABEL_IDX lab;
    New_LABEL (CURRENT_SYMTAB, lab);
    WN *lab_wn = WN_CreateLabel((ST_IDX) 0, lab, 0, NULL);
    WN_INSERT_BlockLast(body_blk, WN_CreateTruebr(lab, cond_wn));

    // handle array element
    adjust_wn = WN_Sub(Pointer_Mtype,
                       WN_Ldid(Pointer_Mtype, tmp_ofst, tmp_st, arg_ptr_ty),
                       WN_Intconst(Pointer_Mtype, TY_size(arg_ty) / num_element));
    wn = WN_Stid(Pointer_Mtype, tmp_ofst, tmp_st, arg_ptr_ty, adjust_wn);
    WN_INSERT_BlockLast(body_blk, wn);
    WN_Set_Linenum(wn, GetSrcPos());

    WN *ldid_wn = WN_Ldid(Pointer_Mtype, tmp_ofst, tmp_st, arg_ptr_ty);
    WN_kid0(WN_kid0(call_wn)) = ldid_wn;
    WN_INSERT_BlockLast(body_blk, call_wn);

    WN_INSERT_BlockLast(blk, lab_wn);
    return blk;
  }
  // setup EH region here, since dtor may be called in _tcf_xx function
  if (emit_exceptions /* && Maybe_do_eh_cleanups()*/)
    call_wn = Setup_eh_region(call_wn, GetSrcPos(), false /* for_unwinding */,
                              true /* is_dtor_or_ctor_call */);
  return call_wn;
}

// Get the handlers for the current try block. Move up in scope and append any
// more handlers that may be present, to INITV.
INITV_IDX
WhirlStmtBuilder::Create_handler_list(int scope_index)
{
  INITV_IDX type_st, prev_type_st=0, start=0;

  Is_True(isa<CXXTryStmt>(scope_cleanup_stack[scope_index].stmt),
                        ("EH Error"));
  for(int i=scope_index; i>=0; i--)
  {
    const Stmt *t = scope_cleanup_stack[i].stmt;
    if(!isa<CXXTryStmt>(t))
      continue;
    HANDLER_ITER iter(cast<CXXTryStmt>(t));
    for(iter.First(); iter.Not_Empty(); iter.Next())
    {
      const CXXCatchStmt *h = iter.Current();

      type_st = New_INITV();
      QualType type = h->getCaughtType();

      ST_IDX st = 0;
      if(type != QualType())
      {
        st = _builder->TB().ConvertRTTIForType(type);
        INITV_Set_VAL(Initv_Table[type_st],
                       Enter_tcon(Host_To_Targ(MTYPE_U4, st)), 1);
      }
      else // catch-all handler
        INITV_Set_ONE(Initv_Table[type_st], MTYPE_U4, 1);

      if (prev_type_st)
        Set_INITV_next(prev_type_st, type_st);
      else
        start = type_st;
      prev_type_st = type_st;

    TYPE_FILTER_ENTRY e;
    e.st = st;
    e.filter = 0; // do not compare based on filter
    vector<TYPE_FILTER_ENTRY>::iterator f =
      find(type_filter_vector.begin(), type_filter_vector.end(), e);
    if (f == type_filter_vector.end())
    {
      e.filter = type_filter_vector.size()+1;
      type_filter_vector.push_back(e);
    }
   }
  }
  return start;
}

LABEL_IDX
New_eh_cleanup_entry(const Stmt *t, DTOR_ST_VECTOR *v, LABEL_IDX goto_idx)
{
  EH_CLEANUP_ENTRY e;

  e.tryhandler = t;
  e.cleanups = v;
  e.goto_idx = goto_idx;
  LABEL_IDX pad;
  New_LABEL(CURRENT_SYMTAB, pad);
  Label_Table[pad].kind = LKIND_BEGIN_HANDLER;
  e.pad = pad;
  New_LABEL(CURRENT_SYMTAB, e.start);
  cleanup_list_for_eh.push_back(e);
  return pad;
}

static bool
manual_unwinding_needed()
{
  Is_True(!processing_handler, ("Cannot be called from inside handler"));

  bool cleanups_seen = false;
  for (int i=scope_cleanup_i; i>=0; i--)
  {
    const Stmt *t = scope_cleanup_stack[i].stmt;
    if (Check_dtor_call) {
      cleanups_seen = true;
      break;
    }
    if (isa<CXXTryStmt>(t))
      Is_True(false, ("manual_unwinding_needed: Cannot reach here"));
  }
  return cleanups_seen;
}

// Zero represents no handler, but possibly an associated landing pad.
static void
Append_cleanup(INITV_IDX& iv)
{
  INITV_IDX tmp = iv;
  while (tmp && INITV_next(tmp))
    tmp = INITV_next(tmp);

  INITV_IDX cleanup = New_INITV();
  INITV_Set_VAL(Initv_Table[cleanup], Enter_tcon(Host_To_Targ(MTYPE_U4, 0)), 1);
  if (tmp)
    Set_INITV_next(tmp, cleanup);
  else
    iv = cleanup;
}

// current: D1 D2 D3, prev: D1 D2 => emit D3 for current, goto prev
// current: D1 D2 D3, prev: D1 D2 D4 => don't optimize now
static bool
Optimize_cleanups(DTOR_ST_VECTOR *current, DTOR_ST_VECTOR *prev)
{
  if (prev->size() >= current->size())
        return false;
  reverse(current->begin(), current->end());
  reverse(prev->begin(), prev->end());
  DTOR_ST_VECTOR::iterator c = current->begin();
  for (DTOR_ST_VECTOR::iterator p = prev->begin(); p != prev->end(); ++p, ++c)
    if (*p != *c)
      return false;
  // all cleanups in prev are in current, so remove them from current
  // first reverse it back
  reverse(current->begin(), current->end());
  reverse(prev->begin(), prev->end());
  for (int i=0; i<prev->size(); ++i)
    current->pop_back();
  return true;
}

LABEL_IDX
WhirlStmtBuilder::Lookup_cleanups(WN *wn, INITV_IDX& iv, bool is_dtor_or_ctor_call) {
  const Stmt *t;
  iv = 0;
  DTOR_ST_VECTOR *cleanups = new DTOR_ST_VECTOR();

  if (scope_cleanup_i == -1)
  {
    iv = New_INITV();
    INITV_Set_ZERO(Initv_Table[iv], MTYPE_U4, 1);
    return 0;
  }

  Expr *temp_cleanup = 0;
  for (int i = temp_cleanup_i; i>=0; --i)
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

  if (scope_cleanup_i != -1 && Check_dtor_call()) {
    DTOR_CALL_STACK may_cleanup = dtor_call_stack;
    bool pop_current_decl = false;
    while (!may_cleanup.empty()) {
      const DTOR_CALL_ITEM& item = may_cleanup.top();
      if (try_stmt)
        if (item.first == try_stmt && item.second.second == ST_IDX_ZERO)
          break;
      if (item.second.second) {
        if (is_dtor_or_ctor_call) {
          if (pop_current_decl)
            cleanups->push_back(item.second);
          else
            pop_current_decl = true;
        } else
          cleanups->push_back(item.second);
      }
      may_cleanup.pop();
    }
  }

  // get cleanups from dtor_for_copy_ctor_stack
  bool has_cleanup = false;
  if (scope_cleanup_i != -1 && Check_dtor_for_copy_ctor()) {
    DTOR_COPY_STACK may_cleanup = dtor_for_copy_ctor_stack;
    while (!may_cleanup.empty()) {
      const DTOR_ST_PAIR& item = may_cleanup.top();
      if (item.second) {
        if (WN_operator(wn) == OPR_CALL && strcmp(ST_name(WN_st(wn)), "__cxa_throw")) {
          cleanups->push_back(item);
          may_cleanup.pop();
          continue;
        }
        if (WN_operator(wn) == OPR_STID && WN_st_idx(wn) == item.second)
          has_cleanup = true;
        else
          if (has_cleanup)
            cleanups->push_back(item);
      }
      may_cleanup.pop();
    }
  }

  const CXXCatchStmt *h = NULL;
  for (scope_index = scope_cleanup_i; scope_index >= 0; scope_index--)
  {
    t = scope_cleanup_stack[scope_index].stmt;
    if (isa<CXXTryStmt>(t))
      break;
    // add cleanup for "__cxa_end_catch"
    if (isa<CXXCatchStmt>(t)) {
      cleanups->push_back(DTOR_ST_PAIR(QualType(), END_CATCH_CLEANUP));
      // handle nested try catch:
      // If the catch stmt is present in another try block,
      // return t as the try stmt, otherwise, return this catch stmt.
      h = cast<CXXCatchStmt>(t);
    }
  }
  if (!isa<CXXTryStmt>(t) && h)
    t = h;

  if (isa<CXXTryStmt>(t) && scope_index >= 0) {
        h = cast<CXXTryStmt>(t)->getHandler(0);
        iv = Create_handler_list(scope_index);
        goto_idx = scope_cleanup_stack[scope_index].cmp_idx;
  }
  else // no enclosing try block
  {
    if (cleanups->empty() && !isa<CXXCatchStmt>(t))
    {
      iv = New_INITV();
      INITV_Set_ZERO(Initv_Table[iv], MTYPE_U4, 1);
      return 0;
    }
  }

  if (!try_block_seen && manual_unwinding_needed())
    Set_PU_needs_manual_unwinding(Get_Current_PU());

  // the following 2 calls can change 'iv'.
  // NOTE: CG expects a zero before eh-spec filter
  if (PU_needs_manual_unwinding(Get_Current_PU()))
  {
     Append_cleanup(iv);
  }

  if (!iv)
  { // not yet assigned
    iv = New_INITV();
    INITV_Set_ZERO(Initv_Table[iv], MTYPE_U4, 1);
  }

  if (cleanup_list_for_eh.empty())
  {
    return New_eh_cleanup_entry(h, cleanups, goto_idx);
  }
  else {
    EH_CLEANUP_ENTRY e = cleanup_list_for_eh.back();

    // check if we are not in any try-block
    if (h == NULL && e.tryhandler == 0 && !processing_handler &&
        cleanups->size() != e.cleanups->size())
    {
      if (Optimize_cleanups(cleanups, e.cleanups))
        return New_eh_cleanup_entry(h, cleanups, e.start);
    }

    if ((h != e.tryhandler) || // different try block
        (cleanups->size() != e.cleanups->size())) // # of cleanups doesn't match
      return New_eh_cleanup_entry(h, cleanups, goto_idx);
    // same tryblock, same # of cleanups
    for (int j=0; j<cleanups->size(); ++j)
      if ((*cleanups)[j] != (*(e.cleanups))[j])
        return New_eh_cleanup_entry (h, cleanups, goto_idx);
      return e.pad;
  }
}

WN *
WhirlStmtBuilder::Setup_eh_region(WN *wn, SRCPOS spos, bool for_unwinding, bool is_dtor_or_ctor_call) {
  Is_True(FindCallWn(wn) != NULL,
          ("EH region should enclose a function call at least"));

  INITV_IDX iv = 0;
  LABEL_IDX pad = 0;

  if (!for_unwinding)
    pad = Lookup_cleanups(wn, iv, is_dtor_or_ctor_call);
  else {
    iv = New_INITV();
    INITV_Set_ZERO(Initv_Table[iv], MTYPE_U4, 1);
  }

  INITV_IDX initv_label = New_INITV();
  if (pad)
    INITV_Init_Label(initv_label, pad, 1);
  else
    INITV_Set_ZERO(Initv_Table[initv_label], MTYPE_U4, 1);

  INITV_IDX blk = New_INITV();
  INITV_Init_Block(blk, initv_label);

  Set_INITV_next(initv_label, iv);

  TY_IDX ty = MTYPE_TO_TY_array[MTYPE_U4];
  ST *ereg = Gen_Temp_Named_Symbol(ty, "dummy1", CLASS_VAR,
                              SCLASS_EH_REGION_SUPP);
  Set_ST_is_initialized(*ereg);
  Set_ST_is_not_used(*ereg);
  INITO_IDX ereg_supp = New_INITO(ST_st_idx(ereg), blk);

  Set_PU_has_region(Get_Current_PU());
  Set_PU_has_exc_scopes(Get_Current_PU());

  WN *region_body = WN_CreateBlock();
  WN_Set_Linenum(wn, spos);
  WN_INSERT_BlockLast(region_body, wn);
  WN_Set_Linenum(region_body, spos);

  WN *region = WN_CreateRegion(REGION_KIND_EH, region_body,
                               WN_CreateBlock(),
                               WN_CreateBlock(),
                               New_Region_Id(), ereg_supp);
  Set_PU_has_region (Get_Current_PU());
  Set_PU_has_exc_scopes (Get_Current_PU());

  return region;
}

void WhirlStmtBuilder::pop_dtor_for_copy_ctor_stack(WN *block, QualType ty) {
  while (!dtor_for_copy_ctor_stack.empty()) {
    QualType type = dtor_for_copy_ctor_stack.top().first;
    ST_IDX copy_st = dtor_for_copy_ctor_stack.top().second;
    dtor_for_copy_ctor_stack.pop();

    const Type *ty1;
    const Type *ty2;
    if (!type.isNull())
      ty1 = type.getCanonicalType().getTypePtr();
    if (!ty.isNull())
      ty2 = ty.getCanonicalType().getTypePtr();
    if (ty1 == ty2 && dtor_for_copy_ctor_stack.empty())
      break;  // don't call destructor for the last one for return stmt

    WN *call_wn = Emit_cxx_destructor_call(type, copy_st);
    Is_True((call_wn != NULL), ("Invalid wn for destructor function"));
    WN_Set_Linenum(call_wn, GetSrcPos());
    WN_INSERT_BlockLast(block, call_wn);
  }
}

// pop_dtor_call_for_return
// generate destructor_call from dtor_call_stack for ReturnStmt
void WhirlStmtBuilder::pop_dtor_call_for_return(WN *block) {
  DTOR_CALL_STACK may_cleanup = dtor_call_stack;

  while (!may_cleanup.empty()) {
    const DTOR_CALL_ITEM &item = may_cleanup.top();
    if (item.second.second) {
      QualType type = item.second.first;
      ST_IDX var_st = item.second.second;
      Is_True(var_st, ("bad var st"));
      WN *call_wn = Emit_cxx_destructor_call(type, var_st);
      Is_True((call_wn != NULL), ("Invalid wn for destructor function"));
      WN_INSERT_BlockAfter(block, WN_last(block), call_wn);
    }
    may_cleanup.pop();
  }
}

// pop_dtor_call_stack
// generate destructor_call from dtor_call_stack, if gen_dtor is TRUE
// otherwise, just pop dtor_call_stack
void WhirlStmtBuilder::pop_dtor_call_stack(WN *block, bool gen_dtor) {
  bool is_region = false;
  WN *orig_blk = block;

  // if no need to generate destructor call, pop VarDecl for current_stmt
  if (!gen_dtor) {
    while (!dtor_call_stack.empty() && dtor_call_stack.top().first == current_stmt) {
      dtor_call_stack.pop();
    }
    return;
  }

  while (dtor_call_stack.top().first == current_stmt && dtor_call_stack.top().second.second != ST_IDX_ZERO) {
    QualType type = dtor_call_stack.top().second.first;
    ST_IDX var_st = dtor_call_stack.top().second.second;
    Is_True(var_st, ("bad var st"));
    WN *call_wn = Emit_cxx_destructor_call(type, var_st);
    Is_True((call_wn != NULL), ("Invalid wn for destructor function"));
    dtor_call_stack.pop();
    WN *last = WN_last(block);

    // insert to call_wn to block
    if (last == NULL) {
      WN_INSERT_BlockLast(block, call_wn);
      continue;
    }

    Is_True(last != NULL, ("block should not be NULL"));
    if (WN_operator(last) == OPR_RETURN ||
        WN_operator(last) == OPR_RETURN_VAL) {
      WN_Set_Linenum(call_wn, GetSrcPos());
      WN_INSERT_BlockBefore(block, last, call_wn);
    } else {
      if (WN_operator(last) == OPR_REGION) {
        is_region = true;
        block = WN_region_body(last);
        last = WN_last(block);
        //continue
      }
      if (WN_operator(last) == OPR_RETURN_VAL) {
        if (WN_operator(WN_kid0(last)) == OPR_INTCONST ||
            WN_operator(WN_kid0(last)) == OPR_LDID) {
          WN_Set_Linenum(call_wn, GetSrcPos());
          WN_INSERT_BlockBefore(block, last, call_wn);
        } else
          Is_True(false, ("unsupported return val"));
      } else {
        if (is_region)
          WN_INSERT_BlockLast(orig_blk, call_wn);
        else
          WN_INSERT_BlockAfter(block, last, call_wn);
      }
    }
  }
  Is_True((dtor_call_stack.top().first == current_stmt &&
           dtor_call_stack.top().second.second == ST_IDX_ZERO),
          ("destructor may be called in wrong scope"));
  dtor_call_stack.pop();
}

void
WhirlStmtBuilder::Pop_scope_and_do_cleanups (void)
{
  Is_True(scope_cleanup_i != -1,
          ("Pop_scope_and_do_cleanups: scope_cleanup-stack is empty"));

  while (scope_cleanup_i != -1) {
    const Stmt* t = scope_cleanup_stack [scope_cleanup_i].stmt;
    if (t == current_stmt || isa<CXXCatchStmt>(t)) {
      --scope_cleanup_i;
      break;
    }
    --scope_cleanup_i;
  }
}

// Setup EH region for wn that contains OPR_CALL/OPR_ICALL
void WhirlStmtBuilder::SetEHRegionForCall(WN *blk, WN *wn) {
  Is_True(wn != NULL, ("invalid wn"));
  Is_True(blk != NULL && WN_operator(blk) == OPR_BLOCK,
          ("invalid block wn"));

  // just return for OPR_REGION
  if (WN_operator(wn) == OPR_REGION) {
    WN_INSERT_BlockLast(blk, wn);
    return;
  }

  if ((WN_operator(wn) == OPR_CALL ||
       WN_operator(wn) == OPR_ICALL) &&
      Check_wn_from_call_region(wn)) {
    WN *call_wn = Setup_eh_region(WN_COPY_Tree(wn), GetSrcPos(),
                                  false/* for_unwinding */);
    WN_INSERT_BlockLast(blk, call_wn);
    return;
  }

  if (WN_operator(wn) == OPR_BLOCK) {
    WN *node = WN_first(wn);
    while (node != NULL)
    {
      WN *next = WN_next(node);
      WN *call_wn = NULL;
      if ((call_wn = FindCallWn(node)) &&
           Check_wn_from_call_region(call_wn)) {
        call_wn = Setup_eh_region(WN_COPY_Tree(node), GetSrcPos(),
                                  false/* for_unwinding */);
        WN_INSERT_BlockLast(blk, call_wn);
      } else
        WN_INSERT_BlockLast(blk, node);
      node = next;
    }
  } else {
    bool need_eh_region = false;
    INT i;
    for (i = 0; i < WN_kid_count(wn); i++) {
      WN *kid = WN_kid(wn, i);
      WN *call_wn = NULL;
      // avoid double set eh region
      if ((call_wn = FindCallWn(kid)) &&
          Check_wn_from_call_region(call_wn)) {
        if (!OPCODE_is_expression(WN_opcode(kid)) ||
            (WN_operator(kid) == OPR_COMMA &&
             !(WN_operator(WN_kid1(kid)) == OPR_LDID &&
               strcmp(ST_name(WN_st(WN_kid1(kid))), ".preg_return_val") == 0))) {
          WN *blk = kid;
          if (WN_operator(kid) == OPR_COMMA) {
            blk = WN_kid0(kid);
            if ((call_wn = FindCallWn(WN_kid1(kid))) &&
                Check_wn_from_call_region(call_wn))
              need_eh_region = true;
          }
          if (WN_operator(blk) == OPR_BLOCK) {
            WN *nw_blk = WN_CreateBlock();
            SetEHRegionForCall(nw_blk, blk);
            blk = nw_blk;
          }
          if (WN_operator(kid) == OPR_COMMA)
            WN_kid0(kid) = blk;
          else
            kid = blk;
          WN_kid(wn, i) = kid;
        } else
          need_eh_region = true;
      }
    }
    if (need_eh_region && FindCallWn(wn))
      wn = Setup_eh_region(WN_COPY_Tree(wn), GetSrcPos(),
                           false/* for_unwinding */);
    WN_INSERT_BlockLast(blk, wn);
  }
}

WN *WhirlStmtBuilder::ConvertCompoundStmt(const CompoundStmt *stmt, ST_IDX st_idx) {
  TRACE_FUNC();
  if (stmt->body_empty())
    return NULL;
  ScopeHelper<const CompoundStmt*> shlp(_builder->Scope(), stmt);
  WN *block = WhirlBlockUtil::nwBlock();
  const CompoundStmt *orig_stmt = current_stmt;
  current_stmt = stmt;
  Push_dtor_call_stack(QualType(), ST_IDX_ZERO);

  Push_scope_cleanup(stmt);

  // get last stmt
  const Stmt *last_stmt = stmt->body_back();
  while (const LabelStmt *label_stmt = dyn_cast<LabelStmt>(last_stmt)) {
    last_stmt = label_stmt->getSubStmt();
  }
  Is_True(last_stmt, ("invalid last stmt for CompoundStmt"));

  const Stmt *handle_stmt = NULL;
  for (CompoundStmt::const_body_iterator it = stmt->body_begin();
       it != stmt->body_end();
       ++it) {
    handle_stmt = *it;
    Mark_call_region(CALL_REGION_STACK_MARK);
    WN *ret = NULL;
    WN *stmt_blk = WhirlBlockUtil::nwBlock();
    if (st_idx != ST_IDX_ZERO && handle_stmt == last_stmt) {
      // if CompoundStmt needs return value, store the last stmt to given st
      WhirlExprBuilder expr_bldr(_builder);
      ret = expr_bldr.ConvertToNode(cast<Expr>(handle_stmt));
      Is_True(ret != NULL, ("invalid return wn"));
      SRCPOS spos = SetSrcPos(getLocation(handle_stmt));
      ST *st = ST_ptr(st_idx);
      Set_ST_Srcpos(*st, spos);
      WN *st_wn = WN_Stid(TY_mtype(ST_type(st)), 0, st, ST_type(st), ret);
      WN_Set_Linenum(st_wn, spos);
      WN_INSERT_BlockLast(stmt_blk, st_wn);
      ret = NULL;
    } else
      ret = ConvertStmt(handle_stmt);

    BOOL need_eh_region = Check_call_region();
    if (ret != NULL) {
      // convert expr to stmt
      if (OPERATOR_is_expression(WN_operator(ret)))
        ret = WN_CreateEval(ret);
      WN_Set_Linenum(ret, SetSrcPos(getLocation(handle_stmt)));

      if (WN_operator(ret) == OPR_BLOCK && WN_first(ret) == NULL) {
        // do not add empty block
        ret = NULL;
      }
      else if (!need_eh_region ||
               (WN_operator(ret) != OPR_RETURN_VAL &&
                WN_operator(ret) != OPR_RETURN)) {
        // if ret is return, don't add to block
        WN_INSERT_BlockLast(stmt_blk, ret);
        ret = NULL;
      }
    }
    WhirlBlockUtil::popCurrentBlock();

    // setup EH region for call
    if (_builder->Lang_CPP() && emit_exceptions && Check_call_region()) {
      WN *node = WN_first(stmt_blk);
      while (node != NULL) {
        WN *next = WN_next(node);
        SetEHRegionForCall(block, node);
        node = next;
      }
    } else
      WN_INSERT_BlockLast(block, stmt_blk);
    Unmark_call_region();

    // add return stmt if it's not added above
    if (ret != NULL) {
      Is_True(need_eh_region &&
              (WN_operator(ret) == OPR_RETURN_VAL ||
               WN_operator(ret) == OPR_RETURN), ("bad ret opr"));
      WN_INSERT_BlockLast(block, ret);
    }

    // destruction for copy constructor if needed
    pop_dtor_for_copy_ctor_stack(block); 
  }
#if 0
  // setup eh region to block
  // in case of exceptions in the called function
  if (_builder->Lang_CPP() && emit_exceptions && FindCallWn(block)) {
    WN *nw_blk = WhirlBlockUtil::nwBlock();
    WN *eh_blk = Setup_eh_region(WN_COPY_Tree(block), GetSrcPos(),
                                 false/* for_unwinding */);
    WN_DELETE_Tree(block);
    block = nw_blk;
    WN_INSERT_BlockLast(block, eh_blk);
    WhirlBlockUtil::popCurrentBlock();
  }
#endif

  pop_dtor_call_stack(block, isa<ReturnStmt>(handle_stmt)? false : true);
  Pop_scope_and_do_cleanups();
  current_stmt = orig_stmt;
  WhirlBlockUtil::popCurrentBlock();
  return block;
}

WN *WhirlStmtBuilder::ConvertContinueStmt(const ContinueStmt *stmt) {
  TRACE_FUNC();
  Is_True(break_continue_info_i >= 0, ("No break/continue info"));
  INT32 i = break_continue_info_i;
  LABEL_IDX label_idx=0;

  // find the enclosing loop
  if (i != -1) {
    while (break_continue_info_stack[i].tree_code == Stmt::SwitchStmtClass)
      --i;
    if (i != -1) {
      label_idx = break_continue_info_stack[i].continue_label_idx;
      if (label_idx == 0) {
        // Control can reach here even while processing an exception handler
        New_LABEL (CURRENT_SYMTAB, label_idx);
        break_continue_info_stack[i].continue_label_idx = label_idx;
      }
    }
  }

  Is_True(label_idx, ("null continue label idx"));
  WN *wn = WN_CreateGoto((ST_IDX)NULL, label_idx);
  WN_Set_Linenum(wn, SetSrcPos(getLocation(stmt)));
  return wn;
}

static void
Set_handler_labels(const CXXTryStmt *stmt)
{
  HANDLER_ITER iter(stmt);
  for (iter.First(); iter.Not_Empty(); iter.Next()) {
    const CXXCatchStmt *handler = iter.Current();
    LABEL_IDX handler_label;
    New_LABEL(CURRENT_SYMTAB, handler_label);
    HANDLER_LABEL(handler) = handler_label;
  }
}

// Called while processing a try-block (from ConvertCXXTryStmt).
// Return in
//      CLEANUPS the set of cleanups that need to be run
//      GOTO_IDX the label to jump to
// if exception is not caught by the handlers of the current try block.
static bool
Get_cleanup_info(DTOR_ST_VECTOR *cleanups, LABEL_IDX *goto_idx)
{
  Is_True(isa<CXXTryStmt>(scope_cleanup_stack[scope_cleanup_i+1].stmt),
          ("EH Processing Error"));

  for (int i=scope_cleanup_i; i>=0; i--)
  {
    DTOR_CALL_STACK may_cleanup = dtor_call_stack;
    if (Check_dtor_call()) {
      while (may_cleanup.top().first == current_stmt &&
             may_cleanup.top().second.second != ST_IDX_ZERO) {
        cleanups->push_back(may_cleanup.top().second);
        may_cleanup.pop();
      }
    }
    if (isa<CXXTryStmt>(scope_cleanup_stack[i].stmt)) {
      *goto_idx = scope_cleanup_stack[i].cmp_idx;
      return false;
    }
  }

  if (!processing_handler)
  {
    *goto_idx = 0;
    return true;
  }
  HANDLER_INFO hi = current_handler;
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
Get_scope_info()
{
  vector<SCOPE_CLEANUP_INFO> *scope = new vector<SCOPE_CLEANUP_INFO>();
  Is_True(isa<CXXTryStmt>(scope_cleanup_stack[scope_cleanup_i].stmt),
          ("Scope Error in Get_scope_info"));
  for (int i=0; i<=scope_cleanup_i; ++i) // Don't include TRY_BLOCK
    scope->push_back(scope_cleanup_stack[i]);
  return scope;
}

static void
Push_handler_info(const CXXTryStmt *handler, DTOR_ST_VECTOR *v,
        vector<SCOPE_CLEANUP_INFO> *scope, vector<TEMP_CLEANUP_INFO> *temp,
        vector<BREAK_CONTINUE_INFO> *break_continue,
        vector<ST_IDX> *handler_list, vector<ST_IDX> *eh_spec,
        LABEL_IDX label_idx, bool outermost, LABEL_IDX cmp_idx[],
        LABEL_IDX goto_idx)
{
   if (++handler_info_i == handler_info_max) {
    handler_info_max = ENLARGE(handler_info_max);
    handler_info_stack =
     (HANDLER_INFO *) realloc(handler_info_stack,
                        handler_info_max * sizeof(HANDLER_INFO));
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
  New_LABEL(CURRENT_SYMTAB, handler_info_stack [handler_info_i].cleanups_idx);
  handler_info_stack [handler_info_i].goto_idx = goto_idx;
  handler_info_stack [handler_info_i].outermost = outermost;
}

void
Emit_end_catch(SRCPOS spos) {
  static ST *cxa_end_catch = Create_function("__cxa_end_catch",
                                              MTYPE_To_TY(MTYPE_V),
                                              TY_IDX_ZERO);
  WN * call_wn = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 0);
  WN_st_idx(call_wn) = ST_st_idx(cxa_end_catch);
  WN_Set_Call_Default_Flags(call_wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), call_wn);
  WN_Set_Linenum(call_wn, spos);
}

void
WhirlStmtBuilder::Emit_cleanup(const QualType type, ST_IDX st_idx, bool need_region) {
  // emit __cxa_end_catch
  if (st_idx == END_CATCH_CLEANUP) {
    Emit_end_catch(GetSrcPos());
    return;
  }

  WN *call_wn = Emit_cxx_destructor_call(type, st_idx);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), call_wn);
}

// for a catch-all clause, pass a typeinfo of ZERO. This typeinfo needs
// to be handled specially. Moreover, we must not pass 0 for any other
// typeinfo.
void
WhirlStmtBuilder::Expand_handlers_or_cleanup(const HANDLER_INFO &handler_info)
{
  const CXXTryStmt *t = handler_info.handler;
  DTOR_ST_VECTOR *cleanups = handler_info.cleanups;
  LABEL_IDX label_idx = handler_info.label_idx;
  LABEL_IDX goto_idx = handler_info.goto_idx;
  LABEL_IDX cleanups_idx = handler_info.cleanups_idx;
  bool outermost = handler_info.outermost;

  HANDLER_ITER iter(t);
  if (emit_exceptions)
  {
    // Generate the compare statements with eh-filter.
    for (iter.First(); iter.Not_Empty(); iter.Next())
    {
      const CXXCatchStmt *t_copy = iter.Current();
      QualType type = t_copy->getCaughtType();

      ST_IDX  sym = 0;
      if (type != QualType())
        sym = _builder->TB().ConvertRTTIForType(type);

      TYPE_FILTER_ENTRY e;
      e.st = sym;
      e.filter = 0; // do not compare based on filter
      vector<TYPE_FILTER_ENTRY>::iterator f =
        find(type_filter_vector.begin(), type_filter_vector.end(), e);
      if (f == type_filter_vector.end())
      {
        e.filter = type_filter_vector.size()+1;
        type_filter_vector.push_back (e);
        if (e.st)
          Build_filter_cmp(e.filter, HANDLER_LABEL(t_copy), false);
        else // catch-all, so do not compare filter
        {
          WN *goto_wn = WN_CreateGoto((ST_IDX) NULL, HANDLER_LABEL(t_copy));
          WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), goto_wn);
          WN_Set_Linenum(goto_wn, SetSrcPos(getLocation(t_copy)));
        }
      }
      else
      {
        if (e.st)
          Build_filter_cmp((*f).filter, HANDLER_LABEL(t_copy), false);
        else // catch-all, so do not compare filter
        {
          WN *goto_wn = WN_CreateGoto((ST_IDX) NULL, HANDLER_LABEL(t_copy));
          WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), goto_wn);
          WN_Set_Linenum(goto_wn, SetSrcPos(getLocation(t_copy)));
        }
      }
    }

    WN *tmp_label_wn = WN_CreateLabel((ST_IDX) 0, cleanups_idx, 0, NULL);
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), tmp_label_wn);
    WN_Set_Linenum(tmp_label_wn, GetSrcPos());
    // Generate any cleanups that need to be executed before going to the outer
    // scope, which would be a handler in the same PU or a call to _Unwind_Resume
    for (DTOR_ST_VECTOR::iterator j=cleanups->begin();
         j!=cleanups->end();++j)
      Emit_cleanup(j->first, j->second, false);

    // generate a call to _Unwind_Resume(struct _Unwind_Exception *)
    if (outermost)
    {
      Is_True(goto_idx == 0, ("Goto label should be 0"));
      Call_unwind(GetSrcPos());
    }
    else {
      WN *goto_wn = WN_CreateGoto((ST_IDX) NULL, goto_idx);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), goto_wn);
      WN_Set_Linenum(goto_wn, GetSrcPos());
    }
  } // emit_exceptions

  // Now, emit the actual exception handler body's.
  for (iter.First(); iter.Not_Empty(); iter.Next()) {
    const CXXCatchStmt *stmt = iter.Current();

    // need a label in front of each handler, so that we can jump to the
    // proper label from 'cmp' above
    WN *label_wn = WN_CreateLabel ((ST_IDX) 0, HANDLER_LABEL(stmt), 0, NULL);
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), label_wn);
    WN_Set_Linenum(label_wn, GetSrcPos());

    Push_scope_cleanup(stmt);
    ConvertCXXCatchStmt(stmt);
    Pop_scope_and_do_cleanups();

    WN *goto_wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), goto_wn);
    WN_Set_Linenum(goto_wn, SetSrcPos(getLocation(stmt)));
  }
}

// Emit all cleanups, and emit a goto after each set of cleanups to the handler.
// FROM == 0, implies, process the entire list. Else, skip FROM cleanups
// from the start of the list.
void
WhirlStmtBuilder::Do_cleanups_for_eh(INT from)
{
  std::list<EH_CLEANUP_ENTRY>::iterator start = cleanup_list_for_eh.begin();

  for (INT i=0; i<from; i++)
    start++;

  for (std::list<EH_CLEANUP_ENTRY>::iterator i = start;
                i != cleanup_list_for_eh.end(); ++i) {
    EH_CLEANUP_ENTRY e = *i;

    WN *pad_wn = WN_CreateLabel((ST_IDX) 0, e.pad, 0, NULL);
    WN_Set_Label_Is_Handler_Begin(pad_wn);
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), pad_wn);
    WN_Set_Linenum(pad_wn, GetSrcPos());

    WN *start_wn = WN_CreateLabel((ST_IDX) 0, e.start, 0, NULL);
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), start_wn);
    WN_Set_Linenum(start_wn, GetSrcPos());

    for (DTOR_ST_VECTOR::iterator j=e.cleanups->begin();
                j!=e.cleanups->end();++j)
      Emit_cleanup(j->first, j->second, true);

    if (e.goto_idx) {
      WN *goto_wn = WN_CreateGoto((ST_IDX) 0, e.goto_idx);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), goto_wn);
      WN_Set_Linenum(goto_wn, GetSrcPos());
    }
    else {
      Call_unwind(GetSrcPos());
    }
  }

  if (from)
    cleanup_list_for_eh.erase(start, cleanup_list_for_eh.end());
  else
    cleanup_list_for_eh.clear();
}


void
WhirlStmtBuilder::Do_handlers(INT cleanups)
{
  if (emit_exceptions)
    processing_handler = true;

  while (handler_info_i != -1) {
    LABEL_IDX start_handlers = handler_info_stack[handler_info_i].cmp_idx[1];
    // Check if we need to mark this label as LABEL_addr_saved
    // Set handler_begin if there is no other entry point for this try-block
    if (start_handlers) {
      Is_True(LABEL_kind(Label_Table[start_handlers]) ==
              LKIND_BEGIN_HANDLER, ("Wrong label kind, expecting handler_begin"));
      WN *cmp_wn = WN_CreateLabel((ST_IDX)0, start_handlers, 0, NULL);
      WN_Set_Label_Is_Handler_Begin(cmp_wn);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), cmp_wn);
      WN_Set_Linenum(cmp_wn, GetSrcPos());
    }

    WN *actual_cmp = WN_CreateLabel((ST_IDX) 0,
                       handler_info_stack[handler_info_i].cmp_idx[0], 0, NULL);
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), actual_cmp);
    WN_Set_Linenum(actual_cmp, GetSrcPos());
    current_handler = handler_info_stack[handler_info_i];

    --handler_info_i;
    Expand_handlers_or_cleanup(handler_info_stack[handler_info_i+1]);
  }

  processing_handler = false;
  Do_cleanups_for_eh(cleanups);
  if (emit_exceptions)
    Is_True(cleanup_list_for_eh.size() == cleanups,
            ("EH Cleanup list not completely processed"));
}

void
WhirlStmtBuilder::Call_unwind(SRCPOS spos) {
  // generate unwind resume
  ST_IDX exc_ptr = TCON_uval(INITV_tc_val(INITO_val(PU_misc_info(Get_Current_PU()))));
  WN* parm_node = WN_Ldid(Pointer_Mtype, 0, ST_ptr(exc_ptr), ST_type(exc_ptr));
  TY_IDX idx = Make_Pointer_Type(ST_type(exc_ptr));
  WN *arg0 = WN_CreateParm(Pointer_Mtype, parm_node, idx, WN_PARM_BY_VALUE);

  static ST *unwind_resume = Create_function("_Unwind_Resume",
                                             MTYPE_To_TY(MTYPE_V),
                                             TY_IDX_ZERO);
  WN * call_wn = WN_Create(OPR_CALL, Pointer_Mtype, MTYPE_V, 1);
  WN_kid0(call_wn) = arg0;
  WN_st_idx(call_wn) = ST_st_idx(unwind_resume);
  WN_Set_Call_Never_Return(call_wn);
  WN_Set_Linenum(call_wn, spos);

  WN *region = call_wn;
  if (emit_exceptions)
    region = Setup_eh_region(call_wn, spos, true/* for_unwinding */);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), region);
}


// generate begin catch call wn
WN *
Call_begin_catch(SRCPOS spos) {
  ST_IDX exc_ptr = TCON_uval(INITV_tc_val(INITO_val(PU_misc_info(Get_Current_PU()))));
  WN* parm_node = WN_Ldid(Pointer_Mtype, 0, ST_ptr(exc_ptr), ST_type(exc_ptr));
  TY_IDX idx = Make_Pointer_Type(ST_type(exc_ptr));
  WN * arg0 = WN_CreateParm(Pointer_Mtype, parm_node, idx, WN_PARM_BY_VALUE);

  static ST *cxa_begin_catch = Create_function("__cxa_begin_catch",
                                               Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)),
                                               Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)),
                                               TY_IDX_ZERO);
  WN * call_wn = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 1);
  WN_kid0(call_wn) = arg0;
  WN_st_idx(call_wn) = ST_st_idx(cxa_begin_catch);
  WN_Set_Call_Default_Flags(call_wn);
  WN_Set_Linenum(call_wn, spos);

  return call_wn;
}

void
WhirlStmtBuilder::Emit_begin_catch(const CXXCatchStmt *stmt) {
  VarDecl *CatchParam = stmt->getExceptionDecl();
  if (!CatchParam) {
    WN *wn = Call_begin_catch(GetSrcPos());
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
    return;
  }

  Is_True((stmt->getCaughtType()) != QualType(), ("invalid CaughtType"));
  QualType caught_ty = stmt->getCaughtType();
  TY_IDX catch_ty = _builder->TB().ConvertType(caught_ty);
  TY_IDX ret_ty = catch_ty;
  bool need_iload = false;
  if (!caught_ty->isReferenceType() && !caught_ty->isPointerType()) {
    ret_ty = Make_Pointer_Type(catch_ty);
    need_iload = true;
  }
  if (caught_ty->isRecordType()) {
    const CXXRecordDecl *record_decl = caught_ty->getAsCXXRecordDecl();
    if (record_decl && !record_decl->hasTrivialDestructor()) {
      ST_IDX var_st = _builder->SB().ConvertSymbol(CatchParam);
      Push_dtor_for_copy_ctor(caught_ty, var_st);
    }
  }

  SRCPOS spos = SetSrcPos(getLocation(stmt));
  WN *blk = WN_CreateBlock();
  WN *wn = Call_begin_catch(spos);
  WN_set_rtype(wn, TY_mtype(ret_ty));
  WN_Set_Linenum(blk, spos);
  WN_INSERT_BlockLast(blk, wn);
  WN *ret = WN_Ldid(TY_mtype(ret_ty), -1, Return_Val_Preg, ret_ty);
  WN *comma = WGEN_CreateComma(WN_rtype(ret), blk, ret);

  WN *st_wn = WGEN_StidTemp(ret_ty, comma, ".anon.");
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
  WN_Set_Linenum(st_wn, spos);

  WN *ldid = WN_Ldid(TY_mtype(ret_ty), WN_offset(st_wn), WN_st(st_wn), ret_ty);
  if (need_iload)
    ldid = WN_Iload(TY_mtype(catch_ty), 0, catch_ty, ldid);
  ST_IDX parm_st = _builder->SB().ConvertSymbol(CatchParam);
  st_wn = WN_Stid(TY_mtype(catch_ty), 0, ST_ptr(parm_st), catch_ty, ldid);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
  WN_Set_Linenum(st_wn, spos);
}

void
WhirlStmtBuilder::Build_filter_cmp(int filter, LABEL_IDX label, bool is_throw = false) {
  WN *cmp_insert_pos;

  // generate compare
  // if the exception is to throw upper stack, no need compare exception type
  if (!is_throw) {
    ST_IDX filter_param = TCON_uval(INITV_tc_val(INITV_next(INITO_val(PU_misc_info(Get_Current_PU())))));
    const TYPE_ID mtype = MTYPE_U8;

    WN * wn_ldid = WN_Ldid(mtype, 0, &St_Table[filter_param], MTYPE_TO_TY_array[mtype]);
    LABEL_IDX real_handler_idx = label;
    WN * goto_wn = WN_CreateGoto(real_handler_idx);
    WN_Set_Linenum(goto_wn, GetSrcPos());
    WN_next(goto_wn) = WN_prev(goto_wn) = NULL;

    WN * if_then = WN_CreateBlock();
    WN_first(if_then) = WN_last(if_then) = goto_wn;

    WN * if_else = WN_CreateBlock();
    WN * cmp_value = WN_Intconst(mtype, filter); // TODO: filter
    WN * cond = WN_Create(OPR_EQ, WN_rtype(wn_ldid), mtype, 2);
    WN_kid0(cond) = wn_ldid;
    WN_kid1(cond) = cmp_value;

    WN * if_blk = WN_CreateIf(cond, if_then, if_else);
    cmp_insert_pos = if_blk;
  }
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), cmp_insert_pos);
  WN_Set_Linenum(cmp_insert_pos, GetSrcPos());
}

WN *WhirlStmtBuilder::ConvertCXXCatchStmt(const CXXCatchStmt *stmt) {
  TRACE_FUNC();

  Emit_begin_catch(stmt);

  in_catch_stmt = TRUE;
  WN *ret = ConvertStmt(stmt->getHandlerBlock());
  in_catch_stmt = FALSE;
  if (ret)
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);

  Emit_end_catch(SetSrcPos(getLocation(stmt)));
  return NULL;
}

WN *WhirlStmtBuilder::ConvertCXXForRangeStmt(const CXXForRangeStmt *stmt) {
  TRACE_FUNC();
  // convert init, range stmt, begin stmt and end stmt
  //if (stmt->getInit())
  //  ConvertStmt(stmt->getInit());
  ConvertStmt(stmt->getRangeStmt());
  ConvertStmt(stmt->getBeginStmt());
  ConvertStmt(stmt->getEndStmt());

  LABEL_IDX break_label = LABEL_IDX_ZERO;
  LABEL_IDX cont_label = LABEL_IDX_ZERO;
  Record_Loop_Switch(stmt->getStmtClass(), break_label, cont_label);


  WN *cond;
  {
    WhirlExprBuilder expr_bldr(_builder);
    cond = expr_bldr.ConvertToNode(stmt->getCond());
  }
  Is_True(cond != NULL && OPERATOR_is_expression(WN_operator(cond)),
          ("wrong cond"));
  cond = Handle_cond_wn(cond);

  // handle body
  WN *body = WhirlBlockUtil::nwBlock();
  ConvertStmt(stmt->getLoopVarStmt());
  const Stmt *body_stmt = stmt->getBody();
  ConvertStmt(body_stmt);

  // pop loop info and append continue label
  Pop_Loop_Switch(stmt->getStmtClass(), break_label, cont_label);
  if (cont_label)
    Append_label(cont_label, SetSrcPos(getEndLocation(body_stmt)));

  // here is a break
  ConvertStmt(stmt->getInc());
  WhirlBlockUtil::popCurrentBlock(); // pop body

  // create loop and append break label
  WN *loop = WN_CreateWhileDo(cond, body);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), loop);
  WN_Set_Linenum(loop, SetSrcPos(getLocation(stmt)));
  if (break_label)
    Append_label(break_label, SetSrcPos(getEndLocation(stmt)));

  return NULL;
}

WN *WhirlStmtBuilder::ConvertCXXThrowExpr(const CXXThrowExpr *stmt) {
  WhirlExprBuilder expr_bldr(_builder);
  Result r = expr_bldr.ConvertExpr(stmt);
  if (r.isNone())
    return NULL;
  Is_True(r.isNode(), ("throw expr not return whirl node"));
  Is_True(OPERATOR_is_stmt(WN_operator(r.Node())), ("node is not stmt"));
  return r.Node();
}

void Check_for_loop_label()
{
  int i = break_continue_info_i;

  if (i != -1) {
    if (!break_continue_info_stack[i].break_label_idx)
      New_LABEL(CURRENT_SYMTAB, break_continue_info_stack[i].break_label_idx);

    while (break_continue_info_stack [i].tree_code == Stmt::SwitchStmtClass) --i;
      if (i != -1) {
        LABEL_IDX label_idx = break_continue_info_stack [i].continue_label_idx;
        if (label_idx == 0) {
          New_LABEL(CURRENT_SYMTAB, label_idx);
          break_continue_info_stack [i].continue_label_idx = label_idx;
        }
      }
  }
}

static vector<BREAK_CONTINUE_INFO> *
Get_break_continue_info()
{
  vector<BREAK_CONTINUE_INFO> *info = new vector<BREAK_CONTINUE_INFO>();

  Check_for_loop_label();
  if (processing_handler)
  {
    HANDLER_INFO hi = current_handler;
    if (hi.break_continue)
      for (vector<BREAK_CONTINUE_INFO>::iterator i = hi.break_continue->begin();
           i != hi.break_continue->end(); ++i)
        info->push_back(*i);
  }
  Is_True(isa<CXXTryStmt>(scope_cleanup_stack[scope_cleanup_i].stmt),
          ("Scope Error in Get_break_continue_info"));
  for (int i=0; i<=break_continue_info_i; ++i)
       info->push_back(break_continue_info_stack[i]);
  return info;
}

WN *WhirlStmtBuilder::ConvertCXXTryStmt(const CXXTryStmt *stmt) {
  TRACE_FUNC();

  LABEL_IDX end_label_idx;
  WN *      end_label_wn;

  // Don't generate anything if there are no statements
  // in the try-block
  if (stmt->getTryBlock()->body_empty())
    return NULL;

  if (!try_block_seen)
  {
    if (manual_unwinding_needed())
      Set_PU_needs_manual_unwinding (Get_Current_PU());
    try_block_seen = true;
  }

  // Set start labels for each handler
  Set_handler_labels(stmt);
  Push_scope_cleanup(stmt);

  vector<SCOPE_CLEANUP_INFO> *scope_cleanup = Get_scope_info ();
  vector<TEMP_CLEANUP_INFO> *temp_cleanup = 0;
  vector<BREAK_CONTINUE_INFO> *break_continue = Get_break_continue_info();
  int handler_count = 0;

  // eh region body for try block
  WN * region_body = NULL;
  if (emit_exceptions) {
    region_body = WN_CreateBlock();
    handler_count = cleanup_list_for_eh.size();
  }

  try_stmt = stmt->getTryBlock();
  // generate code for the try-block
  WN* ret = ConvertCompoundStmt(stmt->getTryBlock());
  try_stmt = NULL;

  if (region_body) {
    WN_INSERT_BlockLast(region_body, ret);
    WN_Set_Linenum(region_body, SetSrcPos(getLocation(stmt->getTryBlock())));
  } else
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);
  --scope_cleanup_i;

  LABEL_IDX start = 0;

  if (emit_exceptions) {
    WN * region_pragmas = WN_CreateBlock();
    Is_True(cleanup_list_for_eh.size() >= handler_count,
            ("cleanups cannot be removed here"));
    LABEL_IDX cmp_idx = scope_cleanup_stack[scope_cleanup_i+1].cmp_idx;
    if (cleanup_list_for_eh.size() > handler_count) {
      std::list<EH_CLEANUP_ENTRY>::iterator iter = cleanup_list_for_eh.begin();
      for (int incr=0; incr<handler_count; ++incr)
        ++iter;
      for (; iter != cleanup_list_for_eh.end(); ++iter) {
        EH_CLEANUP_ENTRY entry = *iter;
        WN *goto_wn = WN_CreateGoto(entry.pad);
        WN_Set_Linenum(goto_wn, GetSrcPos());
        WN_INSERT_BlockLast(region_pragmas, goto_wn);
      }
    } else {
      // use a new label to mark handler-begin, don't use
      // the existing cmp_idx since we may have goto to it.
      // i.e. generate
      // LABEL L1 2
      // LABEL L2
        New_LABEL(CURRENT_SYMTAB, start);
        Set_LABEL_KIND(Label_Table[start], LKIND_BEGIN_HANDLER);
        WN *goto_wn = WN_CreateGoto(start);
        WN_Set_Linenum(goto_wn, GetSrcPos());
        WN_INSERT_BlockLast(region_pragmas, goto_wn);
    }

    // insert the label to go to for an inlined callee
    // This inito is not being used right now.
    TY_IDX ty = MTYPE_TO_TY_array[MTYPE_U4];
    ST * ereg = Gen_Temp_Named_Symbol(ty, "try_label", CLASS_VAR,
                                SCLASS_EH_REGION_SUPP);
    Set_ST_is_initialized(*ereg);
    Set_ST_is_not_used(*ereg);

    INITV_IDX try_label = New_INITV();
    INITV_Init_Label(try_label, cmp_idx, 1);
    INITO_IDX ereg_supp = New_INITO(ST_st_idx(ereg), try_label);

    WN *region = WN_CreateRegion(REGION_KIND_TRY, region_body,
                                  region_pragmas, WN_CreateBlock(),
                                  New_Region_Id(), ereg_supp);
    WN_Set_Linenum(region, GetSrcPos());
    Set_PU_has_region(Get_Current_PU());
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), region);
  }

  DTOR_ST_VECTOR *cleanups = new DTOR_ST_VECTOR();
  LABEL_IDX cmp_idxs[2];
  cmp_idxs[0] = scope_cleanup_stack[scope_cleanup_i+1].cmp_idx;
  cmp_idxs[1] = start;
  LABEL_IDX goto_idx=0;
  bool outermost = 0;
  if (emit_exceptions)
    outermost = Get_cleanup_info(cleanups, &goto_idx);
  vector<ST_IDX> *handler_list = new vector<ST_IDX>();
  vector<ST_IDX> *eh_spec_list = NULL;

  // Generate a label for the handlers to branch back to.
  New_LABEL(CURRENT_SYMTAB, end_label_idx);

  // Jump to code after the try block, in case no exception was thrown.
  WN *goto_end_wn = WN_CreateGoto((ST_IDX) NULL, end_label_idx);
  WN_Set_Linenum(goto_end_wn, GetSrcPos());
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), goto_end_wn);

  Push_handler_info(stmt, cleanups, scope_cleanup, temp_cleanup,
                    break_continue, handler_list, eh_spec_list,
                    end_label_idx, outermost, cmp_idxs, goto_idx);
  Do_handlers(handler_count); // pass the # of cleanups to skip from start
  Append_label(end_label_idx, SetSrcPos(getEndLocation(stmt)));
  return NULL;
}

// Initialize VarDecl
WN *WhirlStmtBuilder::Init_var_decl(const VarDecl *decl) {
  WN *ret = NULL;
  const Expr *init = decl->getInit();
  BOOL isStaticLocal = decl->isStaticLocal();
  SRCPOS spos = _builder->SetSrcPos(decl->getLocation());

  if (const LambdaExpr *lambda = WhirlLambdaHelper::GetLambdaExpr(decl)) {
    _builder->LambdaHelper().AddLambdaVar(lambda, decl);
    // skip init for empty cpature list
    if (lambda->capture_size() > 0) {
      ST_IDX idx = _builder->Get_var_st(decl);
      Is_True(idx, ("bad var st"));
      Result target = Result::nwSym(idx, ST_type(idx));
      WhirlExprBuilder expr_bldr(_builder);
      Result r = expr_bldr.ConvertExpr(init, target, FALSE);
      Is_True(r.isSym() && r.Sym() == idx,
              ("r is not handled here"));
    }
    return NULL;
  }

  // get init expression for ExprWithCleanups
  if (ExprWithCleanups::classof(init)) {
    auto init_expr = ((ExprWithCleanups *)(init))->getSubExpr();
    if (CXXConstructExpr::classof(init_expr))
      init = init_expr;
    if (InitListExpr::classof(init_expr))
      init = init_expr;
  }

  WN* init_blk = isStaticLocal ? WhirlBlockUtil::nwBlock()
                               : WhirlBlockUtil::getCurrentBlock();
  ST_IDX idx = _builder->SB().ConvertSymbol(decl);
  if (isa<CXXConstructExpr>(init)) {
    const CXXConstructExpr *expr = dyn_cast<CXXConstructExpr>(init);
    const CXXDestructorDecl *dtor = NULL;
    if (!expr->getConstructor()->getParent()->hasTrivialDestructor())
      dtor = expr->getConstructor()->getParent()->getDestructor();

    // Initialize CXXConstructExpr
    if (_builder->DeclBuilder().GetNonTrivialInitializer(expr)) {
      ret = Emit_cxx_constructor_call(decl);
    } else {
      // push dtor for the decl which is trivial initializer
      if (!decl->isStaticLocal() && dtor != NULL)
        Push_dtor_call_stack(decl->getType(), idx);
    }
    // handle static local var
    if (decl->isStaticLocal()) {
      WN *ctor = ret;
      // guard init str
      llvm::StringRef name;
      STR_IDX str_idx = _builder->DeclBuilder().ConvertName(decl, name,
                                                            /*is_guard*/true);
      ST_IDX dtor_st = (ST_IDX)0;
      if (dtor != NULL) {
        GlobalDecl gd(dtor, CXXDtorType::Dtor_Complete);
        dtor_st = _builder->Get_func_st(gd);
        Is_True(dtor_st, ("bad dtor st"));
      }

      WhirlBlockUtil::popCurrentBlock();
      if (ctor != NULL)
        WN_INSERT_BlockLast(init_blk, ctor);

      ret = Emit_guarded_init(str_idx, init_blk, dtor_st, spos);
    } else {
      // handle ldid whirle node here
      if (ret && (!OPCODE_is_stmt(WN_opcode(ret)) || OPCODE_is_scf(WN_opcode(ret)))) {
        Is_True(WN_operator(ret) == OPR_LDID, ("unsupported return wn"));
        ST *st = ST_ptr(idx);
        TY_IDX ty_idx = ST_type(st);
        ret = WN_Stid(TY_mtype(ty_idx), 0, st, ty_idx, ret, 0);
      }
    }
  } /* else if (decl->isStaticLocal()) {
    // skip for static local var
    return NULL;
  } */
  else {
    TY_IDX ty_idx = _builder->TB().ConvertType(decl->getType());
    ST *st = ST_ptr(idx);
    // already initialized by INITO
    if (isStaticLocal && ST_is_initialized(st)) {
      WhirlBlockUtil::popCurrentBlock();
      return NULL;
    }

    // just return if set of initializers for InitListExpr is zero
    if (TY_size(ty_idx) == 0 && isa<InitListExpr>(init) &&
        !cast<InitListExpr>(init)->getNumInits())
      return NULL;

    Result tgt_r = Result::nwSym(idx, ty_idx);
    if (decl->getType()->isReferenceType())
      tgt_r.SetRef();
    WhirlExprBuilder expr_bldr(_builder);
    Result r = expr_bldr.ConvertExpr(init, tgt_r, TRUE);
    Is_True(r.isNone() || r.isSym() || r.isNode() || r.isTCon() || r.isIntConst(),
            ("Invalid wn for initialize"));

    // handle return result
    if (!r.isNone()) {
      if (r.isSym() && r.Sym() == idx && !isa<InitListExpr>(init)) {
        if (isStaticLocal) {
          WhirlBlockUtil::popCurrentBlock();
          if (!WN_block_empty(init_blk)) {
            llvm::StringRef name;
            STR_IDX str_idx = _builder->DeclBuilder().ConvertName(decl, name, true);
            ret = Emit_guarded_init(str_idx, init_blk, ST_IDX_ZERO, spos);
            WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);
          }
        }
        return NULL;  // Done, return NULL
      }

      if (r.isNode() && OPERATOR_is_stmt(WN_operator(r.Node()))) {
        if (isStaticLocal) {
          WhirlBlockUtil::popCurrentBlock();
          WN_INSERT_BlockLast(init_blk, r.Node());
          llvm::StringRef name;
          STR_IDX str_idx = _builder->DeclBuilder().ConvertName(decl, name, true);
          ret = Emit_guarded_init(str_idx, init_blk, ST_IDX_ZERO, spos);
          return ret;
        }
        return r.Node();
      }

      WN *init_wn = r.isSym() &&
                    (TY_kind(ty_idx) == KIND_ARRAY || r.IsRef())
                        ? WN_Ldid(TY_mtype(ty_idx), 0, r.Sym(), ty_idx)
                        : r.GetRValue();

      if (isa<StringLiteral>(init) && TY_kind(ty_idx) == KIND_ARRAY) {
        WN *addr_wn = WN_Lda(Pointer_Mtype, 0, st);
        Set_ST_addr_saved(st);
        UINT len = cast<StringLiteral>(init)->getByteLength();
        GenMstoreForString(addr_wn, init_wn, ty_idx, len, 0, GetSrcPos());
      } else if (r.isSym() && decl->getType()->isMemberPointerType()) {
        // initialize for MemberPointer
        Is_True(TY_kind(ty_idx) == KIND_STRUCT,
                ("Invalid type for MemberPointerType"));
        ret = WN_Stid(Pointer_Mtype, 0, st, ty_idx, init_wn, 1);
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);

        UINT fld_id = 0;
        UINT64 ofst = 0;
        FLD_HANDLE fh = get_fld_and_offset(ty_idx, 2, fld_id, ofst);
        Is_True(!fh.Is_Null(), ("not find the field"));
        TYPE_ID fld_mty = TY_mtype(FLD_type(fh));
        ret = WN_Stid(fld_mty, ofst, st, ty_idx,
                      WN_Intconst(fld_mty, 0), 2);
      } else {
        TYPE_ID mtype = TY_mtype(ty_idx);
        // fix up init_wn: generate a lda to pass the address
        if (decl->getType()->isReferenceType() &&
            !init->getType()->isReferenceType()) {
          if (WN_operator(init_wn) == OPR_ILOAD) {
            init_wn = WN_kid0(init_wn);
          } else if (WN_operator(init_wn) == OPR_LDID) {
            ST *init_st = WN_st(init_wn);
            WN_DELETE_Tree(init_wn);
            init_wn = WN_Lda(Pointer_Mtype, 0, init_st, 0);
            Set_ST_addr_saved(init_st);
          }
        }

        ret = WN_Stid(mtype, 0, st, ty_idx, init_wn, 0);
      }
      if (ret)
        WN_Set_Linenum(ret, GetSrcPos());
    } // !r.isNone()

    // handle CXXConstructExpr init in InitListExpr after create INITO
    if (isa<InitListExpr>(init)) {
      if (ret != NULL) {
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);
        ret = NULL;
      }

      const InitListExpr *expr = dyn_cast<InitListExpr>(init);

      // push dtor for constant array
      bool is_constant_array = false;
      int element = 0;
      const CXXRecordDecl *cxx_record =
        _builder->DeclBuilder().ConvertConstantArrayType(decl->getType(), element);
      if (cxx_record && !cxx_record->hasTrivialDestructor()) {
        Push_dtor_call_stack(decl->getType(), idx);
      }
      if (!expr->getType()->isRecordType() || !is_constant_array) {
        if (isStaticLocal) {
          WhirlBlockUtil::popCurrentBlock();
          if (ret != NULL)
            WN_INSERT_BlockLast(init_blk, ret);
          llvm::StringRef name;
          STR_IDX str_idx = _builder->DeclBuilder().ConvertName(decl, name, true);
          ret = Emit_guarded_init(str_idx, init_blk, ST_IDX_ZERO, spos);
          WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);
        }
        return NULL;
      }

      RecordDecl *record = init->getType()->getAs<RecordType>()->getDecl();
      UINT field_num = 0;
      UINT elem_num = 0;
      FLD_HANDLE fld = TY_fld(ty_idx);

      for (RecordDecl::field_iterator field = record->field_begin(),
        fieldEnd = record->field_end(); field != fieldEnd;
        ++field, ++field_num) {
        if (record->isUnion() &&
            expr->getInitializedFieldInUnion() != *field) {
            fld = FLD_next(fld);
            continue;
        }
        if (field->isUnnamedBitfield()) {
          fld = FLD_next(fld);
          continue;
        }

        Is_True(elem_num < expr->getNumInits(),
                ("initializer access out of range"));
        const Expr *init = expr->getInit(elem_num++);

        if (!isa<CXXConstructExpr>(init)) continue;

        WhirlExprBuilder expr_bldr(_builder);
        WN *init_ret = expr_bldr.ConvertToNode(init);

        Is_True(init_ret != NULL, ("invalid init_return type"));
        const clang::CXXConstructorDecl *ctor_decl =
            dyn_cast<CXXConstructExpr>(init)->getConstructor();

        UINT32 cur_field_id = 0;
        UINT64 ofst = 0;
        FLD_HANDLE fld = get_fld_and_offset(ty_idx, field_num + 1,
                                            cur_field_id, ofst);
        Is_True(!fld.Is_Null(), ("not find the field"));
        if (ctor_decl->isTrivial()) {
          // no constructor function
          WN *stid = WN_Stid(MTYPE_M, ofst, st, ty_idx,
                            init_ret, field_num + 1);
          WN_Set_Linenum(stid, GetSrcPos());
          WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
        } else {
          // create parm 0 for constructor function call
          WN *lda = WN_Lda(Pointer_Mtype, ofst,
                           st, field_num + 1);
          Set_ST_addr_saved(st);
          WN *ctor_call = WN_last(WhirlBlockUtil::getCurrentBlock());
          Is_True(WN_operator(ctor_call) == OPR_CALL,
                  ("should be OPR_CALL here"));
          WN_kid(ctor_call, 0) = WGEN_CreateParm(Pointer_Mtype,
                                                 lda, WN_ty(lda));
        }

        fld = FLD_next(fld);
        continue;
      }
    } // InitListExpr

    if (isStaticLocal) {
      WhirlBlockUtil::popCurrentBlock();
      if (ret != NULL)
        WN_INSERT_BlockLast(init_blk, ret);
      llvm::StringRef name;
      STR_IDX str_idx = _builder->DeclBuilder().ConvertName(decl, name, true);
      ret = Emit_guarded_init(str_idx, init_blk, ST_IDX_ZERO, spos);
    }
  }

  return ret;
}

WN *WhirlStmtBuilder::ConvertDeclStmt(const DeclStmt *stmt) {
  TRACE_FUNC();
  WN *block = WhirlBlockUtil::nwBlock();
  for (const auto *decl : stmt->decls()) {
    WhirlDeclBuilder decl_builder(_builder);
    decl_builder.ConvertDecl(const_cast<Decl *>(decl));

    WN *ret = NULL;
    if (isa<VarDecl>(decl)) {
      const VarDecl *var_decl = cast<VarDecl>(decl);
      if (var_decl->hasInit())
        ret = Init_var_decl(var_decl);
    }
    if (ret != NULL)
      WN_INSERT_BlockLast(block, ret);
  }
  WhirlBlockUtil::popCurrentBlock();
  return block;
}

LABEL_IDX
WhirlStmtBuilder::Get_label_idx(const Stmt* stmt)
{
  LABEL_IDX label_idx;
  STMT_LABEL_IDX_MAP::iterator it = _stmt_label_map.find(stmt);
  if (it != _stmt_label_map.end())
    label_idx =  it->second;
  else {
    New_LABEL(CURRENT_SYMTAB, label_idx);
    _stmt_label_map[stmt] = label_idx;
  }
  Is_True(label_idx != 0, ("label idx is NULL"));
  return label_idx;
}

template<typename _T>
WN *WhirlStmtBuilder::ConvertLoopStmt(const _T *stmt) {
  Is_True(stmt->getStmtClass() == Stmt::DoStmtClass ||
          stmt->getStmtClass() == Stmt::ForStmtClass ||
          stmt->getStmtClass() == Stmt::WhileStmtClass, ("bad stmt class"));

  // handle init for for-loop
  if (stmt->getStmtClass() == Stmt::ForStmtClass) {
    const ForStmt *for_stmt = cast<ForStmt>(stmt);
    if (for_stmt->getInit()) {
      WN *init_wn = ConvertStmt(for_stmt->getInit());
      if (init_wn) {
        Is_True(!OPERATOR_is_expression(WN_operator(init_wn)),
                ("bad init wn"));
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), init_wn);
      }
    }
  }

  // handle cond
  WN *cond_wn = NULL;
  if (stmt->getCond()) {
    WN *cond_block = WhirlBlockUtil::nwBlock();
    WhirlExprBuilder expr_bldr(_builder);
    cond_wn = expr_bldr.ConvertToNode(stmt->getCond());
    Is_True(cond_wn != NULL, ("Child 0 of IF can not be NULL!"));
    WhirlBlockUtil::popCurrentBlock();
    if (!WN_block_empty(cond_block)) {
      SRCPOS spos = SetSrcPos(getLocation(stmt->getCond()));
      WN_Set_Linenum(cond_block, spos);
      cond_wn = WGEN_CreateComma(WN_rtype(cond_wn), cond_block, cond_wn);
    }
    cond_wn = Handle_cond_wn(cond_wn);
  }
  else {
    // always true cond
    cond_wn = WN_Intconst(MTYPE_I4, 1);
  }

  // create pseudo label index for break and continue
  LABEL_IDX break_label = (LABEL_IDX)0;
  LABEL_IDX continue_label = (LABEL_IDX)0;
  Record_Loop_Switch(stmt->getStmtClass(), break_label, continue_label);

  // handle body
  const Stmt *body_stmt = stmt->getBody();
  WN *body_block = WhirlBlockUtil::nwBlock();
  if (body_stmt) {
    WN *body_wn = ConvertStmt(body_stmt);
    if (body_wn != NULL) {
      Is_True(!OPERATOR_is_expression(WN_operator(body_wn)), ("bad stmt"));
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), body_wn);
    }
  }

  // handle inc for for-loop
  WN *inc_block = NULL;
  if (stmt->getStmtClass() == Stmt::ForStmtClass) {
    const ForStmt *for_stmt = cast<ForStmt>(stmt);
    if (for_stmt->getInc()) {
      inc_block = WhirlBlockUtil::nwBlock();
      WN *inc_wn = ConvertStmt(for_stmt->getInc());
      WhirlBlockUtil::popCurrentBlock();
      if (inc_wn) {
        Is_True(!OPERATOR_is_expression(WN_operator(inc_wn)),
                ("bad inc wn"));
        SRCPOS spos = SetSrcPos(getLocation(for_stmt->getInc()));
        WN_Set_Linenum(inc_block, spos);
        WN_INSERT_BlockLast(inc_block, inc_wn);
      }
    }
  }

  // get real label index for break and continue and
  // pop the loop structure
  Pop_Loop_Switch(stmt->getStmtClass(), break_label, continue_label);

  // append continue label to loop body
  SRCPOS spos = SetSrcPos(getEndLocation(body_stmt));
  if (continue_label)
    Append_label(continue_label, spos);

  // append inc block
  if (inc_block && !WN_block_empty(inc_block))
    WN_INSERT_BlockLast(body_block, inc_block);

  // pop body block
  WhirlBlockUtil::popCurrentBlock();

  // create while-do
  WN *loop_wn;
  if (stmt->getStmtClass() != Stmt::DoStmtClass)
    loop_wn = WN_CreateWhileDo(cond_wn, body_block);
  else
    loop_wn = WN_CreateDoWhile(cond_wn, body_block);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), loop_wn);
  WN_Set_Linenum(loop_wn, SetSrcPos(getLocation(stmt)));

  // append break label after while-do
  spos = SetSrcPos(getEndLocation(stmt));
  if (break_label)
    Append_label(break_label, spos);

  return NULL;
}

WN *WhirlStmtBuilder::ConvertDoStmt(const DoStmt *stmt) {
  TRACE_FUNC();

  return ConvertLoopStmt(stmt);
}

WN *WhirlStmtBuilder::ConvertStmtExpr(const StmtExpr *stmt) {
  TRACE_FUNC();
  const CompoundStmt *com_stmt = stmt->getSubStmt();
  WN *ret = ConvertCompoundStmt(com_stmt);
  return ret;
}

WN *WhirlStmtBuilder::ConvertForStmt(const ForStmt *stmt) {
  TRACE_FUNC();

  return ConvertLoopStmt(stmt);
}

WN *WhirlStmtBuilder::ConvertGotoStmt(const GotoStmt *stmt) {
  TRACE_FUNC();
  LABEL_IDX label_idx = _builder->Get_label_idx(stmt->getLabel());
  WN* goto_wn = WN_CreateGoto(ST_IDX_ZERO, label_idx);
  WN_Set_Linenum(goto_wn, SetSrcPos(getLocation(stmt)));
  return goto_wn;
}

// handle condition variable with Init_var_decl()
void WhirlStmtBuilder::HandleConditionVar(const Decl *decl) {
  _builder->DeclBuilder().ConvertDecl(decl);
  if (isa<VarDecl>(decl)) {
    WN *ret = NULL;
    const VarDecl *var_decl = cast<VarDecl>(decl);
    if (var_decl->hasInit())
      ret = Init_var_decl(var_decl);
    if (ret != NULL)
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);
  }
}

WN *WhirlStmtBuilder::ConvertIfStmt(const IfStmt *stmt) {
  TRACE_FUNC();
  TY_IDX cond_ty, then_ty, else_ty;
  WN *test, *if_stmt;
  if (stmt->getInit()) {
    Is_True(false, ("append init whirl?"));
    test = ConvertStmt(stmt->getInit());
  }
  if (stmt->getConditionVariable()) {
    HandleConditionVar(stmt->getConditionVariable());
  }
  WhirlExprBuilder expr_bldr(_builder);
  WN *cond_wn = expr_bldr.ConvertToNode(stmt->getCond());
  Is_True(cond_wn != NULL, ("Child 0 of IF can not be NULL!"));

  WN *then_block = WhirlBlockUtil::nwBlock();
  if (stmt->getThen()) {
    WN *then_stmt = ConvertStmt(stmt->getThen());
    if (then_stmt) {
      WN_INSERT_BlockLast(then_block, then_stmt);
      WN_Set_Linenum(then_block, SetSrcPos(getLocation(stmt->getThen()))); 
    }
  }
  WhirlBlockUtil::popCurrentBlock();
  WN *else_block = WhirlBlockUtil::nwBlock();
  if (stmt->getElse()) {
    WN *else_stmt = ConvertStmt(stmt->getElse());
    if (else_stmt) {
      WN_INSERT_BlockLast(else_block, else_stmt);
      WN_Set_Linenum(else_block, SetSrcPos(getLocation(stmt->getElse())));
    }
  }
  WhirlBlockUtil::popCurrentBlock();
  cond_wn = Handle_cond_wn(cond_wn);
  if_stmt = WN_CreateIf(cond_wn, then_block, else_block);
  WN_Set_Linenum(if_stmt, SetSrcPos(getLocation(stmt)));
  return if_stmt;
}

WN *WhirlStmtBuilder::ConvertIndirectGotoStmt(const IndirectGotoStmt *stmt) {
  TRACE_FUNC();
  if (const LabelDecl *target = stmt->getConstantTarget()) {
    Is_True(false, ("unsupported ConstantTarget for ConvertIndirectGotoStmt"));
  }

  // get target wn
  WhirlExprBuilder expr_bldr(_builder);
  WN *addr = expr_bldr.ConvertToNode(stmt->getTarget());

  Set_PU_no_inline(Get_Current_PU());

  if (WN_operator(addr) == OPR_LDA) {
    WN_set_operator(addr, OPR_LDID);
    WN_set_desc(addr, WN_rtype(addr));
    WN_set_ty(addr, TY_pointed(WN_ty(addr)));
  }

  WN *wn = WN_CreateAgoto(addr);
  WN_Set_Linenum(wn, SetSrcPos(getLocation(stmt)));
  return wn;
}

WN *
WhirlStmtBuilder::ConvertLabelStmt(const LabelStmt *stmt) {
  TRACE_FUNC();
  LABEL_IDX label_idx = _builder->Get_label_idx(stmt->getDecl());
  SRCPOS spos = SetSrcPos(getLocation(stmt));
  Append_label(label_idx, spos);
  WN *wn = ConvertStmt(stmt->getSubStmt());
  return wn;
}

WN *WhirlStmtBuilder::ConvertReturnStmt(const ReturnStmt *stmt) {
  TRACE_FUNC();
  WN *ret = NULL;
  const Expr *retv = stmt->getRetValue();
  if (retv == NULL) {
    ret = WN_CreateReturn();
    WN_Set_Linenum(ret, GetSrcPos());
    return ret;
  } else {
    TY_IDX ret_ty = _builder->TB().ConvertType(retv->getType());
    BOOL rv = retv->getType()->isVoidType() ? FALSE : TRUE;
    // InitListExprClass require a dest
    Result dest = retv->IgnoreParenImpCasts()->getStmtClass() == Expr::InitListExprClass
                    ? Result::nwSym(ST_st_idx(Create_tmp_sym(ret_ty, ".ret", GetSrcPos())),
                                    ret_ty)
                    : Result::nwNone();
    WhirlExprBuilder expr_bldr(_builder);
    WN *retw = expr_bldr.ConvertToNode(retv, dest, rv);
    BOOL need_eh_region = Check_call_region();

    if (retv->getType()->isVoidType()) {
      if (retw != NULL) {
        Is_True(OPERATOR_is_stmt(WN_operator(retw)), ("not a stmt"));
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), retw);
      }
      ret = WN_CreateReturn();
      WN_Set_Linenum(ret, GetSrcPos());
      return ret;
    }

    if (retw == NULL) {
      Is_True(!dest.isNone(), ("dest is none"));
      retw = dest.GetRValue();
      ret = WN_CreateReturn_Val(OPR_RETURN_VAL, TY_mtype(ret_ty),
                                MTYPE_V, retw);
      WN_Set_Linenum(ret, GetSrcPos());
      return ret;
    }

    // if the function returns a reference, take the address of the
    // expression rather than the value
    const Decl *decl = _builder->Scope().CurrentDecl();
    Is_True(isa<FunctionDecl>(decl),
            ("current decl for return stmt should be FunctionDecl"));
    QualType rtype =
      dyn_cast<FunctionDecl>(decl)->getFunctionType()->getReturnType();
    if (rtype->isReferenceType() && !retv->getType()->isReferenceType()) {
      ret_ty = _builder->TB().ConvertType(rtype);
      if (WN_operator(retw) == OPR_ILOAD)
        retw = WN_kid0(retw);
      else if (WN_operator(retw) == OPR_LDID) {
        ST *st = WN_st(retw);
        if (TY_kind(ST_type(st)) != KIND_POINTER) {
          WN_DELETE_Tree(retw);
          retw = WN_Lda(Pointer_Mtype, 0, st, 0);
          Set_ST_addr_saved(st);
        }
      }
    }
#if 0
    else if (retv->getType()->isRecordType()) {
      // don't call dtor if return type is RecordType
      if (dtor_call_stack.top().first == current_stmt &&
          dtor_call_stack.top().second != NULL) {
        const VarDecl *var_decl = dtor_call_stack.top().second;
        Is_True(var_decl->getType()->isRecordType(), ("should be RecordType"));
        dtor_call_stack.pop();
      }
    }
#endif

    TYPE_ID mtype = TY_mtype(ret_ty);
    if (WN_rtype(retw) != mtype && mtype == MTYPE_M)
      mtype = WN_rtype(retw);

    if (OPERATOR_is_call(WN_operator(retw))) {
      Is_True(mtype != MTYPE_M, ("mtype is M"));
      mtype = Mtype_comparison(mtype);
      WN* blk = WN_CreateBlock();
      WN_INSERT_BlockLast(blk, retw);
      WN* ldid = WN_Ldid(mtype, -1, Return_Val_Preg, ret_ty);
      retw = WN_CreateComma(OPR_COMMA, mtype, MTYPE_V, blk, ldid);
    }

    WN *cur_blk = WhirlBlockUtil::getCurrentBlock();
    // create a temp so that return stmt is EH-free
    WN *call_wn = NULL;
    if (_builder->Lang_CPP() && emit_exceptions && need_eh_region &&
        WN_operator(retw) != OPR_INTCONST &&
        WN_operator(retw) != OPR_LDID) {
      WN *tmp_wn = WGEN_StidTemp(ret_ty, retw, ".ret.val");
      WN_Set_Linenum(tmp_wn, GetSrcPos());
      WN_INSERT_BlockLast(cur_blk, tmp_wn);
      retw = WN_Ldid(TY_mtype(ret_ty), WN_offset(tmp_wn), WN_st(tmp_wn), ret_ty);
    }

    // pop dtor for copy ctor stack
    pop_dtor_for_copy_ctor_stack(cur_blk, rtype);
    // pop dtor call before return
    pop_dtor_call_for_return(cur_blk);

    Is_True(OPERATOR_is_expression(WN_operator(retw)), ("ret is not value"));
    ret = WN_CreateReturn_Val(OPR_RETURN_VAL, mtype, MTYPE_V, retw);
    WN_Set_Linenum(ret, GetSrcPos());
    return ret;
  }
}

WN *WhirlStmtBuilder::ConvertSwitchStmt(const SwitchStmt *stmt) {
  TRACE_FUNC();

  SRCPOS spos = SetSrcPos(getLocation(stmt));
  WN *init_wn, *if_stmt, *body_blk;
  if (stmt->getInit()) {
    init_wn = ConvertStmt(stmt->getInit());
  }

  if (stmt->getConditionVariable()) {
    HandleConditionVar(stmt->getConditionVariable());
  }

  // handle conditioon
  const Expr *cond = stmt->getCond();
  WhirlExprBuilder expr_bldr(_builder);
  WN* cond_wn = expr_bldr.ConvertToNode(cond);
  Is_True(cond_wn != NULL, ("Child 0 of IF can not be NULL!"));
  WN *switch_blk = WN_CreateBlock();
  // The switch index may be needed more than once if it contains case
  // range. As it may have side-effects like a function call, save the
  // index into a temporary, and used the saved value.
  // when the switch index is a constant,
  // we don't generate the index var, instead, we use constant the as switch
  // expression directly. This helps compiler eliminates the unreachable
  //  branch code even at O0 phase.
  TY_IDX ty_idx = _builder->TB().ConvertType(cond->getType());
  TYPE_ID index_mtype = Mtype_comparison(TY_mtype(ty_idx));
  if (WN_operator(cond_wn) != OPR_INTCONST) {
    ST *save_expr_st = Gen_Temp_Symbol(MTYPE_TO_TY_array[index_mtype], "_switch_index");
    WN *stid = WN_Stid(index_mtype, 0, save_expr_st, MTYPE_TO_TY_array[index_mtype], cond_wn);
    WN_Set_Linenum(stid, GetSrcPos());
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
    cond_wn = WN_Ldid(index_mtype, 0, save_expr_st, MTYPE_TO_TY_array[index_mtype]);
  }
  if (++switch_info_i == switch_info_max) {
    switch_info_max   = ENLARGE(switch_info_max);
    switch_info_stack = (SWITCH_INFO *) realloc (switch_info_stack,
                               switch_info_max * sizeof (SWITCH_INFO));
  }
  switch_info_stack [switch_info_i].index             = cond_wn;
  switch_info_stack [switch_info_i].start_case_index  = case_info_i + 1;
  switch_info_stack [switch_info_i].default_label_idx = 0;
  switch_info_stack [switch_info_i].default_spos      = 0;

  // switch body
  WN *default_blk = WN_CreateBlock();

  const Stmt *body_stmt = stmt->getBody();

  LABEL_IDX body_exit_label_idx = Get_label_idx(body_stmt);
  Record_Loop_Switch(stmt->getStmtClass(), body_exit_label_idx, 0/*continue_label_idx*/);

  if (body_stmt) {
    body_blk = WhirlBlockUtil::nwBlock();
    WN *body_wn = ConvertStmt(body_stmt);
    if (body_wn)
      WN_INSERT_BlockLast(body_blk, body_wn);
    WhirlBlockUtil::popCurrentBlock();
  }

  // generate casegoto statments
  LABEL_IDX exit_label_idx;
  WN *def_goto;
  WN *case_block;
  WN *case_range;
  WN *case_entry;
  WN *switch_wn;
  WN *switch_block;
  INT32 i;
  INT32 n = case_info_i - switch_info_stack [switch_info_i].start_case_index + 1;
  if (break_continue_info_stack [break_continue_info_i].break_label_idx)
    exit_label_idx = break_continue_info_stack [break_continue_info_i].break_label_idx;
  else
    New_LABEL (CURRENT_SYMTAB, exit_label_idx);
  if (switch_info_stack [switch_info_i].default_label_idx) {
    def_goto = WN_CreateGoto (switch_info_stack [switch_info_i].default_label_idx);
    WN_Set_Linenum(def_goto, switch_info_stack [switch_info_i].default_spos);
  } else {
    def_goto = WN_CreateGoto (exit_label_idx);
    WN_Set_Linenum(def_goto, spos);
  }

  case_block = WN_CreateBlock();
  case_range = WN_CreateBlock();
  for (i = switch_info_stack [switch_info_i].start_case_index;
       i <= case_info_i;
       i++) {
    INT64     case_value;
    LABEL_IDX case_label_idx = case_info_stack [i].case_label_idx;
    INT64 low = case_info_stack[i].case_lower_bound_value;
    INT64 high = case_info_stack[i].case_upper_bound_value;
    if (low < high) {
      //TODO
    } else {
      case_entry = WN_CreateCasegoto(low, case_label_idx);
      WN_INSERT_BlockLast(case_block, case_entry);
      WN_Set_Linenum(case_entry, GetSrcPos());
    }
  }
  switch_wn = WN_CreateSwitch(n,
                              switch_info_stack [switch_info_i].index,
                              case_block,
                              def_goto,
                              exit_label_idx);

  switch_block = WhirlBlockUtil::getCurrentBlock();
  WN_INSERT_BlockLast (switch_block, switch_wn);
  WN_Set_Linenum(switch_wn, GetSrcPos());

  if (body_blk)
    WN_INSERT_BlockLast(switch_block, body_blk);

  // TODO: duplicate label wn
  if (WN_operator(WN_last(switch_block)) != OPR_LABEL ||
      WN_label_number(WN_last(switch_block)) != exit_label_idx) {
    WN *wn = WN_CreateLabel(ST_IDX_ZERO, exit_label_idx, 0, NULL);
    WN_INSERT_BlockLast(switch_block, wn);
    WN_Set_Linenum(wn, SetSrcPos(getEndLocation(stmt)));
  }

  case_info_i = switch_info_stack [switch_info_i].start_case_index - 1;
  --switch_info_i;

  --break_continue_info_i;

  if (body_exit_label_idx != exit_label_idx) {
    WN *end = WN_CreateLabel(ST_IDX_ZERO, body_exit_label_idx, 0, NULL);
    WN_INSERT_BlockLast (switch_block, end);
    WN_Set_Linenum(end, SetSrcPos(getEndLocation(stmt)));
  }
  return NULL;
}

WN *WhirlStmtBuilder::ConvertCaseStmt(const CaseStmt *stmt) {
  TRACE_FUNC();
  LABEL_IDX  case_label_idx;

  // handle lower bound
  llvm::APSInt lb;
  bool ret = evaluateAsInt(stmt->getLHS(), lb,  _builder->Context());
  Is_True(ret == true, ("lower bound is not constant"));

  // handle case ranges
  llvm::APSInt ub;
  if (stmt->getRHS()) {
    ret = evaluateAsInt(stmt->getRHS(), ub,  _builder->Context());
    Is_True(ret == true, ("upper bound is not constant"));
  }
  else {
    ub = lb;
  }

#if 0
  WhirlConstBuilder const_bldr(_builder);
  Result ret = const_bldr.ConvertConst(stmt->getLHS());
  Is_True(ret.isIntConst(), ("invalid return type for case val"));
  UINT64 case_val = ret.intconst();
#endif

  if (++case_info_i == case_info_max) {
    case_info_max   = ENLARGE(case_info_max);
    case_info_stack = (CASE_INFO *) realloc(case_info_stack,
                                            case_info_max * sizeof (CASE_INFO));
  }

  // TODO? call SExt or ZExt according to type
  case_info_stack
    [case_info_i].case_lower_bound_value = lb.getSExtValue();
  case_info_stack
    [case_info_i].case_upper_bound_value = ub.getSExtValue();
#if 0
  for (int i = switch_info_stack [switch_info_i].start_case_index;
       i < case_info_i; ++i)
    if (case_val ==
        case_info_stack [i].case_lower_bound_value)
      printf("duplicate case\n");
#endif
  New_LABEL(CURRENT_SYMTAB, case_label_idx);
  case_info_stack [case_info_i].case_label_idx = case_label_idx;

  WN *wn = WN_CreateLabel((ST_IDX) 0, case_label_idx, 0, NULL);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, SetSrcPos(getLocation(stmt)));

  // if body oof the case is just a 'break'
  if (isa<BreakStmt>(stmt->getSubStmt())) {
    // a fallthrouth into this case
  }

  // handle many case stmts nested together, i.e.:
  //  case 1:
  //    case 2:
  //      case 3: etc.
  WN * sub = ConvertStmt(stmt->getSubStmt());
  return sub;
}

WN *WhirlStmtBuilder::ConvertDefaultStmt(const DefaultStmt *stmt) {
  SRCPOS spos = SetSrcPos(getLocation(stmt));
  LABEL_IDX def_label_idx;
  New_LABEL (CURRENT_SYMTAB, def_label_idx);
  switch_info_stack [switch_info_i].default_label_idx = def_label_idx;
  switch_info_stack [switch_info_i].default_spos      = spos;

  WN *wn = WN_CreateLabel ((ST_IDX) 0, def_label_idx, 0, NULL);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // fall through
  return ConvertStmt(stmt->getSubStmt());
}

WN *WhirlStmtBuilder::ConvertWhileStmt(const WhileStmt *stmt) {
  TRACE_FUNC();

  // convert cond var if present
  if (stmt->getConditionVariable()) {
    HandleConditionVar(stmt->getConditionVariable());
  }

  return ConvertLoopStmt(stmt);
}

WN *
WhirlStmtBuilder::ConvertExpr(const Expr *stmt, Result target) {
  TRACE_FUNC();
  SRCPOS srcpos = SetSrcPos(getLocation(stmt));
  WhirlExprBuilder expr_bldr(_builder);
  Result r = expr_bldr.ConvertExpr(stmt, target, FALSE);
  if (r.isNode()) {
    WN *ret = r.Node();
    if (ret && OPCODE_is_expression(WN_opcode(ret)))
      ret = WN_CreateEval(ret);
      WN_Set_Linenum(ret, srcpos);
      return ret;
    // insert node should be a structured control flow node or a statement node
    // TODO: need improvement
    if (!ret || !OPCODE_is_stmt(WN_opcode(ret)) && !OPCODE_is_scf(WN_opcode(ret)))
      return NULL;

    if (OPCODE_has_next_prev(WN_opcode(ret))) {
      WN_Set_Linenum(ret, srcpos);
    }

    return ret;
  } else if (r.isNone() || r.isSym() || r.isTCon() || r.isIntConst()) {
#if 0
    // ignore Non-WHIRL here
    ST_IDX stIdx = r.Sym();
    ST *st = &St_Table[stIdx];
    TY_IDX tyi = ST_type(stIdx);
    WN *ld_wn = WN_Ldid(TY_mtype(tyi), 0, st, tyi);
    WN *st_wn = WN_Stid(TY_mtype(tyi), 0, st, tyi, ld_wn);
    return st_wn;
#endif
  } else {
      Is_True(false, ("Can't goto there."));
  }
  return NULL;
}

WN *WhirlStmtBuilder::ConvertExprWithCleanups(const ExprWithCleanups *stmt) {
  TRACE_FUNC();
  return ConvertExpr(stmt, Result::nwNone());
}

WN *
WhirlStmtBuilder::ConvertCXXNewExpr(const CXXNewExpr *stmt) {
  return ConvertExpr(stmt, Result::nwNone());
}

WN *
WhirlStmtBuilder::ConvertCXXDeleteExpr(const CXXDeleteExpr *stmt) {
  return ConvertExpr(stmt, Result::nwNone());
}

WN *WhirlStmtBuilder::ConvertStmt(const Stmt *stmt) {
  TRACE_FUNC();
  WN *wn = NULL;
  SRCPOS srcpos = SetSrcPos(getLocation(stmt));

  switch (stmt->getStmtClass()) {
    case Stmt::GCCAsmStmtClass:
      wn = ConvertGCCAsmStmt(cast<GCCAsmStmt>(stmt));
      break;
#ifdef C2W_ENABLE_MSVC
    case Stmt::MSAsmStmtClass:
        break;
#endif
    case Stmt::AttributedStmtClass:
      wn = ConvertAttributedStmt(cast<AttributedStmt>(stmt));
      break;
    case Stmt::BreakStmtClass:
      wn = ConvertBreakStmt(cast<BreakStmt>(stmt));
      break;
    case Stmt::CallExprClass:
      wn = ConvertCallExpr(cast<CallExpr>(stmt));
      break;
    case Stmt::CXXMemberCallExprClass:
      wn = ConvertCXXMemberCallExpr(cast<CXXMemberCallExpr>(stmt));
      break;
    case Stmt::CXXOperatorCallExprClass:
      wn = ConvertCXXOperatorCallExpr(cast<CXXOperatorCallExpr>(stmt));
      break;
    case Stmt::CapturedStmtClass:
      wn = ConvertCapturedStmt(cast<CapturedStmt>(stmt));
      break;
    case Stmt::CompoundStmtClass:
      wn = ConvertCompoundStmt(cast<CompoundStmt>(stmt));
      break;
    case Stmt::ContinueStmtClass:
      wn = ConvertContinueStmt(cast<ContinueStmt>(stmt));
      break;
    case Stmt::CXXCatchStmtClass:
      wn = ConvertCXXCatchStmt(cast<CXXCatchStmt>(stmt));
      break;
    case Stmt::CXXForRangeStmtClass:
      wn = ConvertCXXForRangeStmt(cast<CXXForRangeStmt>(stmt));
      break;
    case Stmt::CXXThrowExprClass:
      wn = ConvertCXXThrowExpr(cast<CXXThrowExpr>(stmt));
      break;
    case Stmt::CXXTryStmtClass:
      wn = ConvertCXXTryStmt(cast<CXXTryStmt>(stmt));
      break;
    case Stmt::DeclStmtClass:
      wn = ConvertDeclStmt(cast<DeclStmt>(stmt));
      break;
    case Stmt::DoStmtClass:
      wn = ConvertDoStmt(cast<DoStmt>(stmt));
      break;
    case Stmt::ForStmtClass:
      wn = ConvertForStmt(cast<ForStmt>(stmt));
      break;
    case Stmt::GotoStmtClass:
      wn = ConvertGotoStmt(cast<GotoStmt>(stmt));
      break;
    case Stmt::IfStmtClass:
      wn = ConvertIfStmt(cast<IfStmt>(stmt));
      break;
    case Stmt::IndirectGotoStmtClass:
      wn = ConvertIndirectGotoStmt(cast<IndirectGotoStmt>(stmt));
      break;
    case Stmt::LabelStmtClass:
      wn = ConvertLabelStmt(cast<LabelStmt>(stmt));
      break;
#ifdef C2W_ENABLE_MSVC
    case Stmt::MSDepdendentExistsStmtClass:
        break;
#endif
    case Stmt::NullStmtClass:
      break;
#ifdef C2W_ENABLE_OBJC
    case Stmt::ObjCAtCatchStmtClass:
    case Stmt::ObjCAtFinallyStmtClass:
    case Stmt::ObjCAtSynchronizedStmtClass:
    case Stmt::ObjCAtThrowStmtClass:
    case Stmt::ObjCAtTryStmtClass:
    case Stmt::ObjCAutoreleasePoolStmtClass:
    case Stmt::ObjCForCollectionStmtClass:
        break;
#endif
#ifdef C2W_ENABLE_OPENMP
    case Stmt::OMPExecutableDirectiveClass:
        break;
#endif
    case Stmt::ReturnStmtClass:
      wn = ConvertReturnStmt(cast<ReturnStmt>(stmt));
      break;
#ifdef C2W_ENABLE_MSVC
    case Stmt::SEHExceptStmtClass:
    case Stmt::SEHFinallyStmtClass:
    case Stmt::SEHLeaveStmtClass:
    case Stmt::SEHTryStmtClass:
        break;
#endif
    case Stmt::StmtExprClass:
      wn = ConvertStmtExpr(cast<StmtExpr>(stmt));
      break;
    case Stmt::SwitchStmtClass:
      wn = ConvertSwitchStmt(cast<SwitchStmt>(stmt));
      break;
    case Stmt::CaseStmtClass:
      wn = ConvertCaseStmt(cast<CaseStmt>(stmt));
      break;
    case Stmt::DefaultStmtClass:
      wn = ConvertDefaultStmt(cast<DefaultStmt>(stmt));
      break;
    case Stmt::WhileStmtClass:
      wn = ConvertWhileStmt(cast<WhileStmt>(stmt));
      break;
    case Stmt::BinaryOperatorClass:
    case Stmt::UnaryOperatorClass:
    case Stmt::CompoundAssignOperatorClass:
    case Stmt::ConditionalOperatorClass:
    case Stmt::CStyleCastExprClass:
    case Stmt::ImplicitCastExprClass:
    case Stmt::ParenExprClass:
    case Stmt::AtomicExprClass:
    case Stmt::CXXFunctionalCastExprClass:
    case Stmt::CXXConstructExprClass:
    case Stmt::CXXNoexceptExprClass:
    case Stmt::CXXStaticCastExprClass:
    case Stmt::CXXTemporaryObjectExprClass:
    case Stmt::CXXThisExprClass:
    case Stmt::CXXTypeidExprClass:
    case Stmt::LambdaExprClass:
    case Stmt::MemberExprClass:
    case Stmt::VAArgExprClass:
      wn = ConvertExpr(cast<Expr>(stmt));
      break;
    case Stmt::ExprWithCleanupsClass:
      wn = ConvertExprWithCleanups(cast<ExprWithCleanups>(stmt));
      break;
    case Stmt::CXXNewExprClass:
      wn = ConvertCXXNewExpr(cast<CXXNewExpr>(stmt));
      break;
    case Stmt::CXXDeleteExprClass:
      wn = ConvertCXXDeleteExpr(cast<CXXDeleteExpr>(stmt));
      break;
    case Stmt::IntegerLiteralClass:
    case Stmt::FloatingLiteralClass:
    case Stmt::CharacterLiteralClass:
    case Stmt::DeclRefExprClass:
    case Stmt::StringLiteralClass:
    case Stmt::UnaryExprOrTypeTraitExprClass:
    case Stmt::ArraySubscriptExprClass:
      return NULL;
      break;
    default:
      Is_True(false,
                 ("Unsupported stmt class: %s", stmt->getStmtClassName()));
  }
  if (wn && OPCODE_has_next_prev(WN_opcode(wn))) {
    srcpos = SetSrcPos(getLocation(stmt));
    WN_Set_Linenum(wn, srcpos);
  }
  return wn;
}

} // namespace wgen
