//-*-c++-*-

/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_vsa_tag_checker.cxx
//
// ====================================================================
//

// ====================================================================
// VSA TAG CHECKER
//   Check the TAG value
//   
//
// Algorithm:
//
// ====================================================================

#include "defs.h"
#include "config_vsa.h"
#include "erglob.h"
#include "erbe.h"
#include "opt_defs.h"
#include "opt_bb.h"
#include "opt_main.h"
#include "opt_vsa_util.h"
#include "opt_vsa.h"
#include "opt_dna.h"
#include "opt_addr_util.h"
#include "opt_vsa_rbc.h"
#include "opt_vsa_checker.h"
#include "opt_vsa_var_def.h"

typedef enum {
  COND_TRUE,
  COND_FALSE,
  COND_UNKNOWN,
} COND_RESULT;

typedef std::pair<SRCPOS_TREENODE*, IDTYPE> CURR_SPOS;
typedef std::vector<CURR_SPOS, mempool_allocator<CURR_SPOS> > SPOS_CANDS;
typedef mempool_allocator<SRCPOS_HANDLE*> SH_ALLOCATOR;
typedef vector<SRCPOS_HANDLE*, SH_ALLOCATOR> SRCPOS_HANDLE_VEC;

#define TAG_CHECKER_UD_LIMIT 1024

class TAG_CHECKER : public SPOS_BASE {
private:
  CXX_MEM_POOL   _pool;
  RBC_BASE      *_rbc_base;
  TAG_CHECK_TYPE _check_type;
  UINT32         _tag_id;
  UINT32         _attr_id;
  UINT64_SET    *_visited_tor;
  SPOS_CANDS    *_true_list;    // cache for true result srcpos location
  SPOS_CANDS    *_false_list;   // cache for false result srcpos location
  union {
    BOOL          _tag_set;
    BOOL          _attr_set;
  };
  UINT32          _ud_cnt;      // tag checker ud counter, to avoid infinite loop caused by tor cycle in model phase

  void            Set_tag(BOOL v)       { Is_True_Ret(_check_type == CHECK_BY_TAG,
                                                      ("wrong check type"));
                                          _tag_set = v; }
  void            Set_attr(BOOL v)      { Is_True_Ret(_check_type != CHECK_BY_TAG,
                                                      ("wrong check type"));
                                          _attr_set = v; }
  void            Set_res(BOOL v)       { _check_type == CHECK_BY_TAG ? Set_tag(v) : Set_attr(v); }
  RBC_BASE       *Rbc_base()            { return _rbc_base; }
  MEM_POOL       *Loc_pool()            { return _pool(); }
  UINT32          Tag_id()              { return _tag_id; }
  UINT32          Attr_id()             { return _attr_id; }
  TAG_CHECK_TYPE  Check_type()          { return _check_type; }
  SPOS_CANDS     *True_list()           { return _true_list; }
  SPOS_CANDS     *False_list()          { return _false_list; }
  BOOL            Visited(TAG_OBJ_REP *tor);
  void            Set_visited(TAG_OBJ_REP *tor);
  void            Reset_visited(TAG_OBJ_REP *tor);
  void            Merge_res(TRAV_CONTEXT *ctx, BOOL res);
  CHECKER_STATUS  Ret_cr_status(CODEREP *cr);
  BOOL            Check_ud_limit()        { _ud_cnt++; return (_ud_cnt > TAG_CHECKER_UD_LIMIT) ? TRUE : FALSE; }

public:
  // traversal from dereference
  enum { SUSPECT = CS_VSYM_OBJ | CS_VAR_DEF };
  // need to check coderep
  enum { ENTITY = TE_CODEREP };
  // need to track SRCPOS
  enum { USE_SRCPOS = TRUE };
  // TODO: need to follow eh path?
  enum { FOLLOW_EH = FALSE };

public:
  // TAG_CHECKER
  // Constructor
  TAG_CHECKER(RBC_BASE *rbc_base, TRAV_CONTEXT& ctx, 
              UINT32 tag_id, UINT32 attr_id, TAG_CHECK_TYPE type,
              SRCPOS_HANDLE *sp_h, UINT64_SET *visited)
   : SPOS_BASE(sp_h), _rbc_base(rbc_base), 
     _tag_id(tag_id), _attr_id(attr_id), _check_type(type),
     _pool("Tag_checker_pool", FALSE),
     _true_list(NULL), _false_list(NULL), _ud_cnt(0) {
    if(!visited) {
      _visited_tor = CXX_NEW(
        UINT64_SET(13, UINT64_HASHER(), UINT64_EQUAL(), UINT64_ALLOCATOR(Loc_pool())),
        Loc_pool());
    } else {
      _visited_tor = visited;
    }
    // default value
    // if check by tag, set result to false (no tag)
    // if check by attr, set result to true (attr on)
    Set_res(type == CHECK_BY_TAG ? FALSE : TRUE);
    _true_list = CXX_NEW(SPOS_CANDS(mempool_allocator<CURR_SPOS>(Loc_pool())), Loc_pool());
    _false_list = CXX_NEW(SPOS_CANDS(mempool_allocator<CURR_SPOS>(Loc_pool())), Loc_pool());
    ctx.Set_Tracing(Get_Trace(TP_CHECKER, CHK_TAG_TRACE_FLAG));
  }

  // Checker_name
  // return the name of the checker
  const char     *Checker_name(void) const  { return "TAG"; }
  BOOL            Check_res()               { return (_check_type == CHECK_BY_TAG) ?  _tag_set : _attr_set; }
  CHECKER_STATUS  Check_tag(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);
  CHECKER_STATUS  Check_tag_phi(TAG_OBJ_REP *tor, CHECK_OBJ &obj, TRAV_CONTEXT *ctx);
  CHECKER_STATUS  Check_tag_obj_ud(TAG_OBJ_REP *tor, CHECK_OBJ &obj, TRAV_CONTEXT *ctx);
  BOOL            Check_prog_input(TAG_OBJ_REP *tag_obj_rep, TRAV_CONTEXT *ctx);
  BB_NODE        *Find_tor_bb(PHI_NODE *phi, TAG_OBJ_REP *opnd_tor);
  void            Add_tag_path(TRAV_CONTEXT *ctx, BOOL check_res);
  template <class T>
  void            Check_value_objs(VALUE_OBJS<T> &cands, STMTREP *sr, TRAV_CONTEXT *ctx);
  void            Add_rbc_plist(TRAV_CONTEXT *ctx, BOOL is_true_list);
  BOOL            Set_check_kind(CHECKER_SUSPECT suspect) { return FALSE; }

public:
  // Check_coderep
  // call back to check coderep
  template<CODEKIND  _KIND> CHECKER_STATUS
  Check_coderep(CHECK_OBJ &obj, TRAV_CONTEXT* ctx);

  // Check_stmtrep
  // call back to check stmtrep
  template<OPERATOR _OPR> CHECKER_STATUS
  Check_stmtrep(CHECK_OBJ &obj, TRAV_CONTEXT* ctx);

  // Check_heap_obj
  CHECKER_STATUS
  Check_heap_obj(CHECK_OBJ &obj, TRAV_CONTEXT* ctx) { return CS_CONT; }

  // Check vsym_obj
  CHECKER_STATUS
  Check_vsym_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);

  void
  Add_target(UINT32 file_idx, ST_IDX idx) { }

private:
  BOOL            Check_path(TRAV_CONTEXT *ctx);
  COND_RESULT     Cal_cond_result(STMTREP *sr, CODEREP *cr, COMP_UNIT *cu, TRAV_CONTEXT *ctx);
  INT64           Get_int_value(STMTREP *sr, CODEREP *cr, COMP_UNIT *cu, BOOL &get, TRAV_CONTEXT *ctx);
  BOOL            Set_const_side_effect(CODEREP *cr, TRAV_CONTEXT *ctx);
};


BOOL
TAG_CHECKER::Visited(TAG_OBJ_REP *tor)
{
  if(_visited_tor->find((UINT64)tor) == _visited_tor->end()) {
    return FALSE;
  } else {
    return TRUE;
  }
}

void
TAG_CHECKER::Set_visited(TAG_OBJ_REP *tor)
{
  _visited_tor->insert((UINT64)tor);
}

void
TAG_CHECKER::Reset_visited(TAG_OBJ_REP *tor)
{
  _visited_tor->erase((UINT64)tor);
}

void
TAG_CHECKER::Merge_res(TRAV_CONTEXT *ctx, BOOL res)
{
  BOOL orig_res = Check_res();
  if(Check_type() == CHECK_BY_TAG) {
    // if checker is tag on, result is tag on
    if(res)
      Set_res(TRUE);
  } else {
    // if checker attr is not set, result is not set
    if(!res)
      Set_res(FALSE);
  }
  Is_Trace(ctx->Tracing(), (TFile, "@@@@@Merge Check orig %s with new %s -> %s\n",
                            orig_res ? "Y" : "N",
                            res ? "Y": "N",
                            Check_res() ? "Y" : "N"));
}

CHECKER_STATUS
TAG_CHECKER::Ret_cr_status(CODEREP *cr)
{
  switch (cr->Kind()) {
  case CK_VAR:
    return CS_VAR_UD;
  case CK_IVAR:
    return CS_IVAR_UD;
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
  case CK_OP:
    return CS_DONE;
  default:
    Is_True(FALSE, ("unknown cr kind"));
    break;
  }
  return CS_DONE;
}

template <class T>
void
TAG_CHECKER::Check_value_objs(VALUE_OBJS<T> &cands, STMTREP *sr, TRAV_CONTEXT *ctx)
{
  SRCPOS_TREENODE* cur_node = Sp_h()->Add_children(cands.Size());
  IDTYPE p_idx = Sp_h()->Cur_idx();
  Sp_h()->Append_data(sr, ctx->Dna(), PATHINFO_DUMMY);
  VSYM_TRACKER::VSYM_VECTOR vsym_stack;
  ctx->Tracker()->Save_stack(&vsym_stack);
  // start new tag_checker for each VALUE_OBJ - use same context to avoid infinite loop
  COMP_UNIT *old_cu = ctx->Comp_unit();
  for(int i = 0; i < cands.Size(); i++) {
    Sp_h()->Set_cur_node(cur_node, i);
    COMP_UNIT *def_cu = cands.Value_cu(i);
    DNA_NODE *def_dna = def_cu->Dna();
    STMTREP *def_sr = cands.Value_sr(i);
    CODEREP *def_cr = cands.Value_cr(i);
    CHECK_OBJ def_obj(def_cr, def_sr);
    ctx->Set_context(def_cu);
    CONTEXT_SWITCH def_ctx(def_dna);
    Is_Trace(ctx->Tracing(), (TFile, "%s check: [%d] ", Checker_name(), i+1));
    Is_Trace_cmd(ctx->Tracing(), cands.Value_obj(i)->Print(TFile));
    Sp_h()->Append_data(def_sr, def_dna, PATHINFO_CALL_CHI);
    UD_TRAVELER<TAG_CHECKER> tag_helper(*this, *ctx);
    tag_helper.Continue_trav(def_obj, CS_OP);
    ctx->Tracker()->Restore_stack(&vsym_stack);
  }
  ctx->Set_context(old_cu);
  Sp_h()->Reset_cur_node(cur_node, p_idx);
}

// ====================================================================
// TAG_CHECKER::Check_path
//   Check this path is reachable, if not, ignore the result
// ====================================================================
BOOL
TAG_CHECKER::Check_path(TRAV_CONTEXT *ctx)
{
  SRCPOS_TR_ITER srcpos_tr_iter(Sp_h());
  while (!srcpos_tr_iter.Is_empty()) {
    SRCPOS_NODE vspos = srcpos_tr_iter.Next();
    DNA_NODE *dna = vspos.Dna();
    if (vspos.Info() == PATHINFO_COND_FALSE) {
      STMTREP *sr = vspos.Stmt();
      COND_RESULT result = Cal_cond_result(sr, sr->Rhs(), dna->Comp_unit(), ctx);
      if (result == COND_TRUE) {
        return FALSE;
      }
    } else if (vspos.Info() == PATHINFO_COND_TRUE) {
      STMTREP *sr = vspos.Stmt();
      COND_RESULT result = Cal_cond_result(sr, sr->Rhs(), dna->Comp_unit(), ctx);
      if (result == COND_FALSE) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

// ====================================================================
// TAG_CHECKER::Cal_cond_result
//   Calculate the condation result
// ====================================================================
COND_RESULT
TAG_CHECKER::Cal_cond_result(STMTREP *sr, CODEREP *cr, COMP_UNIT *cu, TRAV_CONTEXT *ctx)
{
  CONTEXT_SWITCH context(cu->Dna());
  BOOL get = FALSE;
  if (cr->Kind() == CK_OP && (cr->Opr() == OPR_EQ || cr->Opr() == OPR_NE)) {
    INT64 opnd0_value = Get_int_value(sr, cr->Opnd(0), cu, get, ctx);
    if (!get)
      return COND_UNKNOWN;
    get = FALSE;
    INT64 opnd1_value = Get_int_value(sr, cr->Opnd(1), cu, get, ctx);
    if (!get)
      return COND_UNKNOWN;
    if (cr->Opr() == OPR_EQ) {
      return opnd0_value == opnd1_value ? COND_TRUE : COND_FALSE;
    } else if (cr->Opr() == OPR_NE) {
      return opnd0_value != opnd1_value ? COND_TRUE : COND_FALSE;
    }
  }
  return COND_UNKNOWN;
}

// ====================================================================
// TAG_CHECKER::Get_int_value
//   if the expression return constant value, calculate the value
// ====================================================================
INT64
TAG_CHECKER::Get_int_value(STMTREP *sr, CODEREP *cr, COMP_UNIT *cu, BOOL &get, TRAV_CONTEXT *ctx)
{
  if (cr->Kind() == CK_CONST) {
    get = TRUE;
    return cr->Const_val();
  } else if (cr->Kind() == CK_VAR) {
    if (cr->Is_var_volatile() || cr->Is_flag_set(CF_IS_ZERO_VERSION) || cr->Is_flag_set(CF_DEF_BY_PHI) || cr->Is_flag_set(CF_DEF_BY_CHI)) {
      get = FALSE;
      return 0;
    }
    STMTREP *defstmt = cr->Defstmt();
    while (defstmt && defstmt->Opr() == OPR_STID && defstmt->Rhs()->Kind() == CK_VAR) {
      defstmt = defstmt->Rhs()->Defstmt();
    }
    if (defstmt && OPERATOR_is_call(defstmt->Opr())) {
      RNA_NODE *rna = cu->Vsa()->Sr_2_rna(defstmt);
      if (ctx->Ipsa()->Rna_has_rbc_op(rna, RBC_OP_SET_FUNC_STR_GET)) {
        EVAL_RET ret = Rbc_base()->Eval__mvsa_container_op(cu->Dna(), rna, Loc_pool(), NULL, FALSE);
        if (ret.first == CHAR_T) {
          get = TRUE;
          return ret.second;
        }
      }
    }
  } else if (cr->Kind() == CK_OP) {
    if (cr->Opr() == OPR_CVTL) {
      return Get_int_value(sr, cr->Opnd(0), cu, get, ctx);
    }
  }
  get = FALSE;
  return 0;
}

BOOL
TAG_CHECKER::Set_const_side_effect(CODEREP *cr, TRAV_CONTEXT *ctx)
{
  Is_True_Ret(cr->Kind() == CK_CONST ||
              cr->Kind() == CK_RCONST ||
              (cr->Kind() == CK_VAR && PU_java_lang(Get_Current_PU())),
              ("invalid cr kind for const"), FALSE);
  if(cr->Kind() == CK_VAR)
  {
    ST *st = ctx->Opt_stab()->St(cr->Aux_id());
    if(!st || !ST_is_class_const_data(st)) {
      return FALSE;
    }
  }
  // cr is const or java const var
  BOOL res;
  if(Check_type() == CHECK_BY_TAG)
    res = FALSE;  // treat const as tag off
  else
    res = TRUE;  // treat const as sanitized

  if (res) {
    Is_Trace(ctx->Tracing(),
            (TFile, "%s : cr%d is const, tag set:false\n",
              Checker_name(), cr->Coderep_id()));
  }
  Add_tag_path(ctx, res);
  return res;
}

// ====================================================================
// TAG_CHECKER::Add_tag_path
//   1. Ignore the path if impossible
//   2. Cache the path and append to Rbc_base()->Plist_true/false later,
//      as the Rbc true/false list will be cleared during the tag
//      checking(rbc symbolic eval call Rbc_init to clear the Plist)
// ====================================================================
void
TAG_CHECKER::Add_tag_path(TRAV_CONTEXT *ctx, BOOL check_result)
{
  if(!Check_path(ctx)) {
    Is_Trace(ctx->Tracing(), (TFile, "@@@@@Ingore check result as path is impossible.]\n"));
    return;
  }
  SPOS_CANDS *list = check_result ? _true_list : _false_list;
  CURR_SPOS cand(Sp_h()->Cur_node(), Sp_h()->Cur_idx());
  list->push_back(cand);
  Is_Trace(ctx->Tracing(), (TFile, "@@@@@Tag check ret: %s\n", check_result ? "Y" : "N"));
  Merge_res(ctx, check_result);
}

// ====================================================================
// TAG_CHECKER::Find_tor_bb
//   Find opnd_tor bb opnd in the phi
//   TODO: change TAG_OBJ_REP phi_list same order as PHI_NODE
// ====================================================================
BB_NODE *
TAG_CHECKER::Find_tor_bb(PHI_NODE *phi, TAG_OBJ_REP *opnd_tor)
{
  BB_NODE* pred = NULL;
  if (phi) {
    BB_NODE *bb_pred;
    BB_LIST_ITER bb_iter;
    INT i = 0;
    FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi->Bb()->Pred())) {
      CODEREP *opnd = phi->OPND(i);
      if (opnd == opnd_tor->Coderep()) {
        pred = bb_pred;
        break;
      }
      i++;
    }
  }
  return pred;
}

CHECKER_STATUS
TAG_CHECKER::Check_tag_phi(TAG_OBJ_REP *tor, CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  if (Check_skip()) {
    Is_Trace(ctx->Tracing(), (TFile, "-%s: CS_DONE reach checker skip limit\n", Checker_name()));
    return CS_DONE;
  }
  if(Visited(tor)) {
    Is_Trace(ctx->Tracing(), (TFile, "\n -%s: Tor phi ", Checker_name()));
    Is_Trace_cmd(ctx->Tracing(), tor->Print(TFile, TRUE));
    Is_Trace(ctx->Tracing(), (TFile, " visited, return CS_DONE\n"));
    return CS_DONE;
  } else {
    Set_visited(tor);
  }
  PHI_NODE *phi = NULL;
  if(tor->Defstmt()) {
    obj.Set_stmtrep(tor->Defstmt());
    Sp_h()->Append_data(obj.Stmtrep(), ctx->Dna(), PATHINFO_COPY);
  } else {
    phi = tor->Def_phi();
    Is_True_Ret(phi, ("no phi found for phi tor"), CS_DONE);
    Sp_h()->Append_data(phi->Bb(), ctx->Dna(), PATHINFO_PHI);
  }
  int phi_size = tor->Phi_list()->size();
  // for only one phi opnd, check the opnd tor instead of invoke new check for fast
  if(phi_size == 1) {
    TAG_OBJ_REP *opnd_tor = tor->Phi_list()->at(0);
    Is_Trace(ctx->Tracing(), 
            (TFile, " -%s: Checking tag phi opnd[1:0]:", Checker_name()));
    Is_Trace_cmd(ctx->Tracing(), tor->Print(TFile, FALSE));
    Is_Trace(ctx->Tracing(), (TFile, "->"));
    Is_Trace_cmd(ctx->Tracing(), opnd_tor->Print(TFile, FALSE));
    BB_NODE *pred = Find_tor_bb(phi, opnd_tor);
    if (pred) {
      if (obj.Bb()) {
        Sp_h()->Append_control_dependency(obj.Bb(), pred, ctx->Dna(), TRUE);
      }
      Sp_h()->Append_data(pred, ctx->Dna(), PATHINFO_BRANCH);
      obj.Set_phi(phi, pred);
    }
    return Check_tag_obj_ud(opnd_tor, obj, ctx);
  }

  SRCPOS_TREENODE* cur_node = Sp_h()->Add_children(tor->Phi_list()->size());
  IDTYPE p_idx = Sp_h()->Cur_idx();

  TAG_OBJ_REP *cur_tor;
  VSYM_TRACKER::VSYM_VECTOR vsym_stack;
  ctx->Tracker()->Save_stack(&vsym_stack);
  for(int phi_idx = 0; phi_idx < phi_size; phi_idx++) {
    CHECK_OBJ opnd_obj(obj);
    cur_tor = tor->Phi_list()->at(phi_idx);
    Sp_h()->Set_cur_node(cur_node, phi_idx);
    Is_Trace(ctx->Tracing(), 
            (TFile, " -%s: Checking tag phi opnd[%d:%d]:", Checker_name(), phi_size, phi_idx));
    Is_Trace_cmd(ctx->Tracing(), tor->Print(TFile, FALSE));
    Is_Trace(ctx->Tracing(), (TFile, "->"));
    Is_Trace_cmd(ctx->Tracing(), cur_tor->Print(TFile, FALSE));
    BB_NODE *pred = Find_tor_bb(phi, cur_tor);
    if (pred) {
      if (opnd_obj.Bb()) {
        Sp_h()->Append_control_dependency(opnd_obj.Bb(), pred, ctx->Dna(), TRUE);
      }
      Sp_h()->Append_data(pred, ctx->Dna(), PATHINFO_BRANCH);
      opnd_obj.Set_phi(phi, pred);
    }
    UD_TRAVELER<TAG_CHECKER> tag_helper(*this, *ctx);
    CHECKER_STATUS sts = Check_tag_obj_ud(cur_tor, opnd_obj, ctx);
    tag_helper.Continue_trav(opnd_obj, sts);
    ctx->Tracker()->Restore_stack(&vsym_stack);
  }
  Sp_h()->Reset_cur_node(cur_node, p_idx);
  Reset_visited(tor);
  return CS_DONE;
}

BOOL
TAG_CHECKER::Check_prog_input(TAG_OBJ_REP *tag_obj_rep, TRAV_CONTEXT *ctx)
{
  Is_True_Ret(tag_obj_rep->Def_attr() == TO_DEF_BY_ENTRY_CHI,
              ("invalid tor def attr"), FALSE);
  // if no caller, set certainty to M
  if(ctx->Dna()->Clby_list()->size() == VAR_INIT_ID) {
    RBC_EVAL_CERTAINTY certainty = REC_MAYBE;
    if (PU_is_mainpu(*(ctx->Dna()->Pu()))) {
      certainty = REC_DEFINITE;
    }
    if (certainty == REC_DEFINITE || (certainty == REC_MAYBE && VSA_Enable_May_Tag)) {
      BOOL res = FALSE;
      TAG_CONF_INFO * info = Rbc_base()->Get_tag_conf_info(tag_obj_rep->Tag_base()->Id());
      if(info != NULL && info->Is_input_set()) {
        Rbc_base()->Rbc_eval_certainty()->push_back(certainty);
        if(Check_type() == CHECK_BY_TAG)
          res = TRUE;  // treat program input as tag on
        else
          res = FALSE; // treat program input as not sanitized

        Is_Trace(ctx->Tracing(),
                (TFile, " -%s : tor is program input, tag set:on ",
                  Checker_name()));
        Is_Trace_cmd(ctx->Tracing(), tag_obj_rep->Print(TFile));
        Add_tag_path(ctx, res);
      }
    }
    return TRUE;
  }
  return FALSE;
}

CHECKER_STATUS
TAG_CHECKER::Check_tag_obj_ud(TAG_OBJ_REP *tag_obj_rep, CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  Is_True_Ret(tag_obj_rep, ("null tag_obj_rep"), CS_DONE);
  Is_Trace(ctx->Tracing(),
          (TFile, "\n -%s: Checking tag U-D by %s: ", Checker_name(),
            tag_obj_rep->Def_attr_str()));
  Is_Trace_cmd(ctx->Tracing(), tag_obj_rep->Print(TFile, TRUE));
  Is_Trace(ctx->Tracing(), (TFile, "\n"));
  if (Check_ud_limit()) {
    Is_Trace(ctx->Tracing(),(TFile, "CS_DONE: exceed UD limit %d\n", TAG_CHECKER_UD_LIMIT));
    return CS_DONE;
  }
  switch(tag_obj_rep->Def_attr()) {
    case TO_DEF_BY_CREATE:
    case TO_DEF_BY_TAG_ATTR:
    {
      STMTREP *defstmt = tag_obj_rep->Defstmt();
      if(defstmt) {
        Sp_h()->Append_data(tag_obj_rep->Defstmt(), ctx->Dna(), PATHINFO_CHI);
        Is_Trace(ctx->Tracing(), (TFile, " def by stmt:\n"));
        Is_Trace_cmd(ctx->Tracing(), ctx->Vsa()->Print_sr(defstmt, TFile));
      }
      if (defstmt && defstmt == obj.Stmtrep()) {
        // tag is created on check stmt; the tag only valid on chi afterwards
        Is_Trace(ctx->Tracing(), (TFile, "CS_DONE: tag def by check stmt"));
        return CS_DONE;
      }
      switch(Check_type()) {
        case CHECK_BY_TAG:
        {
          BOOL check_result = tag_obj_rep->Tag_on();
          Add_tag_path(ctx, check_result);
        }
        break;
        case CHECK_BY_ATTR:
        case CHECK_BY_TAG_ATTR:
        {
          Is_True_Ret(Attr_id() != TAG_INVALID_ID, ("attr id is not set"), CS_DONE);
          if(tag_obj_rep->Def_attr() == TO_DEF_BY_TAG_ATTR &&
             tag_obj_rep->Def_attr_id() != Attr_id()) {
            // not same attr, continue tag U-D
            tag_obj_rep = tag_obj_rep->Deftor();
            return Check_tag_obj_ud(tag_obj_rep, obj, ctx);
          }
          BOOL check_result = tag_obj_rep->Is_set_tag_attr(Attr_id());
          Add_tag_path(ctx, check_result);
        }
        break;
        default:
          Is_True_Ret(FALSE, ("invalid tag check type"), CS_DONE);
      }
    }
    break;
    case TO_DEF_BY_COPY:
      while(tag_obj_rep && (tag_obj_rep->Def_attr() == TO_DEF_BY_COPY)) {
        STMTREP *defstmt = tag_obj_rep->Defstmt();
        if(defstmt) {
          Sp_h()->Append_data(defstmt, ctx->Dna(), PATHINFO_COPY);
          obj.Set_stmtrep(defstmt);
        }
        Is_Trace(ctx->Tracing(), 
                (TFile, "\n -%s: Checking tag U-D by DEF_BY_COPY:", Checker_name()));
        Is_Trace_cmd(ctx->Tracing(), tag_obj_rep->Print(TFile, FALSE));
        Is_Trace(ctx->Tracing(), (TFile, "->"));
        tag_obj_rep = tag_obj_rep->Deftor();
        Is_Trace_cmd(ctx->Tracing(), tag_obj_rep->Print(TFile, FALSE));
        Is_Trace(ctx->Tracing(), (TFile, " def by stmt:\n"));
        if(defstmt) {
          Is_Trace_cmd(ctx->Tracing(), ctx->Vsa()->Print_sr(defstmt, TFile));
        }
      }
      return Check_tag_obj_ud(tag_obj_rep, obj, ctx);
    break;
    case TO_DEF_BY_PHI:
      return Check_tag_phi(tag_obj_rep, obj, ctx);
    break;
    case TO_DEF_BY_ENTRY_CHI:
    {
      STMTREP *defstmt = tag_obj_rep->Defstmt();
      if(defstmt) {
        Sp_h()->Append_data(defstmt, ctx->Dna(), PATHINFO_COPY);
      }
      if(Check_prog_input(tag_obj_rep, ctx)) {
        return CS_DONE;
      }
    }
    case TO_DEF_BY_CHI:
    case TO_DEF_BY_SE:
      switch(tag_obj_rep->Obj_kind()) {
        case TAG_KIND_VAR:
          obj.Update_var(tag_obj_rep->Coderep(), tag_obj_rep->Defstmt());
          return Ret_cr_status(obj.Coderep());
        case TAG_KIND_HOR:
        {
          CODEREP *cr = tag_obj_rep->Hor_cr();
          Is_True_Ret(cr, ("null hor cr"), CS_DONE);
          obj.Update_var(cr, tag_obj_rep->Defstmt());
          return Ret_cr_status(cr);
        }
        case TAG_KIND_VOR:
        {
          VSYM_OBJ_REP *vor = tag_obj_rep->Vsymobjrep();
          ctx->Tracker()->Clear();
          ctx->Tracker()->Push(vor->Vsym_obj()->Fld_rep_ptr());
          obj.Update_vsym(vor, tag_obj_rep->Defstmt());
          return CS_VSYM_UD;
        }
        default:
          Is_True_Ret(FALSE, ("Invalid tor Obj_kind()"), CS_DONE);
      }
    break;
    default:
      Is_True_Ret(FALSE, ("invalid tor attribute"), CS_DONE);
  }
  return CS_DONE;
}

CHECKER_STATUS
TAG_CHECKER::Check_tag(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  Is_True(obj.Is_var(), ("Tag: check obj is not var"));
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  TAG_OBJ_REP *tag_obj_rep = NULL;
  if (VSA_Enable_TAG_OLD) {
    TAGOKIND kind;
    TOR_LIST_OLD *tor_list = ctx->Vsa()->Find_tor_list_from_cr(sr, cr, kind);
    tag_obj_rep = tor_list ? tor_list->Find(Tag_id()) : NULL;
  } else {
    TOR_LIST *tor_list = ctx->Vsa()->Find_tor_list_from_cr(sr, cr, TRUE);
    tag_obj_rep = tor_list ? tor_list->Find(Tag_id()) : NULL;
  }
  if(Check_type() == CHECK_BY_TAG || Check_type() == CHECK_BY_TAG_ATTR) {
    if(tag_obj_rep) {
      return Check_tag_obj_ud(tag_obj_rep, obj, ctx);
    }
  } else {
    // Need to iterate all tor, for each start a new checker?
  }
  BOOL res = FALSE;
  if(Check_type() == CHECK_BY_TAG)
    res = FALSE; // treat no tag as off
  else
    res = TRUE;  // no tag obj rep, treat as sanitized
  Is_Trace(ctx->Tracing(), (TFile, "@@@@@%s : No Tag obj found\n", Checker_name()));
  Add_tag_path(ctx, res);
  return CS_DONE;
}

// ====================================================================
// TAG_CHECKER::Check_coderep<CK_CONST>
//   Check tag if coderep is const
// ====================================================================
template<> CHECKER_STATUS
TAG_CHECKER::Check_coderep<CK_CONST>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_CONST, ("not const"));
  Set_const_side_effect(obj.Coderep(), ctx);
  return CS_DONE;
}

// ====================================================================
// TAG_CHECKER::Check_coderep<CK_RCONST>
//   Check tag if coderep is RCONST
// ====================================================================
template<> CHECKER_STATUS
TAG_CHECKER::Check_coderep<CK_RCONST>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_RCONST, ("not rconst"));
  Set_const_side_effect(obj.Coderep(), ctx);
  return CS_DONE;
}


// ====================================================================
// TAG_CHECKER::Check_coderep<CK_OP>
//   Check tag if coderep is OP
// ====================================================================
template<> CHECKER_STATUS inline
TAG_CHECKER::Check_coderep<CK_OP>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  Is_True(cr->Kind() == CK_OP, ("not op"));
  if(cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL) {
    obj.Update_var(cr->Opnd(0));
    return CS_OP;
  }
  if (obj.Stmtrep() && OPERATOR_is_call(obj.Stmtrep()->Opr()) &&
      (cr = Find_ilod_base(cr)) != NULL) {
    obj.Update_var(cr);
    return Check_tag(obj, ctx);
  }
  Is_Trace(ctx->Tracing(), (TFile, "%s: cr op not supported yet, return CS_DONE\n", Checker_name()));
  return CS_DONE;
}

// ====================================================================
// TAG_CHECKER::Check_coderep<CK_LDA>
//   Check tag if coderep is LDA
// ====================================================================
template<> CHECKER_STATUS inline
TAG_CHECKER::Check_coderep<CK_LDA>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_LDA, ("not lda"));
  return Check_tag(obj, ctx);
}

// ====================================================================
// TAG_CHECKER::Check_coderep<CK_VAR>
//   Check tag if coderep is VAR
// ====================================================================
template<> CHECKER_STATUS
TAG_CHECKER::Check_coderep<CK_VAR>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_VAR, ("not var"));
  if(PU_java_lang(Get_Current_PU()) &&
     Set_const_side_effect(obj.Coderep(), ctx)) {
    return CS_DONE;
  }
  return Check_tag(obj, ctx);
}

// ====================================================================
// TAG_CHECKER::Check_coderep<CK_VAR>
//   Check tag if coderep is IVAR
// ====================================================================
template<> CHECKER_STATUS
TAG_CHECKER::Check_coderep<CK_IVAR>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_IVAR, ("not ivar"));
  return Check_tag(obj, ctx);
}

// ====================================================================
// TAG_CHECKER::Check_stmtrep<OPR_INTRINSIC_CALL>
// ====================================================================
template<> CHECKER_STATUS
TAG_CHECKER::Check_stmtrep<OPR_INTRINSIC_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  Is_True(sr->Opr() == OPR_INTRINSIC_CALL, ("not intrinsic call"));
  INTRINSIC intrn = sr->Rhs()->Intrinsic();
  if(intrn == INTRN_CHECK_CAST) {
    CODEREP *rhs = sr->Rhs();
    Is_True(rhs->Kid_count() == 2, ("bad rhs kids"));
    Is_True(rhs->Opnd(1)->Kind() == CK_IVAR &&
            rhs->Opnd(1)->Opr() == OPR_PARM, ("bad first kid"));
    obj.Update_var(rhs->Opnd(1)->Ilod_base());
    return CS_OP;
  }
  return CS_DONE;
}

// ====================================================================
// TAG_CHECKER::Check_stmtrep<OPR_CALL>
// ====================================================================
template<> CHECKER_STATUS
TAG_CHECKER::Check_stmtrep<OPR_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Stmtrep()->Opr() == OPR_CALL, ("not call"));
  return CS_CONT;
}

// ====================================================================
// TAG_CHECKER::Check_stmtrep<OPR_ICALL>
// ====================================================================
template<> CHECKER_STATUS
TAG_CHECKER::Check_stmtrep<OPR_ICALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  Is_True(sr->Opr() == OPR_ICALL, ("not icall"));
  RNA_NODE *rna = ctx->Vsa()->Sr_2_rna(sr);
  UINT64 visited_key = ctx->Rna_visited_key(rna, TD_DOWN);
  if(rna && ctx->Visited(rna, visited_key)) {
    return CS_DONE;
  }
  RBC_OP get_ops[] = { RBC_OP_SET_FUNC_COLL_GET, RBC_OP_SET_FUNC_MAP_GET };
  if(rna->Is_container_op() &&
     ctx->Ipsa()->Rna_has_rbc_ops(rna, get_ops, RBC_OPS_CNT(get_ops))) {
    BOOL found = FALSE;
    EVAL_RET ret = Rbc_base()->Eval__mvsa_container_op(ctx->Dna(), rna, Loc_pool(), NULL, FALSE);
    if(ret.first == LIST_PTR) {
      LIST_OBJS *cands = (LIST_OBJS *)ret.second;
      if(cands) {
        Check_value_objs(*cands, sr, ctx);
        found = TRUE;
      }
    } else if(ret.first == MAP_PTR) {
      MAP_OBJS *cands = (MAP_OBJS *) ret.second;
      if(cands) {
        Check_value_objs(*cands, sr, ctx);
        found = TRUE;
      }
    }
    if(!found) {
      // if no def found, check base_cr is tainted
      Is_Trace(ctx->Tracing(), (TFile, ("Symbolic eval for collection.get(): no value found, continue check base cr\n")));
      CODEREP *base_cr = rna->Get_arg_with_flag(REF_BASE);
      Is_True_Ret(base_cr != NULL, 
                  ("TAT_CHECKER ERROR: null base cr found for collection get"),
                  CS_DONE);
      obj.Update_var(base_cr);
      return CS_OP;
    }
    return CS_DONE;
  }
  // reset rna visited if continue with regular callee checking
  if(rna) {
    ctx->Reset_visited(visited_key, TRUE);
  }
  return CS_CONT;
}

template<> CHECKER_STATUS
TAG_CHECKER::Check_stmtrep<OPR_OPT_CHI>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  return CS_CONT;
}
// ====================================================================
// TAG_CHECKER::Check_vsym_obj
// ====================================================================
CHECKER_STATUS
TAG_CHECKER::Check_vsym_obj(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  VSYM_OBJ_REP *vor = obj.Vor();
  TAG_OBJ_REP *tag_obj_rep = NULL;
  Is_True_Ret(vor != NULL && vor->Attr() != ROR_DEF_BY_NONE,
              ("invalid vor"), CS_DONE);
  if (!VSA_Enable_TAG_OLD) {
    TOR_LIST *tor_list = vor->Tor_list<TOR_LIST>();
    tag_obj_rep = tor_list ? tor_list->Find(Tag_id()) : NULL;
    if (tag_obj_rep) {
      if (tag_obj_rep->Def_attr() != TO_DEF_BY_CHI &&
          tag_obj_rep->Def_attr() != TO_DEF_BY_ENTRY_CHI &&
          tag_obj_rep->Def_attr() != TO_DEF_BY_SE) {
        return Check_tag_obj_ud(tag_obj_rep, obj, ctx);
      } else {
        return CS_CONT;
      }
    } else {
      return CS_DONE;
    }
  }

  if (vor->Attr() == ROR_DEF_BY_PHI ||
      vor->Attr() == ROR_DEF_BY_HORPHI)
    return CS_CONT;

  Is_True_Ret(vor->Attr() == ROR_DEF_BY_CHI ||
              vor->Attr() == ROR_DEF_BY_ISTORE ||
              vor->Attr() == ROR_DEF_BY_COPY,
              ("unsupported vor attr"), CS_DONE);

  STMTREP *sr = vor->Stmt_def();
  
  if (VSA_Enable_TAG_OLD) {
    TOR_LIST_OLD *tor_list = vor->Tor_list<TOR_LIST_OLD>();
    tag_obj_rep = tor_list ? tor_list->Find(Tag_id()) : NULL;
  } else {
    TOR_LIST *tor_list = vor->Tor_list<TOR_LIST>();
    tag_obj_rep = tor_list ? tor_list->Find(Tag_id()) : NULL;
  }
  // reach to top vor, try to go TAG-UD first, then check VAR-UD
  if (Check_type() == CHECK_BY_TAG &&
      tag_obj_rep != NULL &&
      tag_obj_rep->Def_attr() != TO_DEF_BY_CHI &&
      tag_obj_rep->Def_attr() != TO_DEF_BY_ENTRY_CHI &&
      tag_obj_rep->Def_attr() != TO_DEF_BY_SE)
    return Check_tag_obj_ud(tag_obj_rep, obj, ctx);
  else if(sr == NULL) {
    Is_Trace(ctx->Tracing(), (TFile, " -%s: vo%dv%d def by null sr chi, switch back to VAR-UD\n",
                                     Checker_name(),
                                     vor->Vsym_obj()->Id(), vor->Version()));
    if (obj.Vor_cr()) {
      obj.Update_var(obj.Vor_cr());
      return Check_tag(obj, ctx);
    }
  } else if((vor->Is_entry_chi() || sr->Opr() == OPR_OPT_CHI) &&
            ctx->Dna()->Clby_list()->size() == VAR_INIT_ID) {
    Is_Trace(ctx->Tracing(), (TFile, " -%s: vo%dv%d def by top entry chi, switch back to VAR-UD\n",
                                     Checker_name(),
                                     vor->Vsym_obj()->Id(), vor->Version()));
    if (obj.Vor_cr()) {
      obj.Update_var(obj.Vor_cr());
      return Check_tag(obj, ctx);
    }
  }
  // continue check vsym chi ud by UD_TRAVELER
  return CS_CONT;
}

void
TAG_CHECKER::Add_rbc_plist(TRAV_CONTEXT *ctx, BOOL is_true_list)
{
  SPOS_CANDS *list = is_true_list ? _true_list : _false_list;
  SRCPOS_HANDLE_VEC *rbc_list = is_true_list ? Rbc_base()->Plist_true() : Rbc_base()->Plist_false();
  if(list) {
    for(int idx = 0; idx < list->size(); idx++) {
      CURR_SPOS curr_spos = list->at(idx);
      Sp_h()->Reset_cur_node(curr_spos.first, curr_spos.second);
      SRCPOS_HANDLE *cloned_sp = Sp_h()->Clone();  // clone twice to get right order
      rbc_list->push_back(cloned_sp->Clone());
    }
  }
}

// ====================================================================
// RBC_BASE::Check_tag
//   Test driver for TAG checker
// ====================================================================
BOOL
RBC_BASE::Check_tag(CHECK_OBJ &obj, TRAV_CONTEXT &ctx, TAG_CHECK_TYPE type, TAG_BASE *tag_base,
                    SRCPOS_HANDLE *sp_h,  IDTYPE attr_id)
{
  STMTREP *sr = obj.Stmtrep();
  SRCPOS cur_line = sr ? sr->Linenum() : 0 ;
  if(type == CHECK_BY_TAG) {
    Is_Trace(Get_Trace(TP_CHECKER, CHK_TAG_TRACE_FLAG),
             (TFile, "%sCheck tag [%s] at line %d in func %s\n%s",
              DBar, tag_base->Tag_name(), SRCPOS_linenum(cur_line),
              ctx.Dna()->Fname(), DBar));
  } else {
    Is_Trace(Get_Trace(TP_CHECKER, CHK_TAG_TRACE_FLAG),
             (TFile, "%sCheck tag [%s.%s]:[%d.%d] at line %d in func %s\n%s",
                         DBar, tag_base->Tag_name(), Get_tag_attr_name(attr_id),
                         tag_base->Id(), attr_id, SRCPOS_linenum(cur_line),
                         ctx.Dna()->Fname(), DBar));
  }

  // use ctx.Spos_pool(), the memory will be poped after TRAV_CONTEXT de-construct
  if (sp_h == NULL)
    sp_h = CXX_NEW(SRCPOS_HANDLE(obj.Coderep(), obj.Stmtrep(),
                                 ctx.Dna(), ctx.Spos_pool()),
                   ctx.Spos_pool());

  sp_h->Set_orig_stname(
    sp_h->Find_cr_stname(obj.Coderep(), obj.Stmtrep(), ctx.Dna(), TRUE, TRUE));
  VSYM_TRACKER::VSYM_VECTOR vsym_stack;
  ctx.Tracker()->Save_stack(&vsym_stack);
  ctx.Tracker()->Clear();
  TAG_CHECKER tag_checker(this, ctx, tag_base->Id(),
                          attr_id, type, sp_h, NULL);
  UD_TRAVELER<TAG_CHECKER> tag_helper(tag_checker, ctx);
  tag_helper.Continue_trav(obj, CS_OP);
  Is_Trace(Get_Trace(TP_CHECKER, CHK_TAG_TRACE_FLAG),
           (TFile, "%sEND Check tag %s in func %s\n%s",
                       DBar, tag_base->Tag_name(), ctx.Dna()->Fname(), DBar));
  BOOL ret = tag_checker.Check_res();
  tag_checker.Add_rbc_plist(&ctx, TRUE);  // add true path
  tag_checker.Add_rbc_plist(&ctx, FALSE); // add false path
  ctx.Tracker()->Restore_stack(&vsym_stack);
  return ret;
}

// ====================================================================
// RBC_BASE::Check_tag
//   Test driver for TAG checker
// ====================================================================
BOOL
RBC_BASE::Check_tag(DNA_NODE *dna, STMTREP *sr, CODEREP *cr,
                    TAG_CHECK_TYPE type, TAG_BASE *tag_base,
                    SRCPOS_HANDLE *sp_h, IDTYPE attr_id)
{
  CONTEXT_SWITCH check_ctx(dna);
  TRAV_CONTEXT trav_ctx(dna->Comp_unit(), sr, cr);
  CHECK_OBJ obj(cr, sr);
  return Check_tag(obj, trav_ctx, type, tag_base, sp_h, attr_id);
}
