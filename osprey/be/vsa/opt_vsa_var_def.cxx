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
// Module: opt_vsa_get_def.cxx
//
// ====================================================================
//

// ====================================================================
// VSA Get Define
//
//
// Algorithm:
//
// ====================================================================

#include "defs.h"
#include "erglob.h"
#include "opt_defs.h"
#include "opt_bb.h"
#include "opt_main.h"
#include "opt_vsa_util.h"
#include "opt_vsa_eh.h"
#include "opt_vsa.h"
#include "opt_dna.h"
#include "opt_addr_util.h"
#include "opt_vsa_rbc.h"
#include "opt_vsa_checker.h"
#include "opt_vsa_var_def.h"

#if 0
void
VALUE_TMPL::Print(FILE* fp) const
{
  fprintf(fp, "VALUE_OBJ: cr(%d), sr(%d), fun(%s)\n",
          Val()->Coderep_id(), Sr()->Stmtrep_id(), Cu()->Dna()->Fname());
}
#endif
void
MAP_ENTRY::Print(FILE* fp) const
{
  fprintf(fp, "[cr%d%s]=cr%d|sr%d",
          Key()->Coderep_id(), Key_str() ? Key_str() : "",
          Value()->Coderep_id(),
          Sr()->Stmtrep_id());
}

// =============================================================================
// VAR_DEF_HELPER::VAR_DEF_HELPER
// =============================================================================
VAR_DEF_HELPER::VAR_DEF_HELPER(CODEREP *cr, STMTREP *sr, COMP_UNIT *cu,
                               UINT32 kind, BOOL tracing, SRCPOS_HANDLE *sp_h):
  _pool("Var_def_pool", FALSE),
  _kind(kind), _srcpos_on(FALSE),
  _def_srcpos_cand_on(FALSE), _follow_ctor_ud(FALSE),
  _local_def(NULL), _srcpos_h(sp_h)
{
  _ctx = CXX_NEW(TRAV_CONTEXT(cu, sr, cr, tracing), Mem_pool());
  if (tracing)
    _ctx->Set_Tracing(Get_Trace(TP_CHECKER, CHK_VAR_DEF_TRACE_FLAG));
  _def_info_vec = CXX_NEW(DEF_INFO_VEC(mempool_allocator<DEF_INFO_VEC *>(Mem_pool())), Mem_pool());
  if (kind == FOR_CONTAINER_EVAL) {
    _srcpos_on = TRUE;
    _def_srcpos_cand_on = TRUE;
    _follow_ctor_ud = FALSE;
    // ATTENTION: can't append the entry sr and cr to SRCPOS_HANDLER
    if (_srcpos_h == NULL)
      _srcpos_h = CXX_NEW(SRCPOS_HANDLE(cu->Dna(), Mem_pool()), Mem_pool());
  } else {
    // add entry sr and cr
    if (_srcpos_h == NULL)
      _srcpos_h = CXX_NEW(SRCPOS_HANDLE(cr, sr, cu->Dna(), Mem_pool(), cu->Dna()->Comp_unit()->Vsa()), Mem_pool());
  }
}

VAR_DEF_HELPER::~VAR_DEF_HELPER()
{
  CXX_DELETE(_ctx, Mem_pool());
}

static inline BOOL
Traverse_implicit_ud(VAR_DEF_HELPER *helper, RNA_NODE *rna, CHECK_OBJ &obj, TRAV_CONTEXT *ctx, CHECKER_STATUS *status)
{
  CODEREP *cr = obj.Is_var() ? obj.Coderep() : obj.Vor_cr();
  if (cr == NULL) return FALSE;
  VSYM_TRACKER::VSYM_VECTOR vsym_stack;
  ctx->Tracker()->Save_stack(&vsym_stack);
  SRC_VECTOR *srcs = ctx->Ipsa()->Get_assign_src(rna, cr);
  if(srcs) {
    for(int i = 0; i < srcs->size(); i++) {
      CODEREP *src = rna->Get_arg((*srcs)[i]);
      if(src) {
        Is_Trace(ctx->Tracing(),
                  (TFile,
                  "%sApply implicit assign: tgt %d def by src %d Start a new Var def trav\n%s",
                  SBar, cr->Coderep_id(), src->Coderep_id(), SBar));
        CHECK_OBJ assign_obj(src, rna->Callstmt());
        ctx->Vsa()->Var_def_trav_helper(helper, assign_obj);
        Is_Trace(ctx->Tracing(),
                  (TFile,
                  "%sEnd of Apply implicit assign: tgt %d def by src %d\n%sAdd to parent var def list:\n",
                  SBar, cr->Coderep_id(), src->Coderep_id(), SBar));
        ctx->Tracker()->Restore_stack(&vsym_stack);
      }
    }
    *status = CS_DONE;
    return TRUE;
  }
  return FALSE;
}

class VAR_DEF_TRAV : public SPOS_BASE {
private:
  VAR_DEF_HELPER *_helper;

  void        Set_local_def(CODEREP *cr)          { _helper->Set_local_def(cr);  }
  VAR_DEF_HELPER *Helper()                        { return _helper; }
  UINT32      Kind()                              { return _helper->Kind();      }
  BOOL        Srcpos_on()                         { return _helper->Srcpos_on(); }
  BOOL        Tracing()                           { return _helper->Ctx().Tracing(); }
  BOOL        Set_check_kind(CHECKER_SUSPECT sus) { return FALSE;                }

public:

  enum { SUSPECT = CS_VAR_DEF};
  enum { USE_SRCPOS  = FALSE };
  // TODO: need to follow eh path?
  enum { FOLLOW_EH = FALSE };

public:
  // Constructor
  VAR_DEF_TRAV(VAR_DEF_HELPER *helper)
    :SPOS_BASE(helper->Ctx()), _helper(helper) {}

  VAR_DEF_TRAV(VAR_DEF_HELPER *helper, SRCPOS_HANDLE *sp_h)
    :SPOS_BASE(sp_h), _helper(helper) {}


  const char* Checker_name() const  { return "VAR_DEF"; }

  // Check_heap_obj
  CHECKER_STATUS
  Check_heap_obj(CHECK_OBJ &obj, TRAV_CONTEXT* ctx) { return CS_CONT; }

  // Check_container_op
  CHECKER_STATUS
  Check_container_op(CHECK_OBJ &obj, TRAV_CONTEXT* ctx);

  void        Add_def_info(DNA_NODE *dna, CHECK_OBJ &obj)
  {
    CODEREP *cr = obj.Is_var() ? obj.Coderep() : obj.Vor_cr();
    if (cr == NULL) return;
    CODEREP *formal_cr = cr;
    IDTYPE param_id = INVALID_VAR_IDX;
    if(cr->Kind() == CK_VAR) {
      param_id = dna->Is_param(cr);
    }
    if(param_id != INVALID_VAR_IDX)
      formal_cr = dna->Get_param_cr(param_id);
    Helper()->Add_def_info(dna, formal_cr, obj.Stmtrep(), Sp_h()->Cur_node(), Sp_h()->Cur_idx());
    Is_Trace(Tracing(),
             (TFile, "*Add def cr %d in Dna %s\n",
              formal_cr->Coderep_id(), dna->Fname()));
  }

  BOOL        Has_functional_callee(RNA_NODE *rna, TRAV_CONTEXT *ctx)
  {
    if (rna == NULL) return FALSE;
    if (rna->Callee_list().size() == 0) {
      return FALSE;
    }
    for(CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
        it != rna->Callee_list().end(); it++) {
      DNA_NODE* callee = ctx->Ipsa()->Get_dna(it->Callee());
      if (callee == NULL) {
        continue;
      }
      if (!callee->Non_functional()) {
        return TRUE;
      }
    }
    return FALSE;
  }

public:
  // Check_coderep
  // call back to check coderep
  template<CODEKIND  _KIND> CHECKER_STATUS
  Check_coderep(CHECK_OBJ &obj, TRAV_CONTEXT* ctx);

  // Check_stmtrep
  // call back to check stmtrep
  template<OPERATOR _OPR> CHECKER_STATUS
  Check_stmtrep(CHECK_OBJ &obj, TRAV_CONTEXT* ctx);

  // Check_vsym_obj
  CHECKER_STATUS
  Check_vsym_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx) { return CS_CONT; }

  void
  Add_target(UINT32 file_idx, ST_IDX idx) { }
};

// ====================================================================
// VAR_DEF_TRAV::Check_container_op
//   Check container op if kind is FOR_CONTAINER_EVAL and rna has
// CONTAINER_OP flag
// ====================================================================
CHECKER_STATUS
VAR_DEF_TRAV::Check_container_op(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  RNA_NODE *rna = ctx->Vsa()->Sr_2_rna(sr);
  Is_True_Ret(rna && rna->Is_container_op(),
              ("VAR_DEF_TRAV: OPR_CALL empty rna"), CS_DONE);
  RBC_OP get_ops[] = RBC_OP_CONTAINER_GET;
  if (rna->Is_container_op() &&
      ctx->Ipsa()->Rna_has_rbc_ops(rna, get_ops, RBC_OPS_CNT(get_ops))) {
    CODEREP *base_cr = rna->Get_arg_with_flag(REF_BASE);
    Is_True_Ret(base_cr != NULL,
                ("VAR_DEF_TRAV ERROR: null base cr found for collection get"),
                CS_DONE);
    VSYM_FLD_REP any_vfr(FLD_K_ANY, 0, 0);
    VSYM_OBJ_REP *vor = ctx->Vsa()->Find_vor_mu_vor(sr, base_cr, &any_vfr);
    if(vor) {
      ctx->Tracker()->Push(vor->Vsym_obj()->Fld_rep_ptr());
      obj.Update_vsym(vor, sr, base_cr);
      return CS_VSYM_UD;
    } else {
      Is_Trace(ctx->Tracing(), (TFile, "WARN: vor not created on cr%d stmt:\n", base_cr->Coderep_id()));
      Is_Trace_cmd(ctx->Tracing(), ctx->Vsa()->Print_sr(sr, TFile));
      return CS_DONE;
    }
  }
  RBC_OP upd_ops[] = RBC_OP_CONTAINER_UPDATE;
  if (rna->Is_container_op() &&
      ctx->Ipsa()->Rna_has_rbc_ops(rna, upd_ops, RBC_OPS_CNT(upd_ops))) {
    if (obj.Is_vsym()) {
      VSYM_OBJ_REP *vor = obj.Vor();
      if(vor) {
        CVOR *opnd = ctx->Vsa()->Find_vor_chi_opnd(sr, vor);
        if(opnd != NULL) {
          obj.Update_vsym(opnd->first, sr, opnd->second);
          return CS_VSYM_UD;
        }
      }
    }
    return CS_DONE;
  }
  return CS_DONE;
}

// ====================================================================
// VAR_DEF_TRAV::Check_coderep<CK_LDA>
//   Check VAR_DEF if coderep is LDA
// ====================================================================
template<> CHECKER_STATUS inline
VAR_DEF_TRAV::Check_coderep<CK_LDA>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_LDA, ("not lda"));
  Is_Trace(ctx->Tracing(), (TFile, "--VAR_DEF: Done with LDA\n"));
  Add_def_info(ctx->Dna(), obj);
  return CS_DONE;
}

// ====================================================================
// VAR_DEF_TRAV::Check_coderep<CK_LDA>
//   Check VAR_DEF if coderep is CONST
// ====================================================================
template<> CHECKER_STATUS inline
VAR_DEF_TRAV::Check_coderep<CK_CONST>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_CONST, ("not const"));
  Is_Trace(ctx->Tracing(), (TFile, "--VAR_DEF: Done with const %lld\n", obj.Coderep()->Const_val()));
  Add_def_info(ctx->Dna(), obj);
  return CS_DONE;
}

// ====================================================================
// VAR_DEF_TRAV::Check_coderep<CK_LDA>
//   Check VAR_DEF if coderep is RCONST
// ====================================================================
template<> CHECKER_STATUS inline
VAR_DEF_TRAV::Check_coderep<CK_RCONST>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_RCONST, ("not rconst"));
  Is_Trace(ctx->Tracing(), (TFile, "--VAR_DEF: Done with rconst\n"));
  return CS_DONE;
}

// ====================================================================
// VAR_DEF_TRAV::Check_coderep<CK_LDA>
//   Check VAR_DEF if coderep is OP
// ====================================================================
template<> CHECKER_STATUS inline
VAR_DEF_TRAV::Check_coderep<CK_OP>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_OP, ("not op"));
  return CS_DONE;
}

// ====================================================================
// VAR_DEF_TRAV::Check_coderep<CK_LDA>
//   Check VAR_DEF if coderep is VAR
// ====================================================================
template<> CHECKER_STATUS
VAR_DEF_TRAV::Check_coderep<CK_VAR>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  Is_True(cr->Kind() == CK_VAR, ("not var"));
  ST *st = ctx->Comp_unit()->Opt_stab()->St(cr->Aux_id());
  if(st && ST_is_const_var(st)) {
    Add_def_info(ctx->Dna(), obj);
    Set_local_def(cr);
    return CS_DONE;
  }
  if(cr->Is_flag_set(CF_DEF_BY_CHI)) {
    Set_local_def(cr);
  }
  // continue with the VAR UD
  return CS_VAR_UD;
}

// ====================================================================
// VAR_DEF_TRAV::Check_coderep<CK_LDA>
//   Check VAR_DEF if coderep is IVAR
// ====================================================================
template<> CHECKER_STATUS
VAR_DEF_TRAV::Check_coderep<CK_IVAR>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  Is_True(cr->Kind() == CK_IVAR, ("not ivar"));
  if (cr->Opr() == OPR_PARM) {
    obj.Update_var(cr->Ilod_base());
    return CS_OP;
  }
  Is_Trace(ctx->Tracing(), (TFile, " -VAR_DEF: Check ivar U-D:\n"));
  Is_Trace_cmd(ctx->Tracing(), obj.Stmtrep()->Print(TFile));
  return CS_IVAR_UD;

}

// ====================================================================
// VAR_DEF_TRAV::Check_stmtrep<OPR_INTRINSIC_CALL>
//   Check VAR_DEF if stmtrep is INTRINSIC_CALL
// ====================================================================
template<> CHECKER_STATUS
VAR_DEF_TRAV::Check_stmtrep<OPR_INTRINSIC_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  Is_True(sr->Opr() == OPR_INTRINSIC_CALL, ("not intrinsic call"));
  RNA_NODE *rna = ctx->Vsa()->Sr_2_rna(sr);
  INTRINSIC intrn = sr->Rhs()->Intrinsic();
  if (intrn == INTRN_ALLOC_OBJ ||
      (sr->Call_flags() & WN_CALL_IS_CONSTRUCTOR)) {
    CODEREP* rhs = sr->Rhs();
    Is_True(rhs->Kid_count() >= 1, ("bad rhs kids"));
    Is_True(rhs->Opnd(0)->Kind() == CK_IVAR && rhs->Opnd(0)->Opr() == OPR_PARM, ("bad first kid"));
    obj.Update_var(rhs);
    Add_def_info(ctx->Dna(), obj);
    if (Helper()->Follow_ctr_ud()) {
      Is_True_Ret(rna, ("VAR_DEF_TRAV: OPR_CALL empty rna"), CS_DONE);
      if (rna == NULL) {
        return CS_DONE;
      }
      UINT64 visited_key = ctx->Rna_visited_key(rna, TD_DOWN);
      if (ctx->Visited(rna, visited_key)) {
        return CS_DONE;
      }
      if (rna->Is_flag_set(RBC_SE_IMPLICIT_ASSIGN)) {
        CHECKER_STATUS status;
        BOOL visited = Traverse_implicit_ud(Helper(), rna, obj, ctx, &status);
        if (visited) {
          return status;
        }
      }
      ctx->Reset_visited(visited_key, TRUE);
    }
    return CS_DONE;
  } else if(intrn == INTRN_CHECK_CAST) {
    CODEREP *rhs = sr->Rhs();
    Is_True(rhs->Kid_count() == 2, ("bad rhs kids"));
    Is_True(rhs->Opnd(1)->Kind() == CK_IVAR && rhs->Opnd(1)->Opr() == OPR_PARM, ("bad first kid"));
    obj.Update_var(rhs->Opnd(1)->Ilod_base());
    return CS_OP;
  }
  if (Kind() == FOR_CONTAINER_EVAL && rna->Is_container_op()) {
    return Check_container_op(obj, ctx);
  }
  // TODO: other intrinsics
  return CS_DONE;
}


template<> CHECKER_STATUS
VAR_DEF_TRAV::Check_stmtrep<OPR_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  RNA_NODE *rna = ctx->Vsa()->Sr_2_rna(sr);
  Is_True_Ret(rna, ("VAR_DEF_TRAV: OPR_CALL empty rna"), CS_DONE);
  if (rna == NULL) {
    return CS_DONE;
  }
  UINT64 visited_key = ctx->Rna_visited_key(rna, TD_DOWN);
  if(ctx->Visited(rna, visited_key)) {
    return CS_DONE;
  }
  BOOL is_get_fld_id = ((sr->Opr() == OPR_CALL) &&
                        strcmp(ST_name(sr->St()), "GetFieldID") == 0);
  if(is_get_fld_id && obj.Is_var()) {
    Is_True(Kind() != FOR_GENERAL, ("invalid get field kind"));
    CODEREP *cr = obj.Coderep();
    Is_True(cr, ("null cr"));
    if(Kind() == FOR_FLD_NAME) {
      obj.Update_var(cr->Opnd(2)->Ilod_base());
      return CS_OP;
    } else if(Kind() == FOR_FLD_CLASS) {
      obj.Update_var(cr->Opnd(1)->Ilod_base());
      return CS_OP;
    } else {
      return CS_DONE;
    }
  } else if(Kind() == FOR_CONTAINER_EVAL) {
    if(rna->Callee_list().size() == 0) {
      if(OPERATOR_has_sym(sr->Opr()) &&
         PU_is_constructor(Pu_Table[ST_pu(sr->St())])) {
        if(!ctx->Tracker()->Empty() && obj.Is_vsym()) {
          VSYM_OBJ_REP *vor = obj.Vor();
          Is_True_Ret(vor != NULL,
                      ("VAR_DEF_TRAV ERROR: null vor for collection get"),
                      CS_DONE);
          CVOR *opnd = ctx->Vsa()->Find_vor_chi_opnd(sr, vor);
          if(opnd != NULL) {
            obj.Update_vsym(opnd->first, sr, opnd->second);
            return CS_VSYM_UD;
          }
        }
      }
    }
  } else if(rna->Is_flag_set(RBC_SE_IMPLICIT_ASSIGN)) {
    CHECKER_STATUS status;
    BOOL visited = Traverse_implicit_ud(Helper(), rna, obj, ctx, &status);
    if (visited) {
      return status;
    }
  }
  // reset rna visited if continue with regular callee checking
  ctx->Reset_visited(visited_key, TRUE);
  if (!Has_functional_callee(rna, ctx)) {
    Add_def_info(ctx->Dna(), obj);
  }
  return CS_CONT;
}

template<> CHECKER_STATUS
VAR_DEF_TRAV::Check_stmtrep<OPR_ICALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  RNA_NODE *rna = ctx->Vsa()->Sr_2_rna(sr);
  Is_True_Ret(rna, ("VAR_DEF_TRAV: OPR_CALL empty rna"), CS_DONE);
  if (rna == NULL) {
    return CS_DONE;
  }
  UINT64 visited_key = ctx->Rna_visited_key(rna, TD_DOWN);
  if(ctx->Visited(rna, visited_key)) {
    return CS_DONE;
  }
  if(Kind() == FOR_CONTAINER_EVAL && rna->Is_container_op()) {
    return Check_container_op(obj, ctx);
  }
  else if(rna->Is_flag_set(RBC_SE_IMPLICIT_ASSIGN)) {
    CHECKER_STATUS status;
    BOOL visited = Traverse_implicit_ud(Helper(), rna, obj, ctx, &status);
    if (visited) {
      return status;
    }
  }
  // reset rna visited if continue with regular callee checking
  ctx->Reset_visited(visited_key, TRUE);
  if (!Has_functional_callee(rna, ctx)) {
    Add_def_info(ctx->Dna(), obj);
  }
  return CS_CONT;
}

template<> CHECKER_STATUS
VAR_DEF_TRAV::Check_stmtrep<OPR_OPT_CHI>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Stmtrep()->Opr() == OPR_OPT_CHI, ("VAR_DEF: stmt is not chi"));

  if (ctx->Dna()->Clby_list()->size() == VAR_INIT_ID) {
    CODEREP *cr = obj.Is_var() ? obj.Coderep() : obj.Vor_cr();
    if (cr && cr->Kind() == CK_VAR) {
      IDTYPE parm_id = ctx->Dna()->Is_param(cr);
      // Add def info if reach to top function
      if (parm_id != INVALID_VAR_IDX) {
        Add_def_info(ctx->Dna(), obj);
        return CS_DONE;
      }
    }
  }
  return CS_CONT;
}

// ====================================================================
// VAR_DEF_TRAV_WITH_SPOS: var def with srcpos
// ====================================================================
class VAR_DEF_TRAV_WITH_SPOS : public VAR_DEF_TRAV {
public:
  enum { SUSPECT = CS_VAR_DEF};
  enum { USE_SRCPOS  = TRUE };

  // Constructor
  VAR_DEF_TRAV_WITH_SPOS(VAR_DEF_HELPER *helper)
    : VAR_DEF_TRAV(helper, helper->Srcpos()) {}
};

void
VSA::Var_def_trav_helper(VAR_DEF_HELPER *helper, CHECK_OBJ &obj) const
{
  Is_Trace(Get_Trace(TP_CHECKER, CHK_VAR_DEF_TRACE_FLAG),
           (TFile, "%sStart VAR_DEF_TRAV\n%s", DBar, DBar));
  if(helper->Srcpos_on()) {
    VAR_DEF_TRAV_WITH_SPOS var_finder(helper);
    UD_TRAVELER<VAR_DEF_TRAV_WITH_SPOS> check_var_def_travler(var_finder, helper->Ctx());
    check_var_def_travler.Continue_trav(obj, CS_OP);
  } else {
    VAR_DEF_TRAV var_finder(helper);
    UD_TRAVELER<VAR_DEF_TRAV> check_var_def_travler(var_finder, helper->Ctx());
    check_var_def_travler.Continue_trav(obj, CS_OP);
  }
  Is_Trace(Get_Trace(TP_CHECKER, CHK_VAR_DEF_TRACE_FLAG),
           (TFile, "%sEnd VAR_DEF_TRAV\n%s", DBar, DBar));
}
