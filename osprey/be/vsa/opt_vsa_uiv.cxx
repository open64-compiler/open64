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
// Module: opt_vsa_uiv.cxx
//
// ====================================================================
//

// ====================================================================
// VSA UIV CHECKER
//   Check the UIV (Un-Initialized Variable) issue
//
// Algorithm:
//   Start from ILOAD/ISTORE represented by IVAR CODEREP, check if the
// base address (also represented by CODEREP) can be zero or not.
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
#include "vsa_annot.h"
#include "intrn_info.h"

class UIV_CHECKER : public SPOS_BASE {
  typedef CHECKER_TRAVELER<UIV_CHECKER> CHECKER;
private:
  CHECKER            *_checker;
  UINT32              _kind;

private:
  BOOL                Report_vul_error(TRAV_CONTEXT* ctx, STMTREP *sr, CODEREP *cr,
                                       ISSUE_CERTAINTY ic = IC_DEFINITELY);
  BOOL                Is_skip_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);
  BOOL                Is_vor_parm(VSA *vsa, VSYM_OBJ_REP *vor);
  BOOL                Is_initialized_annot(V_ANNOT annot);
  BOOL                Is_bitfield_assign(STMTREP *sr, CODEREP *cr, TRAV_CONTEXT *ctx);
  BOOL                Check_cr_anno(CODEREP *cr, TRAV_CONTEXT *ctx);
  BOOL                Check_vor_anno(VSYM_OBJ_REP *vor, TRAV_CONTEXT *ctx);
  IDTYPE              Get_obj_aux(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);

public:
  // traversal from dereference
  enum { SUSPECT = CS_VAR_USE | CS_DEREFERENCE | CS_VAR_DEF | CS_INIT | CS_VSYM_OBJ };
  // need to check coderep
  enum { ENTITY = TE_CODEREP };
  // need to track SRCPOS
  enum { USE_SRCPOS = TRUE };
  // need to follow eh path
  enum { FOLLOW_EH = TRUE };

public:
  UIV_CHECKER(TRAV_CONTEXT &ctx, CHECKER *checker)
    : SPOS_BASE(ctx), _checker(checker), _kind(FOR_UIV)
  {
    ctx.Set_Tracing(Get_Trace(TP_CHECKER, CHK_UIV_TRACE_FLAG));
  }
  const char              *Checker_name() const    { return "UIV";    }
  void                     Set_kind(UINT32 kind)   { _kind = kind;    }
  const UINT32             Kind()                  { return _kind;    }
  BOOL                     Set_check_kind(CHECKER_SUSPECT suspect) { return FALSE; }

  template<CODEKIND _KIND> CHECKER_STATUS
  Check_coderep(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);

  template<OPERATOR _OPR> CHECKER_STATUS
  Check_stmtrep(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);

  // Check_heap_obj
  CHECKER_STATUS
  Check_heap_obj(CHECK_OBJ &obj, TRAV_CONTEXT* ctx) { return CS_CONT; }

  // Check_vsym_obj
  CHECKER_STATUS
  Check_vsym_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);

  void
  Add_target(UINT32 file_idx, ST_IDX idx) { }
};

BOOL
UIV_CHECKER::Is_initialized_annot(V_ANNOT annot)
{
  if (annot == VANT_UTIL::Empty()) {
    return FALSE;
  }
  BOOL ret = FALSE;
  if (VANT_UTIL::Get(annot, ANT_ZERO) == ANT_YES) {
    ret = TRUE;
  } else if (VANT_UTIL::Get(annot, ANT_CONST) == ANT_YES) {
    ret = TRUE;
  } else if (VANT_UTIL::Get(annot, ANT_WRITE) == ANT_YES) {
    ret = TRUE;
  } else if (VANT_UTIL::Get(annot, ANT_REALLOC) == ANT_YES) {
    ret = TRUE;
  } else if (VANT_UTIL::Get(annot, ANT_GLOBAL) == ANT_YES) {
    ret = TRUE;
  } else if (VANT_UTIL::Get(annot, ANT_RETVAL) == ANT_YES) {
    ret = TRUE;
  }
  return ret;
}

BOOL
UIV_CHECKER::Check_cr_anno(CODEREP *cr, TRAV_CONTEXT *ctx)
{
  if (!ctx->Tracker()->Empty()) {
    return FALSE;
  }
  V_ANNOT annot = Get_cr_annot(ctx->Vsa(), cr);
  BOOL ret = Is_initialized_annot(annot);
  if (ret) {
    Is_Trace(ctx->Tracing(), (TFile, "--UIV: Done with cr annotation, cr%d, annotation: ", cr->Coderep_id()));
    Is_Trace_cmdn(ctx->Tracing(), VANT_UTIL::Dump(TFile, annot), TFile);
  }
  return ret;
}

BOOL
UIV_CHECKER::Check_vor_anno(VSYM_OBJ_REP *vor, TRAV_CONTEXT *ctx)
{
  if (!ctx->Tracker()->Empty()) {
    // TODO: don't handle this condition for now
    return FALSE;
  }
  V_ANNOT annot = vor->Vsa_annot();
  BOOL ret = Is_initialized_annot(annot);
  if (ret) {
    Is_Trace(ctx->Tracing(), (TFile, "--UIV: Done with vor annotation, vor:"));
    Is_Trace_cmd(ctx->Tracing(), vor->Print(TFile));
    Is_Trace(ctx->Tracing(), (TFile, ", annotation: "));
    Is_Trace_cmdn(ctx->Tracing(), VANT_UTIL::Dump(TFile, annot), TFile);
  }
  return ret;
}

IDTYPE
UIV_CHECKER::Get_obj_aux(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  IDTYPE aux_id = ILLEGAL_AUX_ID;
  VSYM_OBJ_REP *vor = obj.Is_vsym() ? obj.Vor() : NULL;
  if (obj.Is_var()) {
    CODEREP *cr = obj.Coderep();
    if (cr) {
      if (cr->Kind() == CK_VAR) {
        aux_id = cr->Aux_id();
      } else if (cr->Kind() == CK_LDA) {
        aux_id == cr->Lda_aux_id();
      } else if (cr->Kind() == CK_IVAR) {
        vor = ctx->Vsa()->Cr_2_vor(cr);
      }
    }
  }
  if (vor) {
    HEAP_OBJ_REP *hor = vor->Vsym_obj()->Base_hor();
    if (!hor) {
      return FALSE;
    }
    HEAP_OBJ *ho = hor->Heap_obj();
    Is_True(ho != NULL, ("Heap obj is NULL."));
    aux_id = ho->Sym_id();
  }
  return aux_id;
}

BOOL
UIV_CHECKER::Is_skip_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  VSYM_OBJ_REP *vor = obj.Is_vsym() ? obj.Vor() : NULL;
  STMTREP *sr = obj.Stmtrep();
  CODEREP *cr = NULL;
  if (obj.Is_var()) {
    cr = obj.Coderep();
    Is_True_Ret(cr, ("null check obj cr"), TRUE);
    if (sr && _checker->Issue_reported(sr, cr, Kind())) {
      Is_Trace(ctx->Tracing(),
               (TFile, "--UIV: UIV for sr%d cr%d already reported.\n",
                sr->Stmtrep_id(), cr->Coderep_id()));
      return TRUE;
    }
    if (Check_cr_anno(cr, ctx)) {
      return TRUE;
    }
    if (cr->Kind() == CK_IVAR) {
      vor = ctx->Vsa()->Cr_2_vor(cr);
    }
  } else if(obj.Is_vsym()) {
    vor = obj.Vor();
    cr = obj.Vor_cr();
  }
  if (vor) {
    if (cr && _checker->Issue_reported((STMTREP*)vor, cr, Kind())) {
      Is_Trace(ctx->Tracing(),
               (TFile, "--UIV: UIV for vor%d cr%d already reported.\n",
                vor->Vsym_obj_id(), cr->Coderep_id()));
      return TRUE;
    }
    if (Check_vor_anno(vor, ctx)) {
      return TRUE;
    }
  }

  IDTYPE aux_id = Get_obj_aux(obj, ctx);
  if (aux_id != ILLEGAL_AUX_ID) {
    AUX_STAB_ENTRY *sym = ctx->Opt_stab()->Aux_stab_entry(aux_id);
    ST *st = sym->St();
    if (sym->Is_global()) {
      Is_Trace(ctx->Tracing(), 
               (TFile, "--UIV: Done with global symbol: %s.\n",
                        st == NULL ? "" : ST_name(st)));
      return TRUE;
    } else if (st) {
      if(ST_sclass(st) == SCLASS_PSTATIC) {
        Is_Trace(ctx->Tracing(), 
                 (TFile, "--UIV: Done with static symbol: %s.\n", ST_name(st)));
        return TRUE;
      } else if (ST_sclass(st) == SCLASS_TEXT) {
        Is_Trace(ctx->Tracing(), (TFile, "--UIV: CS_DONE st is text.\n"));
        return CS_DONE;
      } else if (ST_class(st) == CLASS_CONST) {
        Is_Trace(ctx->Tracing(), 
                (TFile, "--UIV: Done with const: %s.\n", ST_name(st)));
        return TRUE;
      } else if (Vsa_check_sym_ignore(ST_name(st))) {
        Is_Trace(ctx->Tracing(), 
                (TFile, "--UIV: Done with ignored symbol: %s.\n", ST_name(st)));
        return TRUE;
      } else if (_checker->Issue_reported(ctx->Dna()->File_idx(), st, Kind())) {
        Is_Trace(ctx->Tracing(),
                  (TFile, "--UIV: UIV auto symbol already reported.\n"));
        return TRUE;
      }
    }
  }
  return FALSE;
}

BOOL
UIV_CHECKER::Is_vor_parm(VSA *vsa, VSYM_OBJ_REP *vor)
{
  Is_True_Ret(vor, ("Is_vor_parm: null vor"), FALSE);
  CODEREP *cr = vor->Vsym_obj()->Base_hor()->Heap_obj()->Ho_cr();
  while (cr != NULL) {
    if (cr->Kind() == CK_IVAR) {
      VSYM_OBJ_REP *base_vor = vsa->Cr_2_vor(cr);
      if (base_vor) {
        cr = base_vor->Vsym_obj()->Base_hor()->Heap_obj()->Ho_cr();
      }
    } else if (cr->Kind() == CK_VAR) {
      IDTYPE param = vsa->Dna()->Is_param(cr);
      if (param != INVALID_VAR_IDX) {
        return TRUE;
      }
      break;
    } else if (cr->Kind() == CK_LDA) {
      if (vsa->Dna()->Find_param_cr(cr->Lda_aux_id())) {
        return TRUE;
      }
      ST *st = cr->Lda_base_st();
      Is_True_Ret(st != NULL, ("LDA base st is null"), TRUE);
      if (ST_sclass(st) == SCLASS_FORMAL || ST_sclass(st) == SCLASS_FORMAL_REF) {
        return TRUE;
      }
      break;
    }
  }
  return FALSE;
}

BOOL
UIV_CHECKER::Is_bitfield_assign(STMTREP *sr, CODEREP *cr, TRAV_CONTEXT *ctx)
{
  Is_True_Ret(sr != NULL && cr != NULL, ("bad sr or cr"), FALSE);
  // check if sr has pattern like:
  //    LDID x
  //    LDC bits
  //   BAND
  //   LDC/SHL/BAND value
  //  BIOR
  // STID x
  // when value is 0, the pattern looks like:
  //   LDID x
  //   LDC bits
  //  BAND
  // STID x
  if (sr->Opr() != OPR_STID ||
      sr->Rhs()->Kind() != CK_OP ||
      cr->Kind() != CK_VAR ||
      sr->Lhs()->Aux_id() != cr->Aux_id())
    return FALSE;

  AUX_STAB_ENTRY *sym = ctx->Opt_stab()->Aux_stab_entry(cr->Aux_id());
  if (TY_kind(sym->Ty()) != KIND_STRUCT)
    return FALSE;

  CODEREP *rhs = sr->Rhs();
  if (rhs->Opr() == OPR_BAND && rhs->Opnd(0) == cr)
    return TRUE;  // special pattern value == 0
  if (rhs->Opr() != OPR_BIOR)
    return FALSE;

  // BIOR is commutative, check both opnd(0) and opnd(1)
  CODEREP *opnd = rhs->Opnd(0);
  if (opnd->Kind() == CK_OP &&
      opnd->Opr() == OPR_BAND &&
      opnd->Opnd(0) == cr &&
      opnd->Opnd(1)->Kind() == CK_CONST)
    return TRUE;

  opnd = rhs->Opnd(1);
  if (opnd->Kind() == CK_OP &&
      opnd->Opr() == OPR_BAND &&
      opnd->Opnd(0) == cr &&
      opnd->Opnd(1)->Kind() == CK_CONST)
    return TRUE;

  return FALSE;
}

template<> CHECKER_STATUS inline
UIV_CHECKER::Check_coderep<CK_CONST>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_CONST, ("not const"));
  Is_Trace(ctx->Tracing(), (TFile, "--UIV: Done with const.\n"));
  return CS_DONE;
}

template<> CHECKER_STATUS inline
UIV_CHECKER::Check_coderep<CK_RCONST>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_RCONST, ("not rconst"));
  Is_Trace(ctx->Tracing(), (TFile, "--UIV: Done with rconst.\n"));
  return CS_DONE;
}

template<> CHECKER_STATUS inline
UIV_CHECKER::Check_coderep<CK_LDA>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  CODEREP *cr = obj.Coderep();
  Is_True(cr->Kind() == CK_LDA, ("not lda"));
  STMTREP *sr = obj.Stmtrep();
  Is_True_Ret(sr, ("bad stmt"), CS_DONE);
  ST *st = cr->Lda_base_st();
  if (st == NULL) {
    Is_Trace(ctx->Tracing(), (TFile, "--UIV: Done with lda, NULL st.\n"));
    return CS_DONE;
  }
  if (OPERATOR_is_call(sr->Opr()) && VSA_Extern_Uiv &&
      cr == ctx->Root_cr()) {
    RNA_NODE *rna = ctx->Dna()->Get_callsite_rna(sr);
    IDTYPE parm_idx = rna->Get_arg_with_cr(cr);
    // if no callee, and symbol address passed, the symbol may be 
    // used in callee, continue check the symbol,
    // and report M if UIV
    if (parm_idx != INVALID_VAR_IDX && rna->Callee_cnt() == 0) {
      TY_IDX ty = ST_type(st);
      // if LDA symbol is array address, check UIV for array content
      if (TY_kind(ty) == KIND_ARRAY) {
        VS_FLD_KIND vf_kind = ctx->Vsa()->Get_vfr_kind(cr);
        VSYM_FLD_REP vfr(vf_kind, 0, 0);
        CHI_NODE *chi = ctx->Vsa()->Find_vor_chi(sr, cr, &vfr);
        if (chi) {
          CVOR *opnd = (CVOR*)chi->OPND();
          obj.Update_vsym(opnd->first, sr, opnd->second);
          Set_orig_stname(obj, ctx);
          ctx->Tracker()->Push(opnd->first->Vsym_obj()->Fld_rep_ptr());
          Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);
          Is_Trace(ctx->Tracing(), (TFile, "--UIV: check LDA array vor mu if no callee\n"));
          return CS_VSYM_UD;
        }
      } else {
        // for other symbols check mu cr been initialized or not
        VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
        MU_NODE *mu = ctx->Vsa()->Find_stmt_var_mu(sr, cr->Lda_base_st(), &zero_fld);
        if (mu) {
          obj.Update_var(mu->OPND());
          Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);
          Is_Trace(ctx->Tracing(), (TFile, "--UIV: check LDA symbol cr UIV if no callee\n"));
          return CS_VAR_UD;
        }
      }
    }
  }
  // move the check after extern_uiv, as LDA cr is marked initialized
  if (Is_skip_obj(obj, ctx)) {
    return CS_DONE;
  }
  if (!ctx->Tracker()->Empty()) {
    // base pointer is local variable, and don't be initialized
    // tracker is not empty, we are finding deference content of this variable for now
    if (ST_sclass(st) == SCLASS_AUTO) {
      Is_Trace(ctx->Tracing(), (TFile,
        "--UIV: Report UIV for content from deference lda st, the sclass of st is SCLASS_AUTO, st: %s.\n", ST_name(st)));
      SRCPOS st_pos = ST_Srcpos(*st);
      if(st_pos == 0) {
        st_pos = ctx->Comp_unit()->Cfg()->Entry_spos();
      }
      Sp_h()->Append_data(st, obj.Stmtrep()->Bb(), ctx->Dna(), PATHINFO_ST_DECLARE);
      Sp_h()->Set_key_srcpos(ctx->Dna(), NULL, st_pos, ST_name(st));
      Report_vul_error(ctx, obj.Stmtrep(), cr, IC_DEFINITELY);
      Sp_h()->Remove_last_key_srcpos();
      return CS_DONE;
    }
  }
  Is_Trace(ctx->Tracing(), (TFile, "--UIV: Done with lda.\n"));
  return CS_DONE;
}

template<> CHECKER_STATUS inline
UIV_CHECKER::Check_coderep<CK_VAR>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  CODEREP *cr = obj.Coderep();
  Is_True(cr->Kind() == CK_VAR, ("not var"));

  Is_True(cr->Aux_id() != ctx->Opt_stab()->Default_vsym() &&
          cr->Aux_id() != ctx->Opt_stab()->Return_vsym(),
          ("hit default/return vsym"));
  Is_True(!cr->Is_flag_set(CF_IS_ZERO_VERSION) &&
          !cr->Is_var_volatile(),
          ("hit zero-ver/volatile"));

  if (Is_skip_obj(obj, ctx)) {
    return CS_DONE;
  }

  STMTREP *sr = obj.Stmtrep();
  if (sr != NULL) {
    if (Is_bitfield_assign(sr, cr, ctx)) {
      Is_Trace(ctx->Tracing(),
               (TFile, "--UIV: Done with bit field assignment, cr%d sr%d.\n",
                       cr->Coderep_id(), sr->Stmtrep_id()));
      return CS_DONE;
    }

    AUX_STAB_ENTRY *sym = ctx->Opt_stab()->Aux_stab_entry(cr->Aux_id());
    if (sym->Is_return_preg() && sr->Prev() && sr->Prev() && ctx->Tracker()->Empty()) {
      Is_Trace(ctx->Tracing(),
               (TFile, "--UIV: Done with return preg, cr%d.\n", cr->Coderep_id()));
      return CS_DONE;
    }

    if (sr->Opr() == OPR_STID && cr == sr->Lhs()) {
      Is_Trace(ctx->Tracing(),
               (TFile, "--UIV: Done with stid lhs.\n"));
      return CS_DONE;
    }

    // ignore UIV for function call from stl clang front end for now
    if (OPERATOR_is_call(sr->Opr()) &&
        OPERATOR_has_sym(sr->Opr()) &&
        Vsa_check_regex_sym_ignore(ST_name(sr->St()))) {
      Is_Trace(ctx->Tracing(),
               (TFile, "--UIV: Done, ignore function call from stl clang front-end for now.\n"));
      return CS_DONE;
    }

    // if sym is part of an array and the array has addr passed, make the issue 'Maybe'
    if (sym->St() && TY_kind(ST_type(sym->St())) == KIND_ARRAY) {
      if (ST_is_initialized(sym->St())) {
        Is_Trace(ctx->Tracing(),
                 (TFile, "--UIV: Done, st initialized.\n"));
        return CS_DONE;
      }
      if (ST_addr_passed(sym->St())) {
        Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);
      }
    }
  }

  Set_orig_stname(obj, ctx);
  return CS_VAR_UD;
}

template<> CHECKER_STATUS inline
UIV_CHECKER::Check_coderep<CK_IVAR>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  // No UIV on Vsym if VSA_Vsym_Uiv is off
  if (VSA_Vsym_Uiv == FALSE)
    return CS_DONE;

  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_IVAR, ("not ivar"));
  Is_True(!cr->Is_ivar_volatile(), ("hit volatile"));

  if (sr->Opr() == OPR_ISTORE && cr == sr->Lhs()) {
    Is_Trace(ctx->Tracing(), (TFile, "--UIV: Done with istore lhs.\n"));
    return CS_DONE;
  }
  if (Is_skip_obj(obj, ctx)) {
    return CS_DONE;
  }
  if (cr->Opr() == OPR_MLOAD) {
    obj.Update_var(cr->Mload_size());
    return CS_OP;
  } else if (cr->Opr() == OPR_ILOADX) {
    obj.Update_var(cr->Index());
    return CS_OP;
  } else if (cr->Opr() == OPR_PARM) {
    obj.Update_var(cr->Ilod_base());
    return CS_OP;
  }
  Set_orig_stname(obj, ctx);
  return CS_IVAR_UD;
}

template<> CHECKER_STATUS
UIV_CHECKER::Check_coderep<CK_OP>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_OP, ("The kind of cr is not CK_OP, cr%d kind: %d.", cr->Coderep_id(), cr->Kind()));
  if (cr->Opr() == OPR_ALLOCA) {
    // deference content based on a alloca pointer, report UIV
    if (!ctx->Tracker()->Empty()) {
      Is_Trace(ctx->Tracing(), (TFile, "--UIV: Report UIV for dereference content from an alloca based pointer.\n"));
      Sp_h()->Set_key_srcpos(ctx->Dna(), sr, cr);
      Report_vul_error(ctx, sr, cr, IC_DEFINITELY);
      Sp_h()->Remove_last_key_srcpos();
      return CS_DONE;
    }
  }
  Is_Trace(ctx->Tracing(), (TFile, "--UIV: Done with CK_OP.\n"));
  return CS_DONE;
}

template<> CHECKER_STATUS
UIV_CHECKER::Check_stmtrep<OPR_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  STMTREP *sr = obj.Stmtrep();
  Is_True(sr != NULL && sr->Opr() == OPR_CALL, ("invalid sr"));
  IDTYPE aux_id = Get_obj_aux(obj, ctx);
  // handle LDA param
  if (aux_id != ILLEGAL_AUX_ID) {
    RNA_NODE *rna = ctx->Vsa()->Sr_2_rna(sr);
    Is_True(rna != NULL, ("RNA node is NULL."));
    IDTYPE arg_idx = rna->Get_arg_with_aux(aux_id, ctx->Opt_stab());
    if (arg_idx != INVALID_VAR_IDX) {
      BOOL check_chi_opnd = FALSE;
      if (rna->Callee_cnt() == 0 &&
          arg_idx == 1 && (sr->Call_flags() & WN_CALL_IS_CONSTRUCTOR)) {
        // external constructor, assume no UIV
        Is_Trace(ctx->Tracing(), (TFile, "External constructor, assume no UIV.\n"));
        return CS_DONE;
      } else if (rna->Callee_cnt() == 0 && VSA_Extern_Uiv) {
        Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);
        check_chi_opnd = TRUE;
        Is_Trace(ctx->Tracing(), (TFile, "No callee, check chi_opnd, arg idx:%d.\n", arg_idx));
      } else if (rna->Is_set_arg_flag(arg_idx, REF_ISTORE)) {
        // even if callee have ISTORE, also need to enter callee
        // maybe part of path don't have ISTORE
        // check callee, if callee don't exist, set maybe result
        Is_Trace(ctx->Tracing(), (TFile, "Callee have ISTORE stmt, check callee, arg index: %d.\n", arg_idx));
      } else if (rna->Is_flag_set(RNA_HAS_FUNCTIONAL)) {
        // callee don't have ISTORE, checker chi opnd, set maybe flag
        Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);
        check_chi_opnd = TRUE;
        Is_Trace(ctx->Tracing(), (TFile, "Callee do not have ISTORE stmt, check chi opnd, arg index: %d.\n", arg_idx));
      }
      // set maybe result, and then go on traverse chi opnd
      // depends on the definition of the var mu
      if (check_chi_opnd) {
        if (obj.Is_var()) {
          CODEREP *cr = obj.Coderep();
          if (cr && cr->Kind() == CK_VAR && cr->Is_flag_set(CF_DEF_BY_CHI)) {
            obj.Update_var(cr->Defchi()->OPND());
            return CS_VAR_UD;
          }
        } else if (obj.Is_vsym()) {
          CVOR *opnd = ctx->Vsa()->Find_vor_chi_opnd(sr, obj.Vor());
          Is_True_Ret(opnd, ("no vor chi on stmt"), CS_DONE);
          obj.Update_vsym(opnd->first, sr, opnd->second);
          return CS_VSYM_UD;
        }
      }
    }
  }

  // check by annoation
  V_ANNOT annot = VANT_UTIL::Empty();
  ANT_KIND check_kind = ANT_WRITE;
  if (!ctx->Tracker()->Empty()) {
    CODEREP *cr = NULL;
    if (obj.Is_var()) {
      cr = obj.Coderep();
      annot = Get_cr_annot(ctx->Vsa(), cr);
      check_kind = ANT_VWRITE;
    } else if (obj.Is_vsym()) {
      VSYM_OBJ_REP *vor = obj.Vor();
      cr = obj.Vor_cr();
      annot = vor->Vsa_annot();
    }
    if (annot != VANT_UTIL::Empty() &&
        VANT_UTIL::Get(annot, ANT_MALLOC) == ANT_YES &&
        VANT_UTIL::Get(annot, check_kind) == ANT_NO) {
      // VSYM_UD reach to heap alloc, and no vsym write flag on it report UIV
      Is_Trace(ctx->Tracing(), (TFile, "--%s: Report UIV for vsym UD reach to HEAP ALLOC\n", Checker_name()));
      Sp_h()->Set_key_srcpos(ctx->Dna(), sr, cr);
      Report_vul_error(ctx, sr, cr, IC_DEFINITELY);
      Sp_h()->Remove_last_key_srcpos();
      return CS_DONE;
    }
  }
  return CS_CONT;
}

template<> CHECKER_STATUS
UIV_CHECKER::Check_stmtrep<OPR_ICALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  return CS_CONT;
}

template<> CHECKER_STATUS
UIV_CHECKER::Check_stmtrep<OPR_INTRINSIC_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  Is_True(sr->Opr() == OPR_INTRINSIC_CALL, ("not intrinsic call"));
  INTRINSIC intrn = sr->Rhs()->Intrinsic();
  if (intrn == INTRN_CHECK_CAST) {
    CODEREP *rhs = sr->Rhs();
    Is_True(rhs->Kid_count() == 2, ("bad rhs kids"));
    Is_True(rhs->Opnd(1)->Kind() == CK_IVAR && rhs->Opnd(1)->Opr() == OPR_PARM, ("bad first kid"));
    obj.Update_var(rhs->Opnd(1)->Ilod_base());
    return CS_OP;
  }
  Is_Trace(ctx->Tracing(), (TFile, "--UIV: Done with intrinsic.\n"));
  Is_Trace_cmd(ctx->Tracing(), Print_intrn_entry(TFile, intrn));
  return CS_DONE;
}

template<> CHECKER_STATUS
UIV_CHECKER::Check_stmtrep<OPR_OPT_CHI>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Stmtrep()->Opr() == OPR_OPT_CHI, ("UIV: not chi stmt"));
  if (obj.Is_var()) {
    CODEREP *cr = obj.Coderep();
    Is_True(cr->Kind() == CK_VAR, ("Cr kind is not CK_VAR, cr%d kind: %d.", cr->Coderep_id(), cr->Kind()));
    Is_True(cr->Is_flag_set((CR_FLAG) CF_DEF_BY_CHI), ("Cr is not def by chi, cr%d.", cr->Coderep_id()));
    if (Is_skip_obj(obj, ctx)) {
      return CS_DONE;
    }
    AUX_STAB_ENTRY *sym = ctx->Opt_stab()->Aux_stab_entry(cr->Aux_id());
    ST *st = sym->St();
    if (cr->Def_at_entry() && st != NULL) {
      SRCPOS st_pos = ST_Srcpos(*st);
      if(st_pos == 0) {
        st_pos = ctx->Comp_unit()->Cfg()->Entry_spos();
      }
      if (ST_sclass(st) == SCLASS_AUTO) {
        Is_Trace(ctx->Tracing(),
                 (TFile, "--UIV: Report UIV for symbol CLASS_AUTO, cr: %d\n",
                  cr->Coderep_id()));
        Sp_h()->Append_data(st, obj.Stmtrep()->Bb(), ctx->Dna(), PATHINFO_ST_DECLARE);
        Sp_h()->Set_key_srcpos(ctx->Dna(), NULL, st_pos, ST_name(st));
        BOOL ret = Report_vul_error(ctx, obj.Stmtrep(), cr);
        if (ret && VSA_Xsca) {
          ctx->Vsa()->Report_xsca_error(cr,
                                        Sp_h()->Orig_stname() ? Sp_h()->Orig_stname() : ST_name(st),
                                        "MSR_9_1", IC_DEFINITELY, Sp_h());
        }
        Sp_h()->Remove_last_key_srcpos();
        return CS_DONE;
      }
    }
  } else if (obj.Is_vsym()) {
    CODEREP* base = ctx->Vsa()->Find_vor_chi_cr(obj.Stmtrep(), obj.Vor());
    if (base == NULL) {
      Is_Trace(ctx->Tracing(),
               (TFile, "--UIV: CS_DONE unable to find base cr for vor\n"));
      return CS_DONE;
    }
    if (ctx->Dna()->Is_root() && ctx->Dna()->Is_param(base)) {
      if (VSA_Param_Uiv) {
        Is_Trace(ctx->Tracing(), (TFile, "--UIV: Report UIV for parameter, base pointer is root DNA parameter.\n"));
        Sp_h()->Set_key_srcpos(ctx->Dna(), obj.Stmtrep(), base);
        Report_vul_error(ctx, obj.Stmtrep(), base, IC_MAYBE);
        Sp_h()->Remove_last_key_srcpos();
      } else {
        Is_Trace(ctx->Tracing(), (TFile, "--UIV: Done with root dna parameter.\n"));
      }
      return CS_DONE;
    }
  }
  return CS_CONT;
}

// Check_vsym_obj
CHECKER_STATUS
UIV_CHECKER::Check_vsym_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  VSYM_OBJ_REP *vor = obj.Vor();
  Is_True_Ret(vor != NULL && !ctx->Vsa()->Is_special_vor(vor),
              ("bad vor"), CS_DONE);
  if (Is_skip_obj(obj, ctx)) {
    return CS_DONE;
  }
  // only 1 level vor def by istore, def is found, no need to traverse opnd
  if (vor->Attr() == ROR_DEF_BY_ISTORE && ctx->Tracker()->Size() == 1) {
    STMTREP *def_stmt = vor->Stmt_def();
    Is_True_Ret(def_stmt, ("null def_stmt for vor def by istore"), CS_DONE);
    VSA_ACCESS_INFO info;
    if (vor->Vsym_obj()->Fld_rep().Is_any() &&
        ctx->Comp_unit()->Analyze_access_info(def_stmt, def_stmt->Lhs(), &info) == TRUE) {
      if (obj.Access_info().Subtract(info) > 0) {
        // def_stmt modified vor partitially, continue check mu
        return CS_CONT;
      }
    }
    Is_Trace(ctx->Tracing(), (TFile, "--UIV: CS_DONE vor def by istore\n"));
    Is_Trace_cmd(ctx->Tracing(), def_stmt->Print(TFile));
    return CS_DONE;
  }

  STMTREP *sr = obj.Stmtrep();
  BOOL in_handler = sr ? ctx->In_handler_bb(sr->Bb()) : FALSE;
  if (vor->Is_entry_chi() && vor->Stmt_def() == NULL && !in_handler) {
    Is_Trace(ctx->Tracing(), (TFile, "--UIV: Report UIV for NULL vsym defstmt\n"));
    Is_True_Ret(!ctx->Tracker()->Empty(), ("vsym tracker is empty"), CS_DONE);
    if (Is_vor_parm(ctx->Vsa(), vor)) {
      Is_Trace(ctx->Tracing(), (TFile, "!!!!!Bug: vsym UD of parameter is broken\n"));
      return CS_DONE;
    }
    VSYM_FLD_REP *top_vfr = ctx->Tracker()->Top();
    Is_True_Ret(top_vfr, ("null tracker top vfr"), CS_DONE);
    BOOL maybe = top_vfr->Match(vor->Vsym_obj()->Fld_rep_ptr()) == VS_EXACT_MATCH ? FALSE : TRUE;
    CODEREP *cr = obj.Vor_cr();
    ST *st = NULL;
    if (cr) {
      if (cr->Kind() == CK_VAR) {
        st = ctx->Opt_stab()->Aux_stab_entry(cr->Aux_id())->St();
        if (st && cr->Def_at_entry() &&
            ST_sclass(st) == SCLASS_AUTO) {
          maybe = FALSE;
        }
      } else if (cr->Kind() == CK_LDA) {
        st = cr->Lda_base_st();
        if (sr && st) {
          VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
          MU_NODE *mu = ctx->Vsa()->Find_stmt_var_mu(sr, st, &zero_fld);
          CODEREP *opnd = mu ? mu->OPND() : NULL;
          if (opnd && opnd->Def_at_entry() &&
              ST_sclass(st) == SCLASS_AUTO) {
            maybe = FALSE;
          }
        }
      }
    }
    if (maybe) {
      Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);
    }
    SRCPOS st_pos = 0;
    if (st) {
      st_pos = ST_Srcpos(*st);
      Sp_h()->Append_data(st, obj.Stmtrep()->Bb(), ctx->Dna(), PATHINFO_ST_DECLARE);
    } else {
      Sp_h()->Append_data(ctx->Vsa()->Get_entry_chi_stmt(), ctx->Dna(), PATHINFO_CHI);
    }
    if (st_pos == 0) {
      st_pos = ctx->Comp_unit()->Cfg()->Entry_spos();
    }
    Sp_h()->Set_key_srcpos(ctx->Dna(), NULL, st_pos, st ? ST_name(st) : "");
    Report_vul_error(ctx, sr, cr, IC_DEFINITELY);
    Sp_h()->Remove_last_key_srcpos();
    return CS_DONE;
  }
  return CS_CONT;
}

BOOL
UIV_CHECKER::Report_vul_error(TRAV_CONTEXT* ctx, STMTREP *sr, CODEREP *cr, ISSUE_CERTAINTY ic)
{
  if (sr && cr && _checker->Issue_reported(sr, cr, Kind())) {
    Is_Trace(ctx->Tracing(),
             (TFile, "--UIV: CS_DONE, issue already reported for sr%d cr%d\n",
              sr->Stmtrep_id(), cr->Coderep_id()));
    return FALSE;
  }
  return ctx->Vsa()->Report_vul_error(cr, Sp_h(), (ILODSTORBASE)_kind, ic);
}

void
VSA::Scan_uiv_new()
{
  // java don't have uiv issues
  if (PU_java_lang(Get_Current_PU())) {
    return;
  }
  CHECKER_TRAVELER<UIV_CHECKER> traveler(Comp_unit());
  Is_Trace(Get_Trace(TP_CHECKER, CHK_UIV_TRACE_FLAG),
           (TFile, "%sScan UIV NEW for func %s\n%s", DBar, Dna()->Fname(), DBar));
  traveler.Process();
  Is_Trace(Get_Trace(TP_CHECKER, CHK_UIV_TRACE_FLAG),
           (TFile, "%sEND Scan UIV NEW for func %s\n%s", DBar, Dna()->Fname(), DBar));
}
