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
// Module: opt_vsa_npd.cxx
//
// ====================================================================
//

// ====================================================================
// VSA NPD CHECKER
//   Check the NPD (Null Pointer Dereference) issue
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
#include "opt_cda.h"
#include "opt_addr_util.h"
#include "opt_vsa_rbc.h"
#include "opt_vsa_checker.h"
#include "vsa_annot.h"


class NPD_CHECKER : public SPOS_BASE {
  typedef CHECKER_TRAVELER<NPD_CHECKER> CHECKER;
private:
  CHECKER              *_checker;
  UINT32                _kind;

private:
  BOOL            Ignore_root_cr(TRAV_CONTEXT* ctx);
  void            Report_vul_error(TRAV_CONTEXT* ctx, CODEREP *cr, VAL_RANGE_RESULT rr);

public:
  // traversal from dereference
  enum { SUSPECT = CS_DEREFERENCE | CS_DIVISOR | CS_VAR_DEF | CS_VPTR | CS_CONTAINER_UD };
  // need to check coderep
  enum { ENTITY = TE_CODEREP };
  // need to track USE_SRCPOS
  enum { USE_SRCPOS = TRUE };
  // need to follow eh path
  enum { FOLLOW_EH = TRUE };

public:
  // NPD_CHECKER
  // Constructor
  NPD_CHECKER(TRAV_CONTEXT& ctx, CHECKER *checker)
   : SPOS_BASE(ctx), _checker(checker), _kind(0) {
    ctx.Set_Tracing(Get_Trace(TP_CHECKER, CHK_NPD_TRACE_FLAG));
  }

public:
  // Checker_name
  // return the name of the checker
  const char*     Checker_name() const  { return "NPD"; }

  void            Set_kind(UINT32 kind) { _kind = kind; }
  const           UINT32 Kind()         { return _kind;}
  BOOL            Set_check_kind(CHECKER_SUSPECT suspect);

public:
  void Get_ret(STMTREP *call_sr, STMTREP **ret_sr, CODEREP **ret_cr, TRAV_CONTEXT* ctx);
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

  // Check_vsym_obj
  CHECKER_STATUS
  Check_vsym_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx) { return CS_CONT; }

  void
  Add_target(UINT32 file_idx, ST_IDX idx) { }
};

BOOL
NPD_CHECKER::Set_check_kind(CHECKER_SUSPECT suspect)
{
  switch (suspect) {
    case CS_DEREFERENCE:
      Set_kind(FOR_ILOD_BASE);
      return TRUE;
    default:
      return FALSE;
  }
  return FALSE;
}

BOOL
NPD_CHECKER::Ignore_root_cr(TRAV_CONTEXT* ctx)
{
  if (!VSA_Ignore_Dup) {
    Is_True_Ret(ctx->Root_vsa()->Ignore_cr_set() == NULL, ("Root vsa ignore cr set is not null."), FALSE);
    return FALSE;
  }
  Is_True_Ret(ctx->Root_vsa()->Ignore_cr_set() != NULL, ("Root vsa ignore cr set is null."), FALSE);
  CODEREP_SET *ignore_cr_set = ctx->Root_vsa()->Ignore_cr_set();
  SRCPOS_HANDLE* sp_h = Sp_h();
  CODEREP *cr = sp_h->Root_x();
  STMTREP *sr = sp_h->Root_stmt();
  if (cr == NULL || sr == NULL) {
    return FALSE;
  }
  if (ignore_cr_set->find(cr) != ignore_cr_set->end()) {
    return TRUE;
  }
  ignore_cr_set->insert(cr);
  return FALSE;
}

// ====================================================================
// NPD_CHECKER::Report_vul_error
//   Report or ignore the error
// ====================================================================
void
NPD_CHECKER::Report_vul_error(TRAV_CONTEXT* ctx, CODEREP *cr, VAL_RANGE_RESULT rr)
{
  BOOL ignore = Ignore_root_cr(ctx);
  if (ignore) {
    Is_Trace(ctx->Tracing(), (TFile, "--NPD: ignore vul report for root cr\n"));
    return;
  }
  ISSUE_CERTAINTY ic = (rr == VAL_May_OOR) ? IC_MAYBE : IC_DEFINITELY;
  ctx->Vsa()->Report_vul_error(cr, Sp_h(), (ILODSTORBASE)_kind, ic);
}

// ====================================================================
// NPD_CHECKER::Check_coderep<CK_LDA>
//   Check NPD if coderep is LDA
// ====================================================================
template<> CHECKER_STATUS inline
NPD_CHECKER::Check_coderep<CK_LDA>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_LDA, ("not lda"));
  Is_Trace(ctx->Tracing(), (TFile, "--NPD: Done with LDA\n"));
  return CS_DONE;
}

// ====================================================================
// NPD_CHECKER::Check_coderep<CK_LDA>
//   Check NPD if coderep is CONST
// ====================================================================
template<> CHECKER_STATUS inline
NPD_CHECKER::Check_coderep<CK_CONST>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_CONST, ("not const"));
  if (cr->Const_val() == 0) {
    if (_checker->Issue_reported(sr, cr, Kind())) {
      Is_Trace(ctx->Tracing(),
               (TFile, "--NPD: NPD-D with const 0 already reported.\n"));
      return CS_DONE;
    }
    // check for pattern of
    // if (p) {
    //   p = ...;
    // } else {
    //   p = 0;
    // }
    CDA_VALUE cda = ctx->Comp_unit()->Cda()->Get_cda(sr->Bb());
    if (cda.Kind() == CDA_RHS_TRUE || cda.Kind() == CDA_RHS_FALSE) {
      STMTREP *last = cda.Get_as<STMTREP>();
      Is_True(last && (last->Opr() == OPR_TRUEBR || last->Opr() == OPR_FALSEBR),
              ("unexpected stmt operator"));
      CODEREP *cmp = last->Rhs();
      if (cmp->Kind() == CK_OP &&
          OPERATOR_is_compare(cmp->Opr()) &&
          cmp->Opnd(1)->Kind() == CK_CONST &&
          cmp->Opnd(1)->Const_val() == 0 &&
          ((cmp->Opr() == OPR_EQ && cda.Kind() == CDA_RHS_TRUE) ||
           (cmp->Opr() == OPR_NE && cda.Kind() == CDA_RHS_FALSE))) {
         CODEREP *opnd = cmp->Opnd(0);
         if (opnd->Kind() == CK_VAR &&
             TY_kind(opnd->object_ty()) == KIND_POINTER) {
           obj.Update_var(cmp->Opnd(0), last);
           Is_Trace(ctx->Tracing(),
                    (TFile, "--NPD: hit p=p?f(p):0. continue with cr%d in sr%d BB%d.\n",
                            cmp->Opnd(0)->Coderep_id(), last->Stmtrep_id(), last->Bb()->Id()));

           return CS_OP;
         }
       }
    }
    if (cr != Sp_h()->Root_x()) {
      Append_stpath(sr, cr, ctx->Dna(), FALSE);
    }
    Is_Trace(ctx->Tracing(), (TFile, "--NPD: Report NPD-D with const 0\n"));
    // report M NPD if vsym base is NULL
    if (!ctx->Tracker()->Empty())
      Sp_h()->Set_flag(SRCPOS_FLAG_MAYBE);
    // treat the sr which causes cr to be 0 as the "key" srcpos for the NPD issue
    Sp_h()->Set_key_srcpos(ctx->Dna(), sr, cr);
    Report_vul_error(ctx, cr, VAL_OOR);
    Sp_h()->Remove_last_key_srcpos();
    return CS_DONE;
  }
  else {
    Is_Trace(ctx->Tracing(), (TFile, "--NPD: Done with const %lld\n", cr->Const_val()));
  }
  return CS_DONE;
}

// ====================================================================
// NPD_CHECKER::Check_coderep<CK_LDA>
//   Check NPD if coderep is RCONST
// ====================================================================
template<> CHECKER_STATUS inline
NPD_CHECKER::Check_coderep<CK_RCONST>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_RCONST, ("not rconst"));
  if (cr->Has_const_fval() && cr->Const_fval() == 0.0) {
    if (_checker->Issue_reported(sr, cr, Kind())) {
      Is_Trace(ctx->Tracing(),
               (TFile, "--NPD: NPD-D with const 0.0 already reported.\n"));
      return CS_DONE;
    }
    if (cr != Sp_h()->Root_x()) {
      Append_stpath(sr, cr, ctx->Dna(), FALSE);
    }
    Is_Trace(ctx->Tracing(), (TFile, "--NPD: Report NPD-D with rconst 0.0 \n"));
    // treat the sr which causes cr to be 0 as the "key" srcpos
    Sp_h()->Set_key_srcpos(ctx->Dna(), sr, cr);
    Report_vul_error(ctx, cr, VAL_OOR);
    Sp_h()->Remove_last_key_srcpos();
    return CS_DONE;
  } else {
    Is_Trace(ctx->Tracing(), (TFile, "--NPD: Done with rconst\n"));
  }
  return CS_DONE;
}

// ====================================================================
// NPD_CHECKER::Check_coderep<CK_OP>
//   Check NPD if coderep is OP
// ====================================================================
template<> CHECKER_STATUS inline
NPD_CHECKER::Check_coderep<CK_OP>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_OP, ("not op"));
  if (_kind == 0 && VSA_Dbz &&
      (cr->Opr() == OPR_DIV || cr->Opr() == OPR_MOD ||
       cr->Opr() == OPR_REM || cr->Opr() == OPR_DIVREM)) {
    _kind |= FOR_DIVISOR;
    obj.Update_var(cr->Opnd(1));
    Set_orig_stname(obj, ctx);
    Is_Trace(ctx->Tracing(), (TFile, "%sFind div expr candidate:\n", SBar));
    return CS_OP;
  }
  if ((_kind & (FOR_ILOD_BASE | FOR_ISTOR_BASE)) != 0) {
    //
    // Add a check if we are checking whether cr
    // is LOOKUP_VIRT_FUNC
    // If the cr is LOOKUP_VIRT_FUNC, then we would
    // want to continue with its first parameter.
    //
    if (Is_valid_lookup_virt_op(cr)) {
      // Extract original value
      // (ILOAD for virtual / LOOKUP_IF for interface call)
      CODEREP *org = Get_lookup_virt_original_target_info(cr);
      Is_True(Is_lookup_virt_op_interface_call(org) ||
              Is_stmt_virtual_call(sr), ("Some intrinsic lookup_virt_func is neither "
                                         "intfc nor virtual call"));
      if (org && org != cr) {
        obj.Update_var(org);
        // set status to CS_OP to trigger callback first before check UD
        return CS_OP;
      } else {
        Is_True_Ret(FALSE, ("Unable to get virtual target"), CS_DONE);
      }
    }
    if (cr->Opr() == OPR_BAND || cr->Opr() == OPR_ALLOCA) {
      Is_Trace(ctx->Tracing(), (TFile, "--NPD: Done with BAND or OPR_ALLOCA\n"));
      return CS_DONE;  // don't handle BAND and OPR_ALLOCA so far
    }
    CODEREP* base = ctx->Comp_unit()->Analyze_base_info(sr, cr, FALSE);
    if (base != NULL) {
      // continue with the base UD
      if (base->Kind() == CK_LDA) {
        Is_Trace(ctx->Tracing(), (TFile, "--NPD: Done with expr base LDA\n"));
        return CS_DONE;
      }
      // only VAR and IVAR is allowed
      Is_True(base->Kind() == CK_VAR || base->Kind() == CK_IVAR, ("bad base"));
      obj.Update_var(base);
      // set status to CS_OP to trigger callback first before check UD
      return CS_OP;
    }
    else {
      Is_Trace(ctx->Tracing(), (TFile, "##NPD: expr base NULL\n"));
      if (VSA_NPD_Nullbase) {
        if (_checker->Issue_reported(sr, cr, Kind())) {
          Is_Trace(ctx->Tracing(),
                   (TFile, "--NPD: NPD-D with base NULL already reported.\n"));
          return CS_DONE;
        }
        // doesn't find the base, treat the sr as the "key" srcpos for the NPD issue
        Sp_h()->Set_key_srcpos(ctx->Dna(), sr, cr);
        Report_vul_error(ctx, cr, VAL_OOR);
        Sp_h()->Remove_last_key_srcpos();
      }
      return CS_DONE;
    }
  } else if ((_kind & FOR_DIVISOR) != 0) {
    Is_Trace(ctx->Tracing(), (TFile, "%s##DIV: Check OP opnds U-D:\n", SBar));
    Is_Trace_cmd(ctx->Tracing(), sr->Print(TFile));
    if (cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL) {
      obj.Update_var(cr->Opnd(0));
      return CS_OP;
    }
    // those expressions should be evaluated
    // just copy the code from old DBZ checker for now, and need a expression evaluation later
    // ex: a = b + c, we should evaluate the value of (b + c), not just evaluate b and c saparately
    if (cr->Opr() == OPR_MPY || cr->Opr() == OPR_BAND || cr->Opr() == OPR_BXOR) {
      VSYM_TRACKER::VSYM_VECTOR vsym_stack;
      ctx->Tracker()->Save_stack(&vsym_stack);
      {
        CHECK_OBJ opnd0(cr->Opnd(0), sr);
        UD_TRAVELER<NPD_CHECKER> dbz_helper(*this, *ctx);
        dbz_helper.Continue_trav(opnd0, CS_OP);
        ctx->Tracker()->Restore_stack(&vsym_stack);
      }
      {
        CHECK_OBJ opnd1(cr->Opnd(1), sr);
        UD_TRAVELER<NPD_CHECKER> dbz_helper(*this, *ctx);
        dbz_helper.Continue_trav(opnd1, CS_OP);
        ctx->Tracker()->Restore_stack(&vsym_stack);
      }
    }
  }
  if (_kind == 0 && cr->Opr() == OPR_ASM_INPUT) {
    ST_IDX st = cr->Asm_constraint();
    Is_True(st != ST_IDX_ZERO && ST_name(st) != NULL, ("no constraint string"));
    if (st != ST_IDX_ZERO && ST_name(st) != NULL) {
      const char* cstr = ST_name(st);
      if (*cstr == 'p') {
        // address opnd dereference
        obj.Update_var(cr->Opnd(0));
        Set_orig_stname(obj, ctx);
        _kind |= FOR_ILOD_BASE;
        return CS_OP;
      }
    }
  }
  return CS_DONE;
}

// ====================================================================
// NPD_CHECKER::Check_coderep<CK_LDA>
//   Check NPD if coderep is VAR
// ====================================================================
template<> CHECKER_STATUS
NPD_CHECKER::Check_coderep<CK_VAR>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_VAR, ("not var"));

  Is_True(cr->Aux_id() != ctx->Opt_stab()->Default_vsym() &&
          cr->Aux_id() != ctx->Opt_stab()->Return_vsym(),
          ("hit default/return vsym"));
  Is_True(!cr->Is_flag_set(CF_IS_ZERO_VERSION) &&
          !cr->Is_var_volatile(),
          ("hit zero-ver/volatile"));

  // check value range for VAR at first
  if (ctx->Tracker()->Empty()) {
    Is_Trace(ctx->Tracing(), (TFile, "--NPD: check vra in bb%d\n", obj.Bb()->Id()));
    BOOL is_vra = FALSE;
    VAL_RANGE_RESULT rr = ctx->Vsa()->Check_expr_zero(cr, obj.Bb(), is_vra);
    if (rr == VAL_INR && is_vra == TRUE) {
      Is_Trace(ctx->Tracing(), (TFile, "--NPD: Done with VRA says cr is not zero\n"));
      return CS_DONE;
    }
  }

  // uncomment these lines once the flags are reliable
  //if (!cr->Value_invalid_addr() && !cr->Value_maydangling()) {
  //  Is_Trace(ctx->Tracing(), (TFile, "--NPD: Done. CR is not invalid_addr or maydangling\n"));
  //  return CS_DONE;
  //}

  // continue with the VAR UD
  return CS_VAR_UD;
}

// ====================================================================
// NPD_CHECKER::Check_coderep<CK_LDA>
//   Check NPD if coderep is IVAR
// ====================================================================
template<> CHECKER_STATUS
NPD_CHECKER::Check_coderep<CK_IVAR>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_IVAR, ("not ivar"));
  // for div, we need not to trace base cr, just find vsym ud
  if (_kind & FOR_ILOD_BASE || _kind & FOR_ISTOR_BASE || _kind & FOR_DIVISOR) {
    // check if there is check of "cr == 0"
    BOOL is_vra = FALSE;
    VAL_RANGE_RESULT rr = ctx->Vsa()->Check_expr_zero(cr, obj.Bb(), is_vra);
    if (rr == VAL_INR && is_vra == TRUE) {
      Is_Trace(ctx->Tracing(), (TFile, "--NPD: Done with VRA says cr%d is not zero in BB%d\n",
                                cr->Coderep_id(), obj.Bb()->Id()));
      return CS_DONE;
    }
    // already ILOD_BASE, **p, continue with the IVAR vsym
    obj.Update_var(cr);
    Is_Trace(ctx->Tracing(), (TFile, " -NPD: Check ivar U-D:\n"));
    Is_Trace_cmd(ctx->Tracing(), sr->Print(TFile));
    return CS_IVAR_UD;
  }

  CODEREP *base = NULL;
  if ((sr->Opr() == OPR_ISTORE || sr->Opr() == OPR_ISTBITS ||
       sr->Opr() == OPR_MSTORE || sr->Opr() == OPR_ISTOREX) &&
      cr == sr->Lhs()) {
    // can only be ISTOR_BASE
    Is_True((_kind & (FOR_ILOD_BASE | FOR_ISTOR_BASE)) == 0, ("bad istore base, kind: %d.", _kind));
    _kind |= FOR_ISTOR_BASE;
    base = cr->Istr_base();
  }
  else {
    // ILOD_BASE
    _kind |= FOR_ILOD_BASE;
    base = cr->Ilod_base();
  }

  Is_True(base != NULL, ("ivar base is NULL"));
  CODEREP *base_sav = base;
  base = Find_ilod_base(base);
  if (base) {
    if (base->Kind() == CK_LDA) {
      Is_Trace(ctx->Tracing(), (TFile, "--NPD: Done with LDA cr%d\n",
                                base->Coderep_id()));
      return CS_DONE;
    }
    // no NPD on vsym if VSA_Vsym_Npd is off
    if (VSA_Vsym_Npd == FALSE && base->Kind() == CK_IVAR)
      return CS_DONE;

    // check if there is check of "base == 0"
    BOOL is_vra = FALSE;
    VAL_RANGE_RESULT rr = ctx->Vsa()->Check_expr_zero(base, obj.Bb(), is_vra);
    if (rr == VAL_INR && is_vra == TRUE) {
      Is_Trace(ctx->Tracing(), (TFile, "--NPD: Done with VRA says cr%d is not zero in BB%d\n",
                                base->Coderep_id(), obj.Bb()->Id()));
      return CS_DONE;
    }
    // continue with base U-D
    obj.Update_var(base);
    Set_orig_stname(obj, ctx);
    return base->Kind() == CK_VAR ? CS_VAR_UD : CS_IVAR_UD;
  }
  else {
    Is_Trace(ctx->Tracing(), (TFile, "--NPD: continue with base_sav so that path is complete\n"));
    obj.Update_var(base_sav);
    Set_orig_stname(obj, ctx);
    return CS_OP;
  }
}

// ====================================================================
// NPD_CHECKER::Check_stmtrep<OPR_INTRINSIC_CALL>
//   Check NPD if stmtrep is INTRINSIC_CALL
// ====================================================================
template<> CHECKER_STATUS
NPD_CHECKER::Check_stmtrep<OPR_INTRINSIC_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  Is_True(sr->Opr() == OPR_INTRINSIC_CALL, ("not intrinsic call"));
  INTRINSIC intrn = sr->Rhs()->Intrinsic();
  if (intrn == INTRN_CHECK_CAST) {
    CODEREP* rhs = sr->Rhs();
    Is_True(rhs->Kid_count() == 2, ("bad rhs kids"));
    Is_True(rhs->Opnd(1)->Kind() == CK_IVAR && rhs->Opnd(1)->Opr() == OPR_PARM, ("bad first kid"));
    obj.Update_var(rhs->Opnd(1)->Ilod_base());
    return CS_OP;
  }
  // TODO: other intrinsics

  RNA_NODE *rna = ctx->Vsa()->Sr_2_rna(sr);
  RBC_OP get_ops[] = RBC_OP_CONTAINER_GET;
  if (rna->Is_container_op() &&
      ctx->Ipsa()->Rna_has_rbc_ops(rna, get_ops, RBC_OPS_CNT(get_ops)))
    return CS_CONT;
  return CS_DONE;
}

void
NPD_CHECKER::Get_ret(STMTREP *call_sr, STMTREP **ret_sr, CODEREP **ret_cr, TRAV_CONTEXT* ctx) {
  CODEREP *cr = ctx->Comp_unit()->Find_return_value(call_sr);
  STMTREP *sr = call_sr;
  if (cr != NULL) {
    STMTREP *rstmt = call_sr->Next();
    if (rstmt != NULL && rstmt->Opr() == OPR_STID && rstmt->Rhs() == cr) {
      cr = rstmt->Lhs();
      sr = rstmt;
      rstmt = rstmt->Next();
      if (rstmt != NULL &&
          (rstmt->Opr() == OPR_STID || rstmt->Opr() == OPR_ISTORE) &&
          rstmt->Rhs() == cr) {
        cr = rstmt->Lhs();
        sr = rstmt;
      }
    }
    *ret_cr = cr;
    *ret_sr = sr;
  }
}

template<> CHECKER_STATUS
NPD_CHECKER::Check_stmtrep<OPR_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  if ((_kind & FOR_DIVISOR) && obj.Is_var() && (obj.Coderep()->Kind() == CK_VAR)) {
    // tainted value maybe 0, report DBZ
    CODEREP *cr = obj.Coderep();
    BOOL tainted = ctx->Vsa()->Is_var_tainted(cr);
    if (tainted) {
      if (_checker->Issue_reported(sr, cr, Kind())) {
        Is_Trace(ctx->Tracing(),
                 (TFile, "--NPD: DBZ-D with tainted cr already reported.\n"));
        return CS_DONE;
      }
      Is_Trace(ctx->Tracing(), (TFile, "--DBZ: Report DBZ-D with dividing tainted argument or return\n"));
      Sp_h()->Set_key_srcpos(ctx->Dna(), sr, cr);
      Report_vul_error(ctx, cr, VAL_OOR);
      Sp_h()->Remove_last_key_srcpos();
      return CS_DONE;
    } else {
      AUX_STAB_ENTRY *sym = ctx->Opt_stab()->Aux_stab_entry(cr->Aux_id());
      if (sym->Is_return_preg()) {
        CODEREP *ret_cr = NULL;
        STMTREP *ret_sr = NULL;
        Get_ret(sr, &ret_sr, &ret_cr, ctx);
        if (ret_cr != NULL) {
          // ATTENTION: we don't bind tag on preg cr, need to find preg stored cr, and then check
          CHECK_OBJ ret_obj(ret_cr, ret_sr);
          RBC_BASE *rbc = ctx->Vsa()->Rbc();
          TAG_BASE *tag_base = rbc->Find_tag_base((char *) "tainted");
          if (tag_base) {
            // why resue existing context?
            tainted = rbc->Check_tag(ret_obj, *ctx, CHECK_BY_TAG, tag_base);
            if (tainted) {
              if (_checker->Issue_reported(ret_sr, ret_cr, Kind())) {
                Is_Trace(ctx->Tracing(),
                         (TFile, "--NPD: DBZ-D with tainted cr already reported.\n"));
                return CS_DONE;
              }
              Is_Trace(ctx->Tracing(), (TFile, "--DBZ: Report DBZ-D with dividing tainted tag argument or return\n"));
              Sp_h()->Set_key_srcpos(ctx->Dna(), ret_sr, ret_cr);
              Report_vul_error(ctx, cr, VAL_OOR);
              Sp_h()->Remove_last_key_srcpos();
              return CS_DONE;
            }
          }
        }
      }
    }
  }
  if (obj.Is_vsym()) {
    VSYM_OBJ_REP *vor = obj.Vor();
    HEAP_OBJ_REP *hor = vor->Hor();
    V_ANNOT annot = VANT_UTIL::Empty();
    if (hor && hor->Attr() == ROR_DEF_BY_ALLOC) {
      Is_True(hor->Stmt_def() != NULL, ("no stmt def for alloc hor"));
      V_ANNOT annot = vor->Vsa_annot();
      // VSYM_UD reach to heap alloc, and no vsym write flag on it report error
      // candidates for UIV, report NPD/DBZ for now, as no UIV new checker yet
      if (annot != VANT_UTIL::Empty() &&
          VANT_UTIL::Get(annot, ANT_VWRITE) == ANT_NO) {
        if (_checker->Issue_reported(hor->Stmt_def(), obj.Vor_cr(), Kind())) {
          Is_Trace(ctx->Tracing(),
                   (TFile, "--NPD: DBZ-D with alloc base already reported.\n"));
          return CS_DONE;
        }
        Is_Trace(ctx->Tracing(), (TFile, "--%s: Report NPD/DBZ for vsym's UD reach to HEAP ALLOC\n", Checker_name()));
        Sp_h()->Set_key_srcpos(ctx->Dna(), hor->Stmt_def(), NULL);
        Report_vul_error(ctx, obj.Vor_cr(), VAL_OOR);
        Sp_h()->Remove_last_key_srcpos();
        return CS_DONE;
      }
    }
  }
  return CS_CONT;
}

template<> CHECKER_STATUS
NPD_CHECKER::Check_stmtrep<OPR_ICALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  return CS_CONT;
}

template<> CHECKER_STATUS
NPD_CHECKER::Check_stmtrep<OPR_OPT_CHI>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Stmtrep()->Opr() == OPR_OPT_CHI, ("NPD: stmt is not chi"));
  if ((_kind & FOR_DIVISOR) != 0) {
    return CS_CONT;
  }
  if (ctx->Dna()->Clby_list()->size() == VAR_INIT_ID) {
    CODEREP *cr = obj.Is_var() ? obj.Coderep() : obj.Vor_cr();
    if (cr && cr->Kind() == CK_VAR) {
      IDTYPE parm_id = ctx->Dna()->Is_param(cr);
      // report may NPD if reach to top function
      if (parm_id != INVALID_VAR_IDX) {
        if (_checker->Issue_reported(obj.Stmtrep(), cr, Kind())) {
          Is_Trace(ctx->Tracing(),
                   (TFile, "--NPD: DBZ-M with parm base already reported.\n"));
          return CS_DONE;
        }
        Is_Trace(ctx->Tracing(), (TFile, (" - NPD: parm reach to top\n")));
        Sp_h()->Set_key_srcpos(ctx->Dna(), obj.Stmtrep(), cr);
        Report_vul_error(ctx, cr, VAL_May_OOR);
        Sp_h()->Remove_last_key_srcpos();
        return CS_DONE;
      }
    }
  }
  return CS_CONT;
}

// ====================================================================
// VSA::Scan_npd_new
//   Test driver for NPD checker
// ====================================================================
void
VSA::Scan_npd_new()
{
  MEM_POOL lpool;
  if (VSA_Ignore_Dup) {
    OPT_POOL_Initialize(&lpool, "ignore cr temp pool", FALSE, VSA_DUMP_FLAG);
    OPT_POOL_Push(&lpool, VSA_DUMP_FLAG);
    Initialize_ignore_cr_set(&lpool);
  }
  CHECKER_TRAVELER<NPD_CHECKER> traveler(Comp_unit());
  Is_Trace(Get_Trace(TP_CHECKER, CHK_NPD_TRACE_FLAG),
           (TFile, "%sScan NPD NEW for func %s\n%s", DBar, Dna()->Fname(), DBar));
  traveler.Process();
  Is_Trace(Get_Trace(TP_CHECKER, CHK_NPD_TRACE_FLAG),
           (TFile, "%sEND Scan NPD NEW for func %s\n%s", DBar, Dna()->Fname(), DBar));
  if (VSA_Ignore_Dup) {
    Finalize_ignore_cr_set();
    OPT_POOL_Pop(&lpool, VSA_DUMP_FLAG);
  }
}

void
VSA::Scan_npd_new(CODEREP *cr, STMTREP *sr)
{
  MEM_POOL lpool;
  if (VSA_Ignore_Dup) {
    OPT_POOL_Initialize(&lpool, "ignore cr temp pool", FALSE, VSA_DUMP_FLAG);
    OPT_POOL_Push(&lpool, VSA_DUMP_FLAG);
    Initialize_ignore_cr_set(&lpool);
  }
  Is_Trace(Get_Trace(TP_CHECKER, CHK_NPD_TRACE_FLAG),
           (TFile, "%sScan NPD NEW for cr%d sr%d %s",
            DBar, cr->Coderep_id(), sr->Stmtrep_id(), DBar));
  CHECKER_TRAVELER<NPD_CHECKER> traveler(Comp_unit());
  traveler.Start_check(cr, sr, CS_DEREFERENCE);
  if (VSA_Ignore_Dup) {
    Finalize_ignore_cr_set();
    OPT_POOL_Pop(&lpool, VSA_DUMP_FLAG);
  }
}
