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
// Module: opt_vsa_aofb.cxx
//
// ====================================================================
//

// ====================================================================
// VSA AOFB CHECKER
//   Check Array Out Of Fixed Bound (AOFB)
//
// Algorithm:
//   Only works for array with fixed bound. In C/C++, the lower bound
// always starts from 0, which is fixed. 
//   Because array/buffer always starts from 0 in C/C++, the checker
// only needs to check if subscript may less than zero regardless of
// the array/buffer size. The checker starts from ILOAD/ISTORE base,
// extract subscript and check if the subscript may less than zero
// along with the U-D.
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
#include "opt_vsa_checker.h"
#include "opt_vsa_graph.h"
#include "vsa_annot.h"
#include "intrn_info.h"

// AOFB checker
class AOFB_CHECKER : public SPOS_BASE {
  typedef CHECKER_TRAVELER<AOFB_CHECKER> CHECKER;
private:
  CHECKER            *_checker;
  VALUE_GRAPH         _graph;
  VSA                *_prev_vsa;
  CODEREP            *_prev_cr;
  UINT32              _kind;

private:
  void                Report_vul_error(TRAV_CONTEXT* ctx, CODEREP *cr, ISSUE_CERTAINTY ic = IC_DEFINITELY);

public:
  // traversal from dereference
  enum { SUSPECT = CS_DEREFERENCE };
  // need to check coderep
  enum { ENTITY = TE_CODEREP };
  // need to track SRCPOS
  enum { USE_SRCPOS = TRUE };
  // need to follow eh path
  enum { FOLLOW_EH = TRUE };

public:
  AOFB_CHECKER(TRAV_CONTEXT &ctx, CHECKER *checker)
    : SPOS_BASE(ctx), _checker(checker), _graph(ctx.Mem_pool()),
      _prev_vsa(NULL), _prev_cr(NULL), _kind(FOR_NONSPECIFIC) {
    ctx.Set_Tracing(Get_Trace(TP_CHECKER, CHK_AOFB_TRACE_FLAG));
  }
  const char  *Checker_name() const    { return "AOFB";  }
  void         Set_kind(UINT32 kind)   { _kind |= kind;  }
  const UINT32 Kind()                  { return _kind;   }
  BOOL         Set_check_kind(CHECKER_SUSPECT suspect) { return FALSE; }

  template<CODEKIND _KIND> CHECKER_STATUS
  Check_coderep(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);

  template<OPERATOR _OPR> CHECKER_STATUS
  Check_stmtrep(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);

  CHECKER_STATUS
  Check_heap_obj(CHECK_OBJ &obj, TRAV_CONTEXT* ctx) { return CS_CONT; }

  CHECKER_STATUS
  Check_vsym_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx) { return CS_DONE; }

  void
  Add_target(UINT32 file_idx, ST_IDX idx) { }
};

template<> CHECKER_STATUS inline
AOFB_CHECKER::Check_coderep<CK_CONST>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  CODEREP *cr = obj.Coderep();
  if ((Kind() & FOR_AOB) != FOR_AOB) {
    Is_Trace(ctx->Tracing(), (TFile, "--AOFB: Done with const %lld. kind=0x%x\n",
                                     cr->Const_val(), Kind()));
    return CS_DONE;
  }

  Is_True(_prev_vsa != NULL && _prev_cr != NULL, ("bad prev_vsa or prev_cr"));
  Is_True(cr->Kind() == CK_CONST, ("not const"));
  Is_Trace(ctx->Tracing(), (TFile, "--AOFB: Done with const %lld.\n",
                                   cr->Const_val()));
  OP_RESULT ret = _graph.Add_assign(_prev_vsa, _prev_cr, ctx->Vsa(), cr);
  Is_Trace(ctx->Tracing(), (TFile, "--AOFB: Add cr%d==%lld to graph, ret=%d\n",
                                   _prev_cr->Coderep_id(), cr->Const_val(), ret));
  if (ret == OP_VIOLATION) {
    Report_vul_error(ctx, cr, IC_DEFINITELY);
  }
  return CS_DONE;
}

template<> CHECKER_STATUS inline
AOFB_CHECKER::Check_coderep<CK_RCONST>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  CODEREP *cr = obj.Coderep();
  Is_True(obj.Coderep()->Kind() == CK_RCONST, ("not rconst"));
  Is_Trace(ctx->Tracing(), (TFile, "--AOFB: Done with rconst %lf.\n",
                                   cr->Const_fval()));
  return CS_DONE;
}

template<> CHECKER_STATUS inline
AOFB_CHECKER::Check_coderep<CK_LDA>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_LDA, ("not rconst"));
  Is_True(FALSE, ("LDA used as subscript?"));
  return CS_DONE;
}

template<> CHECKER_STATUS inline
AOFB_CHECKER::Check_coderep<CK_VAR>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_VAR, ("not var"));
  if (sr == NULL) {
    Is_Trace(ctx->Tracing(), (TFile, "--AOFB: Done with def by phi, cr%d.\n", cr->Coderep_id()));
    return CS_DONE;
  }

  IDTYPE aux_id = cr->Aux_id();
  if (aux_id == ctx->Opt_stab()->Default_vsym() ||
      aux_id == ctx->Opt_stab()->Return_vsym()) {
    Is_Trace(ctx->Tracing(), (TFile, "--AOFB: Done with def/ret vsym, cr%d.\n", cr->Coderep_id()));
    return CS_DONE;
  }
  if (cr->Is_flag_set(CF_IS_ZERO_VERSION)) {
    Is_Trace(ctx->Tracing(), (TFile, "--AOFB: Done with zero-version, cr%d.\n", cr->Coderep_id()));
    return CS_DONE;
  }

  BB_NODE *def_bb = NULL;
  if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = cr->Defphi();
    Is_True(phi != NULL, ("not find cr def phi"));
    def_bb = phi->Bb();
  }
  else {
    STMTREP *def = cr->Defstmt();
    Is_True(def != NULL, ("not find cr def stmt"));
    def_bb = def->Bb();
  }

  BB_NODE *cur_bb = sr->Bb();
  // TODO: phi_node is missing here, need to figure out the real def_bb
  //Is_True(def_bb && def_bb->Dominates(cur_bb),
  //        ("invalid def bb"));

  if (def_bb && !cur_bb->Dominates(def_bb)) {
    // TODO: add control dependency here
  }
  return CS_VAR_UD;
}

template<> CHECKER_STATUS inline
AOFB_CHECKER::Check_coderep<CK_IVAR>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_IVAR, ("not ivar"));
  if (cr->Opr() == OPR_MLOAD ||
      cr->Opr() == OPR_ILOADX ||
      cr->Opr() == OPR_PARM)
    return CS_DONE;

  COMP_UNIT *cu = ctx->Comp_unit();
  if ((Kind() & FOR_AOB) != FOR_AOB) {
    Is_Trace(ctx->Tracing(), (TFile, "--AOFB: start checkint cr%d at LINE %d:\n",
                                     cr->Coderep_id(),
                                     Srcpos_To_Line(sr->Linenum())));
    Is_Trace_cmd(ctx->Tracing(), cr->Print(1, TFile));

    VSA_ADDRESS_INFO info;
    if (cu->Analyze_address_info(sr, cr, &info,
                                 sr->Lhs() == cr, FALSE) == FALSE) {
      Is_Trace(ctx->Tracing(), (TFile, "--AOFB: failed to analyze addr expr.\n"));
      return CS_DONE;
    }
    Is_Trace(ctx->Tracing(), (TFile, "--AOFB: find the addr expr:\n"));
    Is_Trace_cmd(ctx->Tracing(), info.Print(cu, TFile));
    // TODO: check if base is fixed
    if ((info.Pos_index().size() + info.Neg_index().size() +
         info.Pos_offset().size() + info.Neg_offset().size()) != 1) {
      Is_Trace(ctx->Tracing(), (TFile, "--AOFB: ignore, more than 1 variant offset.\n"));
      return CS_DONE;
    }

    if (info.Pos_index().size() == 1) {
      CODEREP *index = info.Pos_index().front().first;
      CODEREP *scale = info.Pos_index().front().second;
      Is_True(scale && scale->Kind() == CK_CONST,
              ("scale is not int const"));

      if (index->Kind() == CK_OP) {
        Is_Trace(ctx->Tracing(), (TFile, "--AOFB: subscript cr%d is OP %s, give up.\n",
                                         cr->Coderep_id(), OPERATOR_name(cr->Opr()) + 4));
        return CS_DONE;
      }

      Is_Trace(ctx->Tracing(), (TFile, "--AOFB: find subscript=cr%d scale=%lld ofst=%lld:\n",
                                       index->Coderep_id(), scale->Const_val(),
                                       info.Fix_ofst()));
      Is_Trace_cmd(ctx->Tracing(), index->Print(1, TFile));

      // adjust offset
      INT64 ofst = info.Fix_ofst();
      if (ofst != 0) {
        ofst /= scale->Const_val();
      }
      OP_RESULT ret = _graph.Set_target(ctx->Vsa(), E_GE, index, -ofst);
      _prev_vsa = ctx->Vsa();
      _prev_cr = index;
      Is_Trace(ctx->Tracing(), (TFile, "--AOFB: set target to cr%d >= %lld, ret=%d\n",
                                index->Coderep_id(), -ofst, ret));
      Is_True_Ret(index, ("null index"), CS_DONE);
      obj.Update_var(index);
      Set_orig_stname(obj, ctx);
      Set_kind(FOR_AOB);
      return CS_OP;
    }

    return CS_DONE;
  }

  VSYM_OBJ_REP *vor = ctx->Vsa()->Cr_2_vor(cr);
  if (vor == NULL) {
    // Assert? Add trace
    return CS_IVAR_UD;
  }

  if (vor->Is_entry_chi() || ctx->Vsa()->Is_special_vor(vor)) {
    // Add trace
    return CS_DONE;
  }

  BB_NODE *def_bb = NULL;
  switch (vor->Attr()) {
  case ROR_DEF_BY_CHI:
  case ROR_DEF_BY_COPY:
  case ROR_DEF_BY_ISTORE:
    Is_True(vor->Stmt_def() != NULL, ("defstmt is NULL"));
    def_bb = vor->Stmt_def()->Bb();
    break;

  case ROR_DEF_BY_PHI:
  case ROR_DEF_BY_HORPHI:
    Is_True(vor->Phi_def() != NULL, ("defphi is NULL"));
    def_bb = vor->Phi_def()->Bb();
    break;

  default:
    Is_True(FALSE, ("not handled attr"));
    break;
  }

  BB_NODE *cur_bb = sr->Bb();
  // TODO: phi_node is missing. need to figure out real def_bb
  //Is_True(def_bb != NULL && def_bb->Dominates(cur_bb),
  //        ("invalid def bb"));

  if (def_bb && !cur_bb->Dominates(def_bb)) {
    // TODO: add control dependency here
  }

  ctx->Tracker()->Push(vor->Vsym_obj()->Fld_rep_ptr());
  obj.Update_vsym(vor);

  return CS_VSYM_UD;
}

template<> CHECKER_STATUS
AOFB_CHECKER::Check_coderep<CK_OP>(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  if ((Kind() & FOR_AOB) != FOR_AOB) {
    Is_Trace(ctx->Tracing(), (TFile, "--AOFB: CK_OP not for AOB\n"));
    return CS_DONE;
  }

  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_OP, ("The kind of cr is not CK_OP, cr%d kind: %d.",
                                cr->Coderep_id(), cr->Kind()));
  // TODO: add whole OP to graph?
  Is_Trace(ctx->Tracing(), (TFile, "--AOFB: Done with CK_OP:\n"));
  Is_Trace_cmd(ctx->Tracing(), cr->Print(1, TFile));
  COMP_UNIT *cu = ctx->Comp_unit();

  VSA_ADDRESS_INFO info;
  if (cu->Analyze_pointer_info(NULL, cr, &info, FALSE) == FALSE) {
    Is_Trace(ctx->Tracing(), (TFile, "--AOFB: failed to analyze pointer.\n"));
    return CS_DONE;
  }

  Is_Trace(ctx->Tracing(), (TFile, "--AOFB: continue with index info:\n"));
  Is_Trace_cmd(ctx->Tracing(), info.Print(cu, TFile));
  if ((info.Pos_index().size() + info.Neg_index().size() +
       info.Pos_offset().size() + info.Neg_offset().size()) != 1) {
    Is_Trace(ctx->Tracing(), (TFile, "--AOFB: ignore, more than 1 variant offset.\n"));
    return CS_DONE;
  }

  if (info.Pos_offset().size() == 1) {
    CODEREP *offset = info.Pos_offset().front();
    // TODO: track fixed offset
    Is_Trace(ctx->Tracing(), (TFile, "--AOFB: update subscript=cr%d ofst=%lld:\n",
                                     offset->Coderep_id(), info.Fix_ofst()));
    Is_Trace_cmd(ctx->Tracing(), offset->Print(1, TFile));

    Is_True(_prev_vsa != NULL && _prev_cr != NULL, ("bad prev_vsa or prev_cr"));
    OP_RESULT ret = _graph.Add_assign(_prev_vsa, _prev_cr, ctx->Vsa(), cr);
    _prev_vsa = ctx->Vsa();;
    _prev_cr = offset;
    Is_True_Ret(offset, ("null offset"), CS_DONE);
    obj.Update_var(offset);
    return CS_OP;
  }

  Is_Trace(ctx->Tracing(), (TFile, "--AOFB: TODO done with CK_OP.\n"));
  return CS_DONE;
}

template<> CHECKER_STATUS
AOFB_CHECKER::Check_stmtrep<OPR_OPT_CHI>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_Trace(ctx->Tracing(), (TFile, "--AOFB: continue with OPT_CHI of %s\n",
                                   ctx->Dna()->Fname()));
  return CS_CONT;
}

template<> CHECKER_STATUS
AOFB_CHECKER::Check_stmtrep<OPR_INTRINSIC_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_Trace(ctx->Tracing(), (TFile, "--AOFB: Done with Intrinsic call\n"));
  return CS_DONE;
}

template<> CHECKER_STATUS
AOFB_CHECKER::Check_stmtrep<OPR_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  return CS_CONT;
}

template<> CHECKER_STATUS
AOFB_CHECKER::Check_stmtrep<OPR_ICALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  return CS_CONT;
}

void
AOFB_CHECKER::Report_vul_error(TRAV_CONTEXT* ctx, CODEREP *cr, ISSUE_CERTAINTY ic)
{
  ctx->Vsa()->Report_vul_error(cr, Sp_h(), FOR_AOB, ic);
}

void
VSA::Perform_aofb_analysis()
{
  CHECKER_TRAVELER<AOFB_CHECKER> traveler(Comp_unit());
  Is_Trace(Get_Trace(TP_CHECKER, CHK_AOFB_TRACE_FLAG),
           (TFile, "%sScan AOFB for func %s\n%s", DBar, Dna()->Fname(), DBar));
  traveler.Process();
  Is_Trace(Get_Trace(TP_CHECKER, CHK_AOFB_TRACE_FLAG),
           (TFile, "%sEND Scan AOFB for func %s\n%s", DBar, Dna()->Fname(), DBar));
}
