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

#include "defs.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_vra.h"
#include "opt_vsa.h"
#include "erbe.h"

// =============================================================================
//
// class to test vra API
// 
// =============================================================================
class VRA_test_api {
private:
  const VRA* _vra;

public:
  VRA_test_api(const VRA* vra) : _vra(vra) { }

private:
  // test Prop_const_scalar
  void Check_var(CODEREP* cr, BB_NODE* bb, SRCPOS spos) const
  {
    Is_True(cr->Kind() == CK_VAR, ("CR not VAR"));
    CODEREP* ret = _vra->Prop_const_scalar(cr, bb);
    if (ret != NULL) {
      Is_True(ret->Kind() == CK_CONST, ("ret is not CONST"));
      fprintf(TFile, "Check ");
      _vra->Print_coderep(cr, TFile);
      fprintf(TFile, " in BB%d(line:%lld:%lld) constant: %lld. _CHK_ %lld_CONST_%lld\n",
                     bb->Id(), spos, spos,
                     ret->Const_val(),
                     spos, ret->Const_val());
    }
  }

public:
  void Check_coderep(CODEREP* cr, BB_NODE* bb, SRCPOS spos) const
  {
    INT i;
    switch (cr->Kind()) {
    case CK_LDA:
    case CK_CONST:
    case CK_RCONST:
      break;
    case CK_VAR:
      Check_var(cr, bb, spos);
      break;
    case CK_IVAR:
      if (cr->Opr() == OPR_PARM) {
        Check_coderep(cr->Ilod_base(), bb, spos);
        break;
      }
      if (cr->Ilod_base() != NULL) {
        Check_coderep(cr->Ilod_base(), bb, spos);
      }
      if (cr->Istr_base() != NULL) {
        Check_coderep(cr->Istr_base(), bb, spos);
      }
      break;
    case CK_OP:
      for (i = 0; i < cr->Kid_count(); ++i) {
        Check_coderep(cr->Opnd(i), bb, spos);
      }
      break;
    default:
      Is_True(FALSE, ("invalid cr kind %d", cr->Kind()));
    }
  }
};

// Entry point to test VRA api
void
VRA::Test_api()
{
  fprintf(TFile, "Test_prop_const_scalar\n");
  BB_NODE *bb;
  CFG_ITER cfg_iter;
  VRA_test_api tester(this);
  FOR_ALL_ELEM (bb, cfg_iter, Init(_cfg)) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (stmt->Lhs() != NULL) {
        tester.Check_coderep(stmt->Lhs(), bb, stmt->Linenum());
      }
      if (stmt->Rhs() != NULL) {
        tester.Check_coderep(stmt->Rhs(), bb, stmt->Linenum());
      }
    }
  }
}


// =============================================================================
//
// class to test null pointer dereference (NPD) via VRA api
// 
// =============================================================================
class VRA_test_npd
{
private:
  const VRA* _vra;

public:
  VRA_test_npd(const VRA* vra) : _vra(vra) { }

private:
  void Check_conditional_npd(CODEREP* cr, BB_NODE* bb, SRCPOS spos) const
  {
    //ARptMsg arptm;
    BOOL trace = Get_Trace(TP_WOPT2, VSA_VRA_DEMO_FLAG);
    Is_True(cr->Value_invalid_addr() || cr->Value_maydangling(),
            ("not a correct candidate"));
    if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
        cr->Is_flag_set(CF_DEF_BY_CHI))
      return;
    Is_True(! cr->Is_flag_set(CF_IS_ZERO_VERSION) &&
            ! cr->Is_flag_set(CF_DEF_BY_CHI),
            ("not a correct candidate"));
    Is_Trace(trace, (TFile, "VRA::Test_npd sym%dv%d cr%d %s in BB%d\n",
                            cr->Aux_id(), cr->Version(), cr->Coderep_id(),
                            _vra->Var_name(cr), bb->Id()));
    {
      PATH_SELECTED paths;
      VRA_RESULT ret = _vra->Var_cmp_val<OPR_EQ>(cr, bb->Id(), (INT64)0, paths);
      if (ret == VA_NO) {
        Is_Trace(trace, (TFile, "NPD[%d:%d]: sym%dv%d cr%d %s [%c] NPD: NO by VRA\n",
                                SRCPOS_filenum(spos), Srcpos_To_Line(spos),
                                cr->Aux_id(), cr->Version(), cr->Coderep_id(),
                                _vra->Var_name(cr),
                                cr->Value_invalid_addr() ? 'D' : 'M'));
        return;;
      }
    }
    if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
      PHI_NODE* phi = cr->Defphi();
      BB_NODE* phi_bb = phi->Bb();

      Is_Trace(trace, (TFile, "  def by PHI, check the PATH availability\n"));

      BB_NODE* cd;
      BB_NODE_SET_ITER cd_iter;
      std::vector< std::pair<BB_NODE*, BB_NODE*> > use_cds;
      FOR_ALL_ELEM(cd, cd_iter, Init(bb->Rcfg_dom_frontier())) {
        if (phi_bb->Dominates(cd)) {
          BB_NODE* succ;
          BB_LIST_ITER succ_iter;
          FOR_ALL_ELEM(succ, succ_iter, Init(cd->Succ())) {
            if (succ == bb || succ->Dominates(bb)) {
              Is_Trace(trace, (TFile, "  add BB%d --> BB%d to use CD list\n",
                                      cd->Id(), succ->Id()));
              // TODO: for CD's CD, if it's dominated by phi bb, add to vector
              use_cds.push_back(std::make_pair(cd, succ));
            }
          }
        }
      }

      for (INT i = 0; i < phi->Size(); ++i) {
        CODEREP* opnd = phi->OPND(i);
        if (opnd->Value_invalid_addr()) {
          BB_NODE* opnd_bb = phi_bb->Nth_pred(i);
          STMTREP* last_sr = NULL;
          std::vector< std::pair<BB_NODE*, BB_NODE*> > def_cds;
          if (opnd_bb->Dominates(phi_bb)) {
            // empty then or else block, need to check the opnd_bb last stmt
            Is_True(phi_bb->Idom() == opnd_bb,
                    ("opnd bb is not the immediate dom of phi bb"));
            Is_True(opnd_bb->Succ()->Pos(phi_bb) != -1,
                    ("phi bb is not in opnd bb's successor list"));
            if (opnd_bb->Succ()->Multiple_bbs())
              def_cds.push_back(std::make_pair(opnd_bb, phi_bb));
          }
          else {
            // in then or else block, need to check CDs dominated by phi_bb's idom
            BB_NODE* phi_idom = phi_bb->Idom();

            FOR_ALL_ELEM(cd, cd_iter, Init(opnd_bb->Rcfg_dom_frontier())) {
              if (!phi_idom->Dominates(cd))
                continue;
              BB_NODE* succ;
              BB_LIST_ITER succ_iter;
              FOR_ALL_ELEM(succ, succ_iter, Init(cd->Succ())) {
                if (succ == opnd_bb || succ->Dominates(opnd_bb)) {
                Is_Trace(trace, (TFile, "  add BB%d --> BB%d to def CD list\n",
                                        cd->Id(), succ->Id()));
                  // TODO: for CD's CD, if it's dominated by phi bb, add to vector
                  def_cds.push_back(std::make_pair(cd, succ));
                }
              }
            }
          }
          VRA_RESULT res = def_cds.size() == 0 || use_cds.size() == 0 ? VA_YES :
                               _vra->Is_path_possible(def_cds, use_cds, phi_bb->Id(), bb->Id());
          Is_Trace(trace,
                   (TFile, "NPD[%d:%d]: sym%dv%d cr%d %s [D] NPD %s: sym%dv%d cr%d is 0\n",
                           SRCPOS_filenum(spos), Srcpos_To_Line(spos),
                           cr->Aux_id(), cr->Version(), cr->Coderep_id(), _vra->Var_name(cr),
                           VRA_RESULT_NAME(res),
                           opnd->Aux_id(), opnd->Version(), opnd->Coderep_id()));
          //if (res != VA_NO)
          //  ErrMsgSrcpos(EC_VSA_Nullptr_deref, spos, arptm.getCat("NPD", TRUE, TRUE, TRUE), _vra->Var_name(cr), "XXX", "YYY");
          Is_Trace(trace,
                   (TFile, "sym%dv%d cr%d %s [D] NPD %s: sym%dv%d cr%d is 0\n",
                           cr->Aux_id(), cr->Version(), cr->Coderep_id(), _vra->Var_name(cr),
                           VRA_RESULT_NAME(res),
                           opnd->Aux_id(), opnd->Version(), opnd->Coderep_id()));
        }
        else if (opnd->Value_maydangling()) {
          Is_Trace(trace,
                   (TFile, "NPD[%d:%d]: sym%dv%d cr%d %s [M] NPD: sym%dv%d cr%d is 0\n",
                           SRCPOS_filenum(spos), Srcpos_To_Line(spos),
                           cr->Aux_id(), cr->Version(), cr->Coderep_id(), _vra->Var_name(cr),
                           opnd->Aux_id(), opnd->Version(), opnd->Coderep_id()));
          //ErrMsgSrcpos(EC_VSA_Nullptr_may_deref, spos, arptm.getCat("NPD", TRUE, TRUE, TRUE), _vra->Var_name(cr), "XXX", "YYY");
          Is_Trace(trace,
                   (TFile, "TODO: sym%dv%d cr%d %s [M] NPD: sym%dv%d cr%d is 0\n",
                    cr->Aux_id(), cr->Version(), cr->Coderep_id(), _vra->Var_name(cr),
                    opnd->Aux_id(), opnd->Version(), opnd->Coderep_id()));
        }
      }
    }
    else {
      STMTREP* sr = cr->Defstmt();
      Is_True(sr != NULL, ("not find the defstmt"));
      BB_NODE* defbb = sr->Bb();

      Is_Trace(trace,
               (TFile, "NPD[%d:%d]: sym%dv%d cr%d %s [D] NPD: def by stmt",
                       SRCPOS_filenum(spos), Srcpos_To_Line(spos),
                       cr->Aux_id(), cr->Version(), cr->Coderep_id(), _vra->Var_name(cr)));
      //ErrMsgSrcpos(EC_VSA_Nullptr_deref, spos, arptm.getCat("NPD", TRUE, TRUE, TRUE), _vra->Var_name(cr), "XXX", "YYY");
      Is_Trace(trace,
               (TFile, "  def by stmt, report warning directly\n"));
      Is_Trace(trace,
               (TFile, "  sym%dv%d cr%d %s [D] NPD: def by ",
                       cr->Aux_id(), cr->Version(), cr->Coderep_id(), _vra->Var_name(cr)));
      Is_Trace_cmd(trace, _vra->Print_stmtrep(sr, TFile));
    }
  }


  void Check_coderep_null(CODEREP* cr, BB_NODE* bb, SRCPOS spos) const
  {
    //ARptMsg arptm;
    // test code for conditional npd
    if (cr->Kind() == CK_VAR &&
        (cr->Value_invalid_addr() || cr->Value_maydangling()))
      Check_conditional_npd(cr, bb, spos);

    // only check CK_VAR
    if (cr->Kind() == CK_VAR) {
      PATH_SELECTED paths;
      // compare var with "0"
      VRA_RESULT ret = _vra->Var_cmp_val<OPR_EQ>(cr, bb->Id(), (INT64)0, paths);
      fprintf(TFile, "Check ");
      _vra->Print_coderep(cr, TFile);
      fprintf(TFile, " in BB%d(%d:%d) nullptr deref? %d (%s). _CHK_ %d_NULLPTR_%s\n",
                     bb->Id(), SRCPOS_filenum(spos), Srcpos_To_Line(spos),
                     ret, VRA_RESULT_NAME(ret),
                     Srcpos_To_Line(spos), VRA_RESULT_NAME(ret));
      //if (ret != VA_NO)
      //  ErrMsgSrcpos(EC_VSA_Nullptr_deref, spos, arptm.getCat("NPD", TRUE, TRUE, TRUE), _vra->Var_name(cr), "XXX", "YYY");
      if (ret != VA_NO)
        _vra->Print_path(paths, 1, TFile);
    }
    else {
      // TODO, *(p+1), etc, should be job of AOB
    }
  }

public:
  void Check_coderep(CODEREP* cr, BB_NODE* bb, SRCPOS spos) const
  {
    INT i;
    switch (cr->Kind()) {
    case CK_LDA:
    case CK_CONST:
    case CK_RCONST:
      break;
    case CK_VAR:
      break;
    case CK_IVAR:
      if (cr->Opr() == OPR_PARM) {
        Check_coderep(cr->Ilod_base(), bb, spos);
        break;
      }
      if (cr->Ilod_base() != NULL) {
        Check_coderep_null(cr->Ilod_base(), bb, spos);
      }
      if (cr->Istr_base() != NULL) {
        Check_coderep_null(cr->Istr_base(), bb, spos);
      }
      break;
    case CK_OP:
      for (i = 0; i < cr->Kid_count(); ++i) {
        Check_coderep(cr->Opnd(i), bb, spos);
      }
      break;
    default:
      Is_True(FALSE, ("invalid cr kind %d", cr->Kind()));
    }
  }
};

// entry point to test NPD
void
VRA::Test_npd()
{
  fprintf(TFile, "Test_nullref\n");
  BB_NODE *bb;
  CFG_ITER cfg_iter;
  VRA_test_npd checker(this);
  FOR_ALL_ELEM (bb, cfg_iter, Init(_cfg)) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (stmt->Lhs() != NULL) {
        checker.Check_coderep(stmt->Lhs(), bb, stmt->Linenum());
      }
      if (stmt->Rhs() != NULL) {
        checker.Check_coderep(stmt->Rhs(), bb, stmt->Linenum());
      }
    }
  }
}


// =============================================================================
//
// class to test array out-of-bound (AOB) via VRA api
// 
// =============================================================================
class VRA_test_aob {
private:
  const VRA *_vra;
  COMP_UNIT         *_cu;
  OPT_STAB          *_stab;

public:
  VRA_test_aob(const VRA* vra, COMP_UNIT* cu)
    : _vra(vra), _cu(cu), _stab(cu->Opt_stab())
  {
  }

private:
  BOOL Is_sym_array(AUX_ID id, TY_IDX& ety, INT& lb, INT& ub) const
  {
    ST* st = _stab->St(id);
    if (st != NULL) {
      TY_IDX ty = ST_type(st);
      if (TY_kind(ty) == KIND_ARRAY) {
        ARB_HANDLE arb = TY_arb(ty);
        if (ARB_dimension(arb) == 1 &&
            ARB_const_lbnd(arb[0]) &&
            ARB_const_ubnd(arb[0])) {
          lb = ARB_lbnd_val(arb[0]);
          ub = ARB_ubnd_val(arb[0]);
          ety = TY_etype(ty);
          return TRUE;
        }
      }
    }
    return FALSE;
  }

  CODEREP* Skip_cvt(CODEREP* cr) const
  {
    while (cr->Kind() == CK_OP && cr->Opr() == OPR_CVT)
      cr = cr->Opnd(0);
    return cr;
  }

  BOOL Is_coderep_si(CODEREP* cr, CODEREP*& scale, CODEREP*& index) const
  {
    Is_True(cr->Kind() == CK_OP, ("Invalid cr kind %d", cr->Kind()));
    cr = Skip_cvt(cr);
    if (cr->Kind() == CK_OP && cr->Opr() == OPR_MPY) {
      CODEREP* op0 = cr->Opnd(0);
      CODEREP* op1 = cr->Opnd(1);
      if (op0->Kind() == CK_CONST) {
        op1 = Skip_cvt(op1);
        if (op1->Kind() == CK_VAR) {
          scale = op0;
          index = op1;
          return TRUE;
        }
      }
      else if (op1->Kind() == CK_CONST) {
        op0 = Skip_cvt(op0);
        if (op0->Kind() == CK_VAR) {
          scale = op1;
          index = op0;
          return TRUE;
        }
      }
    }
    else {
      if (cr->Kind() == CK_VAR) {
        scale = NULL;
        index = cr;
        return TRUE;
      }
    }
    return FALSE;
  }

  BOOL Is_coderep_sib(CODEREP* cr, CODEREP*& base, CODEREP*& scale, CODEREP*& index) const
  {
    if (cr->Kind() == CK_OP &&
        (cr->Opr() == OPR_ADD || cr->Opr() == OPR_SUB)) {
      CODEREP* op0 = cr->Opnd(0);
      CODEREP* op1 = cr->Opnd(1);
      if (op0->Kind() == CK_LDA && op1->Kind() == CK_OP) {
        if (Is_coderep_si(op1, scale, index) == TRUE) {
          base = op0;
          return TRUE;
        }
      }
      else if (op1->Kind() == CK_LDA && op0->Kind() == CK_OP) {
        if (Is_coderep_si(op0, scale, index) == TRUE) {
          base = op1;
          return TRUE;
        }
      }
    }
    return FALSE;
  }

  void Check_ivar_oob(CODEREP* cr, BB_NODE* bb, SRCPOS spos) const
  {
    CODEREP* base = NULL;
    CODEREP* scale = NULL;
    CODEREP* index = NULL;
    if (Is_coderep_sib(cr, base, scale, index) == TRUE) {
      // Is_sym_array(AUX_ID id, TY_IDX& ety, INT& lb, INT& ub)
      Is_True(base != NULL && base->Kind() == CK_LDA, ("Invalid base found"));
      Is_True(scale == NULL || scale->Kind() == CK_CONST, ("Invalid scale found"));
      Is_True(index != NULL && index->Kind() == CK_VAR, ("Invalid index found"));
      TY_IDX ety = 0;
      INT lb = 0, ub = 0;
      if (Is_sym_array(base->Lda_aux_id(), ety, lb, ub) == FALSE)
        return;
      PATH_SELECTED paths;
      // check if index is in range of [lb, ub+1)
      VRA_RESULT ret =_vra->Is_expr_out_range(index, bb, lb, ub+1, paths);
      fprintf(TFile, "Check ");
      _vra->Print_coderep(cr, TFile);
      fprintf(TFile, " in BB%d(%d:%d) oob deref? %d (%s). _CHK_ %d_OOB_%s\n",
                     bb->Id(), SRCPOS_filenum(spos), Srcpos_To_Line(spos),
                     ret, VRA_RESULT_NAME(ret),
                     Srcpos_To_Line(spos), VRA_RESULT_NAME(ret));
      if (ret != VA_NO)
        _vra->Print_path(paths, 1, TFile);
    }
    else {
      // TODO, check non-sib form on Iload/Istore
    }
  }

  void Check_var_oob(CODEREP* cr, BB_NODE* bb, SRCPOS spos) const
  {
    TY_IDX ety;
    INT lb = 0, ub = 0;
    if (Is_sym_array(cr->Aux_id(), ety, lb, ub) == TRUE) {
      VRA_RESULT ret;
      if (cr->Offset() < lb * TY_size(ety) ||
          cr->Offset() >= (ub + 1) * TY_size(ety)) {
        ret = VA_YES;
      }
      else {
        ret = VA_NO;
      }
      fprintf(TFile, "Check ");
      _vra->Print_coderep(cr, TFile);
      fprintf(TFile, " in BB%d(%d:%d) oob deref? %d (%s). _CHK_ %d_OOB_%s\n",
                     bb->Id(), SRCPOS_filenum(spos), Srcpos_To_Line(spos),
                     ret, VRA_RESULT_NAME(ret),
                     Srcpos_To_Line(spos), VRA_RESULT_NAME(ret));
    }
  }

public:
  void Check_coderep(CODEREP* cr, BB_NODE* bb, SRCPOS spos) const
  {
    INT i;
    ST* st;
    switch (cr->Kind()) {
    case CK_LDA:
    case CK_CONST:
    case CK_RCONST:
      break;
    case CK_VAR:
      st = _stab->St(cr->Aux_id());
      if (st != NULL && TY_kind(ST_type(st)) == KIND_ARRAY)
        Check_var_oob(cr, bb, spos);
      break;
    case CK_IVAR:
      if (cr->Ilod_base() != NULL) {
        Check_ivar_oob(cr->Ilod_base(), bb, spos);
      }
      if (cr->Istr_base() != NULL) {
        Check_ivar_oob(cr->Istr_base(), bb, spos);
      }
      break;
    case CK_OP:
      for (i = 0; i < cr->Kid_count(); ++i) {
        Check_coderep(cr->Opnd(i), bb, spos);
      }
      break;
    default:
      Is_True(FALSE, ("invalid cr kind %d", cr->Kind()));
    }
  }

  void Check_stmtrep(STMTREP* sr, BB_NODE* bb, SRCPOS spos) const
  {
    if (sr->Lhs() != NULL)
      Check_coderep(sr->Lhs(), bb, spos);
    if (sr->Rhs() != NULL)
      Check_coderep(sr->Rhs(), bb, spos);
  }
};

// entry point to test AOB
void
VRA::Test_aob()
{
  fprintf(TFile, "Test_aob\n");
  BB_NODE *bb;
  CFG_ITER cfg_iter;
  VRA_test_aob checker(this, _comp_unit);
  FOR_ALL_ELEM (bb, cfg_iter, Init(_cfg)) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      checker.Check_stmtrep(stmt, bb, Srcpos_To_Line(stmt->Linenum()));
    }
  }
}


// =============================================================================
//
// class to test pointer out-of-bound (POB) via VRA api
// 
// =============================================================================
class VRA_test_pob {
private:
  const VRA *_vra;
  COMP_UNIT         *_cu;
  OPT_STAB          *_stab;

public:
  VRA_test_pob(const VRA* vra, COMP_UNIT* cu)
    : _vra(vra), _cu(cu), _stab(cu->Opt_stab())
  {
  }

private:
  BOOL Is_sym_array(AUX_ID id, TY_IDX& ety, INT& lb, INT& ub) const
  {
    ST* st = _stab->St(id);
    if (st != NULL) {
      TY_IDX ty = ST_type(st);
      if (TY_kind(ty) == KIND_ARRAY) {
        ARB_HANDLE arb = TY_arb(ty);
        if (ARB_dimension(arb) == 1 &&
            ARB_const_lbnd(arb[0]) &&
            ARB_const_ubnd(arb[0])) {
          lb = ARB_lbnd_val(arb[0]);
          ub = ARB_ubnd_val(arb[0]);
          ety = TY_etype(ty);
          return TRUE;
        }
      }
    }
    return FALSE;
  }

  CODEREP* Skip_cvt(CODEREP* cr) const
  {
    while (cr->Kind() == CK_OP && cr->Opr() == OPR_CVT)
      cr = cr->Opnd(0);
    return cr;
  }

  BOOL Is_coderep_si(CODEREP* cr, CODEREP*& scale, CODEREP*& index) const
  {
    Is_True(cr->Kind() == CK_OP, ("Invalid cr kind %d", cr->Kind()));
    cr = Skip_cvt(cr);
    if (cr->Kind() == CK_OP && cr->Opr() == OPR_MPY) {
      CODEREP* op0 = cr->Opnd(0);
      CODEREP* op1 = cr->Opnd(1);
      if (op0->Kind() == CK_CONST) {
        op1 = Skip_cvt(op1);
        if (op1->Kind() == CK_VAR) {
          scale = op0;
          index = op1;
          return TRUE;
        }
      }
      else if (op1->Kind() == CK_CONST) {
        op0 = Skip_cvt(op0);
        if (op0->Kind() == CK_VAR) {
          scale = op1;
          index = op0;
          return TRUE;
        }
      }
    }
    else {
      if (cr->Kind() == CK_VAR) {
        scale = NULL;
        index = cr;
        return TRUE;
      }
    }
    return FALSE;
  }

  BOOL Is_coderep_sib(CODEREP* cr, CODEREP*& base, CODEREP*& scale, CODEREP*& index) const
  {
    if (cr->Kind() == CK_OP &&
        (cr->Opr() == OPR_ADD || cr->Opr() == OPR_SUB)) {
      CODEREP* op0 = cr->Opnd(0);
      CODEREP* op1 = cr->Opnd(1);
      if (op0->Kind() == CK_LDA && op1->Kind() == CK_OP) {
        if (Is_coderep_si(op1, scale, index) == TRUE) {
          base = op0;
          return TRUE;
        }
      }
      else if (op1->Kind() == CK_LDA && op0->Kind() == CK_OP) {
        if (Is_coderep_si(op0, scale, index) == TRUE) {
          base = op1;
          return TRUE;
        }
      }
    }
    return FALSE;
  }

  void Check_coderep_oob(CODEREP* cr, BB_NODE* bb, SRCPOS spos) const
  {
    CODEREP* base = NULL;
    CODEREP* scale = NULL;
    CODEREP* index = NULL;
    if (Is_coderep_sib(cr, base, scale, index) == TRUE) {
      // Is_sym_array(AUX_ID id, TY_IDX& ety, INT& lb, INT& ub)
      Is_True(base != NULL && base->Kind() == CK_LDA, ("Invalid base found"));
      Is_True(scale == NULL || scale->Kind() == CK_CONST, ("Invalid scale found"));
      Is_True(index != NULL && index->Kind() == CK_VAR, ("Invalid index found"));
      TY_IDX ety = 0;
      INT lb = 0, ub = 0;
      if (Is_sym_array(base->Lda_aux_id(), ety, lb, ub) == FALSE)
        return;
      PATH_SELECTED paths;
      // check if index is in range of [lb, ub+1)
      VRA_RESULT ret =_vra->Is_expr_out_range(index, bb, lb, ub+1, paths);
      fprintf(TFile, "Check ");
      _vra->Print_coderep(cr, TFile);
      fprintf(TFile, " in BB%d(%d:%d) oob deref? %d (%s). _CHK_ %d_OOB_%s\n",
                     bb->Id(), SRCPOS_filenum(spos), Srcpos_To_Line(spos),
                     ret, VRA_RESULT_NAME(ret),
                     Srcpos_To_Line(spos), VRA_RESULT_NAME(ret));
      if (ret != VA_NO)
        _vra->Print_path(paths, 1, TFile);
    }
  }

public:
  void Check_coderep(CODEREP* cr, BB_NODE* bb, SRCPOS spos) const
  {
    INT i;
    switch (cr->Kind()) {
    case CK_LDA:
    case CK_CONST:
    case CK_RCONST:
      break;
    case CK_VAR:
      break;
    case CK_IVAR:
      if (cr->Ilod_base() != NULL) {
        Check_coderep_oob(cr->Ilod_base(), bb, spos);
      }
      if (cr->Istr_base() != NULL) {
        Check_coderep_oob(cr->Istr_base(), bb, spos);
      }
      break;
    case CK_OP:
      for (i = 0; i < cr->Kid_count(); ++i) {
        Check_coderep(cr->Opnd(i), bb, spos);
      }
      break;
    default:
      Is_True(FALSE, ("invalid cr kind %d", cr->Kind()));
    }
  }

  void Check_stmtrep(STMTREP* sr, BB_NODE* bb, SRCPOS spos) const
  {
    if (sr->Lhs() != NULL)
      Check_coderep(sr->Lhs(), bb, spos);
    if (sr->Rhs() != NULL)
      Check_coderep(sr->Rhs(), bb, spos);
  }
};

// entry point to test POB
void
VRA::Test_pob()
{
  fprintf(TFile, "Test_pob\n");
  BB_NODE *bb;
  CFG_ITER cfg_iter;
  VRA_test_pob checker(this, _comp_unit);
  FOR_ALL_ELEM (bb, cfg_iter, Init(_cfg)) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      checker.Check_stmtrep(stmt, bb, Srcpos_To_Line(stmt->Linenum()));
    }
  }
}

