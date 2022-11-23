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
#include "config_vsa.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_cfg.h"
#include "opt_htable.h"
#include "opt_mu_chi.h"
#include "opt_fold.h"
#include "opt_vra.h"
#include "targ_sim.h"
#include "opt_vsa.h"
#include "opt_dna.h"
#include "java_defs.h"
#include "opt_cda.h"
#include "opt_cr_util.h"

// ==================================================================
// CDA_BUILDER
// ==================================================================

// CDA_BUILDER
// build CDA/VRA for a function
class CDA_BUILDER {
  // renaming stack for CDA/VRA
  typedef std::vector< std::stack<CDA_VALUE> > CDA_STACK;
  // BB_NODE vector for unresolved BB, which has LOOP_INFO but no IV
  typedef std::vector<BB_NODE*>                BB_VECTOR;

private:
  CDA                  *_cda;             // CDA/VRA manager
  CFG                  *_cfg;             // CFG
  COMP_UNIT            *_comp_unit;       // COMP_UNIT
  std::vector<bool>     _generated;       // CDA/VRA already generated for this BB
  std::vector<bool>     _visited;         // BB already visited
  std::stack<CODEREP*>  _iv_stack;        // track iv for nested loops
  std::stack<STMTREP*>  _br_stack;        // track branch stmt
  std::stack<CDA_VALUE> _cda_stack;       // track cda value
  CDA_STACK             _var_stack;       // renaming stack for variable
  CDA_STACK             _vor_stack;       // renaming stack for virtual variable
  BB_VECTOR             _to_be_resolved;  // bb has loop info but no IV or trip count
  IDTYPE                _first_vo;        // Id of first vo
  BOOL                  _trace;           // trace cda builder

private:
  // temporary use for CODEREP related utilities
  CR_UTIL Util()   { return CR_UTIL(_comp_unit); }

private:
  // initialize the builder
  void Initialize();

  // finalize the builder
  void Finalize();

private:
  // create cda phi based on var/vor phi
  template<typename MAP_TYPE>
  void Create_vra_phi(BB_NODE *bb, INT pos, CDA_VALUE cda, PHI_LIST *phi_list, const MAP_TYPE& map);

  // update cda phi opnd
  template<typename MAP_TYPE>
  void Update_vra_phi(BB_NODE *bb, INT pos, CDA_VALUE cda, const MAP_TYPE& map);

  // rename successor for CDA
  void Rename_cda_succ(BB_NODE *bb, INT pos, CDA_VALUE cda);

  // rename successor for VRA
  void Rename_vra_succ(BB_NODE *bb, INT pos, CDA_VALUE cda);

  // process BB_LOOP for bb
  BOOL Process_bb_loop(BB_NODE *bb);

  // process GOTO
  void Process_goto(BB_NODE *bb);

  // process TRUEBR/FALSEBR
  void Process_condbr(BB_NODE *bb, STMTREP *sr, BOOL truebr);

  // process COMPGOTO
  void Process_compgoto(BB_NODE *bb, STMTREP *sr);

  // process stmt
  template<bool fwd>
  void Process_stmt(STMTREP *sr);

  // forward process stmt
  void Process_stmt_fwd(BB_NODE *bb);

  // backward process stmt
  void Process_stmt_rev(BB_NODE *bb);

  // process bb
  void Process_bb(BB_NODE *bb);

public:
  // constructor
  CDA_BUILDER(CDA *cda, COMP_UNIT *cu)
    : _cda(cda), _cfg(cu->Cfg()), _comp_unit(cu), _first_vo(UINT32_MAX) {
    _trace = Get_Trace(TP_VSA, VSA_CDA_DUMP_FLAG);
  }

public:
  // build CDA?VRA by traversing CFG
  void Build() {
    // initialize builder
    Initialize();

    // traverse DOM tree starts from entry bb
    Process_bb(_comp_unit->Cfg()->Entry_bb());

    // finalize builder
    Finalize();
  }
};

// CDA_BUILDER::Initialize
// initialize cda/vra builder
void
CDA_BUILDER::Initialize()
{
  //  resize _visited, all values are initialized to 0
  _visited.resize(_cfg->Total_bb_count());
  _generated.resize(_cfg->Total_bb_count());
  // resize var rename stack
  _var_stack.resize(_comp_unit->Opt_stab()->Lastidx() + 1);
  // resize vor rename stack
  INT vo_count = _comp_unit->Vsa()->Count_vo_list();
  if (vo_count > 0) {
    _vor_stack.resize(vo_count);
    _first_vo = _comp_unit->Vsa()->Vsym_obj_list()->Head()->Id();
  }
}

// CDA_BUILDER::Finalize
// finalize cda/vra builder
void
CDA_BUILDER::Finalize()
{
  // do nothing
}

// CDA_BUILDER::Rename_cda_succ
// rename cda phi operand in successor bb
void
CDA_BUILDER::Rename_cda_succ(BB_NODE *bb, INT pos, CDA_VALUE cda)
{
  Is_True(bb->Pred() && bb->Pred()->Multiple_bbs(),
          ("bad pred"));
  Is_True(pos >= 0 && pos < bb->Pred()->Len(),
          ("bad pos"));

  // check if phi node is already generated
  CDA_PHI_NODE *phi = NULL;
  if (_generated[bb->Id()]) {
    CDA_VALUE val = _cda->Get_cda(bb);
    if (val.Kind() != CDA_PHI) {
      Is_True(val.Is_null() || val.Kind() == CDA_RHS_TRUE ||
              val.Kind() == CDA_RHS_FALSE, ("bad existing val"));
      return;
    }
    phi = val.Get_phi();
  }
  else {
    phi = _cda->Add_cda_phi(bb, bb->Pred()->Len(), CDA_PHI);
    _generated[bb->Id()] = true;
  }

  Is_True(phi != NULL, ("bad cda phi"));
  phi->Set_opnd(pos, cda);
}

// CDA_BUILDER::Update_vra_phi
// update vra phi operand in successor bb
template<typename MAP_TYPE> void
CDA_BUILDER::Update_vra_phi(BB_NODE *bb, INT pos, CDA_VALUE cda, const MAP_TYPE& map)
{
  for (typename MAP_TYPE::const_iterator it = map.begin();
       it != map.end(); ++it) {
    CDA_VALUE val = _cda->Get_vra(bb, it->second);
    if (val.Is_null())
      continue;
    if (val.Kind() != CDA_PHI) {
      Is_True(FALSE, ("not a phi"));
      continue;
    }
    CDA_PHI_NODE *phi = val.Get_phi();
    Is_True(phi->Bb() == bb->Id() && pos < phi->Size(),
            ("pos out of range"));
    CDA_VALUE opnd = phi->Opnd(pos);
    if (opnd.Is_null()) {
      phi->Set_opnd(pos, cda);
    }
    else {
      Is_True(opnd.Kind() == CDA_CR, ("not a cr"));
    }
  }
}

// CDA_BUILDER::Create_vra_phi
// create vra phi according to var/vor phi
template<typename MAP_TYPE> void
CDA_BUILDER::Create_vra_phi(BB_NODE *bb, INT pos, CDA_VALUE cda, PHI_LIST *phi_list, const MAP_TYPE& map)
{
  PHI_NODE *phi;
  PHI_LIST_ITER phi_iter;
  FOR_ALL_NODE (phi, phi_iter, Init(phi_list)) {
    if (!phi->Live())
      continue;
    typename MAP_TYPE::const_iterator it = map.find(phi->Aux_id());
    if (it == map.end())
      continue;
    CDA_PHI_NODE *vra_phi = _cda->Add_vra_phi(bb, phi->Size(), it->second, CDA_PHI);
    for (int i = 0; i < phi->Size(); ++i) {
      if (phi->OPND(i) != (CODEREP*)it->second) {
        vra_phi->Set_opnd(i, CDA_VALUE(phi->OPND(i)));
      }
      else if (i == pos) {
        vra_phi->Set_opnd(i, cda);
      }
    }
  }
}

// CDA_BUILDER::Rename_vra_succ
// rename vra phi operand in succrssor bb
void
CDA_BUILDER::Rename_vra_succ(BB_NODE *bb, INT pos, CDA_VALUE cda)
{
  if (_br_stack.empty())
    return;

  STMTREP *sr = _br_stack.top();
  Is_True(sr != NULL && sr->Rhs() != NULL &&
          (sr->Opr() == OPR_TRUEBR ||
           sr->Opr() == OPR_FALSEBR ||
           sr->Opr() == OPR_COMPGOTO),
          ("bad br stmt"));

  CODEREP_MAP var_map;
  VOR_MAP vor_map;
  Util().Analyze_cr(sr->Rhs(), var_map, vor_map, FALSE);

  if (_visited[bb->Id()]) {
    if (!var_map.empty())
      Update_vra_phi(bb, pos, cda, var_map);
    if (!vor_map.empty())
      Update_vra_phi(bb, pos, cda, vor_map);
  }
  else {
    // process var phi
    PHI_LIST *var_phi = bb->Phi_list();
    if (var_phi && !var_phi->Is_Empty() && !var_map.empty()) {
      Create_vra_phi(bb, pos, cda, var_phi, var_map);
    }
    // process vor phi
    PHI_LIST *vor_phi = _comp_unit->Vsa()->Bb_vo_philist(bb);
    if (vor_phi && !vor_phi->Is_Empty() && !vor_map.empty()) {
      Create_vra_phi(bb, pos, cda, vor_phi, vor_map);
    }
  }
}

// CDA_BUILDER::Process_bb_loop
// process BB_LOOP available in bb
BOOL
CDA_BUILDER::Process_bb_loop(BB_NODE *bb)
{
  // check if BB_LOOP is available
  BB_LOOP *loop_info = bb->Loop();
  if (loop_info == NULL)
    return FALSE;
  if (loop_info->Body() != bb) {
    Is_Trace(_trace,
             (TFile, "   BB%d: not body of the loop.\n", bb->Id()));
    return FALSE;
  }
  Is_True(bb->Pred() && bb->Pred()->Multiple_bbs(),
          ("loop body doesn\'t have multiple preds"));

  // check IV in BB_LOOP
  CODEREP *iv = loop_info->Iv();
  STMTREP* sr = loop_info->Trip_count_stmt();
  CODEREP *tc = sr ? sr->Rhs() : loop_info->Trip_count_expr();
  if (iv == NULL || tc == NULL) {
    _to_be_resolved.push_back(bb);
    Is_Trace(_trace,
             (TFile, "   BB%d: no iv/tc, resolve later.\n", bb->Id()));
    return FALSE;
  }
  _iv_stack.push(iv);

  // find out loop pre header
  std::vector<BB_NODE *> pred_vec;
  BB_NODE *pred;
  BB_LIST_ITER pred_iter;
  INT32 preheader = 0;
  FOR_ALL_ELEM(pred, pred_iter, Init(bb->Pred())) {
    if (loop_info->Preheader() == pred) {
      break;
    }
    ++ preheader;
  }
  Is_True(loop_info->Preheader() == pred, ("not find loop pred"));

  // create vra for iv
  PHI_NODE* phi;
  PHI_LIST_ITER phi_iter;
  FOR_ALL_NODE(phi, phi_iter, Init(bb->Phi_list())) {
    if (!phi->Live())
      continue;
    CODEREP* phi_res = phi->RESULT();
    if (iv->Aux_id() == phi->Aux_id()) {
      Is_True(!phi_res->Is_flag_set(CF_IS_ZERO_VERSION) &&
              !phi_res->Is_var_volatile(),
              ("bad iv phi"));
      CODEREP *opnd = preheader != phi->Size() ? phi->OPND(preheader)
                                               : phi->RESULT();
      BOOL inc = TRUE;
      CODEREP *step_op = CR_UTIL::Find_assign(loop_info->Step(), iv->Aux_id());
      if (step_op && step_op->Kind() == CK_OP &&
          (step_op->Opr() == OPR_ADD || step_op->Opr() == OPR_SUB)) {
        CODEREP *step_cr = step_op->Opnd(1);
        if (step_cr->Kind() == CK_VAR && step_cr->Aux_id() == iv->Aux_id())
          step_cr = step_op->Opnd(0);
        inc = step_op->Opr() == OPR_ADD &&
                     (step_cr->Kind() != CK_CONST || step_cr->Const_val() > 0);
      }
      CODEREP *lb = (opnd->Is_flag_set(CF_DEF_BY_CHI) ||
                     opnd->Is_flag_set(CF_DEF_BY_PHI)) ? opnd
                                                       : opnd->Defstmt()->Rhs();
      Is_True(lb != NULL, ("not find iv lb"));
      OPCODE opc = OPCODE_make_op(inc ? OPR_ADD : OPR_SUB, lb->Dtyp(), MTYPE_V);
      CODEREP *ub = Util().New_binary_cr(opc, lb, tc, TRUE);
      CODEREP *gle = Util().New_cmp_cr(inc ? OPR_GE : OPR_LE, phi_res, lb);
      CODEREP *lgt = Util().New_cmp_cr(inc ? OPR_LT : OPR_GT, phi_res, ub);
      CDA_PHI_NODE *conj = _cda->Add_vra_phi(bb, 2, phi_res, CDA_CONJ);
      conj->Set_opnd(0, CDA_VALUE(gle));
      conj->Set_opnd(1, CDA_VALUE(lgt));
      _var_stack[iv->Aux_id()].push(CDA_VALUE(conj, CDA_CONJ));
      break;
    }
  }
  Is_True(phi && phi->Aux_id() == iv->Aux_id(),
          ("not find iv phi"));
  return TRUE;
}

// CDA_BUILDER::Process_goto
// process GOTO
void
CDA_BUILDER::Process_goto(BB_NODE *bb)
{
  Is_True(bb->Succ() && !bb->Succ()->Multiple_bbs(),
          ("bad succ"));
  BB_NODE *succ = bb->Succ()->Node();
  Is_True(succ && succ->Pred(),
          ("bad pred"));
  if (!succ->Pred()->Multiple_bbs()) {
    Is_True(_generated[succ->Id()] == false,
            ("cda already generated"));
    Is_Trace(_trace,
             (TFile, " BB%d: no cda, single pred/succ.\n", bb->Id()));
    return;
  }

  // update CDA for nth pred
  if (!_cda_stack.empty()) {
    INT pos = succ->Pred()->Pos(bb);
    Rename_cda_succ(succ, pos, _cda_stack.top());
  }
}

// CDA_BUILDER::Process_condbr
// process TRUEBR/FALSEBR
void
CDA_BUILDER::Process_condbr(BB_NODE *bb, STMTREP *sr, BOOL truebr)
{
  Is_True(sr && (sr->Opr() == OPR_TRUEBR || sr->Opr() == OPR_FALSEBR),
          ("not TRUEBR/FALSEBR"));
  Is_True(bb->Succ() && bb->Succ()->Next() && !bb->Succ()->Next()->Next(),
          ("bad succ"));

  BB_IFINFO *ifinfo = bb->Ifinfo();
  BB_NODE *merge = bb->Ifinfo() ? bb->Ifinfo()->Merge() : NULL;

  BB_NODE *succ;
  BB_LIST_ITER succ_iter;
  FOR_ALL_ELEM (succ, succ_iter, Init(bb->Succ())) {
    Is_True(succ->Pred(), ("bad pred for succ"));
    BOOL cond = (succ == bb->Next())
                  ? (truebr ? FALSE : TRUE)
                  : (truebr ? TRUE : FALSE);
    if (!succ->Pred()->Multiple_bbs()) {
      // unique pred, set CDA directly
      Is_True(!_generated[succ->Id()], ("cda already generated"));
      _generated[succ->Id()] = true;
      BOOL generated = FALSE;

      // check if exit bb of loop
      BB_LOOP *loop_info = NULL;
      CODEREP *iv = NULL;
      if ((loop_info = bb->Loop()) != NULL &&
          (iv = bb->Loop()->Iv()) != NULL &&
          sr->Rhs()->Contains(iv->Aux_id())) {
        Is_True(iv->Is_flag_set(CF_DEF_BY_PHI), ("iv not def by phi"));
        Is_True(sr->Rhs()->Kind() == CK_OP, ("TODO: br rhs is not op"));
        CODEREP* step_op = CR_UTIL::Find_assign(loop_info->Step(), iv->Aux_id());
        if (step_op &&
            step_op->Kind() == CK_OP &&
            (step_op->Opr() == OPR_ADD || step_op->Opr() == OPR_SUB)) {
          CODEREP *step_cr = step_op->Opnd(1);
          if (step_cr->Kind() == CK_VAR && step_cr->Aux_id() == iv->Aux_id())
            step_cr = step_op->Opnd(0);
          BOOL inc = (step_op->Opr() == OPR_ADD) &&
                     (step_cr->Kind() != CK_CONST || step_cr->Const_val() > 0);
          CODEREP *var = NULL;
          CODEREP *ub = NULL;
          if (sr->Rhs()->Kind() != CK_OP) {
            var = sr->Rhs();
            ub = Util().New_const_cr(iv->Dtyp(), 0);
          }
          else {
            Is_True(OPERATOR_is_compare(sr->Rhs()->Opr()), ("not compare op"));
            var = sr->Rhs()->Opnd(0);
            ub = sr->Rhs()->Opnd(1);
            if (ub->Kind() == CK_VAR && ub->Aux_id() == iv->Aux_id()) {
              var = ub;
              ub = sr->Rhs()->Opnd(0);
            }
            OPERATOR opr = cond ? sr->Rhs()->Opr()
                                : CR_UTIL::Complement_opr(sr->Rhs()->Opr());
            if (inc && opr == OPR_GT) {
              OPCODE opc = OPCODE_make_op(OPR_ADD, ub->Dtyp(), MTYPE_V);
              ub = Util().New_binary_cr(OPCODE_make_op(OPR_ADD, ub->Dtyp(), MTYPE_V),
                                        ub, step_cr, TRUE);
            }
            else if (!inc && opr == OPR_LT) {
              ub = Util().New_binary_cr(OPCODE_make_op(OPR_SUB, ub->Dtyp(), MTYPE_V),
                                        ub, step_cr, TRUE);
            }
          }
          CODEREP *cmp = Util().New_cmp_cr(OPR_EQ, var, ub);
          _cda->Add_cda(succ, cmp);
          generated = TRUE;
          Is_Trace(_trace,
                   (TFile, " BB%d: generate cr%d for loop exit as CDA:\n",
                           succ->Id(), cmp->Coderep_id()));
          Is_Trace_cmd(_trace, cmp->Print(TFile));
        }
      }
      if (!generated) {
        _cda->Add_cda(succ, sr, cond);
      }
      Is_Trace(_trace,
               (TFile, " BB%d: single pred after cond-br: ", succ->Id()));
      Is_Trace_cmd(_trace, _cda->Get_cda(succ).Dump(TFile));
      Is_Trace(_trace, (TFile, "\n"));
    }
    else if (succ != merge) {
      // multiple successors
      // add cda for all succ to handle edge from bb1 to bb3
      //      bb1
      //    /    \
      //   bb2   /
      //     \  /
      //     bb3
      INT  pos = -1;
      pos = succ->Pred()->Pos(bb);
      Is_True(pos != -1, ("bb%d not pred of bb%d, bb->Id(), succ->Id()"));
      if (pos != -1) {
        Rename_cda_succ(succ, pos, CDA_VALUE(sr, cond));
      }
#if 0
      BB_NODE *pred;
      BB_LIST_ITER pred_iter;
      BOOL dom = TRUE;
      INT    i = -1;
      // check if bb dominates all predecessors
      FOR_ALL_ELEM (pred, pred_iter, Init(succ->Pred())) {
        ++i;
        if (pos == -1 && pred == bb)
          pos = i;
        if (dom && !bb->Dominates(pred)) {
          dom = FALSE;
          if (pos != -1)
            break;
        }
      }

      if (!dom) {
        // not dom all preds, need a phi
        Rename_cda_succ(succ, pos, CDA_VALUE(sr, cond));
      }
      else {
        // dom all preds, no special CDA on this node
        //_generated[succ->Id()] = true;
        Is_True(_generated[succ->Id()] == false,
                ("cda already generated"));
        Is_Trace(_trace,
                 (TFile, " BB%d: no cda, all preds dominated.\n", succ->Id()));
      }
#endif
    }
    else {
      // ignore merge node which is the successor
      Is_True(bb->Dominates(succ) && succ->Postdominates(bb),
              ("not really merge?"));
      //_generated[succ->Id()] = true;
      Is_True(_generated[succ->Id()] == false,
              ("cda already generated"));
      Is_Trace(_trace,
               (TFile, " BB%d: no cda, merge bb.\n", succ->Id()));
    }
  }
}

// CDA_BUILDER::Process_compgoto
// process COMPGOTO
void
CDA_BUILDER::Process_compgoto(BB_NODE *bb, STMTREP *sr)
{
  Is_True(sr && sr->Opr() == OPR_COMPGOTO, ("not COMPGOTO"));
  BB_SWITCH* switch_info = bb->Switchinfo();
  Is_True(switch_info != NULL, ("BB Switchinfo is NULL"));
  CODEREP* rhs = sr->Rhs();
  Is_True(rhs != NULL, ("COMPGOTO rhs is NULL"));
  CODEREP* var = NULL;
  INT64 adjust;
  if (rhs->Kind() == CK_OP &&
      (rhs->Opr() == OPR_CVT || rhs->Opr() == OPR_CVTL))
    rhs = rhs->Opnd(0);
  if (rhs->Kind() == CK_OP) {
    // handle var +/- const
    if ((rhs->Opr() == OPR_ADD || rhs->Opr() == OPR_SUB) &&
        rhs->Opnd(1)->Kind() == CK_CONST) {
      adjust = rhs->Opnd(1)->Const_val();
      var = rhs->Opnd(0);
      while (var->Kind() == CK_OP &&
             (var->Opr() == OPR_CVT || var->Opr() == OPR_CVTL ||
              var->Opr() == OPR_TRUNC)) {
        // TODO: for CVTL, check if the length matches with the size of the ST
        //  I4I1LDID st
        // I4STID preg
        // COMPGOTO
        //    I4I4LDID preg
        //   I4CVTL 8
        //  I8I4CVT
        //  BLOCK
        //  ...
        //  END_BLOCK
        var = var->Opnd(0);
      }
    }
  }
  if (var == NULL) {
    var = rhs;
    adjust = 0;
  }

  BB_NODE *def_targ = bb->Switchdefault();
  Is_True(def_targ->Pred(), ("bad pred"));

  // traverse all switch entries and add them to targets
  // key in targets is the BB and value is the "value" in "case" statement
  // multiple "case" values may jump to the same target BB
  std::map<BB_NODE*, std::vector<CODEREP*> > targets;
  for (INT32 i = 0; i < bb->Switchentries(); ++i) {
    BB_NODE* targ = bb->Switchcase(i);
    CODEREP* eq = Util().New_cmp_cr(OPR_EQ, var, (INT64)i - adjust);
    targets[targ].push_back(eq);
  }

  // traverse the multimap to generate value range for each target
  std::map<BB_NODE*, std::vector<CODEREP*> >::iterator it = targets.begin();
  for (; it != targets.end(); ++it) {
    BB_NODE* targ = it->first;
    std::vector<CODEREP*> &vec = it->second;
    Is_True(vec.size() > 0, ("bad vec"));
    if (targ == def_targ) {
      // add other conditions to vector for default target
      CODEREP* def_lt = Util().New_cmp_cr(OPR_LT, var, -adjust);
      vec.push_back(def_lt);
      CODEREP* def_gt = Util().New_cmp_cr(OPR_GT, var, (INT64)bb->Switchentries() - 1 - adjust);
      vec.push_back(def_gt);
    }
    CDA_PHI_NODE *phi = NULL;
    if (vec.size() > 1) {
      phi = _cda->Create_cda_phi(bb, vec.size());
      for (INT i = 0; i < vec.size(); ++i) {
        phi->Set_opnd(i, CDA_VALUE(vec[i]));
      }
    }
    if (targ->Pred()->Multiple_bbs()) {
      CDA_VALUE val = phi ? CDA_VALUE(phi, CDA_PHI)  // need CDA_DISJ?
                          : CDA_VALUE(vec[0]);
      INT pos = targ->Pred()->Pos(bb);
      Rename_cda_succ(targ, pos, val);
    }
    else {
      Is_True(_generated[targ->Id()] == false, ("cda already generated"));
      if (phi == NULL)
        _cda->Add_cda(targ, vec[0]);
      else
        _cda->Add_cda_phi(targ, phi, CDA_PHI);       // need CDA_DISJ?
      _generated[targ->Id()] = true;
      Is_Trace(_trace,
               (TFile, " BB%d: compgoto target ", targ->Id()));
      Is_Trace_cmd(_trace, _cda->Get_cda(targ).Dump(TFile));
      Is_Trace(_trace, (TFile, "\n"));
    }
  }
}

// CDA_BUILDER::Process_stmt
// process stmt which may change value range
template<bool fwd> void
CDA_BUILDER::Process_stmt(STMTREP *sr)
{
  OPERATOR opt = sr->Opr();
  // TODO: handle __builtin_assume/__builtin_assert
}

// CDA_BUILDER::Process_stmt_fwd
// process stmt in bb forward
void
CDA_BUILDER::Process_stmt_fwd(BB_NODE *bb)
{
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    Process_stmt<true>(stmt);
  }
}

// CDA_BUILDER::Process_stmt_rev
// process stmt in bb backward
void
CDA_BUILDER::Process_stmt_rev(BB_NODE *bb)
{
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
    Process_stmt<false>(stmt);
  }
}

// CDA_BUILDER::Process_bb
// process bb and dom bb
void
CDA_BUILDER::Process_bb(BB_NODE *bb)
{
  Is_True(_visited[bb->Id()] == false,
          ("BB %d already visited", bb->Id()));
  _visited[bb->Id()] = true;

  Is_Trace(_trace,
           (TFile, "CDAB: enter BB%d %s\n", bb->Id(),
            bb->Last_stmtrep() ? OPERATOR_name(bb->Last_stmtrep()->Opr())
                               : "-nil-"));

  BOOL pop_cda_stack = TRUE;
  if (_generated[bb->Id()]) {
    CDA_VALUE top = _cda->Get_cda(bb);
    _cda_stack.push(top);
    Is_Trace(_trace, (TFile, "   Push cda-stack "));
    Is_Trace_cmd(_trace, top.Dump(TFile));
    Is_Trace(_trace, (TFile, "\n"));
  }
  else if (bb->Pred() && bb->Pred()->Multiple_bbs()) {
    _generated[bb->Id()] = TRUE;
    CDA_PHI_NODE *phi = _cda->Add_cda_phi(bb, bb->Pred()->Len(), CDA_PHI);
    CDA_VALUE top(phi, CDA_PHI);
    _cda_stack.push(top);
    Is_Trace(_trace, (TFile, "   Gen and push cda-stack "));
    Is_Trace_cmd(_trace, top.Dump(TFile));
    Is_Trace(_trace, (TFile, "\n"));
  }
  else {
    pop_cda_stack = FALSE;
  }

  BOOL pop_vra_stack = Process_bb_loop(bb);

  // process stmt in the bb
  Process_stmt_fwd(bb);

  // check last stmt and process falsebr/truebr/compgoto/others respectively
  STMTREP *sr = bb->Last_stmtrep();
  OPERATOR opr = sr ? sr->Opr() : OPERATOR_UNKNOWN;
  BOOL pop_br_stack = FALSE;
  switch (opr) {
  case OPR_FALSEBR:
    pop_br_stack = TRUE;
    _br_stack.push(sr);
    Is_Trace(_trace,
             (TFile, "   Push stmt-stack FALSEBR %d\n", sr->Stmtrep_id()));
    Process_condbr(bb, sr, FALSE);
    break;
  case OPR_TRUEBR:
    pop_br_stack = TRUE;
    _br_stack.push(sr);
    Is_Trace(_trace,
             (TFile, "   Push stmt-stack TRUEBR %d\n", sr->Stmtrep_id()));
    Process_condbr(bb, sr, TRUE);
    break;
  case OPR_COMPGOTO:
    pop_br_stack = TRUE;
    _br_stack.push(sr);
    Is_Trace(_trace,
             (TFile, "   Push stmt-stack COMPGOTO %d\n", sr->Stmtrep_id()));
    Process_compgoto(bb, sr);
    break;
  case OPR_AGOTO:
    // ignore AGOTO
    break;
  default:
    if (bb != _cfg->Fake_entry_bb() && bb->Succ())
      Process_goto(bb);
    break;
  }

  // process dom bb
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Process_bb(dom_bb);
  }

  // pop-up br stack
  if (pop_br_stack) {
    Is_True(!_br_stack.empty() && _br_stack.top() == sr,
            ("br stack mismatch"));
    Is_Trace(_trace,
             (TFile, "   Pop stmt-stack %s %d\n",
              OPERATOR_name(sr->Opr()), sr->Stmtrep_id()));
    _br_stack.pop();
  }

  // reverse proocess stmt
  Process_stmt_rev(bb);

  // pop-up vra stack
  if (pop_vra_stack) {
    CODEREP *iv = bb->Loop() ? bb->Loop()->Iv() : NULL;
    Is_True(iv && bb->Loop()->Body() == bb,
            ("bad bb loop"));
    Is_True(!_iv_stack.empty() && iv == _iv_stack.top(),
            ("bad iv stack"));
    Is_True(!_var_stack[iv->Aux_id()].empty() &&
            _var_stack[iv->Aux_id()].top() == _cda->Get_vra(bb, iv),
            ("bad vra stack"));
    if (iv) {
      _iv_stack.pop();
      _var_stack[iv->Aux_id()].pop();
    }
  }

  // pop-up cda stack
  if (pop_cda_stack) {
    Is_True(!_cda_stack.empty() && _cda_stack.top() == _cda->Get_cda(bb),
            ("cda stack mismatch"));
    Is_Trace(_trace, (TFile, "   Pop cda-stack "));
    Is_Trace_cmd(_trace, _cda_stack.top().Dump(TFile));
    Is_Trace(_trace, (TFile, "\n"));
    _cda_stack.pop();
  }
  Is_Trace(_trace, (TFile, "CDAB: exit BB%d\n", bb->Id()));
}

// ==================================================================
// CDA_VALUE
// ==================================================================

// CDA_VALUE::Bb
// get bb where the CDA_VALUE is defined. Input bb is where the
// CDA_VALUE is annotated
IDTYPE
CDA_VALUE::Bb(BB_NODE *bb) const
{
  Is_True_Ret(!Is_null(), ("value is null"), 0);
  CODEREP *cr;
  switch (Kind()) {
  case CDA_CR:
    cr = Get_as<CODEREP>();
    if (cr->Kind() == CK_VAR || cr->Kind() == CK_IVAR) {
      Is_True(cr->Get_defstmt(), ("no defstmt for cr"));
      return cr->Get_defstmt()->Bb()->Id();
    }
    else if (cr->Kind() == CK_OP) {
      Is_True(cr->Opr() == OPR_EQ || cr->Opr() == OPR_NE ||
              cr->Opr() == OPR_GE || cr->Opr() == OPR_GT ||
              cr->Opr() == OPR_LE || cr->Opr() == OPR_LT,
              ("invalid cr"));
      Is_True(bb->Pred() && !bb->Pred()->Multiple_bbs(),
              ("bb pred invalid"));
      BB_NODE *pred = bb->Pred()->Node();
      Is_True(pred->Kind() == BB_VARGOTO, ("not vargoto"));
      return pred->Id();
    }
    Is_True(FALSE, ("bad cr kind"));
    return 0;
  case CDA_CONJ:
  case CDA_PHI:
    return Get_as<CDA_PHI_NODE>()->Bb();
  case CDA_RHS_TRUE:
  case CDA_RHS_FALSE:
    return Get_as<STMTREP>()->Bb()->Id();
  case CDA_BB:
    return Get_as<BB_NODE>()->Id();
  default:
    Is_True(FALSE, ("bad kind"));
    return 0;
  }
}

// CDA_VALUE::Dump
// dump CDA_VALUE to FILE*
void
CDA_VALUE::Dump(FILE *fp) const
{
  if (Is_null()) {
    fprintf(fp, "-nil-");
    return;
  }
  switch (Kind()) {
  case CDA_CR:
    fprintf(fp, "cr%d", Get_as<CODEREP>()->Coderep_id());
    break;
  case CDA_CONJ:
    fprintf(fp, "CONJ ");
    Get_as<CDA_PHI_NODE>()->Dump(fp);
    break;
  case CDA_PHI:
    fprintf(fp, "PHI ");
    Get_as<CDA_PHI_NODE>()->Dump(fp);
    break;
  case CDA_RHS_TRUE:
    {
      STMTREP *sr = Get_as<STMTREP>();
      fprintf(fp, "BB%d sr%d cr%d TRUE", sr->Bb()->Id(),
             sr->Stmtrep_id(), sr->Rhs()->Coderep_id());
    }
    break;
  case CDA_RHS_FALSE:
    {
      STMTREP *sr = Get_as<STMTREP>();
      fprintf(fp, "BB%d sr%d cr%d FALSE", sr->Bb()->Id(),
             sr->Stmtrep_id(), sr->Rhs()->Coderep_id());
    }
    break;
  case CDA_BB:
    fprintf(fp, "BB%d", Get_as<BB_NODE>()->Id());
    break;
  default:
    Is_True(FALSE, ("bad kind"));
    break;
  }
}

// ==================================================================
// CDA_PHI_NODE
// ==================================================================

// CDA_PHI_NODE::Dump
// dump CDA_PHI_NODE to FILE*
void
CDA_PHI_NODE::Dump(FILE *fp) const
{
  fprintf(fp, "BB%d (", _bb);
  for (UINT i = 0; i < _size; ++i) {
    if (i > 0)
      fprintf(fp, ", ");
    Opnd(i).Dump(fp);
  }
  fprintf(fp, ")");
}

// ==================================================================
// CDA
// ==================================================================

// CDA::Build
// build CDA/VRA for function
void
CDA::Build()
{
  Is_True(_cda_map == NULL, ("CDA_MAP already created"));
  Is_True(_vra_map == NULL, ("VRA_MAP already created"));

  // initialize members
  Initialize();

  // build CDA/VRA
  CDA_BUILDER bldr(this, _comp_unit);
  bldr.Build();

  // dump CDA/VRA if trace is on
  BOOL trace = Get_Trace(TP_VSA, VSA_CDA_DUMP_FLAG);
  Is_Trace(trace,
           (TFile, "++++ CDA/VRA dump for %s:\n",
            _comp_unit->Dna()->Fname()));
  Is_Trace_cmd(trace, _comp_unit->Cfg()->Print(TFile));
  Is_Trace_cmd(trace, Dump(TFile));
}

// CDA::Compose_vra_key
// compose key for CK_VAR or CK_IVAR
UINT64
CDA::Compose_vra_key(BB_NODE *bb, CODEREP *cr) const
{
  Is_True(cr->Kind() == CK_VAR || cr->Kind() == CK_IVAR,
          ("not var or ivar"));
  bool is_vor = (cr->Kind() == CK_IVAR);
  UINT32 obj_id;
  if (is_vor) {
    // for CK_IVAR, use vsym_obj's ID
    VSYM_OBJ_REP *vor = _comp_unit->Vsa()->Cr_2_vor(cr);
    Is_True_Ret(vor != NULL,
                ("not find vor"), 0);
    obj_id = vor->Vsym_obj()->Id();
  }
  else {
    // for CK_VAR, use coderep_id (should be AUX_ID?)
    obj_id = cr->Coderep_id();
  }
  return Compose_vra_key(is_vor, obj_id, bb->Id());
}

// CDA::Compose_vra_key
// compose key for VSYM_OBJ_REP

// CDA::Dump
// dump CDA to FILE*
void
CDA::Dump(FILE *fp) const
{
  // dump control dependency annotations
  fprintf(fp, "CDA %s - control dependency dump:\n",
          _comp_unit->Dna()->Fname());
  for (CDA_MAP::const_iterator it = _cda_map->begin();
       it != _cda_map->end(); ++it) {
    fprintf(fp, " BB%d: ", it->first);
    it->second.Dump(fp);
    fprintf(fp, "\n");
  }

  // dump value range annotations
  fprintf(fp, "VRA %s - value range dump:\n",
          _comp_unit->Dna()->Fname());
  for (VRA_MAP::const_iterator it = _vra_map->begin();
       it != _vra_map->end(); ++it) {
    bool is_vor = FALSE;
    UINT32 obj_id = 0;
    UINT32 bb_id = 0;
    Extract_vra_key(it->first, is_vor, obj_id, bb_id);
    fprintf(fp, " BB%d %s%d: ",
            bb_id, is_vor ? "vsym" : "sym", obj_id);
    it->second.Dump(fp);
    fprintf(fp, "\n");
  }
}

// CDA::Print
// print single bb's CDA and VRA
void
CDA::Print(IDTYPE bb_id) const
{
  // dump bb's control dependency annotations
  CDA_MAP::const_iterator it = _cda_map->find(bb_id);
  if (it == _cda_map->end()) {
    fprintf(stdout, "CDA BB%d: no cda\n", bb_id);
    return;
  }
  fprintf(stdout, "CDA BB%d: ", bb_id);
  it->second.Dump(stdout);
  fprintf(stdout, "\n");

  // dump bb's value range annotations
  for (VRA_MAP::const_iterator it = _vra_map->begin();
       it != _vra_map->end(); ++it) {
    bool is_vor = FALSE;
    UINT32 obj = 0;
    UINT32 bb = 0;
    Extract_vra_key(it->first, is_vor, obj, bb);
    if (bb_id != bb)
      continue;
    fprintf(stdout, "VRA %s:%d BB%d: ",
            is_vor ? "VOR" : "VAR", obj, bb);
    it->second.Dump(stdout);
    fprintf(stdout, "\n");
  }
}

// ==================================================================
// COMP_UNIT::Do_cda
// ==================================================================
void
COMP_UNIT::Do_cda(IPSA* ipsa_mgr)
{
  Is_True(_cda == NULL, ("cra already created"));
  _cda = CXX_NEW(CDA(this), _mem_pool);
  _cda->Build();
}
