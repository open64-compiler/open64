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
#include "id_map.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_cfg.h"
#include "opt_htable.h"
#include "opt_mu_chi.h"
#include "opt_fold.h"
#include "opt_vra.h"
#include "opt_vsa.h"

#include <vector>
#include <stack>
#include <map>
#include <algorithm>

#include "opt_vra_tmpl.h"   // for symbolc execution of vra expressions

// explicit instantialization some template functions
void Only_for_template_initialization(VRA* vra, CODEREP* lhs, CODEREP* rhs,
                                      DNA_NODE* dna, const PATH_SELECTED& path)
{
  vra->Compare_cr_xfa<OPR_LT>(lhs, 0, rhs, dna, path);
  vra->Compare_cr_xfa<OPR_GE>(lhs, 0, rhs, dna, path);
}

// =============================================================================
//
// Calculate key for CR
//
// =============================================================================
UINT64
VRA::Key(CODEREP* cr, UINT32 bb_id) const
{
  VSYM_OBJ_REP *vor;
  if ((cr->Kind() == CK_IVAR) &&
      (vor = Vsa()->Cr_2_vor(cr)) != NULL &&
      !Vsa()->Is_special_vor(vor)) {
    Is_True(Vsa()->Vsym_obj_list() != NULL &&
            Vsa()->Vsym_obj_list()->Head() != NULL,
            ("invalid vo list"));
    Is_True(vor->Vsym_obj()->Id() >= Vsa()->Vsym_obj_list()->Head()->Id() &&
            vor->Vsym_obj()->Id() < Vsa()->Last_vsymobj_id(),
            ("bad vo id"));
    INT32 vsym_id = vor->Vsym_obj()->Id() - Vsa()->Vsym_obj_list()->Head()->Id();
    INT32 version = vor->Version();
    // VSYM_ID: 24b, Versin: 19b, BB_ID: 20b, flag: 1b
    Is_True((vsym_id >= 0) && (vsym_id <= 0xFFFFFF),
            ("vsym_id out of bound"));
    Is_True((version >= -1) && (version <= 0x7FFFF),
            ("version out of bound"));
    Is_True((bb_id >= 0) && (bb_id <= 0xFFFFF),
            ("bb_id out of bound"));
    return ((UINT64)vsym_id << 40) | ((UINT64)version << 21) | (bb_id << 1) | 1;
  }
  else {
    UINT32 cr_id = cr->Coderep_id();
    // cr_id: 32b, bb_id: 31b, flag: 1b
    return ((UINT64)cr_id << 32) | ((UINT64)bb_id << 1);
  }
}

// =============================================================================
//
// Analyzer qeery interface
// 
// =============================================================================

// check if cr in bb is in range of [lb, ub)
VRA_RESULT
VRA::Is_expr_in_range(CODEREP* cr, BB_NODE* bb, INT64 lb, INT64 ub, const PATH_SELECTED& paths) const
{
  CODEREP* lhs = Alloc_stack_cr(0);
  Init_const(lhs, cr->Dtyp(), lb);
  CODEREP* rhs = Alloc_stack_cr(0);
  Init_const(rhs, cr->Dtyp(), ub);
  return Is_expr_in_range(cr, bb, lhs, rhs, paths);
}

// check if cr in bb is in range of [lb, ub)
VRA_RESULT
VRA::Is_expr_in_range(CODEREP* cr, BB_NODE* bb, CODEREP* lb, CODEREP* ub, const PATH_SELECTED& paths) const
{
  VRA_RESULT lb_res;
  {
    COUNT_ARRAY visited;
    visited.resize(_expr_table.Size());
    lb_res = Compare_cr<OPR_GE>(cr, bb->Id(), lb, FALSE, paths, visited);
  }
  VRA_RESULT ub_res;
  {
    COUNT_ARRAY visited;
    visited.resize(_expr_table.Size());
    ub_res = Compare_cr<OPR_LT>(cr, bb->Id(), ub, FALSE, paths, visited);
  }
  if (lb_res == VA_NO || ub_res == VA_NO)
    return VA_NO;
  if (lb_res == VA_POSSIBLE || ub_res == VA_POSSIBLE)
    return VA_POSSIBLE;
  if (lb_res == VA_UNKNOWN || ub_res == VA_UNKNOWN)
    return VA_UNKNOWN;
  return VA_YES;
}

// =============================================================================
// check if cr in bb is out range of [lb, ub)
VRA_RESULT
VRA::Is_expr_out_range(CODEREP* cr, BB_NODE* bb, INT64 lb, INT64 ub, const PATH_SELECTED& paths) const
{
  CODEREP* lhs = Alloc_stack_cr(0);
  Init_const(lhs, cr->Dtyp(), lb);
  CODEREP* rhs = Alloc_stack_cr(0);
  Init_const(rhs, cr->Dtyp(), ub);
  return Is_expr_out_range(cr, bb, lhs, rhs, paths);
}

// check if expr in bb is out range of [lb, ub)
VRA_RESULT
VRA::Is_expr_out_range(CODEREP* cr, BB_NODE* bb, CODEREP* lb, CODEREP* ub, const PATH_SELECTED& paths) const
{
  VRA_RESULT lb_res;
  {
    COUNT_ARRAY visited;
    visited.resize(_expr_table.Size());
    lb_res = Compare_cr<OPR_LT>(cr, bb->Id(), lb, FALSE, paths, visited);
  }
  VRA_RESULT ub_res;
  {
    COUNT_ARRAY visited;
    visited.resize(_expr_table.Size());
    ub_res = Compare_cr<OPR_GE>(cr, bb->Id(), ub, FALSE, paths, visited);
  }
  if (lb_res == VA_YES || ub_res == VA_YES)
    return VA_YES;
  if (lb_res == VA_POSSIBLE || ub_res == VA_POSSIBLE)
    return VA_POSSIBLE;
  if (lb_res == VA_UNKNOWN || ub_res == VA_UNKNOWN)
    return VA_UNKNOWN;
  return VA_NO;
}

// =============================================================================
//
// Check if var cr or expr cr in bb is zero
// 
// =============================================================================

VRA_RESULT
VRA::Is_var_zero(CODEREP* cr, BB_NODE* bb) const
{
  Is_True(cr->Kind() == CK_VAR,
          ("unexpected cr kind %d", cr->Kind()));
  PATH_SELECTED paths;
  return Var_cmp_val<OPR_EQ>(cr, bb->Id(), (INT64)0, paths);
}

VRA_RESULT
VRA::Is_expr_zero(CODEREP* cr, BB_NODE* bb) const
{
  if (cr->Kind() == CK_VAR)
    return Is_var_zero(cr, bb);
  CODEREP* zero = New_cr(MTYPE_I8, 0);
  PATH_SELECTED paths;
  VRA_RESULT res = Var_cmp_val<OPR_EQ>(cr, bb->Id(), zero, paths);
  if (res != VA_UNKNOWN)
    return res;
#if 0
  // TODO
  // convert the check of 'a + b == 0' to 'a == -b'
  hash_set<CODEREP*> vars;
  if (Analyze_coderep_vars(cr, vars) == FALSE ||
      vars.size() == 0)
    return VA_UNKNOWN;
  CODEREP* var = *vars.begin();
  CODEREP* out = NULL;
  if (Canonicalize_coderep(cr, var, out) == FALSE ||
      out == NULL)
#endif
  return VA_UNKNOWN;
}

// =============================================================================
// Value range based constant propagation
// =============================================================================

// return const coderep if cr in bb can be evaluated to a constant
CODEREP*
VRA::Prop_const_coderep(TYPE_ID dtyp, CODEREP* cr, BB_NODE* bb, VALBOUND_MAP& cache) const
{
  CODEREP *r0 = NULL, *r1 = NULL;
  INT64 v0, v1;
  OPERATOR opr;
  switch (cr->Kind()) {
  case CK_LDA:
  case CK_RCONST:
    // TODO: handle LDA/RCONST in Prop_const_coderep
    // return NULL because the ofst is possibly lost
    return NULL;

  case CK_CONST:
    return cr;

  case CK_OP:
    opr = cr->Opr();
    if (cr->Kid_count() == 0)
      return NULL;
    if (opr == OPR_EQ) {
      // for OPR_EQ, check if opnd(1) is a constant
      r1 = Prop_const_coderep(dtyp, cr->Opnd(1), bb, cache);
      if (r1 != NULL) {
        Is_True(r1->Kind() == CK_CONST, ("Opnd(1) is not CK_CONST"));
        return r1;
      }
      break;
    }
    // check if opnd(0) is a constant
    r0 = Prop_const_coderep(dtyp, cr->Opnd(0), bb, cache);
    if (r0 == NULL)
      break;
    Is_True(r0->Kind() == CK_CONST, ("Opnd(0) is not CK_CONST"));
    v0 = r0->Const_val();
    if (opr == OPR_NEG) {
      return New_cr(dtyp, -v0);  // TODO: check the type ???
    }
    else if (opr == OPR_ABS) {
      return New_cr(dtyp, v0 > 0 ? v0 : -v0); // TODO: check the type ???
    }
    if (cr->Kid_count() == 1) {
      // TODO: Other unary operators
      return NULL;
    }
    // check if opnd(1) is a constant
    r1 = Prop_const_coderep(dtyp, cr->Opnd(1), bb, cache);
    if (r1 == NULL)
      break;
    Is_True(r1->Kind() == CK_CONST, ("Opnd(1) is not CK_CONST"));
    v1 = r1->Const_val();
    switch (opr) {
    case OPR_ADD:
      return New_cr(dtyp, v0 + v1);
    case OPR_SUB:
      return New_cr(dtyp, v0 - v1);
    case OPR_MPY:
      return New_cr(dtyp, v0 * v1);
    case OPR_DIV:
      return v1 == 0 ? NULL : New_cr(dtyp, v0 / v1);
    case OPR_MAX:
      return New_cr(dtyp, (v0 > v1) ? v0 : v1);
    case OPR_MIN:
      return New_cr(dtyp, (v0 < v1) ? v0 : v1);
    // TODO: Other binary operators
    }
    // TODO: Other ternry or N-ary operators
    return NULL;

  default:
    break;
  }
  UINT32 expr_id = Value_range_rec(cr, bb->Id());
  return Prop_const_scalar(dtyp, expr_id, bb, cache);
}

// check if value range expr_id in bb is a constant
CODEREP*
VRA::Prop_const_scalar(TYPE_ID dtyp, UINT32 expr_id, BB_NODE* bb, VALBOUND_MAP& cache) const
{
  // only visit the same entry less than VISIT_MAX_COUNT times
  VALBOUND_MAP::iterator it = cache.find(expr_id);
  if (it != cache.end()) {
    return (it->second.Vra_bound() != VA_NB && it->second.Min() == it->second.Max())
             ? New_cr(dtyp, it->second.Min()) : NULL;
  }

  const VRA_EXPR& expr = Expr(expr_id);
  CODEREP *ret = NULL;
  switch (expr.Opr()) {
  case OPR_TOP:
  case OPR_BOTTOM:
  case OPR_EMPTY:
  case OPR_PLACEHOLDER:
    cache[expr_id] = VALBOUND(VA_NB, -INT64_MAX, INT64_MAX);
    return NULL;

  case OPR_CODEREP:
    // check if the coderep is a constant
    ret = expr.Cr();
    Is_True(ret != NULL, ("Invalid CODEREP ptr"));
    cache[expr_id] = VALBOUND(VA_NB, -INT64_MAX, INT64_MAX);
    ret = Prop_const_coderep(dtyp, ret, bb, cache);
    if (ret != NULL) {
      Is_True(ret->Kind() == CK_CONST, ("not const"));
      cache[expr_id] = VALBOUND(VA_FB, ret->Const_val(), ret->Const_val());
    }
    return ret;

  case OPR_CONJUNCTION:
  case OPR_DISJUNCTION:
    {
      // check if the conj/disj generates a unique constant value
      INT64 min = -INT64_MAX, max = INT64_MAX;
      if (Get_bounds(dtyp, expr_id, bb, min, max, cache) == TRUE) {
        ret = (min == max) ? New_cr(dtyp, min) : NULL;
      }
      cache[expr_id] = VALBOUND(min == max ? VA_FB : VA_NB, min, max);
    }
    return ret;

  default:
    break;
  }
  cache[expr_id] = VALBOUND(VA_NB, -INT64_MAX, INT64_MAX);
  return NULL;
}

// check if a coderep in bb can be a constant by value range analysis
CODEREP*
VRA::Prop_const_scalar(CODEREP* cr, BB_NODE* bb) const
{
  VALBOUND_MAP cache;
  return Prop_const_coderep(cr->Dtyp(), cr, bb, cache);
}

// Get control variables may impact the reachability of the bb
BOOL
VRA::Get_control_variables(BB_NODE* bb, hash_set<CODEREP*>& vars) const
{
  // TODO: instead of search preds, choose a node in CD?
  Is_True(bb->Pred() != NULL,
          ("first bb does not have a pred"));
  if (bb->Pred()->Multiple_bbs()) {
    Is_True(FALSE, ("TODO: handle multuple bbs"));
    // check preds and do checking against each pred
    return FALSE;
  }
  BB_NODE* pred = bb->Pred()->Node();
  STMTREP* sr = pred->Last_stmtrep();
  Is_True(sr != NULL,
          ("cannot find last stmtrep in pred bb of first bb"));
  if (sr->Opr() == OPR_AGOTO || sr->Opr() == OPR_COMPGOTO) {
    Is_True(FALSE, ("TODO: handle %s", OPERATOR_name(sr->Opr())));
    return FALSE;
  }
  if (sr->Opr() != OPR_TRUEBR && sr->Opr() != OPR_FALSEBR) {
    if (pred->Pred() == NULL)
      return FALSE;
    Is_True(! pred->Pred()->Multiple_bbs(), ("pred should not have multiple preds"));
    return Get_control_variables(pred, vars);
  }
  BOOL ret = Analyze_coderep_vars(sr->Rhs(), vars);
  if (ret == FALSE) {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
             (TFile, ("Get_control_variables: failed to analyze coderep vars")));
    Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 Print_coderep(sr->Rhs(), TFile));
  }
  return ret;
}

// select a control variable easier to analyze the reachability between bb1 and bb2
CODEREP*
VRA::Select_control_variable(BB_NODE* bb1, BB_NODE* bb2) const
{
  hash_set<CODEREP*> bb1_vars;
  hash_set<CODEREP*> bb2_vars;

  if (Get_control_variables(bb1, bb1_vars) == FALSE) {
    return NULL;
  }
  Is_True(bb1_vars.begin() != bb1_vars.end(),
          ("Not find a variable in bb1 control variables"));

  if (Get_control_variables(bb2, bb2_vars) == FALSE) {
    return NULL;
  }
  Is_True(bb2_vars.begin() != bb2_vars.end(),
          ("Not find a variable in bb2 control variables"));

  // find the first common variable
  for (hash_set<CODEREP*>::iterator it = bb1_vars.begin();
       it != bb1_vars.end();
       ++ it) {
    CODEREP* var = *it;
    if (bb2_vars.find(var) != bb2_vars.end()) {
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "Choose this cr to check bb reachability: sym%dv%d cr%d %s",
                       var->Aux_id(), var->Version(), var->Coderep_id(), Var_name(var)));
      return var;
    }
  }
  // no common variables, use the first variable in bb1.
  return *bb1_vars.begin();
}

// Find phi node's pred at given BB
PHI_NODE*
VRA::Find_phi_pred_at_bb(PHI_NODE* phi, BB_NODE* bb, std::vector<bool>& visited) const
{
  Is_True(phi->Bb() != bb, ("wong phi passed"));
  CODEREP *opnd;
  PHI_OPND_ITER opnd_iter(phi);
  FOR_ALL_ELEM(opnd, opnd_iter, Init()) {
    if (opnd->Is_flag_set(CF_DEF_BY_PHI)) {
      PHI_NODE* opnd_phi = opnd->Defphi();
      if (opnd_phi->Bb() == bb)
        return opnd_phi;
      if (visited[opnd_phi->Bb()->Id()] == false) {
        visited[opnd_phi->Bb()->Id()] = true;
        return Find_phi_pred_at_bb(opnd_phi, bb, visited);
      }
    }
  }
  return NULL;
}

// check if there is a path on CFG from first to second
BOOL
VRA::Cfg_has_path(BB_NODE* first, BB_NODE* second, std::vector<UINT32>& visited, UINT32 cnt) const
{
  if (first->Dominates(second))
    return TRUE;
  BB_NODE* df;
  BB_NODE_SET_ITER df_iter;
  FOR_ALL_ELEM(df, df_iter, Init(first->Dom_frontier())) {
    if (df == second)
      return TRUE;
    if (visited[df->Id()] == cnt)
      continue;
    visited[df->Id()] = cnt;
    if (Cfg_has_path(df, second, visited, cnt))
      return TRUE;
  }
  return FALSE;
}

// check if second bb is reachable when the first bb is reachable
VRA_RESULT
VRA::Is_bb_reachable(BB_NODE* first, BB_NODE* second) const
{
  STMTREP* sr = first->Last_stmtrep();
  if (sr->Opr() != OPR_TRUEBR && sr->Opr() != OPR_FALSEBR)
    return VA_UNKNOWN;

  hash_set<CODEREP*> vars;
  if (Analyze_coderep_vars(sr->Rhs(), vars) == FALSE)
    return VA_UNKNOWN;

  BOOL fall_thr = (first->Next() == second);

  for (hash_set<CODEREP*>::iterator it = vars.begin();
       it != vars.end();
       ++ it) {
    CODEREP* var = *it;
    UINT32 expr = Value_range_rec(var, second->Id());
    if (expr == VR_EMPTY)
      return VA_NO;
    expr = Value_range_rec(var, first->Id());
    if (expr == VR_NOT_FOUND ||
        expr == VR_BOTTOM ||
        expr == VR_TOP)
      continue;
    CODEREP* canon_cr = NULL;
    if (Canonicalize_coderep(sr->Rhs(), var, canon_cr) == FALSE)
      continue;

    COUNT_ARRAY visited;
    visited.resize(_expr_table.Size());
    PATH_SELECTED paths;
    VRA_RESULT ret = VA_UNKNOWN;
    BOOL zext = MTYPE_is_unsigned(var->Dtyp());
    switch (canon_cr->Kind()) {
    case CK_OP:
      switch (canon_cr->Opr()) {
      case OPR_EQ:
        ret = Compare_vra_expr<OPR_EQ>(expr, first->Id(), canon_cr->Opnd(1),
                                       zext, paths, visited);
        break;
      case OPR_NE:
        ret = Compare_vra_expr<OPR_NE>(expr, first->Id(), canon_cr->Opnd(1),
                                       zext, paths, visited);
        break;
      case OPR_GT:
        ret = Compare_vra_expr<OPR_GT>(expr, first->Id(), canon_cr->Opnd(1),
                                       zext, paths, visited);
        break;
      case OPR_GE:
        ret = Compare_vra_expr<OPR_GE>(expr, first->Id(), canon_cr->Opnd(1),
                                       zext, paths, visited);
        break;
      case OPR_LT:
        ret = Compare_vra_expr<OPR_LT>(expr, first->Id(), canon_cr->Opnd(1),
                                       zext, paths, visited);
        break;
      case OPR_LE:
        ret = Compare_vra_expr<OPR_LE>(expr, first->Id(), canon_cr->Opnd(1),
                                       zext, paths, visited);
        break;
      }
      break;
    default:
      break;
    }
    if (ret == VA_NO &&
        ((fall_thr && sr->Opr() == OPR_FALSEBR) ||
         (!fall_thr && sr->Opr() == OPR_TRUEBR)))
      return VA_NO;
    // ret == VA_YES may not be reliable
    //if (ret == VA_YES &&
    //    ((fall_thr && sr->Opr() == OPR_TRUEBR) ||
    //     (!fall_thr && sr->Opr() == OPR_FALSEBR)))
    //  return VA_NO;
  }
  return VA_UNKNOWN;
}

// Check if use's value is from def
BOOL
VRA::Is_assign(CODEREP* def, CODEREP* use, BOOL follow_phi) const
{
  Is_True(use != NULL && def != NULL, ("invalid use or def"));
  if (use == def)
    return TRUE;     // check if cr equal at first

  if (use->Kind() != CK_VAR)
    return FALSE;    // ignore non-CK_VAR

  if (use->Is_flag_set(CF_DEF_BY_CHI))
    return FALSE;    // ignore chi def

  // check phi opnd
  if (use->Is_flag_set(CF_DEF_BY_PHI)) {
    if (!follow_phi)
      return FALSE;  // ignore phi if follow_phi is FALSE
    PHI_NODE* phi = use->Defphi();
    for (INT i = 0; i < phi->Size(); ++i) {
      // not follow phi to avoid recursion now
      if (Is_assign(def, phi->OPND(i), FALSE))
        return TRUE;
    }
    return FALSE;
  }

  // check cr def
  STMTREP* sr = use->Defstmt();
  if (sr != NULL)
    return Is_assign(def, sr->Rhs(), follow_phi);
  return FALSE;
}

// Find common cr between lhs and rhs. If common cr found, do canon
// on lhs and rhs, store canon result into lhs_canon and rhs_canon
BOOL
VRA::Find_and_canon_common_cr(CODEREP* lhs, CODEREP* rhs, CODEREP* &lhs_canon, CODEREP* &rhs_canon) const
{
  BOOL ret;
  hash_set<CODEREP*> lhs_vars;
  ret = Analyze_coderep_vars(lhs, lhs_vars);
  if (ret == FALSE)
    return FALSE;
  hash_set<CODEREP*> rhs_vars;
  ret = Analyze_coderep_vars(rhs, rhs_vars);
  if (ret == FALSE)
    return FALSE;
  for (hash_set<CODEREP*>::iterator it = lhs_vars.begin();
       it != lhs_vars.end();
       ++ it) {
    CODEREP* var = *it;
    for (hash_set<CODEREP*>::iterator rit = rhs_vars.begin();
         rit != rhs_vars.end();
         ++ rit) {
      CODEREP* rvar = *rit;
      // check if two CRs are the same or rvar = var
      if (var == rvar ||
          Is_assign(var, rvar, TRUE)) {
        // canon lhs based on var
        ret = Canonicalize_coderep(lhs, var, lhs_canon);
        if (ret == FALSE)
          return FALSE;
        Is_True(lhs_canon != NULL &&
                lhs_canon->Kind() == CK_OP &&
                (lhs_canon->Opnd(0) == var ||
                 (lhs_canon->Opnd(0)->Kind() == CK_OP &&
                  lhs_canon->Opnd(0)->Opr() == OPR_ABS &&
                  lhs_canon->Opnd(0)->Opnd(0) == var)),
            ("invalid lhs canon cr"));
        // canon rhs based on rvar
        ret = Canonicalize_coderep(rhs, rvar, rhs_canon);
        if (ret == FALSE)
          return FALSE;
        Is_True(rhs_canon != NULL &&
                rhs_canon->Kind() == CK_OP &&
                (rhs_canon->Opnd(0) == rvar ||
                 (rhs_canon->Opnd(0)->Kind() == CK_OP &&
                  rhs_canon->Opnd(0)->Opr() == OPR_ABS &&
                  rhs_canon->Opnd(0)->Opnd(0) == rvar)),
            ("invalid rhs canon cr"));
        return TRUE;
      }
    }
  }
  return FALSE;
}

VRA_RESULT
VRA::Compare_cr(CODEREP* def, CODEREP* use, UINT32 def_bb, UINT32 use_bb) const
{
  BOOL trace = Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG);
  Is_Trace(trace,
           (TFile, "VRA::Compare_cr:\n   def in BB%d:", def_bb));
  Is_Trace_cmd(trace, Print_coderep(def, TFile)); 
  Is_Trace(trace,
           (TFile, "\n   use in BB%d:", use_bb));
  Is_Trace_cmd(trace, Print_coderep(use, TFile));
  Is_Trace(trace, (TFile, "\n"));
  CODEREP* def_canon = NULL;
  CODEREP* use_canon = NULL;
  BOOL ret = Find_and_canon_common_cr(def, use, def_canon, use_canon);
  if (ret == FALSE) {
    Is_Trace(trace,
             (TFile, "  Failed to find common var between def and use. Need U-D traversal.\n"));
    return VA_UNKNOWN;
  }

  Is_Trace(trace, (TFile, "VRA::Compare_cr canon def cr:\n"));
  Is_Trace_cmd(trace, Print_coderep(def_canon, TFile));
  Is_Trace(trace, (TFile, "\n"));
  Is_Trace(trace, (TFile, "VRA::Compare_cr canon use cr:\n"));
  Is_Trace_cmd(trace, Print_coderep(use_canon, TFile));
  Is_Trace(trace, (TFile, "\n"));

  PATH_SELECTED paths;
  COUNT_ARRAY visited;
  BOOL zext = MTYPE_is_unsigned(def_canon->Opnd(0)->Dtyp());
  if (zext &&
      use_canon->Opr() == OPR_LT &&
      MTYPE_is_signed(use_canon->Dsctyp()) &&
      use_canon->Opnd(1)->Kind() == CK_CONST &&
      use_canon->Opnd(1)->Const_val() == 0) {
    // convert (x < 0) into ((unsigned x >= 0x80000000) when zext is TRUE
    TYPE_ID dtyp = use_canon->Opnd(0)->Dtyp();
    use_canon = New_cmp_cr(OPR_GE, use_canon->Opnd(0),
                           New_cr(dtyp, 1ULL << (MTYPE_bit_size(dtyp) - 1)));
  }

  visited.resize(_expr_table.Size());
  switch (def_canon->Opr()) {
  case OPR_EQ:
    return Compare_op_cr<OPR_EQ>(use_canon, use_bb,
                                 def_canon->Opnd(1), zext, paths, visited);
  case OPR_NE:
    return Compare_op_cr<OPR_NE>(use_canon, use_bb,
                                 def_canon->Opnd(1), zext, paths, visited);
  case OPR_GT:
    return Compare_op_cr<OPR_GT>(use_canon, use_bb,
                                 def_canon->Opnd(1), zext, paths, visited);
  case OPR_GE:
    return Compare_op_cr<OPR_GE>(use_canon, use_bb,
                                 def_canon->Opnd(1), zext, paths, visited);
  case OPR_LT:
    return Compare_op_cr<OPR_LT>(use_canon, use_bb,
                                 def_canon->Opnd(1), zext, paths, visited);
  case OPR_LE:
    return Compare_op_cr<OPR_LE>(use_canon, use_bb,
                                 def_canon->Opnd(1), zext, paths, visited);
  default:
    Is_True(FALSE, ("TODO, handle operator %s\n", OPERATOR_name(def_canon->Opr())));
  }
  return VA_UNKNOWN;
}

VRA_RESULT
VRA::Check_match(CODEREP* def, CODEREP* use, UINT32 use_bb) const
{
  Is_True(use->Kind() == CK_OP &&
          (use->Opr() == OPR_EQ || use->Opr() == OPR_NE ||
           use->Opr() == OPR_GT || use->Opr() == OPR_GE ||
           use->Opr() == OPR_LT || use->Opr() == OPR_LE),
          ("invalid use kind or opr"));
  VRA_RESULT ret = VA_UNKNOWN;
  PATH_SELECTED paths;
  COUNT_ARRAY visited;
  visited.resize(_expr_table.Size());
  switch(use->Opr()) {
  case OPR_EQ:
    ret = Compare_cr<OPR_EQ>(def, use_bb, use->Opnd(1), FALSE, paths, visited);
    break;
  case OPR_NE:
    ret = Compare_cr<OPR_NE>(def, use_bb, use->Opnd(1), FALSE, paths, visited);
    break;
  case OPR_GT:
    ret = Compare_cr<OPR_GT>(def, use_bb, use->Opnd(1), FALSE, paths, visited);
    break;
  case OPR_GE:
    ret = Compare_cr<OPR_GE>(def, use_bb, use->Opnd(1), FALSE, paths, visited);
    break;
  case OPR_LT:
    ret = Compare_cr<OPR_LT>(def, use_bb, use->Opnd(1), FALSE, paths, visited);
    break;
  case OPR_LE:
    ret = Compare_cr<OPR_LE>(def, use_bb, use->Opnd(1), FALSE, paths, visited);
    break;
  default:
    Is_True(FALSE, ("unexpected opr %s", OPERATOR_name(use->Opr())));
  }
  return ret;
}


CODEREP*
VRA::Get_cond_expr(BB_NODE* pred, BB_NODE* succ) const
{
  Is_True(pred->Succ() != NULL && pred->Succ()->Multiple_bbs(),
          ("bb does not have multiple successors"));
  STMTREP* sr = pred->Last_stmtrep();
  OPERATOR opr = sr->Opr();
  Is_True(opr == OPR_TRUEBR || opr == OPR_FALSEBR ||
          opr == OPR_COMPGOTO || opr == OPR_AGOTO,
          ("unexpected opr %s", OPERATOR_name(opr)));
  if (opr == OPR_COMPGOTO || opr == OPR_AGOTO)
    return NULL;
  CODEREP* cond = sr->Rhs();
  if (cond->Kind() == CK_CONST || cond->Kind() == CK_RCONST)
    return NULL;
  if (cond->Kind() == CK_VAR ||
      cond->Kind() == CK_IVAR ||
      (cond->Kind() == CK_OP &&
       cond->Opr() != OPR_EQ && cond->Opr() != OPR_NE &&
       cond->Opr() != OPR_GE && cond->Opr() != OPR_GT &&
       cond->Opr() != OPR_LE && cond->Opr() != OPR_LT)) {
    // convert if (a) into if (a != 0)
    cond = New_cmp_cr(OPR_NE, cond, (INT64)0);
  }

  if ((opr == OPR_FALSEBR && pred->Next() == succ) ||
      (opr == OPR_TRUEBR && pred->Next() != succ)) {
    return cond;
  }
  else if ((opr == OPR_FALSEBR && pred->Next() != succ) ||
           (opr == OPR_TRUEBR && pred->Next() == succ)) {
    return Complement_cr(cond);
  }
  else {
    Is_True(FALSE, ("wrong pred BB%d - succ BB%d pair", pred->Id(), succ->Id()));
  }
  return NULL;
}

void
VRA::Collect_control_dependencies(BB_NODE* use_bb, BB_NODE* def_bb,
                                  std::vector< std::pair<BB_NODE*, BB_NODE*> >& cds,
                                  std::vector<bool>& visited,
                                  std::vector<UINT32>& df_visited,
                                  UINT32 df_visit_cnt) const
{
  Is_True(use_bb->Id() < _cfg->Total_bb_count(), 
          ("vector overflow %d:%d, may caused by wrong context",
          use_bb->Id(), _cfg->Total_bb_count()));
  if (use_bb->Id() >= _cfg->Total_bb_count() || visited[use_bb->Id()] == true)
    return;
  visited[use_bb->Id()] = true;
  BB_NODE* cd;
  BB_NODE_SET_ITER cd_iter;
  // TODO: change to working list???
  FOR_ALL_ELEM(cd, cd_iter, Init(use_bb->Rcfg_dom_frontier())) {
    Is_True(cd->Succ() != NULL,
            ("cd bb does not have successors"));
    if (!cd->Succ()->Multiple_bbs()) {
      // happens when there are infinite loops and no real edge to exit
      continue;
    }
    if (use_bb->Dominates(cd))
      continue;  // ignore back edge
    if (cd == def_bb ||
        Cfg_has_path(def_bb, cd, df_visited, df_visit_cnt++)) {
      BB_NODE* succ;
      BB_LIST_ITER succ_iter;
      FOR_ALL_ELEM(succ, succ_iter, Init(cd->Succ())) {
        if (succ == use_bb || succ->Dominates(use_bb)) {
          cds.push_back(std::make_pair(cd, succ));
        }
      }
      if (cd != use_bb && cd != def_bb)
        Collect_control_dependencies(cd, def_bb, cds, visited, df_visited, df_visit_cnt++);
    }
  }
}


VRA_RESULT
VRA::Is_path_possible(const std::vector< std::pair<BB_NODE*, BB_NODE*> >& defs,
                      const std::vector< std::pair<BB_NODE*, BB_NODE*> >& uses,
                      UINT32 def_bb, UINT32 use_bb) const
{
  BOOL trace = Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG);
  VRA_RESULT ret = VA_UNKNOWN;
  std::vector< std::pair<BB_NODE*, BB_NODE*> >::const_iterator dit;
  std::vector< std::pair<BB_NODE*, BB_NODE*> >::const_iterator uit;

  for (uit = uses.begin(); uit != uses.end(); ++uit) {
    CODEREP* use = Get_cond_expr(uit->first, uit->second);
    if (use == NULL) {
      Is_Trace(trace,
               (TFile, "VRA::Is_path_possible: failed to get use cond BB%d --> BB%d\n",
                       uit->first->Id(), uit->second->Id()));
      continue;
    }

    Is_Trace(trace,
             (TFile, "VRA::Is_path_possible use cond from BB%d --> BB%d\n   ",
                     uit->first->Id(), uit->second->Id()));
    Is_Trace_cmd(trace, Print_coderep(use, TFile));
    Is_Trace(trace, (TFile, "\n"));

    VRA_RESULT uret = VA_UNKNOWN;
    for (dit = defs.begin(); dit != defs.end(); ++dit) {
      CODEREP* def = Get_cond_expr(dit->first, dit->second);
      if (def == NULL) {
        Is_Trace(trace,
                 (TFile, "VRA::Is_path_possible: failed to get def cond BB%d --> BB%d\n",
                         dit->first->Id(), dit->second->Id()));
        continue;
      }

      Is_Trace(trace,
               (TFile, "VRA::Is_path_possible def cond from BB%d --> BB%d\n   ",
                       dit->first->Id(), dit->second->Id()));
      Is_Trace_cmd(trace, Print_coderep(def, TFile));
      Is_Trace(trace, (TFile, "\n"));

      // compare def & use
      VRA_RESULT dret = Compare_cr(def, use, def_bb, use_bb);
      Is_Trace(trace,
               (TFile, "  returns %s\n", VRA_RESULT_NAME(dret)));
      if (dret == VA_NO) {
        return VA_NO;
      }
      else if (dret > uret) {
        // ATTN: assume the order is UNKNOWN < POSSIBLE < YES
        uret = dret;
      }
    }
    if (uret > ret)
      ret = uret;
  }
  return ret;
}

VRA_RESULT
VRA::Is_path_possible_internal(BB_NODE* use_bb, PHI_NODE* phi, INT opnd) const
{
  BOOL maybe_use = FALSE;
  BOOL maybe_def = FALSE;
  BB_NODE* phi_bb = phi->Bb();
  // check CDs from phi_bb to use_bb
  std::vector< std::pair<BB_NODE*, BB_NODE*> > use_cds;
  {
    std::vector<bool> visited;
    visited.resize(_cfg->Total_bb_count());
    std::vector<UINT32> df_visited;
    df_visited.resize(_cfg->Total_bb_count());
    Collect_control_dependencies(use_bb, phi_bb, use_cds,
                                 visited, df_visited, 1);
  }

  // guard: check if the value of variables in opnd bb satisfied with
  // the value used in CD for this case
  // if (cond) {
  //   a <- blah
  //   flag = 1;
  // }
  // if (flag == 1) {
  //   blah <- a
  // }
  BB_NODE* opnd_bb = phi_bb->Nth_pred(opnd);
  std::vector< std::pair<BB_NODE*, BB_NODE*> >::iterator uit;
  for (uit = use_cds.begin(); uit != use_cds.end(); ++uit) {
    // guard: check if use is reachable on all edge in the CDs
    VRA_RESULT ret = Is_bb_reachable(uit->first, uit->second);
    if (ret == VA_NO)
      return VA_NO;
    // guard: check the cond described above
    CODEREP* cond = Get_cond_expr(uit->first, uit->second);
    if (cond == NULL)
      continue;
    hash_set<CODEREP*> vars;
    if (Analyze_coderep_vars(cond, vars) == false || vars.empty())
      continue;
    hash_set<CODEREP*>::iterator vit;
    for (vit = vars.begin(); vit != vars.end(); ++vit) {
      CODEREP* cond_var = *vit;
      CODEREP* cond_var_def = cond_var;
      // continue cond_var UD if not phi to get more chance to eval
      // condition value
      while (cond_var_def && cond_var_def->Kind() == CK_VAR &&
             !cond_var_def->Is_flag_set(CF_DEF_BY_CHI) &&
             !cond_var_def->Is_flag_set(CF_DEF_BY_PHI)) {
        STMTREP *defsr = cond_var_def->Defstmt();
        if (defsr) {
          cond_var_def = defsr->Rhs();
        } else {
          break;
        }
      }
      if (!cond_var_def || !cond_var_def->Is_flag_set(CF_DEF_BY_PHI))
        continue;
      PHI_NODE* cond_phi = cond_var_def->Defphi();
      Is_True(cond_phi != NULL,
              ("cannot find cond variable defphi"));
      if (cond_phi->Bb() != phi->Bb()) {
        std::vector<bool> visited;
        visited.resize(_cfg->Total_bb_count());
        cond_phi = Find_phi_pred_at_bb(cond_phi, phi->Bb(), visited);
        if (cond_phi == NULL) {
          // if unable to find cond_phi, set the path to maybe
          maybe_use = TRUE;
          continue;
        }
      }
      CODEREP* cond_def = cond_phi->OPND(opnd);
      Is_True(cond_def != NULL,
              ("cannot find cond variable phi opnd"));
      CODEREP* cond_use = NULL;
      if (Canonicalize_coderep(cond, cond_var, cond_use) == FALSE ||
          cond_use == NULL)
        continue;
      VRA_RESULT ret = Check_match(cond_def, cond_use, opnd_bb->Id());
      if (ret == VA_NO) {
        return VA_NO;
      }
      if (ret == VA_UNKNOWN && cond_def->Is_flag_set(CF_DEF_BY_PHI)) {
        // usually vra can't get precise result for phi result
        maybe_use = TRUE;
      }
    }
  }

  // guard: check if the opnd's CD satisfied with the use's CD for this
  // case:
  // if (cond) {
  //    a <- blah
  // }
  // if (cond) {
  //    blah <- a
  // }
  std::vector< std::pair<BB_NODE*, BB_NODE*> > def_cds;
  if (opnd_bb->Dominates(phi_bb)) {
    // empty then or else block
    if (opnd_bb->Succ()->Multiple_bbs()) {
      def_cds.push_back(std::make_pair(opnd_bb, phi_bb));
    }
  }
  else {
    // in then or else block, check the CDs
    BB_NODE* cd;
    BB_NODE_SET_ITER cd_iter;
    BB_NODE* phi_idom = phi_bb->Idom();
    vector<UINT32> visited;
    visited.resize(_cfg->Total_bb_count());
    FOR_ALL_ELEM(cd, cd_iter, Init(opnd_bb->Rcfg_dom_frontier())) {
      if (!phi_idom->Dominates(cd) || cd == opnd_bb ||
          !cd->Succ()->Multiple_bbs())
        continue;
      BB_NODE* succ;
      BB_LIST_ITER succ_iter;
      FOR_ALL_ELEM(succ, succ_iter, Init(cd->Succ())) {
        visited.clear();
        if (succ == opnd_bb || succ->Dominates(opnd_bb)) {
          def_cds.push_back(std::make_pair(cd, succ));
        } else if (!maybe_def && Cfg_has_path(succ, opnd_bb, visited, 1)) {
          // if exists path from succ->opnd_bb, not dominiates opnd bb
          // then there are combined conditions from cd to opnd_bb
          // mark maybe
          maybe_def = TRUE;
        }
      }
    }
  }

  // guard: check if def is reachable on all edges in the CDs
  if (def_cds.size() > 0) {
    std::vector< std::pair<BB_NODE*, BB_NODE*> >::const_iterator dit;
    for (dit = def_cds.begin(); dit != def_cds.end(); ++dit) {
      VRA_RESULT ret = Is_bb_reachable(dit->first, dit->second);
      if (ret == VA_NO)
        return VA_NO;
    }
  }

  // guard: check if conidtions in def and use' CDs are match
  VRA_RESULT res = VA_UNKNOWN;
  if (use_cds.size() == 0) {
    res = VA_YES;
  } else if (def_cds.size() == 0) {
    res = maybe_def ? VA_POSSIBLE : VA_YES;
  } else {
    res = Is_path_possible(def_cds, use_cds, opnd_bb->Id(), use_bb->Id());
    if (res != VA_NO && maybe_use) {
      res = VA_POSSIBLE;
    }
  }
  return res;
}

// =============================================================================
// Is_path_possible
// Check if the path is poosible at given bb if var == val
// =============================================================================
VRA_RESULT
VRA::Is_path_possible(BB_NODE* use, CODEREP *var, CODEREP *val) const
{
  Is_True(var->Kind() == CK_VAR, ("invalid var cr"));
  UINT32 expr = Value_range_rec(var, use->Id());
  if (expr == VR_EMPTY)
    return VA_NO;
  if (expr == VR_NOT_FOUND ||
      expr == VR_BOTTOM ||
      expr == VR_TOP)
    return VA_UNKNOWN;
  PATH_SELECTED paths;
  COUNT_ARRAY visited;
  visited.resize(_expr_table.Size());
  return Compare_vra_expr<OPR_EQ>(expr, use->Id(), val,
                                  MTYPE_is_unsigned(var->Dtyp()), paths, visited);
}

// =============================================================================
// VRA driver
// =============================================================================

void
VRA::Perform(IPSA* ipsa_mgr)
{
  // trace CFG before value range analysis
  Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
           (TFile, "CFG %s before VALUE RANGE ANALYSIS:\n",
                   ST_name(WN_st(_comp_unit->Input_tree()))));
  Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
           (TFile, ("----------------------------\n")));
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               _comp_unit->Cfg()->Print(TFile));
  Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
           (TFile, ("----------------------------\n")));
  
  // build value range expr table
  Build_table(ipsa_mgr);

  // dump value range expr table and coderep value range map
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               Print(TFile));

  // run vra demo checkers to validate value range
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DEMO_FLAG),
               Test_api());
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DEMO_FLAG),
               Test_npd());
  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DEMO_FLAG),
               Test_aob());
}

// entry point for COMP_UNIT
void
COMP_UNIT::Do_vra(IPSA* ipsa_mgr)
{
  if (_vra != NULL)
    return;
  _vra = CXX_NEW(VRA(this), _mem_pool);
  _vra->Perform(ipsa_mgr);
}

