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

// ==================================================================
//
// Module: opt_vsa_graph.cxx
//
// ==================================================================

//
// collect values and dependencies during the U-D or D-U traversal and
// put them in the graph. when there is a circle in the graph, that
// means the assumption we made is confirmed or the path has conflict
// control dependencies.
//

#include "defs.h"
#include "opt_defs.h"
#include "vsa_defs.h"
#include "opt_bb.h"
#include "opt_htable.h"
#include "opt_vsa.h"
#include "opt_dna.h"
#include "opt_vsa_graph.h"
#include "opt_cda.h"
#include "opt_cr_util.h"

const char *
OP_RESULT_name(OP_RESULT res)
{
  switch (res) {
  case OP_CONTINUE:  return "continue";
  case OP_PROVEN:    return "proven";
  case OP_VIOLATION: return "violation";
  case OP_CONFLICT:  return "conflict";
  default:           return "error";
  }
}

VALUE_GRAPH::NODE VALUE_GRAPH::_null_node(Malloc_Mem_Pool, VG_NULL_IDX);
VALUE_GRAPH::EDGE VALUE_GRAPH::_null_edge(VG_NULL_IDX);

// ==================================================================
// VALUE_GRAPH::EDGE::Dump
// ==================================================================
void
VALUE_GRAPH::EDGE::Dump(EDGE_IDX idx, FILE *fp) const
{
  static const char* edge_oper_name[E_LAST] = {
    "n/a", "==", "!=", ">=", "> ", "<=", "<"
  };
  Is_True(_opr >= E_NONE && _opr < E_LAST, ("wrong opr"));
  fprintf(fp, " %.2d: %d %s %d %c%c%c",
              idx, _lhs, edge_oper_name[_opr], _rhs,
              (_flag & EF_TARGET) == EF_TARGET ? 't' : '-',
              (_flag & EF_DISJ) == EF_DISJ ? 'd' : '-',
              (_flag & EF_EVAL) == EF_EVAL ? 'e' : '-');
  if ((_flag & EF_DISJ) == EF_DISJ)
    fprintf(fp, " disj(%d)", _disj);
  fprintf(fp, "\n");
}

// ==================================================================
// VALUE_GRAPH::NODE::Dump
// ==================================================================
void
VALUE_GRAPH::NODE::Dump(NODE_IDX idx, FILE *fp) const
{
  fprintf(fp, "%.2d: val=%lld cnt=%d next=%d parent=%d\n out edges: ",
              idx, _const_val, _const_num, _next_node, _par_node);
  // out edgesa
  BOOL need_comma = FALSE;
  for (EIDX_ITER it = Out_edge_begin(); it != Out_edge_end(); ++it) {
    if (need_comma)
      fprintf(fp, ", ");
    else
      need_comma = TRUE;
    fprintf(fp, "%d", *it);
  }
  // in edges
  fprintf(fp, "\n in edges:\n");
  need_comma = FALSE;
  for (EIDX_ITER it = In_edge_begin(); it != In_edge_end(); ++it) {
    if (need_comma)
      fprintf(fp, ", ");
    else
      need_comma = TRUE;
    fprintf(fp, "%d", *it);
  }
  // stack
  fprintf(fp, "\n stack:\n");
  MARK_STACK::const_iterator stack_end = _mark_stack.end();
  for (MARK_STACK::const_iterator it = _mark_stack.begin();
       it != stack_end; ++it) {
    fprintf(fp, "  in_edge=%d out_edge=%d const=%d next=%d par=%d\n",
                it->_in_edge, it->_out_edge, it->_const_num,
                it->_next_node, it->_par_node);
  }
}

// ==================================================================
// VALUE_GRAPH::Dump_map
// ==================================================================
static inline void
Dump_key(INT64 key, FILE *fp)
{
  fprintf(fp, "%lld", key);
}

static inline void
Dump_key(CODEREP *cr, FILE *fp)
{
  switch (cr->Kind()) {
  case CK_LDA:
    fprintf(fp, "LDA TODO");
    break;

  case CK_CONST:
    fprintf(fp, "%lld", cr->Const_val());
    break;

  case CK_RCONST:
    fprintf(fp, "%lf", cr->Const_fval());
    break;

  case CK_VAR:
    fprintf(fp, "VAR sym%dv%d cr%d",
                cr->Aux_id(), cr->Version(),
                cr->Coderep_id());
    break;

  case CK_IVAR:
    fprintf(fp, "IVAR %s cr%d *(",
                OPERATOR_name(cr->Opr()) + 4,
                cr->Coderep_id());
    Dump_key(cr->Ilod_base(), fp);
    fprintf(fp, ")");
    break;

  case CK_OP:
    fprintf(fp, "OP %s cr%d (",
                OPERATOR_name(cr->Opr()),
                cr->Coderep_id());
    for (INT i = 0; i < cr->Kid_count(); ++i) {
      if (i > 0)
        fprintf(fp, ", ");
      Dump_key(cr->Opnd(i), fp);
    }
    fprintf(fp, ")");
    break;
  default:
    Is_True(FALSE, ("Unknown cr kind"));
    break;
  }
}

static inline void
Dump_key(VSYM_OBJ_REP *vor, FILE *fp)
{
  fprintf(fp, "VOR vo%dv%d",
              vor->Vsym_obj()->Id(), vor->Version());
}

template<typename _KEY> void
VALUE_GRAPH::Dump_map(const VALUE_GRAPH::NODE_MAP &map, FILE *fp) const
{
  NODE_MAP::const_iterator map_end = map.end();
  for (NODE_MAP::const_iterator it = map.begin();
       it != map_end; ++it) {
    fprintf(fp, " ");
    Dump_key((_KEY)it->first, fp);
    fprintf(fp, " -> %d\n", it->second);
  }
}

// ==================================================================
// VALUE_GRAPH::Dump
// ==================================================================
void
VALUE_GRAPH::Dump(NODE_IDX idx, FILE *fp) const
{
  if (idx >= _nodes.size()) {
    fprintf(fp, "Error: idx(%d) >= size(%ld).\n", idx, _nodes.size());
    return;
  }

  const NODE *node = Node(idx);
  fprintf(fp, "%.2d: val=%lld cnt=%d next=%d parent=%d\n out edges:\n",
              idx,
              node->_const_val, node->_const_num,
              node->_next_node, node->_par_node);
  EIDX_ITER it;
  // out edge
  for (it = node->Out_edge_begin(); it != node->Out_edge_end(); ++it) {
    if (*it >= _edges.size()) {
      fprintf(fp, "Error: idx(%d) >= size(%ld).\n", *it, _edges.size());
      continue;
    }
    const EDGE *edge = Edge(*it);
    edge->Dump(*it, fp);
  }
  // in  edgea
  fprintf(fp, " in edges:\n");
  for (it = node->In_edge_begin(); it != node->In_edge_end(); ++it) {
    if (*it >= _edges.size()) {
      fprintf(fp, "Error: idx(%d) >= size(%ld).\n", *it, _edges.size());
      continue;
    }
    const EDGE *edge = Edge(*it);
    edge->Dump(*it, fp);
  }

  fprintf(fp, " stack:\n");
  NODE::MARK_STACK::const_iterator stack_end = node->_mark_stack.end();
  for (NODE::MARK_STACK::const_iterator it = node->_mark_stack.begin();
       it != stack_end; ++it) {
    fprintf(fp, "  in_edge=%d out_edge=%d const=%d next=%d par=%d\n",
                it->_in_edge, it->_out_edge, it->_const_num,
                it->_next_node, it->_par_node);
  }
}

// ==================================================================
// VALUE_GRAPH::Dump
// ==================================================================
void
VALUE_GRAPH::Dump(FILE *fp) const
{
  fprintf(fp, "Graph: nodes=%ld edges=%ld\n", _nodes.size(), _edges.size());
  for (NODE_IDX idx = VG_INIT_IDX; idx < _nodes.size(); ++idx) {
    Dump(idx, fp);
  }
  fprintf(fp, "Constant map:\n");
  Dump_map<INT64>(_cst_map, fp);
  fprintf(fp, "Variable map:\n");
  Dump_map<CODEREP*>(_var_map, fp);
  fprintf(fp, "Vsym obj map:\n");
  Dump_map<VSYM_OBJ_REP*>(_vor_map, fp);
  fprintf(fp, "Operator map:\n");
  Dump_map<CODEREP*>(_op_map, fp);
}

// ==================================================================
// VALUE_GRAPH::Const_val
// ==================================================================
UINT64
VALUE_GRAPH::Const_val(CODEREP *cr)
{
  Is_True(cr->Kind() == CK_CONST ||
          cr->Kind() == CK_RCONST ||
          cr->Kind() == CK_LDA, ("bad cr"));
  if (cr->Kind() == CK_CONST) {
    return (cr->Const_val() << 2) | INT_CONST;
  }
  else if (cr->Kind() == CK_RCONST) {
    double d = cr->Const_fval();
    return *reinterpret_cast<uint64_t*>(&d) | FLOAT_CONST;
  }
  else if (cr->Kind() == CK_LDA) {
    ST *st = cr->Lda_base_st();
    if (ST_level(st) == GLOBAL_SYMTAB) {
      // TODO: switch to def file_index and st_index
      return ((UINT64)ST_st_idx(st)) << 32 | File_Index << 2 | GLOBAL_LDA;
    }
    else {
      return reinterpret_cast<UINT64>(cr) | LOCAL_LDA;
    }
  }
  else {
    return INT64_MAX;
  }
}

// ==================================================================
// evaluate if op2 is satisfied or conflict when op1 is satisfied
// ==================================================================
static OP_RESULT
Eval_opr(EDGE_OPER op1, EDGE_OPER op2)
{
  static const OP_RESULT res[E_LAST - 1][E_LAST - 1] = {
    // op1 is EQ, op2 is from EQ to LT
    { OP_PROVEN, OP_CONFLICT, OP_PROVEN, OP_CONFLICT, OP_PROVEN, OP_CONFLICT },
    // op1 is NE, op2 is from EQ to LT
    { OP_CONFLICT, OP_PROVEN, OP_CONTINUE, OP_PROVEN, OP_CONTINUE, OP_PROVEN },
    // op1 is GE, op2 is from EQ to LT
    { OP_CONTINUE, OP_CONTINUE, OP_PROVEN, OP_CONTINUE, OP_CONTINUE, OP_CONFLICT },
    // op1 is GT, op2 is from EQ to LT
    { OP_CONFLICT, OP_PROVEN, OP_PROVEN, OP_PROVEN, OP_CONFLICT, OP_CONFLICT },
    // op1 is LE, op2 is from EQ to LT
    { OP_CONTINUE, OP_CONTINUE, OP_CONTINUE, OP_CONFLICT, OP_PROVEN, OP_CONTINUE },
    // op1 is LT, op2 is from EQ to LT
    { OP_CONFLICT, OP_PROVEN, OP_CONFLICT, OP_CONFLICT, OP_PROVEN, OP_PROVEN },
  };
  Is_True(op1 > E_NONE && op1 < E_LAST, ("bad opr"));
  Is_True(op2 > E_NONE && op2 < E_LAST, ("bad opr"));
  return res[op1 - 1][op2 - 1];
}

// ==================================================================
// evaluate if v1 op v2 is satistified or conflict
// ==================================================================
static OP_RESULT
Eval_const(EDGE_OPER op, INT64 v1, INT64 v2)
{
  switch (op) {
  case E_EQ:
    return v1 == v2 ? OP_PROVEN : OP_CONFLICT;
  case E_NE:
    return v1 != v2 ? OP_PROVEN : OP_CONFLICT;
  case E_GE:
    return v1 >= v2 ? OP_PROVEN : OP_CONFLICT;
  case E_GT:
    return v1 >  v2 ? OP_PROVEN : OP_CONFLICT;
  case E_LE:
    return v1 <= v2 ? OP_PROVEN : OP_CONFLICT;
  case E_LT:
    return v1 <  v2 ? OP_PROVEN : OP_CONFLICT;
  default:
    Is_True(FALSE, ("bad opr"));
    return OP_ERROR;
  }
}

// ==================================================================
// evaluate the edge and check if it's satisfied or conflicted
// ==================================================================
OP_RESULT
VALUE_GRAPH::Eval_node(NODE_IDX idx, EVAL_MAP &map) const
{
  Is_True(idx >= VG_INIT_IDX && idx < _nodes.size(),
          ("node idx out of range"));
  const NODE* node = Node(idx);
  BOOL is_const = node->Is_const();
  EIDX_ITER it;
  for (it = node->Out_edge_begin(); it != node->Out_edge_end(); ++it) {
    const EDGE *edge = Edge(*it);
    Is_True(edge->Lhs() == idx, ("lhs mismatch"));
    Is_True(edge->Rhs() >= VG_INIT_IDX && edge->Rhs() < _nodes.size(),
            ("rhs idx out of range"));
    BOOL is_target = edge->Is_target();
    EDGE_OPER opr = edge->Opr();
    EVAL_MAP::iterator it = map.find(edge->Rhs());
    if (it != map.end()) {
      // node visited before
      EDGE_OPER prev_opr = it->second;
      OP_RESULT res = Eval_opr(it->second, opr);
      if (res == OP_CONTINUE)
        continue;
      if (is_target && res == OP_CONFLICT)
        return OP_VIOLATION;
      return (is_target || res != OP_PROVEN) ? res : OP_CONTINUE;
    }
    const NODE* rhs = Node(edge->Rhs());
    if (rhs->Is_const()) {
      // node is constant
      if (is_const) {
        OP_RESULT res = Eval_const(opr, node->Const_val(), rhs->Const_val());
        if (res == OP_CONTINUE)
          continue;
        if (is_target && res == OP_CONFLICT)
          return OP_VIOLATION;
        return (is_target || res != OP_PROVEN) ? res : OP_CONTINUE;
      }
      map[edge->Rhs()] = opr;
      continue;
    }
    // add node and continue with rhs
    map[edge->Rhs()] = opr;
    return Eval_node(edge->Rhs(), map);
  }
  return OP_CONTINUE;
}

// ==================================================================
// check if target/control dependence is satisfied or not
// ==================================================================
OP_RESULT
VALUE_GRAPH::Check_node_const(NODE_IDX idx, NODE_IDX const_idx) const
{
  Is_True(idx >= VG_INIT_IDX && idx < _nodes.size(),
          ("idx out of range"));
  Is_True(const_idx >= VG_INIT_IDX && const_idx < _nodes.size(),
          ("const_idx out of range"));
  EVAL_MAP map;
  map[const_idx] = E_EQ;
  return Eval_node(idx, map);
}

// ==================================================================
// handle OPR_NE
// ==================================================================
template<> OP_RESULT
VALUE_GRAPH::Handle_compare<OPR_NE>(NODE_IDX lhs, NODE_IDX rhs)
{
  EVAL_MAP map;
  EDGE_OPER eopr = E_NE;
  map[rhs] = eopr;
  // check if the compare confict with existing graph
  OP_RESULT vg_ret = Eval_node (lhs, map);
  if (vg_ret == OP_CONTINUE) {
    Add_edge(lhs, rhs, VG_NULL_IDX, eopr, EF_NONE);
  }
  return vg_ret;
}

template<> OP_RESULT
VALUE_GRAPH::Handle_compare<OPR_NE>(NODE_IDX idx, VSA *vsa, CODEREP *cr)
{
  return OP_ERROR;
}

// ==================================================================
// handle OPR_GE
// ==================================================================
template<> OP_RESULT
VALUE_GRAPH::Handle_compare<OPR_GE>(NODE_IDX lhs, NODE_IDX rhs)
{
  EVAL_MAP map;
  map[rhs] = E_GE;
  // check if the compare confict with existing graph
  OP_RESULT vg_ret = Eval_node (lhs, map);
  if (vg_ret == OP_CONTINUE) {
    Add_edge(lhs, rhs, VG_NULL_IDX, E_GE, EF_NONE);
  }
  return vg_ret;
}

template<> OP_RESULT
VALUE_GRAPH::Handle_compare<OPR_GE>(NODE_IDX idx, VSA *vsa, CODEREP *cr)
{
  return OP_ERROR;
}

// ==================================================================
// handle OPR_GT
// ==================================================================
template<> OP_RESULT
VALUE_GRAPH::Handle_compare<OPR_GT>(NODE_IDX lhs, NODE_IDX rhs)
{
  EVAL_MAP map;
  EDGE_OPER eopr = E_GT;
  map[rhs] = eopr;
  // check if the compare confict with existing graph
  OP_RESULT vg_ret = Eval_node (lhs, map);
  if (vg_ret == OP_CONTINUE) {
    Add_edge(lhs, rhs, VG_NULL_IDX, eopr, EF_NONE);
  }
  return vg_ret;
}

template<> OP_RESULT
VALUE_GRAPH::Handle_compare<OPR_GT>(NODE_IDX idx, VSA *vsa, CODEREP *cr)
{
  return OP_ERROR;
}

// ==================================================================
// handle OPR_LE
// ==================================================================
template<> OP_RESULT
VALUE_GRAPH::Handle_compare<OPR_LE>(NODE_IDX lhs, NODE_IDX rhs)
{
  EVAL_MAP map;
  EDGE_OPER eopr = E_LE;
  map[rhs] = eopr;
  // check if the compare confict with existing graph
  OP_RESULT vg_ret = Eval_node (lhs, map);
  if (vg_ret == OP_CONTINUE) {
    Add_edge(lhs, rhs, VG_NULL_IDX, eopr, EF_NONE);
  }
  return vg_ret;
}

template<> OP_RESULT
VALUE_GRAPH::Handle_compare<OPR_LE>(NODE_IDX idx, VSA *vsa, CODEREP *cr)
{
  return OP_ERROR;
}

// ==================================================================
// handle OPR_LT
// ==================================================================
template<> OP_RESULT
VALUE_GRAPH::Handle_compare<OPR_LT>(NODE_IDX lhs, NODE_IDX rhs)
{
  EVAL_MAP map;
  EDGE_OPER eopr = E_LT;
  map[rhs] = eopr;
  // check if the compare confict with existing graph
  OP_RESULT vg_ret = Eval_node (lhs, map);
  if (vg_ret == OP_CONTINUE) {
    Add_edge(lhs, rhs, VG_NULL_IDX, eopr, EF_NONE);
  }
  return vg_ret;
}

template<> OP_RESULT
VALUE_GRAPH::Handle_compare<OPR_LT>(NODE_IDX idx, VSA *vsa, CODEREP *cr)
{
  return OP_ERROR;
}

// ==================================================================
// Append OP cr
// ==================================================================
OP_RESULT
VALUE_GRAPH::Append_op(NODE_IDX idx, VSA *vsa, CODEREP *cr)
{
  Is_True(idx >= VG_INIT_IDX && idx < VG_MAX_IDX,
          ("invalid idx"));
  Is_True(cr && cr->Kind() == CK_OP,
          ("not op"));
  switch (cr->Opr()) {
  case OPR_ADD: {
    NODE_IDX op0_idx = Add_cr_node(vsa, cr->Opnd(0));
    if (op0_idx == VG_NULL_IDX || op0_idx == VG_MAX_IDX)
      return OP_ERROR;
    NODE_IDX op1_idx = Add_cr_node(vsa, cr->Opnd(1));
    if (op1_idx == VG_NULL_IDX || op1_idx == VG_MAX_IDX)
      return OP_ERROR;
    NODE *op0_node = Node(op0_idx);
    NODE *op1_node = Node(op1_idx);
    // put constant to opnd1
    if (op0_node->Is_const()) {
      NODE *tmp = op0_node;
      op0_node = op1_node;
      op1_node = tmp;
      op0_idx = op1_idx;
    }
    else if (!op1_node->Is_const()) {
      //Is_True(FALSE, ("TODO: add two variables"));
      return Append_variable(_var_map, idx, (uintptr_t)cr);;
    }

    Is_True(op1_node->Is_const(), ("op1 is not const"));
    if (op0_node->Is_const()) {
      INT64 val = op0_node->Const_val() + op1_node->Const_val();
      return Append_const(idx, val);
    }
    else {
      NODE *onode = Node(idx);
      EIDX_ITER it;
      for (it = onode->Out_edge_begin(); it != onode->Out_edge_end(); ++it) {
        EDGE_IDX eidx = *it;
        EDGE* edge = Edge(eidx);
        Is_True(edge->Lhs() == idx,
                ("lhs mismatch"));
        NODE* rhs = Node(edge->Rhs());
        if (!rhs->Is_const()) {
          continue;
        }
        INT64 nval = rhs->Const_val() - op1_node->Const_val();
        NODE_IDX nrhs = Add_const_node(nval);
        Add_edge(op0_idx, nrhs, VG_NULL_IDX, edge->Opr(), edge->Flag());
        return OP_CONTINUE;
      }
      return Append_variable(_var_map, idx, (uintptr_t)cr);
    }
    break;
  }
  case OPR_CVT:
  case OPR_CVTL:
    return Append_cr(idx, vsa, cr->Opnd(0));
  default:
    return Append_variable(_var_map, idx, (uintptr_t)cr);
    // Is_True(FALSE, ("TODO: %s", OPERATOR_name(cr->Opr()) + 4));
    break;
  }
  return OP_ERROR;
}

// ==================================================================
// Add_var_ud/Add_vor_ud
// add U-D of secondary var/vor into value graph
// ==================================================================
OP_RESULT
VALUE_GRAPH::Add_var_ud(VSA *vsa, CODEREP *cr) {
  Is_True(cr && cr->Kind() == CK_VAR, ("bad var cr"));
  if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
      cr->Is_flag_set(CF_DEF_BY_PHI) ||
      cr->Is_var_volatile())
    return OP_CONTINUE;
  STMTREP *sr = cr->Defstmt();
  Is_True_Ret(sr, ("bad defstmt"), OP_CONTINUE);
  if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    if (OPERATOR_is_call(sr->Opr())) {
      // TODO: handle return value here?
    }
    return OP_CONTINUE;
  }
  CODEREP *rhs = sr->Rhs();
  Is_True(rhs != NULL, ("null rhs"));
  OP_RESULT res = OP_CONTINUE;
  if (rhs->Kind() == CK_IVAR) {
    VSYM_OBJ_REP *opnd = vsa->Cr_2_vor(rhs);
    if (opnd && !vsa->Is_special_vor(opnd)) {
      res = Add_assign(vsa, cr, vsa, opnd);
      if (res == OP_CONTINUE) {
        res = Add_vor_ud(vsa, opnd);
      }
    }
  }
  else {
    res = Add_assign(vsa, cr, vsa, rhs);
    if (res == OP_CONTINUE && rhs->Kind() == CK_VAR) {
      res = Add_var_ud(vsa, rhs);
    }
  }
  return res;
}

OP_RESULT
VALUE_GRAPH::Add_vor_ud(VSA *vsa, VSYM_OBJ_REP *vor) {
  Is_True(vor && !vsa->Is_special_vor(vor), ("bad vor"));
  STMTREP *sr;
  CODEREP *rhs;
  OP_RESULT res = OP_CONTINUE;
  switch (vor->Attr()) {
  case ROR_DEF_BY_CHI:
    sr = vor->Stmt_def();
    if (sr && OPERATOR_is_call(sr->Opr())) {
      // TODO: handle return vsym here?
    }
    break;
  case ROR_DEF_BY_ISTORE:
  case ROR_DEF_BY_COPY:
    sr = vor->Stmt_def();
    Is_True_Ret(sr, ("bad def stmt"), OP_CONTINUE);
    rhs = sr->Rhs();
    Is_True(rhs != NULL, ("null rhs"));
    if (rhs->Kind() == CK_IVAR) {
      VSYM_OBJ_REP *opnd = vsa->Cr_2_vor(rhs);
      if (opnd && !vsa->Is_special_vor(opnd)) {
        res = Add_assign(vsa, vor, vsa, opnd);
        if (res == OP_CONTINUE) {
          res = Add_vor_ud(vsa, opnd);
        }
      }
    }
    else {
      res = Add_assign(vsa, vor, vsa, rhs);
      if (res == OP_CONTINUE && rhs->Kind() == CK_VAR) {
        res = Add_var_ud(vsa, rhs);
      }
    }
    break;
  default:
    break;
  }
  return res;
}

// ==================================================================
// Add_control_dependency
// TODO: change to use control dependency annotation
// ==================================================================
OP_RESULT
VALUE_GRAPH::Add_control_dependency(DNA_NODE *dna, BB_NODE *pred, BB_NODE *succ)
{
  Is_True(pred->Dominates(succ), ("pred not dom succ"));

  if (succ->Postdominates(pred))
    return OP_CONTINUE;

  CDA *cda = dna->Comp_unit()->Cda();
  Is_True(cda != NULL, ("cda is null"));
  VSA *vsa = dna->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("vsa is null"));
  OP_RESULT res = OP_CONTINUE;

  do {
    CDA_VALUE val = cda->Get_cda(succ);
    if (val.Is_null()) {
      if (succ->Idom()) {
        succ = succ->Idom();
        continue;
      } else {
        return OP_CONTINUE;
      }
    }
    switch (val.Kind()) {
      case CDA_RHS_TRUE:
      case CDA_RHS_FALSE:
      {
        res = Add_cmp_cda(dna, val);
        // move to next succ
        STMTREP *sr = val.Get_as<STMTREP>();
        succ = sr->Bb();
        break;
      }
      case CDA_PHI:
      {
        if (succ->Idom()) {
          succ = succ->Idom();
        } else {
          return OP_CONTINUE;
        }
        break;
      }
      default:
        //Is_Trace(Tracing(), (TFile, "TODO: handle CDA kind %d\n", val.Kind()));
        return OP_CONTINUE;
    }
  } while(!succ->Postdominates(pred) && pred->Dominates(succ) && res != OP_CONFLICT);

  return res;
}


// ==================================================================
// Add_phi_opnd
// ==================================================================
OP_RESULT
VALUE_GRAPH::Add_phi_opnd(DNA_NODE *dna, PHI_NODE *phi, INT32 opnd_idx, BOOL &maybe)
{
  if (VSA_Value_Graph_Lazy) {
    _phis.push_back(PHI_OPND(dna, phi, opnd_idx));
    return OP_CONTINUE;
  } else {
    return Eval_phi_opnd(dna, phi, opnd_idx, maybe);
  }
}

// ==================================================================
// Eval_phi_opnd
// ==================================================================
OP_RESULT
VALUE_GRAPH::Eval_phi_opnd(DNA_NODE *dna, PHI_NODE *phi, INT32 opnd_idx, BOOL &maybe)
{
  BB_NODE *phi_bb = phi->Bb();
  Is_True(opnd_idx < phi_bb->Pred()->Len(), ("phi opnd idx:%d outof range", opnd_idx));
  BB_NODE *bb_opnd = phi_bb->Nth_pred(opnd_idx);
  CDA *cda = dna->Comp_unit()->Cda();
  Is_True(cda != NULL, ("cda is null"));
  VSA *vsa = dna->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("vsa is null"));
  OP_RESULT res = OP_CONTINUE;

  // add phi's CDA at first
  CDA_VALUE val = cda->Get_cda(phi_bb);
  if (!val.Is_null()) {
    switch (val.Kind()) {
      case CDA_PHI:
      {
        CDA_PHI_NODE *cda_phi = val.Get_phi();
        CDA_VALUE cda_opnd = cda_phi->Opnd(opnd_idx);
        if (!cda_opnd.Is_null() &&
            (cda_opnd.Kind() == CDA_CR ||
             cda_opnd.Kind() == CDA_RHS_TRUE ||
             cda_opnd.Kind() == CDA_RHS_FALSE)) {
          res = Add_cmp_cda(dna, cda_opnd);
        }
        else {
          maybe = TRUE;
        }
      }
      break;
      default:
        // TODO
      break;
    }
  }

  // add assignment from phi operand to phi result for cr/vor in graph
  // and defined by phi on this BB. For case like:
  // if () {
  //   val = ...; cond = ...;
  // } else {
  //   cond = ...;
  // }
  // if (cond) {
  //   ... = val;
  // }

  BOOL primary_phi_handled = FALSE;
  if (res == OP_CONTINUE) {
    // var phi
    PHI_NODE *bb_phi;
    PHI_LIST_ITER bb_phi_iter;
    OPT_STAB *stab = vsa->Opt_stab();
    FOR_ALL_ELEM (bb_phi, bb_phi_iter, Init(phi_bb->Phi_list())) {
      if (!bb_phi->Live())
        continue;
      if (bb_phi->Aux_id() == vsa->Opt_stab()->Default_vsym() ||
          bb_phi->Aux_id() == vsa->Opt_stab()->Return_vsym())
        continue;
      CODEREP *phi_res = bb_phi->RESULT();
      if (_var_map.find((uintptr_t)phi_res) == _var_map.end())
        continue;
      CODEREP *phi_opnd = bb_phi->OPND(opnd_idx);
      if (phi_res != phi_opnd) {
        res = Add_assign(vsa, phi_res, vsa, phi_opnd);
        if (res == OP_CONTINUE && bb_phi != phi) {
          // not the primary variable, add more UD to graph
          res = Add_var_ud(vsa, phi_opnd);
        }
        if (res != OP_CONTINUE) {
          return res;
        }
      }
    }
  }
  if (res == OP_CONTINUE) {
    // vor phi
    PHI_NODE *bb_phi;
    PHI_LIST_ITER bb_phi_iter;
    FOR_ALL_ELEM (bb_phi, bb_phi_iter, Init(vsa->Bb_vo_philist(phi_bb))) {
      VSYM_OBJ_REP *phi_res = (VSYM_OBJ_REP *)bb_phi->RESULT();
      if (_vor_map.find((uintptr_t)phi_res) == _vor_map.end())
        continue;
      VSYM_OBJ_REP *phi_opnd = (VSYM_OBJ_REP *)bb_phi->OPND(opnd_idx);
      Is_True(!vsa->Is_special_vor(phi_res), ("phi res is special vor"));
      if (phi_res != phi_opnd && !vsa->Is_special_vor(phi_opnd)) {
        res = Add_assign(vsa, phi_res, vsa, phi_opnd);
        if (res == OP_CONTINUE && bb_phi != phi) {
          // not the primary variable, add more UD to graph
          res = Add_vor_ud(vsa, phi_opnd);
        }
        if (res != OP_CONTINUE) {
          return res;
        }
      }
    }
  }

  return res;
}

// ==================================================================
// Add_cmp_cda
// ==================================================================
OP_RESULT
VALUE_GRAPH::Add_cmp_cda(DNA_NODE *dna, CDA_VALUE cda)
{
  Is_True_Ret(cda.Kind() == CDA_CR ||
              cda.Kind() == CDA_RHS_TRUE ||
              cda.Kind() == CDA_RHS_FALSE, ("CDA kind not true/false"), OP_CONTINUE);

  CODEREP *expr = NULL;
  if (cda.Kind() == CDA_CR) {
    expr = cda.Get_as<CODEREP>();
  } else {
    STMTREP *sr = cda.Get_as<STMTREP>();
    Is_True(sr &&
            (sr->Opr() == OPR_TRUEBR || sr->Opr() == OPR_FALSEBR),
            ("invalid sr or rhs"));
    expr = sr->Rhs();
  }

  OPERATOR opr;
  CODEREP *lhs;
  CODEREP *rhs;
  if (expr->Kind() != CK_OP ||
      !OPERATOR_is_compare(expr->Opr())) {
    opr = (cda.Kind() == CDA_RHS_FALSE) ? OPR_EQ : OPR_NE;
    lhs = expr;
    rhs = CR_UTIL(dna->Comp_unit()).New_const_cr(lhs->Dtyp(), 0);
  } else {
    opr = expr->Opr();
    lhs = expr->Opnd(0);
    rhs = expr->Opnd(1);
    // swap lhs & rhs if needed
    BOOL need_swap = FALSE;
    if (lhs->Kind() != CK_VAR && lhs->Kind() != CK_IVAR &&
        (rhs->Kind() == CK_VAR || rhs->Kind() == CK_IVAR)) {
      CODEREP *tmp = lhs;
      lhs = rhs;
      rhs = tmp;
      need_swap = TRUE;
    }
    if (need_swap) {
      opr = CR_UTIL::Exchange_opr(opr);
    }
    if (cda.Kind() == CDA_RHS_FALSE) {
      opr = CR_UTIL::Complement_opr(opr);
    }
  }

  VSA *vsa = dna->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("vsa is null"));
  VSYM_OBJ_REP *vor;
  if (lhs->Kind() == CK_VAR) {
    Add_var_ud(vsa, lhs);
  } else if (lhs->Kind() == CK_IVAR &&
             (vor = vsa->Cr_2_vor(lhs)) != NULL &&
             !vsa->Is_special_vor(vor)) {
    Add_vor_ud(vsa, vor);
  }
  if (rhs->Kind() == CK_VAR) {
    Add_var_ud(vsa, rhs);
  } else if (rhs->Kind() == CK_IVAR &&
             (vor = vsa->Cr_2_vor(rhs)) != NULL &&
             !vsa->Is_special_vor(vor)) {
    Add_vor_ud(vsa, vor);
  }
  return Add_cmp_cr(opr, vsa, lhs, rhs);
}

// ==================================================================
// Eval_graph
// ==================================================================
OP_RESULT
VALUE_GRAPH::Eval_graph(BOOL &maybe)
{
  OP_RESULT res = OP_CONTINUE;
  PHI_VEC::iterator it = _phis.begin();
  for (; it != _phis.end(); ++it) {
    res = Eval_phi_opnd(it->Dna(), it->Phi(), it->Opnd(), maybe);
    if (res != OP_CONTINUE)
      break;
  }
  return res;
}

