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

// =============================================================================
// VRA utilities
// =============================================================================

// get the name of value range additional operators
const char*
VRA_OPERATOR_NAME(VRA_OPERATOR opr)
{
  switch (opr) {
  case OPERATOR_UNKNOWN:
    return "ERROR";
  case OPR_CODEREP:
    return "CR";
  case OPR_PLACEHOLDER:
    return "PLACEHOLDER";
  case OPR_TOP:
    return "TOP";
  case OPR_BOTTOM:
    return "BOTTOM";
  case OPR_EMPTY:
    return "EMPTY";
  case OPR_CONJUNCTION:
    return "CONJ";
  case OPR_DISJUNCTION:
    return "DISJ";
  default:
    return "UNKNOWN";
  }
};

// get the name of value range query result
const char*
VRA_RESULT_NAME(VRA_RESULT res)
{
  static const char* name[] = {
    "UNKNOWN", "POSSIBLE", "YES", "NO"
  };
  return name[res];
}

// =============================================================================
// VRA utilities
// =============================================================================

// get variable name of a CK_VAR coderep
const char *
VRA::Var_name(CODEREP* cr) const {
  Is_True(cr->Kind() == CK_VAR, ("not a CK_VAR"));
  AUX_STAB_ENTRY *sym = _comp_unit->Opt_stab()->Aux_stab_entry(cr->Aux_id());
  return sym->St() != NULL ? ST_name(sym->St()) : NULL;
}

// print BB_LOOP info
void
VRA::Print_bb_loop(BB_NODE *bb, FILE *f) const
{
  BB_LOOP* loop_info = bb->Loop();
  fprintf(f, "Loop info for BB%d:%s", bb->Id(), loop_info ? "" : " <null>");
  if (loop_info) {
    if (loop_info->Iv() != NULL) {
      fprintf(f, " iv ");
      Print_coderep(loop_info->Iv(), f);
    }
    if (loop_info->Trip_count_stmt() != NULL) {
      fprintf(f, " tripcount ");
      Print_coderep(loop_info->Trip_count_stmt()->Rhs(), f);
    }
    if (loop_info->Trip_count_expr() != NULL) {
      fprintf(f, " tripcount ");
      Print_coderep(loop_info->Trip_count_expr(), f);
    }
    if (loop_info->Start() != NULL)
      fprintf(f, " start BB%d", loop_info->Start()->Id());
    if (loop_info->Dohead() != NULL)
      fprintf(f, " dostart BB%d", loop_info->Dohead()->Id());
    if (loop_info->End() != NULL)
      fprintf(f, " end BB%d", loop_info->End()->Id());
    if (loop_info->Body() != NULL)
      fprintf(f, " body BB%d", loop_info->Body()->Id());
    if (loop_info->Step() != NULL)
      fprintf(f, " step BB%d", loop_info->Step()->Id());
    if (loop_info->Merge() != NULL)
      fprintf(f, " merge BB%d", loop_info->Merge()->Id());
    if (loop_info->Dotail() != NULL)
      fprintf(f, " dotail BB%d", loop_info->Dotail()->Id());
    if (loop_info->Header() != NULL)
      fprintf(f, " header BB%d", loop_info->Header()->Id());
    if (loop_info->Tail() != NULL)
      fprintf(f, " tail BB%d", loop_info->Tail()->Id());
    if (loop_info->Preheader() != NULL)
      fprintf(f, " preheader BB%d", loop_info->Preheader()->Id());
    if (loop_info->Loopback() != NULL)
      fprintf(f, " loopback BB%d", loop_info->Loopback()->Id());
    if (loop_info->Body_set() != NULL) {
      fprintf(f, "\n  body set ");
      BB_NODE_SET_ITER iter;
      BB_NODE* bb;
      FOR_ALL_ELEM(bb, iter, Init(loop_info->Body_set())) {
        fprintf(f, "BB%d ", bb->Id());
      }
    }
    if (loop_info->True_body_set() != NULL) {
      fprintf(f, "\n  true body set ");
      BB_NODE_SET_ITER iter;
      BB_NODE* bb;
      FOR_ALL_ELEM(bb, iter, Init(loop_info->True_body_set())) {
        fprintf(f, "BB%d ", bb->Id());
      }
    }
  }
  fprintf(f, "\n");
}

// print coderep
void
VRA::Print_coderep(CODEREP *cr, FILE *f) const
{
  AUX_STAB_ENTRY* aux;
  INT32 i;
  switch (cr->Kind()) {
  case CK_LDA:
    fprintf(f, "&%s", _comp_unit->Opt_stab()->St_name(cr->Lda_aux_id()));
    break;
  case CK_RCONST:
    fprintf(f, "rconst");
    break;
  case CK_CONST:
    fprintf(f, "%lld", cr->Const_val());
    break;
  case CK_VAR:
    aux = _comp_unit->Opt_stab()->Aux_stab_entry(cr->Aux_id());
    fprintf(f, "cr%d sym%dv%d:%s",
               cr->Coderep_id(), cr->Aux_id(), cr->Version(),
               aux->St() ? ST_name(aux->St()) : "<nost>");
    break;
  case CK_IVAR:
    fprintf(f, "*(");
    if (cr->Ilod_base() != NULL)
      Print_coderep(cr->Ilod_base(), f);
    else if (cr->Istr_base() != NULL)
      Print_coderep(cr->Istr_base(), f);
    fprintf(f, ")");
    break;
  case CK_OP:
    fprintf(f, "(");
    fprintf(f, "%s ", OPERATOR_name(cr->Opr()) + 4);
    for (i = 0; i < cr->Kid_count(); ++i) {
      if (i > 0)
        fprintf(f, " ");
      Print_coderep(cr->Opnd(i), f);
    }
    fprintf(f, ")");
    break;
  }
}

// print stmtrep
void
VRA::Print_stmtrep(STMTREP *sr, FILE *f) const
{
  fprintf(f, "%s ", OPERATOR_name(sr->Opr()) + 4);
  if (sr->Lhs() != NULL) {
    Print_coderep(sr->Lhs(), f);
    fprintf(f, " <- ");
  }
  if (sr->Rhs() != NULL) {
    Print_coderep(sr->Rhs(), f);
  }
  fprintf(f, "\n");
}

// print value range expression
void
VRA::Print_expr(UINT32 i, FILE *f) const
{
  const VRA_EXPR& expr = _expr_table[i];
  fprintf(f, "expr[%d]: %s in bb %d: ", i, VRA_OPERATOR_NAME(expr.Opr()), expr.Bb_id());
  switch (expr.Opr()) {
  case OPERATOR_UNKNOWN:
  case OPR_TOP:
  case OPR_BOTTOM:
  case OPR_EMPTY:
    fprintf(f, "\n");
    break;
  case OPR_CODEREP:
  case OPR_PLACEHOLDER:
    fprintf(f, " cr%d ", expr.Cr()->Coderep_id());
    Print_coderep(expr.Cr(), f);
    fprintf(f, "\n");
    break;
  case OPR_CONJUNCTION:
  case OPR_DISJUNCTION:
    fprintf(f, "left: %d right: %d\n", expr.Op0(), expr.Op1());
    break;
  }
}

// get linenum for a bb entry
static UINT32
BB_get_linenum(BB_NODE* bb)
{
  if (bb->First_stmtrep() != NULL && bb->First_stmtrep()->Linenum() != 0)
    return Srcpos_To_Line(bb->First_stmtrep()->Linenum());
  BB_NODE* pred = bb;
  do {
    if (pred->Pred() && !pred->Pred()->Multiple_bbs())
      pred = pred->Pred()->Node();
    else
      break;
    if (pred != NULL && pred->Last_stmtrep() != NULL)
      return Srcpos_To_Line(pred->Last_stmtrep()->Linenum());
  } while (pred != NULL);
  return Srcpos_To_Line(bb->Linenum());
}

// print value range path
void
VRA::Print_path(const PATH_SELECTED& paths, INT ident, FILE *f) const
{
#if 0
  // TODO, wait for new implementation
  PATH_ARRAY::const_iterator i = paths.Paths().begin();
  for (; i != paths.Paths().end(); ++i) {
    fprintf(f, "%*s %s from ", ident, "", VRA_RESULT_NAME(i->Result()));
    PATH::const_iterator j = i->Path().begin();
    for (; j != i->Path().end(); ++j) {
      if (j != i->Path().begin()) {
        fprintf(f, "->");
      }
      fprintf(f, "BB%d", *j);
      BB_NODE* bb = _cfg->Get_bb(*j);
      UINT32 linenum = BB_get_linenum(bb);
      if (linenum > 0)
        fprintf(f, "(line:%d)", BB_get_linenum(bb));
      else
        fprintf(f, "(line:n/a)");
    }
    fprintf(f, "\n");
  }
#endif
}

// print value range
void
VRA::Print_valrange(FILE* f) const
{
  VALRANGE_MAP::const_iterator it = _valrange_map.begin();
  for (; it != _valrange_map.end(); ++it) {
    uint64_t key = it->first;
    if ((key & 0x1) == 0) {
      // cr
      fprintf(f, "cr%ld bb%ld: expr %d\n",
                 key >> 32, (key & 0xFFFFFFFF) >> 1, it->second);
    }
    else {
      // vor
      fprintf(f, "vo%ldv%ld bb%ld: expr %d\n",
                 key >> 40, (key >> 21) & 0x7FFFF,
                 (key >> 1) & 0xFFFFF, it->second);
    }
  }
}

// print value range analysis result
void
VRA::Print(FILE* f) const
{
  UINT32 i;
  fprintf(f, "VRA DUMP BEGIN\n");
  fprintf(f, "============================\n");
  fprintf(f, "EXPR TABLE:\n");
  fprintf(f, "----------------------------\n");
  for (i = 0; i < _expr_table.Size(); ++i) {
    Print_expr(i, f);
  }
  fprintf(f, "----------------------------\n");
  fprintf(f, "VALUE RANGE MAP:\n");
  fprintf(f, "----------------------------\n");
  Print_valrange(f);
  fprintf(f, "----------------------------\n");
  fprintf(f, "============================\n");
}

