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
#include "opt_htable.h"
#include "opt_fold.h"
#include "opt_vra.h"

#include <vector>
#include <stack>
#include <map>
#include <algorithm>

// =============================================================================
//
// Value range bounds API implementation
// 
// =============================================================================

// calculate bounds represented in coderep for conjunction
BOOL
VRA::Calc_conj_bounds(TYPE_ID dtyp, CODEREP* min1, CODEREP* max1, CODEREP* min2, CODEREP* max2, CODEREP* &min, CODEREP* &max) const
{
  if (min1->Kind() == CK_CONST && min2->Kind() == CK_CONST) {
    min = min1->Const_val() > min2->Const_val() ? min1 : min2;
  }
  else if (min1->Kind() == CK_CONST && min1->Const_val() == -INT64_MAX) {
    min = min2;
  }
  else if (min2->Kind() == CK_CONST && min2->Const_val() == -INT64_MAX) {
    min = min1;
  }
  else {
    return FALSE;
  }

  if (max1->Kind() == CK_CONST && max2->Kind() == CK_CONST) {
    max = max1->Const_val() < max2->Const_val() ? max1 : max2;
  }
  else if (max1->Kind() == CK_CONST && max1->Const_val() == INT64_MAX) {
    max = max2;
  }
  else if (max2->Kind() == CK_CONST && max2->Const_val() == INT64_MAX) {
    max = max1;
  }
  else {
    return FALSE;
  }

  return TRUE;
}

// calculate bounds represented in coderep for disjunction
BOOL
VRA::Calc_disj_bounds(TYPE_ID dtyp, CODEREP* min1, CODEREP* max1, CODEREP* min2, CODEREP* max2, CODEREP* &min, CODEREP* &max) const
{
#if 0
  // TODO: not implemented yet
#if 0
  if (MTYPE_is_unsigned(dtyp)) {
    UINT64 umin1 = (UINT64)min1;
    UINT64 umax1 = (UINT64)max1;
    UINT64 umin2 = (UINT64)min2;
    UINT64 umax2 = (UINT64)max2;
    Is_True(umin1 <= umax1, ("Invalid umin1 %llu or umax1 %llu", umin1, umax1));
    Is_True(umin2 <= umax2, ("Invalid umin2 %llu or umax2 %llu", umin2, umax2));
    // TODO: no intersection
    min = umin1 < umin2 ? min1 : min2;
    max = umax1 > umax2 ? max1 : max2;
  }
  else {
#endif
    Is_True(min1 <= max1, ("Invalid min1 %d or max1 %d", min1, max1));
    Is_True(min2 <= max2, ("Invalid min2 %d or max2 %d", min2, max2));
    // TODO: no intersection
    min = min1 < min2 ? min1 : min2;
    max = max1 > max2 ? max1 : max2;
#if 0
  }
#endif
#endif
  return FALSE;
}

// calculate bounds represented by coderep for coderep
BOOL
VRA::Get_bounds(TYPE_ID dtyp, CODEREP* cr, BB_NODE* bb, CODEREP* &min, CODEREP* &max, COUNT_ARRAY& visited) const
{
  CODEREP* one = NULL;
  switch (cr->Kind()) {
  case CK_CONST:
    max = min = cr;
    return TRUE;
  case CK_OP:
    if (cr->Opnd(0)->Kind() == CK_VAR) {
      switch (cr->Opr()) {
      case OPR_EQ:
        max = min = cr->Opnd(1);
        return TRUE;
      case OPR_GT:
        one = New_cr(MTYPE_I8, (INT64)1);
        min = New_cr(OPCODE_make_op(OPR_ADD, MTYPE_I8, MTYPE_V),
                     cr->Opnd(1), one, TRUE);;
        max = New_cr(MTYPE_I8, INT64_MAX);
        return TRUE;
      case OPR_GE:
        min = cr->Opnd(1);
        max = New_cr(MTYPE_I8, INT64_MAX);  // TODO: check type???
        return TRUE;
      case OPR_LT:
        one = New_cr(MTYPE_I8, (INT64)1);
        max = New_cr(OPCODE_make_op(OPR_SUB, MTYPE_I8, MTYPE_V),
                     cr->Opnd(1), one, TRUE);
        min = New_cr(MTYPE_I8, -INT64_MAX); // TODO: check type???
        return TRUE;
      case OPR_LE:
        max = cr->Opnd(1);
        min = New_cr(MTYPE_I8, -INT64_MAX); // TODO: check type???
        return TRUE;
      default:
        return FALSE;
      }
    }
    break;
  default:
    break;
  }
  UINT32 expr_id = Value_range_rec(cr, bb->Id());
  return Get_bounds(dtyp, expr_id, bb, min, max, visited);
}

// calculate bounds represented by coderep for value range expression
BOOL
VRA::Get_bounds(TYPE_ID dtyp, UINT32 expr_id, BB_NODE* bb, CODEREP* &min, CODEREP* &max, COUNT_ARRAY& visited) const
{
  if (visited[expr_id] >= VISIT_MAX_COUNT)
    return FALSE;
  ++ visited[expr_id];

  const VRA_EXPR& expr = Expr(expr_id);
  CODEREP *min1, *max1, *min2, *max2;
  BOOL ret1, ret2;
  CODEREP *cr;
  switch (expr.Opr()) {
  case OPR_TOP:
  case OPR_BOTTOM:
  case OPR_EMPTY:
  case OPR_PLACEHOLDER:
    return FALSE;

  case OPR_CODEREP:
    // get bounds for coderep
    cr = expr.Cr();
    if (cr->Kind() == CK_OP && cr->Opr() == OPR_CALL)
      return FALSE;
    return Get_bounds(dtyp, cr, bb, min, max, visited);

  case OPR_CONJUNCTION:
  case OPR_DISJUNCTION:
    // get bounds for opnd(0)
    ret1 = Get_bounds(dtyp, expr.Op0(), bb, min1, max1, visited);
    if (ret1 == FALSE)
      return FALSE;
    // get bounds for opnd(1)
    ret2 = Get_bounds(dtyp, expr.Op1(), bb, min2, max2, visited);
    if (ret2 == FALSE)
      return FALSE;
    // calculate bounds for conj or disj
    return expr.Opr() == OPR_CONJUNCTION ?
               Calc_conj_bounds(dtyp, min1, max1, min2, max2, min, max) :
               Calc_disj_bounds(dtyp, min1, max1, min2, max2, min, max);
  default:
    Is_True(FALSE, ("Invalid Expr operator"));
  }
  return FALSE;
}

// calculate bounds represented in coderep for cr in bb
BOOL
VRA::Get_bounds(CODEREP* cr, BB_NODE* bb, CODEREP* &min, CODEREP* &max) const
{
  COUNT_ARRAY visited;
  visited.resize(_expr_table.Size());
  FOLD_CONTEXT fctx(_htable);
  return Get_bounds(MTYPE_I8, cr, bb, min, max, visited);
}

// calculate bounds in constant for conjunction expression
BOOL
VRA::Calc_conj_bounds(TYPE_ID dtyp, INT64 min1, INT64 max1, INT64 min2, INT64 max2, INT64 &min, INT64 &max) const
{
  // TODO: check types???
#if 0
  if (MTYPE_is_unsigned(dtyp)) {
    UINT64 umin1 = (UINT64)min1;
    UINT64 umax1 = (UINT64)max1;
    UINT64 umin2 = (UINT64)min2;
    UINT64 umax2 = (UINT64)max2;
    Is_True(umin1 <= umax1, ("Invalid umin1 %llu or umax1 %llu", umin1, umax1));
    Is_True(umin2 <= umax2, ("Invalid umin2 %llu or umax2 %llu", umin2, umax2));
    if (umax1 < umin2 || umax2 < umin1)
      return FALSE;
    min = umin1 > umin2 ? min1 : min2;
    max = umax1 < umax2 ? max1 : max2;
  }
  else {
#endif
    Is_True(min1 <= max1, ("Invalid min1 %d or max1 %d", min1, max1));
    Is_True(min2 <= max2, ("Invalid min2 %d or max2 %d", min2, max2));
    if (max1 < min2 || max2 < min1)
      return FALSE;
    min = min1 > min2 ? min1 : min2;
    max = max1 < max2 ? max1 : max2;
#if 0
  }
#endif
  return TRUE;
}

// calculate bounds in constant for disjunction expression
BOOL
VRA::Calc_disj_bounds(TYPE_ID dtyp, INT64 min1, INT64 max1, INT64 min2, INT64 max2, INT64 &min, INT64 &max) const
{
  // TODO: check types???
#if 0
  if (MTYPE_is_unsigned(dtyp)) {
    UINT64 umin1 = (UINT64)min1;
    UINT64 umax1 = (UINT64)max1;
    UINT64 umin2 = (UINT64)min2;
    UINT64 umax2 = (UINT64)max2;
    Is_True(umin1 <= umax1, ("Invalid umin1 %llu or umax1 %llu", umin1, umax1));
    Is_True(umin2 <= umax2, ("Invalid umin2 %llu or umax2 %llu", umin2, umax2));
    // TODO: no intersection
    min = umin1 < umin2 ? min1 : min2;
    max = umax1 > umax2 ? max1 : max2;
  }
  else {
#endif
    //Is_True(min1 <= max1, ("Invalid min1 %d or max1 %d", min1, max1));
    //Is_True(min2 <= max2, ("Invalid min2 %d or max2 %d", min2, max2));
    // TODO: no intersection
    min = min1 < min2 ? min1 : min2;
    max = max1 > max2 ? max1 : max2;
#if 0
  }
#endif
  return TRUE;
}

// calculate bounds in constant for coderep
VRA_BOUND
VRA::Get_bounds(TYPE_ID dtyp, CODEREP* cr, BB_NODE* bb, INT64 &min, INT64 &max, VALBOUND_MAP& cache) const
{
  VRA_BOUND vb;
  switch (cr->Kind()) {
  case CK_CONST:
    // get bounds for const coderep
    min = cr->Const_val();
    max = min;
    return VA_FB;
  case CK_OP:
    // get bounds for op coderep
    if (cr->Kid_count() == 2 &&    // TODO: unary operators
        cr->Opnd(0)->Kind() == CK_VAR &&
        cr->Opnd(1)->Kind() == CK_CONST) {
      switch (cr->Opr()) {
      case OPR_EQ:
        max = min = cr->Opnd(1)->Const_val();
        return VA_FB;
      case OPR_GT:
        min = cr->Opnd(1)->Const_val() + 1;
        max = INT64_MAX;
        return VA_LB;
      case OPR_GE:
        min = cr->Opnd(1)->Const_val();
        max = INT64_MAX;
        return VA_LB;
      case OPR_LT:
        max = cr->Opnd(1)->Const_val() - 1;
        min = -INT64_MAX;
        return VA_UB;
      case OPR_LE:
        max = cr->Opnd(1)->Const_val();
        min = -INT64_MAX;
        return VA_UB;
      case OPR_ADD:
        vb = Get_bounds(dtyp, cr->Opnd(0), bb, min, max, cache);
        if (vb != VA_NB) {
          min += cr->Opnd(1)->Const_val();
          if (max != INT64_MAX)
            max += cr->Opnd(1)->Const_val();
        }
        return vb;
      case OPR_SUB:
        vb = Get_bounds(dtyp, cr->Opnd(0), bb, min, max, cache);
        if (vb != VA_NB) {
          if (min != -INT64_MAX)
            min -= cr->Opnd(1)->Const_val();
          max -= cr->Opnd(1)->Const_val();
        }
        return vb;
      case OPR_MPY:
        vb = Get_bounds(dtyp, cr->Opnd(0), bb, min, max, cache);
        if (vb != VA_NB) {
          if (min != -INT64_MAX)
            min *= cr->Opnd(1)->Const_val();
          if (max != INT64_MAX)
            max *= cr->Opnd(1)->Const_val();
        }
        return vb;
      case OPR_REM:
        // TODO: check Opnd(0)'s bounds as well
        min = 0;
        max = cr->Opnd(1)->Const_val() - 1;
        return VA_FB;
      case OPR_BAND:
        // TODO: check Opnd(0)'s bounds as well
        min = 0;
        max = cr->Opnd(1)->Const_val();
        return VA_FB;
      default:
        return VA_NB;
      }
    }
    break;
  default:
    break;
  }
  // get bounds from value range expression
  UINT32 expr_id = Value_range_rec(cr, bb->Id());
  return Get_bounds(dtyp, expr_id, bb, min, max, cache);
}

// calculate bounds in constant for value range expression
VRA_BOUND
VRA::Get_bounds(TYPE_ID dtyp, UINT32 expr_id, BB_NODE* bb, INT64 &min, INT64 &max, VALBOUND_MAP& cache) const
{
  VALBOUND_MAP::iterator it = cache.find(expr_id);
  if (it != cache.end()) {
    min = it->second.Min();
    max = it->second.Max();
    return it->second.Vra_bound();
  }
  cache[expr_id] = VALBOUND(VA_NB, -INT64_MAX, INT64_MAX);

  const VRA_EXPR& expr = Expr(expr_id);
  INT64 min1, max1, min2, max2;
  VRA_BOUND ret1, ret2;
  CODEREP *cr;
  switch (expr.Opr()) {
  case OPR_TOP:
  case OPR_BOTTOM:
  case OPR_EMPTY:
  case OPR_PLACEHOLDER:
    min = -INT64_MAX;
    max = INT64_MAX;
    return VA_NB;

  case OPR_CODEREP:
    // get bounds for coderep
    cr = expr.Cr();
    ret1 = Get_bounds(dtyp, cr, bb, min, max, cache);
    if (ret1 != VA_NB) {
      cache[expr_id] = VALBOUND(ret1, min, max);
      Is_True(cache[expr_id].Min() == min &&
              cache[expr_id].Max() == max, ("bad cache"));
    }
    return ret1;

  case OPR_CONJUNCTION:
  case OPR_DISJUNCTION:
    // get bounds for opnd(0)
    ret1 = Get_bounds(dtyp, expr.Op0(), bb, min1, max1, cache);
    if (ret1 == VA_NB) {
      min1 = -INT64_MAX;
      max1 = INT64_MAX;
    }
    // get bounds for opnd(1)
    ret2 = Get_bounds(dtyp, expr.Op1(), bb, min2, max2, cache);
    if (ret2 == VA_NB) {
      min2 = -INT64_MAX;
      max2 = INT64_MAX;
    }
    // calculate bounds according to conj or disj
    if (expr.Opr() == OPR_CONJUNCTION) {
      Calc_conj_bounds(dtyp, min1, max1, min2, max2, min, max);
      ret1 = (VRA_BOUND)(ret1 | ret2);
    }
    else {
      Calc_disj_bounds(dtyp, min1, max1, min2, max2, min, max);
      ret1 = (VRA_BOUND)(ret1 & ret2);
    }
    if (ret1 != VA_NB) {
      cache[expr_id] = VALBOUND(ret1, min, max);
      Is_True(cache[expr_id].Min() == min &&
              cache[expr_id].Max() == max, ("bad cache"));
    }
    return ret1;

  default:
    Is_True(FALSE, ("Invalid Expr operator"));
  }
  return VA_NB;
}

// calculate bounds in constant for cr in bb
VRA_BOUND
VRA::Get_bounds(CODEREP* cr, BB_NODE* bb, INT64& min, INT64& max) const {
  VALBOUND_MAP cache;
  return Get_bounds(MTYPE_I8, cr, bb, min, max, cache);
}

// calculate upper bound in coderep for cr in bb
BOOL
VRA::Get_ub(CODEREP* cr, BB_NODE* bb, CODEREP*& ub, COUNT_ARRAY& visited) const
{
  if (cr->Kind() == CK_OP) {
    switch (cr->Opr()) {
    case OPR_LT:
    case OPR_LE:
      ub = cr->Opnd(1);
      return TRUE;
    case OPR_NE:
    case OPR_GT:
    case OPR_GE:
      return FALSE;
    }
  }
  else if (cr->Kind() == CK_CONST ||
           cr->Kind() == CK_RCONST ||
           cr->Kind() == CK_LDA)
    return TRUE;
  UINT32 expr_id = Value_range_rec(cr, bb->Id());
  if (expr_id == VR_NOT_FOUND)
    return FALSE;
  return Get_ub(expr_id, bb, ub, visited);
}

// calculate upper bound in coderep for expr_id in bb
BOOL
VRA::Get_ub(UINT32 expr_id, BB_NODE* bb, CODEREP*& ub, COUNT_ARRAY& visited) const
{
  if (visited[expr_id] == TRUE)
    return FALSE;
  visited[expr_id] = TRUE;

  const VRA_EXPR& expr = Expr(expr_id);
  switch (expr.Opr()) {
  case OPR_TOP:
  case OPR_BOTTOM:
  case OPR_EMPTY:
  case OPR_PLACEHOLDER:
    return FALSE;
  case OPR_CODEREP:
    return Get_ub(expr.Cr(), bb, ub, visited);
  case OPR_DISJUNCTION:
    if (Get_ub(expr.Op0(), bb, ub, visited) == FALSE)
      return FALSE;
    return Get_ub(expr.Op1(), bb, ub, visited);
  case OPR_CONJUNCTION:
    if (Get_ub(expr.Op0(), bb, ub, visited) == TRUE)
      return TRUE;
    return Get_ub(expr.Op1(), bb, ub, visited);
  }
  return FALSE;
}

// calculate upper bound in coderep for cr in bb
BOOL
VRA::Get_ub(CODEREP* cr, BB_NODE* bb, CODEREP*& ub) const
{
  switch (cr->Kind()) {
  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
    ub = cr;
    return TRUE;
  case CK_VAR:
  case CK_IVAR:
    {
      UINT32 expr_id = Value_range_rec(cr, bb->Id());
      if (expr_id == VR_NOT_FOUND)
        return FALSE;
      COUNT_ARRAY visited;
      visited.resize(_expr_table.Size());
      return Get_ub(expr_id, bb, ub, visited);
    }
  case CK_OP:
    for (INT i = 0; i < cr->Kid_count(); ++i) {
      CODEREP* tmp;
      if (Get_ub(cr->Opnd(i), bb, tmp) == FALSE)
        return FALSE;
    }
    ub = cr;
    return TRUE;
  default:
    return FALSE;
  }
}

