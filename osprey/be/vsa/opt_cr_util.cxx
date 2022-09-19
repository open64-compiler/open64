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

// =============================================================================
// opt_cr_util.cxx
//
// implementation for coderep utilities for VSA
// =============================================================================
#include "opt_cr_util.h"
#include "opt_vsa.h"

// my_log_2
// calculate y=log2(x) where x=pow(2, y)
static inline INT
my_log_2(INT v)
{
  Is_True(v >= 0,
          ("v is less than 0"));
  INT res = 0;
  while (v > 1) {
    v >>= 1;
    ++ res;
  }
  return res;
}

// CR_UTIL::Analyze_cr
// find out var and vor used in cr
void
CR_UTIL::Analyze_cr(CODEREP *cr, CODEREP_MAP &var_map, VOR_MAP &vor_map, BOOL ivar_kid) const {
  VSYM_OBJ_REP *vor;
  switch(cr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    break;  // do nothing
  case CK_VAR:
    var_map.insert(std::make_pair(cr->Aux_id(), cr));
    break;
  case CK_IVAR:
    if (cr->Opr() == OPR_PARM) {
      Analyze_cr(cr->Ilod_base(), var_map, vor_map, ivar_kid);
      return;
    }
    vor = _cu->Vsa()->Cr_2_vor(cr);
    if (vor) {
      vor_map.insert(std::make_pair(vor->Vsym_obj()->Id(), vor));
    }
    if (ivar_kid) {
      Analyze_cr(cr->Ilod_base(), var_map, vor_map, ivar_kid);
      if (cr->Opr() == OPR_MLOAD) {
        Analyze_cr(cr->Mstore_size() ? cr->Mstore_size() : cr->Mload_size(),
                   var_map, vor_map, ivar_kid);
      }
      else if (cr->Opr() == OPR_ILOADX) {
        Analyze_cr(cr->Index(), var_map, vor_map, ivar_kid);
      }
    }
    break;
  case CK_OP:
    for (INT i = 0; i < cr->Kid_count(); ++i) {
      Analyze_cr(cr->Opnd(i), var_map, vor_map, ivar_kid);
    }
    break;
  default:
    Is_True(FALSE, ("Unknown cr kind %d", cr->Kind()));
  }
}

// CR_UTIL::Analyze_cmp_cr
// find out var and vor used in lhs and rhs of comparison coderep
void
CR_UTIL::Analyze_cmp_cr(CODEREP *cr, CODEREP_MAP &lhs_var, VOR_MAP &lhs_vor,
                        CODEREP_MAP &rhs_var, VOR_MAP &rhs_vor) const
{
  VSYM_OBJ_REP *vor;
  switch(cr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    break;
  case CK_VAR:
    lhs_var.insert(std::make_pair(cr->Aux_id(), cr));
    break;
  case CK_IVAR:
    Is_True(cr->Opr() != OPR_PARM, ("ivar is parm"));
    if ((vor = _cu->Vsa()->Cr_2_vor(cr)) != NULL)
      lhs_vor.insert(std::make_pair(vor->Vsym_obj()->Id(), vor));
    break;
  case CK_OP:
    if (cr->Opr() == OPR_INTRINSIC_OP)
      break;
    if (cr->Kid_count() == 1) {
      Analyze_cmp_cr(cr->Opnd(0), lhs_var, lhs_vor, rhs_var, rhs_vor);
    }
    else if (cr->Kid_count() == 2) {
      switch (cr->Opr()) {
      case OPR_ADD:
      case OPR_MPY:
        Analyze_cmp_cr(cr->Opnd(0), lhs_var, lhs_vor, rhs_var, rhs_vor);
        Analyze_cmp_cr(cr->Opnd(1), lhs_var, lhs_vor, rhs_var, rhs_vor);
        break;
      case OPR_SUB:
      case OPR_EQ:
      case OPR_NE:
      case OPR_GE:
      case OPR_GT:
      case OPR_LE:
      case OPR_LT:
        Analyze_cmp_cr(cr->Opnd(0), lhs_var, lhs_vor, rhs_var, rhs_vor);
        Analyze_cmp_cr(cr->Opnd(1), rhs_var, rhs_vor, lhs_var, lhs_vor);
        break;
      default:
        break;
      }
    }
    break;
  default:
    Is_True(FALSE, ("Unknown cr kind %d", cr->Kind()));
  }
}

// CR_UTIL::Transform_unary_op
// transform cr for given var and return new cr with var in cr in first kid
CODEREP *
CR_UTIL::Transform_unary_op(CODEREP *cr, CODEREP *var) const
{
  Is_True(cr->Kind() == CK_OP && cr->Kid_count() == 1,
          ("not unary op"));
  CODEREP *ret;
  switch (cr->Opr()) {
  case OPR_CVT:
  case OPR_CVTL:
  case OPR_TRUNC:
  case OPR_FLOOR:
    return Transform_cr(cr->Opnd(0), var);

  case OPR_ABS:
    ret = Transform_cr(cr->Opnd(0), var);
    if (ret) {
      ret = New_unary_cr(OPCODE_make_op(OPR_ABS, cr->Dtyp(), cr->Dsctyp()),
                         ret);
    }
    return ret;

  case OPR_NEG:
    ret = Transform_cr(cr->Opnd(0), var);
    if (ret) {
      if (ret->Kind() != CK_OP ||
          (ret->Opr() != OPR_ADD && ret->Opr() != OPR_SUB)) {
        return New_unary_cr(OPCODE_make_op(OPR_NEG, cr->Dtyp(), cr->Dsctyp()),
                           ret, TRUE);
      }
      // -(op0 +/- op1) ==> -op0 -/+ op1
      CODEREP *op0 = ret->Opnd(0);
      if (op0->Kind() == CK_OP && op0->Opr() == OPR_NEG) {
        // -(-op0) ==> op0
        op0 = op0->Opnd(0);
      }
      else {
        op0 = New_unary_cr(OPCODE_make_op(OPR_NEG, cr->Dtyp(), cr->Dsctyp()),
                           op0, TRUE);
      }
      CODEREP *op1 = ret->Opnd(1);
      OPERATOR opr = ret->Opr();
      if (op1->Kind() == CK_OP && op1->Opr() == OPR_NEG) {
        // -(-op1) ==> op1
        op1 = op1->Opnd(0);
      }
      else {
        opr = ret->Opr() == OPR_ADD ? OPR_SUB : OPR_ADD;
      }
      ret = New_binary_cr(OPCODE_make_op(opr, cr->Dtyp(), MTYPE_V),
                          op0, op1);
    }
    return ret;

  case OPR_EXTRACT_BITS:
    return NULL;

  default:
    Is_True(FALSE, ("TODO: handle %s", OPERATOR_name(cr->Opr())));
    return NULL;
  }
}

// CR_UTIL::Transform_binary_op
// transform cr for given var and return new cr with var in cr in first kid
CODEREP *
CR_UTIL::Transform_binary_op(CODEREP *cr, CODEREP *var) const
{
  Is_True(cr->Kind() == CK_OP && cr->Kid_count() == 2,
          ("not binary op"));

  if (cr->Opnd(0) == var ||
      (cr->Kind() == CK_OP && cr->Opnd(0) == var))
    return cr;

  CODEREP *lhs = Transform_cr(cr->Opnd(0), var);
  CODEREP *rhs = Transform_cr(cr->Opnd(1), var);
  if (lhs == NULL && rhs == NULL) {
    // var not found or cr contains unhandled operator
    return NULL;
  }
  if (lhs != NULL && rhs != NULL) {
    // var occurs in both kids
    return NULL;
  }

  BOOL exchange;
  if (rhs) {
    exchange = TRUE;
    lhs = rhs;
    rhs = cr->Opnd(0);
  }
  else {
    exchange = FALSE;
    rhs = cr->Opnd(1);
  }

  CODEREP *lhs_op0 = lhs;
  CODEREP *lhs_op1 = NULL;
  OPERATOR lhs_opr = OPERATOR_UNKNOWN;
  if (lhs->Kind() == CK_OP) {
    if (lhs->Kid_count() == 1) {
      Is_True(lhs->Opr() == OPR_NEG || lhs->Opr() == OPR_ABS ||
              lhs->Opr() == OPR_TRUNC || lhs->Opr() == OPR_FLOOR,
              ("TODO: %s", OPERATOR_name(lhs->Opr())));
    }
    else if (lhs->Opr() == OPR_ADD || lhs->Opr() == OPR_SUB) {
      lhs_op0 = lhs->Opnd(0);
      lhs_op1 = lhs->Opnd(1);
      lhs_opr = lhs->Opr();
      Is_True(lhs_op0->Opnd(0) == var ||
              (lhs_op0->Kind() == CK_OP &&
               lhs_op0->Kid_count() == 1 &&
               lhs_op0->Opnd(0) == var),
              ("bad lhs op0"));
    }
    else {
      Is_True(FALSE, ("TODO: %s", OPERATOR_name(lhs->Opr())));
      return NULL;
    }
  }

  OPERATOR cr_opr = cr->Opr();
  switch (cr_opr) {
  case OPR_ADD:
    // (lhs_op0 lhs_opr lhs_op1) + rhs ==> lhs_op0 + (rhs lhs_opr lhs_op1)
    break;
#if 0
    if (lhs_op1) {
      Is_True(lhs_opr == OPR_ADD || lhs_opr == OPR_SUB,
              ("TODO: %s", OPERATOR_name(lhs_opr)));
      rhs = New_binary_cr(OPCODE_make_op(lhs_opr, lhs->Dtyp(), lhs->Dsctyp()),
                          rhs, lhs_op1, TRUE);
    }
    return New_binary_cr(OPCODE_make_op(OPR_ADD, cr->Dtyp(), cr->Dsctyp()),
                         lhs_op0, rhs);
#endif
  case OPR_SUB:
    // exchange == FALSE:
    // (lhs_op0 lhs_opr lhs_op1) - rhs ==> lhs_op0 - (rhs ~lhs_opr lhs_op1)
    // exchange == TRUE:
    // rhs - (lhs_op0 lhs_opr lhs_op1) ==> -lhs_op0 + (rhs ~lhs_opr lhs_op1)
    if (lhs_op1) 
      lhs_opr = (lhs_opr == OPR_ADD) ? OPR_SUB : OPR_ADD;

    if (exchange) {
      lhs_op0 = New_unary_cr(OPCODE_make_op(OPR_NEG, lhs->Dtyp(), MTYPE_V),
                             lhs_op0, TRUE);
      cr_opr = OPR_ADD;
    }
    break;
#if 0
    if (lhs_op1) {
      Is_True(lhs_opr == OPR_ADD || lhs_opr == OPR_SUB,
              ("TODO: %s", OPERATOR_name(lhs_opr)));
      lhs_opr = (lhs_opr == OPR_ADD) ? OPR_SUB : OPR_ADD;
      rhs = New_binary_cr(OPCODE_make_op(lhs_opr, lhs->Dtyp(), lhs->Dsctyp()),
                          rhs, lhs_op1, TRUE);
    }
    if (exchange == TRUE) {
      lhs_op0 = New_unary_cr(OPCODE_make_op(OPR_NEG, lhs->Dtyp(), MTYPE_V),
                             lhs_op0, TRUE);
      cr_opr = OPR_ADD;
    }
    return New_binary_cr(OPCODE_make_op(cr_opr, cr->Dtyp(), cr->Dsctyp()),
                         lhs_op0, rhs);
#endif

  case OPR_EQ:
    // exchange == FALSE:
    // (lhs_op0 lhs_opr lhs_op1) > rhs ==> lhs_op0 > (rhs ~lhs_opr lhs_op1)
    // exchange == TRUE:
    // rhs > (lhs_op0 lhs_opr lhs_op1) ==> lhs_op0 < (rhs ~lhs_opr lhs_op1)
    if (lhs_op1)
      lhs_opr = (lhs_opr == OPR_ADD) ? OPR_SUB : OPR_ADD;
    break;

  case OPR_NE:
    if (lhs_op1)
      lhs_opr = (lhs_opr == OPR_ADD) ? OPR_SUB : OPR_ADD;
    break;

  case OPR_GE:
    if (lhs_op1)
      lhs_opr = (lhs_opr == OPR_ADD) ? OPR_SUB : OPR_ADD;

    if (exchange)
      cr_opr = OPR_LE;
    break;

  case OPR_GT:
    if (lhs_op1)
      lhs_opr = (lhs_opr == OPR_ADD) ? OPR_SUB : OPR_ADD;

    if (exchange)
      cr_opr = OPR_LT;
    break;

  case OPR_LE:
    if (lhs_op1)
      lhs_opr = (lhs_opr == OPR_ADD) ? OPR_SUB : OPR_ADD;

    if (exchange)
      cr_opr = OPR_GE;
    break;

  case OPR_LT:
    if (lhs_op1)
      lhs_opr = (lhs_opr == OPR_ADD) ? OPR_SUB : OPR_ADD;

    if (exchange)
      cr_opr = OPR_GT;
    break;

  default:
    Is_True(FALSE, ("TODO: handle %s", OPERATOR_name(cr->Opr())));
    return NULL;
  }

  // (lhs_op0 lhs_opr lhs_op1) cr_opr rhs ==> lhs_op0 cr_opr (rhs ~lhs_opr lhs_op1)
  if (lhs_op1) {
    Is_True(lhs_opr == OPR_ADD || lhs_opr == OPR_SUB,
            ("TODO: %s", OPERATOR_name(lhs_opr)));
    rhs = New_binary_cr(OPCODE_make_op(lhs_opr, lhs->Dtyp(), lhs->Dsctyp()),
                        rhs, lhs_op1, TRUE);
  }
  return New_binary_cr(OPCODE_make_op(cr_opr, cr->Dtyp(), cr->Dsctyp()),
                       lhs_op0, rhs);
}

// CR_UTIL::New_div_cr
// create and simplify division coderep
CODEREP *
CR_UTIL::New_div_cr(CODEREP* dividend, INT64 divisor) const
{
  switch (dividend->Kind()) {
  case CK_LDA:
  case CK_VAR:
  case CK_IVAR:
    Is_True(FALSE, ("div with LDA/VAR/IVAR"));
    return NULL;

  case CK_RCONST:
    Is_True(FALSE, ("TODO: div with RCONST"));
    return NULL;

  case CK_CONST:
    return New_const_cr(dividend->Dtyp(), dividend->Const_val() / divisor);

  case CK_OP:
    if (dividend->Opr() == OPR_MPY) {
      if (dividend->Opnd(1)->Kind() == CK_CONST &&
          dividend->Opnd(1)->Const_val() % divisor == 0) {
        INT val = dividend->Opnd(1)->Const_val() / divisor;
        return val == 1 ? dividend->Opnd(0)
                        : New_binary_cr(dividend->Op(), dividend->Opnd(0),
                                        New_const_cr(MTYPE_I8, val), TRUE);
      }
      CODEREP* op0 = New_div_cr(dividend->Opnd(0), divisor);
      if (op0 != NULL)
        return New_binary_cr(dividend->Op(), op0, dividend->Opnd(1), TRUE);
      CODEREP* op1 = New_div_cr(dividend->Opnd(1), divisor);
      return op1 == NULL ? NULL :
                New_binary_cr(dividend->Op(), dividend->Opnd(0), op1, TRUE);
    }
    else if (dividend->Opr() == OPR_ASHR || dividend->Opr() == OPR_LSHR ||
             dividend->Opr() == OPR_SHL) {
      if (dividend->Opnd(1)->Kind() == CK_CONST &&
          (divisor & (divisor - 1)) == 0) {
        INT shift = dividend->Opnd(1)->Const_val();
        INT exp = my_log_2(divisor);
        if (dividend->Opr() == OPR_SHL) {
          if (shift > exp)
            return New_binary_cr(OPCODE_make_op(OPR_SHL, dividend->Dtyp(), MTYPE_V),
                                 dividend->Opnd(0), shift - exp);
          else if (shift < exp)
            return New_binary_cr(OPCODE_make_op(OPR_LSHR, dividend->Dtyp(), MTYPE_V),
                                 dividend->Opnd(0), exp - shift);
          else
            return dividend->Opnd(0);
        }
        else
          return New_binary_cr(OPCODE_make_op(dividend->Opr(), dividend->Dtyp(), MTYPE_V),
                               dividend->Opnd(0), shift + exp);
      }
      else
        return New_binary_cr(OPCODE_make_op(OPR_DIV, dividend->Dtyp(), MTYPE_V),
                             dividend, divisor);
    }
    else if (dividend->Kid_count() == 1) {
      if (dividend->Opr() == OPR_EXTRACT_BITS) // No idea to handle extract bits
        return NULL;
      Is_True(dividend->Opr() == OPR_NEG || dividend->Opr() == OPR_CVT ||
              dividend->Opr() == OPR_CVTL || dividend->Opr() == OPR_TRUNC,
              ("unexpected operator %s", OPERATOR_name(dividend->Opr())));
      CODEREP* opnd = New_div_cr(dividend->Opnd(0), divisor);
      if (opnd == NULL)
        return NULL;
      if (dividend->Opr() == OPR_CVT)
        return opnd;
      return New_unary_cr(dividend->Op(), opnd);
    }
    else if (dividend->Opr() == OPR_ADD || dividend->Opr() == OPR_SUB) {
      CODEREP* op0 = New_div_cr(dividend->Opnd(0), divisor);
      CODEREP* op1 = New_div_cr(dividend->Opnd(1), divisor);
      return (op0 == NULL || op1 == NULL) ? NULL :
                 New_binary_cr(dividend->Op(), op0, op1, TRUE);
    }
    return NULL;

  default:
    Is_True(FALSE, ("unknown cr kind %d", dividend->Kind()));
  }
  return NULL;
}

