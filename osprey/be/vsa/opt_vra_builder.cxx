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

#include <vector>
#include <stack>
#include <map>
#include <algorithm>
#include <ext/hash_set>
using __gnu_cxx::hash_set;

// =============================================================================
//
// Condition analyzer
// 
// =============================================================================

// analyze condition expression and do:
//  - collect all scalars used in the expression
//  - canonicalize the condition expression for given var
//  - compute the complement condition expression for given var
//
// for example:
// 1 > x ==> x < 1
// x > a + 1 ==> x > a + 1
//           ==> a < x - 1
// create complement coderep for false branch
// x > 1 ==> x > 1 for true branch
//       ==> x <=1 for false branch
//
// TODO: *p can be treated as a virtual scalar?
class CODEREP_ANALYZER {
private:
  const VRA *_vra;

  // disable copy constructor
  CODEREP_ANALYZER(const CODEREP_ANALYZER&);
  // disable assign operator
  CODEREP_ANALYZER operator=(const CODEREP_ANALYZER&);

public:
  // default constructor
  CODEREP_ANALYZER(const VRA *vra) : _vra(vra) { }

  // check if opr is reducible for canonicalization
  BOOL Is_opr_reducible(OPERATOR opr) const
  {
    switch (opr) {
    case OPR_EQ:
    case OPR_NE:
    case OPR_LT:
    case OPR_LE:
    case OPR_GT:
    case OPR_GE:
      return TRUE;
    case OPR_ADD:
    case OPR_SUB:
    case OPR_NEG:
    case OPR_ABS:
      return TRUE;
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_BNOR:
    case OPR_BXOR:
      return FALSE;
    case OPR_LAND:
    case OPR_LIOR:
      return FALSE;
    case OPR_SHL:
    case OPR_ASHR:
    case OPR_LSHR:
      return FALSE;
    default:
      return FALSE;
    }
  }

  // return opr's complement operator
  OPERATOR Complement_opr(OPERATOR opr) const
  {
    switch (opr) {
    case OPR_EQ:
      return OPR_NE;
    case OPR_NE:
      return OPR_EQ;
    case OPR_LT:
      return OPR_GE;
    case OPR_LE:
      return OPR_GT;
    case OPR_GT:
      return OPR_LE;
    case OPR_GE:
      return OPR_LT;
    case OPR_ADD:
      return OPR_SUB;
    case OPR_SUB:
      return OPR_ADD;
    default:
      return OPERATOR_UNKNOWN;
    }
  }

  // return new operator if two operands of opr are exchanged
  OPERATOR Exchange_opr(OPERATOR opr) const
  {
    switch (opr) {
    case OPR_ADD:
    case OPR_NE:
    case OPR_EQ:
      return opr;
    case OPR_GE:
      return OPR_LE;
    case OPR_GT:
      return OPR_LT;
    case OPR_LE:
      return OPR_GE;
    case OPR_LT:
      return OPR_GT;
    default:
      return OPERATOR_UNKNOWN;
    }
  }

  // pickup the opnd according to var
  // if opnd(0) == var, lhs is opnd(0), rhs is opnd(1)
  // if opnd(1) == var, lhs is opnd(1), rhs is opnd(0)
  void Pickup_opnd(CODEREP* expr, CODEREP* var, CODEREP* &lhs, CODEREP* &rhs)
  {
    Is_True(expr->Kind() == CK_OP &&
            (expr->Opr() == OPR_ADD || expr->Opr() == OPR_SUB),
            ("only support ADD or SUB expr"));
    // When entering expr into hash table, two operands may be exchanged
    if (expr->Opr() == OPR_ADD && expr->Opnd(1) == var) {
      lhs = expr->Opnd(1);
      rhs = expr->Opnd(0);
    }
    else {
      lhs = expr->Opnd(0);
      rhs = expr->Opnd(1);
    }
  }

  // canonicalize unary expr for var. for example:
  // if expr is -(a), out is -(a)
  // if expr is -(a+b), out is -(a) - (b)
  BOOL Canon_una_op_cr(CODEREP* expr, CODEREP* var, CODEREP* &out)
  {
    Is_True(expr->Kind() == CK_OP && expr->Kid_count() == 1,
            ("invalid expr cr"));
    if (expr->Opr() == OPR_EXTRACT_BITS)
      return FALSE;  // No idea to handle EXTRACT_BITS

    Is_True(expr->Opr() == OPR_NEG || expr->Opr() == OPR_ABS, ("Only NEG/ABS is supported"));
    CODEREP* opnd = expr->Opnd(0);
    switch (opnd->Kind()) {
    case CK_LDA:
    case CK_CONST:
    case CK_RCONST:
      Is_True(FALSE, ("impossible NEG op with LDA, CONST and RCONST"));
      out = expr;
      return FALSE;
    case CK_VAR:
    case CK_IVAR:
      out = expr;
      return (opnd == var) ? TRUE : FALSE;
    case CK_OP:
      if (opnd->Opr() != OPR_CVT &&
          opnd->Opr() != OPR_CVTL &&
          opnd->Kid_count() != 2)
        return FALSE;
      Is_True(opnd->Opr() == OPR_CVT ||
              opnd->Opr() == OPR_CVTL ||
              opnd->Kid_count() == 2, ("Only handle binary op and cvt/cvtl"));
      // process binary op later
      break;
    default:
      Is_True(FALSE, ("Unexpected cr kind %d", opnd->Kind()));
      return FALSE;
    }

    if (expr->Opr() == OPR_ABS) {
      out = expr;
      return FALSE;
    }

    CODEREP* canon_opnd = NULL;
    BOOL ret = Canon_bin_op_cr(opnd, var, canon_opnd);
    if (ret == FALSE) {
      out = expr;
      return FALSE;
    }
    else {
      Is_True(canon_opnd->Kind() == CK_OP &&
              (canon_opnd->Opr() == OPR_ADD ||
               canon_opnd->Opr() == OPR_SUB),
              ("TODO: only support ADD and SUB"));
      if (canon_opnd->Opr() != OPR_ADD && canon_opnd->Opr() != OPR_SUB) {
        out = NULL;
        return FALSE;
      }
      CODEREP *lhs;
      CODEREP *rhs;
      Pickup_opnd(canon_opnd, var, lhs, rhs);
      Is_True(lhs == var ||
              (lhs->Kind() == CK_OP &&
               lhs->Opr() == OPR_NEG &&
               lhs->Opnd(0) == var), ("var is not placed on lhs"));
      OPERATOR opr = canon_opnd->Opr() == OPR_ADD ? OPR_SUB : OPR_ADD;
      lhs = (lhs->Kind() == CK_OP && lhs->Opr() == OPR_NEG)
              ? lhs->Opnd(0)
              : _vra->New_unary_cr(OPCODE_make_op(OPR_NEG, expr->Dtyp(), MTYPE_V),
                                   lhs, TRUE);
      out = _vra->New_cr(OPCODE_make_op(opr, canon_opnd->Dtyp(), MTYPE_V),
                         lhs, rhs);
      return TRUE;
    }
  }

  // canonicalize binary expr for var. for example:
  // if expr is a + 5 > b,
  //   for var = a, out is a > b - 5
  //   for var = b, out is b < a + 5
  // return TRUE if canonicalization succeeded and out contains
  // the new canonicalized cr.
  BOOL Canon_bin_op_cr(CODEREP* expr, CODEREP* var, CODEREP* &out)
  {
    Is_True(expr->Kind() == CK_OP, ("Invalid expr cr"));
    Is_True(var->Kind() == CK_VAR ||
            var->Kind() == CK_LDA ||
            (VSA_Vra_Ivar && var->Kind() == CK_IVAR), ("Invalid lhs cr"));
    if (Is_opr_reducible(expr->Opr()) == FALSE)
      return FALSE;
    CODEREP* lhs = NULL;
    BOOL lhs_ret = Canon_cr(expr->Opnd(0), var, lhs);
    if (lhs == NULL) // failed to canonicalize
      return FALSE;
    CODEREP* rhs = NULL;
    BOOL rhs_ret = Canon_cr(expr->Opnd(1), var, rhs);
    if (rhs == NULL) // failed to canonicalize
      return FALSE;
    if (lhs_ret == FALSE && rhs_ret == FALSE) {
      out = expr;
      return FALSE;
    }

    if (lhs_ret == TRUE && rhs_ret == TRUE) {
      // TODO: handle this case later
      out = NULL;
      return FALSE;
    }
    
    BOOL lhs_neg = (lhs_ret == TRUE &&
                    lhs->Kind() == CK_OP &&
                    (lhs->Opr() == OPR_NEG ||              /* -var */
                     (lhs->Opnd(0)->Kind() == CK_OP &&
                      lhs->Opnd(0)->Opr() == OPR_NEG)));   /* -var op x */
    BOOL rhs_neg = (rhs_ret == TRUE &&
                    rhs->Kind() == CK_OP &&
                    (rhs->Opr() == OPR_NEG ||              /* -var */
                     (rhs->Opnd(0)->Kind() == CK_OP &&
                     rhs->Opnd(0)->Opr() == OPR_NEG)));    /* -var op x */

    OPERATOR expr_opr = expr->Opr();
    if (lhs_neg == TRUE || rhs_neg == TRUE) {
      if (expr_opr == OPR_ADD || expr_opr == OPR_SUB) {
        if (lhs_neg && lhs->Opr() != OPR_NEG) {
          Is_True(lhs->Opnd(0)->Kind() == CK_OP && lhs->Opnd(0)->Opr() == OPR_NEG,
                  ("lhs->Opnd(0) is not NEG"));
          // lhs_reg == true:
          // (-var + x) + rhs --> -var + (rhs + x)
          // (-var - x) + rhs --> -var + (rhs - x)
          // (-var + x) - rhs --> -var - (rhs - x)
          // (-var - x) - rhs --> -var - (rhs + x)
          OPERATOR rhs_opr = expr_opr == OPR_ADD ? lhs->Opr() :
                                                   Complement_opr(lhs->Opr());
          rhs = _vra->New_cr(OPCODE_make_op(rhs_opr, expr->Dtyp(), MTYPE_V),
                                      rhs, lhs->Opnd(1), TRUE);
          lhs = lhs->Opnd(0);
        }
        else if (rhs_neg && rhs->Opr() != OPR_NEG) {
          Is_True(rhs->Opnd(0)->Kind() == CK_OP && rhs->Opnd(0)->Opr() == OPR_NEG,
                  ("rhs->Opnd(0) is not NEG"));
          // rhs_reg == true:
          // lhs + (-var + x) --> -var + (lhs + x)
          // lhs + (-var - x) --> -var + (lhs - x)
          // lhs - (-var + x) -->  var + (lhs - x)
          // lhs - (-var - x) -->  var + (lhs + x)
          OPERATOR rhs_opr = expr_opr == OPR_ADD ? rhs->Opr() :
                                                   Complement_opr(rhs->Opr());
          CODEREP* tmp = rhs;
          rhs = _vra->New_cr(OPCODE_make_op(rhs_opr, expr->Dtyp(), MTYPE_V),
                             lhs, tmp->Opnd(1), TRUE);
          lhs = expr_opr == OPR_ADD ? tmp->Opnd(0) : tmp->Opnd(0)->Opnd(0);
          if (expr_opr != OPR_ADD)
            expr_opr = OPR_ADD;
        }
        else if (rhs_neg) {
          Is_True(rhs->Opr() == OPR_NEG, ("rhs is not NEG"));
          // rhs_reg == true:
          // lhs + (-var) --> -var + lhs
          // lhs - (-var) --> var + lhs
          CODEREP* tmp = rhs;
          rhs = lhs;
          lhs = expr_opr == OPR_ADD ? tmp : tmp->Opnd(0);
          if (expr_opr != OPR_ADD)
            expr_opr = OPR_ADD;
        }
        out = _vra->New_cr(OPCODE_make_op(expr_opr, expr->Dtyp(), MTYPE_V),
                           lhs, rhs);
        return TRUE;
      }
      // -var op x --> var Exchange_opr -x
      // x op -var --> var op -x
      CODEREP *new_lhs = NULL;
      CODEREP *new_rhs = NULL;
      if (lhs_neg) {
        // exchange operator but not exchange operands
        expr_opr = Exchange_opr(expr_opr);
      }
      else {
        // exchange operands but not exchange operator
        CODEREP *tmp = lhs;
        lhs = rhs;
        rhs = tmp;
      }

      if (lhs->Opr() == OPR_NEG) {
        // handle -(var)
        Is_True(lhs->Opnd(0) == var,
                ("lhs is not the variable expected"));
        new_lhs = lhs->Opnd(0);
        new_rhs = _vra->New_unary_cr(OPCODE_make_op(OPR_NEG, rhs->Dtyp(), MTYPE_V),
                               rhs, TRUE);
      }
      else {
        // handle -(var) +/- x
        Is_True(lhs->Opr() == OPR_ADD || lhs->Opr() == OPR_SUB,
                ("Only support ADD and SUB"));
        Is_True(lhs->Opnd(0)->Kind() == CK_OP &&
                lhs->Opnd(0)->Opr() == OPR_NEG &&
                lhs->Opnd(0)->Opnd(0) == var,
                ("lhs is not the expr with expected variable"));
        new_lhs = lhs->Opnd(0)->Opnd(0);
        CODEREP *lhs_rhs = lhs->Opnd(1);
        // -(var) + y op z --> var Exchange_opr y - z
        // -(var) - y op z --> var Exchange_opr -y - z
        if (lhs->Opr() == OPR_SUB) {
          lhs_rhs = _vra->New_unary_cr(OPCODE_make_op(OPR_NEG, lhs->Dtyp(), MTYPE_V),
                                       lhs_rhs, TRUE);
        }
        new_rhs = _vra->New_cr(OPCODE_make_op(OPR_SUB, expr->Dtyp(), MTYPE_V),
                               lhs_rhs, rhs, TRUE);
      }
      out = _vra->New_cr(OPCODE_make_op(expr_opr, expr->Dtyp(), expr->Dsctyp()),
                         new_lhs, new_rhs);
      return TRUE;
    }

    BOOL exchanged = FALSE;
    if (lhs_ret == FALSE && rhs_ret == TRUE) {
      // swap lhs & rhs
      exchanged = TRUE;
      CODEREP* temp;
      switch (expr_opr) {
      case OPR_ADD:  // b + a  --> a + b
      case OPR_EQ:   // b == a --> a == b
      case OPR_NE:   // b != a --> a != b
      case OPR_GT:   // b > a  --> a < b
      case OPR_GE:   // b >= a --> a <= b
      case OPR_LT:   // b < a  --> a > b
      case OPR_LE:   // b <= a --> a >= b
        expr_opr = Exchange_opr(expr_opr);
        temp = lhs;
        lhs = rhs;
        rhs = temp;
        break;
      case OPR_SUB:  // b - a --> (-a) + b
        expr_opr = OPR_ADD;
        if (rhs->Kind() == CK_OP) {
          if (rhs->Opr() == OPR_NEG) {
            Is_True(FALSE, ("Really hit a NEG here?"));
            Is_True(rhs->Opnd(0) == var,
                    ("var is placed wrongly"));
            temp = rhs->Opnd(0);
            rhs = lhs;
            lhs = temp;
            break;
          }
          if (rhs->Opr() != OPR_ADD && rhs->Opr() != OPR_SUB) {
            out = NULL;
            return FALSE;
          }
          // x - (a +|- y) --> (-a) + (x -|+ y)
          Is_True(rhs->Opr() == OPR_ADD || rhs->Opr() == OPR_SUB,
                  ("rhs opr %s not supported", OPERATOR_name(rhs->Opr())));
          CODEREP *rhs_lhs;
          CODEREP *rhs_rhs;
          Pickup_opnd(rhs, var, rhs_lhs, rhs_rhs);
          Is_True(rhs_lhs == var, ("var is placed wrongly"));
          temp = _vra->New_unary_cr(OPCODE_make_op(OPR_NEG, rhs->Dtyp(), MTYPE_V),
                             rhs_lhs, TRUE);
          OPERATOR lhs_opr = rhs->Opr() == OPR_ADD ? OPR_SUB : OPR_ADD;
          rhs = _vra->New_cr(OPCODE_make_op(lhs_opr, rhs->Dtyp(), MTYPE_V),
                             lhs, rhs_rhs);
          lhs = temp;
        }
        else {
          Is_True(rhs == var, ("var is not placed wrongly"));
          temp = _vra->New_unary_cr(OPCODE_make_op(OPR_NEG, expr->Dtyp(), MTYPE_V),
                             rhs, TRUE);
          rhs = lhs;
          lhs = temp;
        }
        break;
      default:
        Is_True(FALSE, ("Unexpected operator %s", OPERATOR_name(expr_opr)));
        break;
      }
    }

    if (exchanged == TRUE ||
        (lhs_ret == TRUE && rhs_ret == FALSE)) {
      if (lhs == var ||
          (lhs->Kind() == CK_OP &&
           (lhs->Opr() == OPR_NEG ||
            lhs->Opr() == OPR_ABS) &&
           (lhs->Opnd(0) == var ||
            ((lhs->Opnd(0)->Opr() == OPR_CVT || lhs->Opnd(0)->Opr() == OPR_CVTL) &&
             lhs->Opnd(0)->Opnd(0) == var)))) {
        out = _vra->New_cr(OPCODE_make_op(expr_opr, expr->Dtyp(), expr->Dsctyp()),
                           lhs, rhs);
      }
      else {
        Is_True(lhs->Kind() == CK_OP, ("Invalid lhs cr"));
        if (lhs->Opr() != OPR_ADD && lhs->Opr() != OPR_SUB) {
          out = expr;
          return FALSE;
        }
        Is_True(lhs->Opr() == OPR_ADD || lhs->Opr() == OPR_SUB, ("Only ADD/SUB is supported"));
        // move lhs's rhs to rhs
        CODEREP *lhs_lhs;
        CODEREP *lhs_rhs;
        Pickup_opnd(lhs, var, lhs_lhs, lhs_rhs);
        // make sure lhs_lhs matches with var, or
        // lhs_lhs's first opnd matches with var if lhs_lhs is NEG/ABS
        Is_True(lhs_lhs == var ||
                (lhs_lhs->Kind() == CK_OP &&
                 (lhs_lhs->Opr() == OPR_NEG ||
                  lhs_lhs->Opr() == OPR_ABS) &&
                 (lhs_lhs->Opnd(0) == var ||
                  ((lhs->Opnd(0)->Opr() == OPR_CVT || lhs->Opnd(0)->Opr() == OPR_CVTL) &&
                   lhs->Opnd(0)->Opnd(0) == var))),
                ("var is not placed on lhs"));
        OPCODE new_rhs_opc = OPCODE_make_op(expr_opr == OPR_ADD ? lhs->Opr() : Complement_opr(lhs->Opr()),
                                            lhs->Dtyp(),
                                            lhs->Dsctyp());
        CODEREP* new_rhs = _vra->New_cr(new_rhs_opc, rhs, lhs_rhs, TRUE);
        out = _vra->New_cr(OPCODE_make_op(expr_opr, expr->Dtyp(), expr->Dsctyp()),
                           lhs_lhs, new_rhs);
      }
      return TRUE;
    }
    else if (lhs_ret == TRUE && rhs_ret == TRUE) {
      // TODO: handle this case
      out = NULL;
      return FALSE;
    }
    return FALSE;
  }

  // canonicalize the expr for var.
  // return TRUE if the canonicalization succeesed and out contains the new cr.
  // for example,
  // expr is a + 5 > b and var is a, out is:
  //  a > b - 5
  BOOL Canon_cr(CODEREP* expr, CODEREP* var, CODEREP* &out)
  {
    switch (expr->Kind()) {
    case CK_LDA:
    case CK_CONST:
    case CK_RCONST:
    case CK_VAR:
    case CK_IVAR:
      out = expr;
      return (expr == var) ? TRUE : FALSE;
    case CK_OP:
      if (expr->Opr() == OPR_CVT || expr->Opr() == OPR_CVTL ||
          expr->Opr() == OPR_TRUNC)
        return Canon_cr(expr->Opnd(0), var, out);
      else if (expr->Kid_count() == 1)
        return Canon_una_op_cr(expr, var, out);
      else if (expr->Kid_count() == 2)
        return Canon_bin_op_cr(expr, var, out);
      else {
        Is_True(FALSE, ("TODO: handle op %s", OPERATOR_name(expr->Opr())));
        return FALSE;
      }
    default:
      Is_True(FALSE, ("Unexpected cr kind %d", expr->Kind()));
      return FALSE;
    }
  }

  // analyze conditional crs used in TRUEBR/FALSEBR and collect
  // all scalars used in the cr into vector st
  // return TRUE if cr only contains "reducible" operators
  BOOL Analyze_cond_vars(CODEREP *cr, hash_set<CODEREP*>& st) const
  {
    INT i;
    BOOL ret = TRUE;
    switch(cr->Kind()) {
    case CK_LDA:
    case CK_CONST:
    case CK_RCONST:
      /* ignore */
      break;
    case CK_VAR:
      st.insert(cr);
      ret = TRUE;
      break;
    case CK_IVAR:
      if (VSA_Vra_Ivar) {
        st.insert(cr);
        ret = TRUE;
      }
      else {
        ret = FALSE;
      }
      break;
    case CK_OP:
      for (i = 0; i < cr->Kid_count(); ++i) {
        if (ret == TRUE)
          ret = Is_opr_reducible(cr->Opr());
        CODEREP *opnd = cr->Get_opnd(i);
        if (ret == TRUE && opnd->Kind() == CK_OP && opnd->Opr() == OPR_CVT)
          opnd = opnd->Get_opnd(0);
        BOOL ret1 = Analyze_cond_vars(opnd, st);
        if (ret == TRUE && ret1 == FALSE)
          ret = FALSE;
      }
      break;
    default:
      Is_True(FALSE, ("Unknown cr kind %d", cr->Kind()));
    }
    return ret;
  }

  // generate complement cr for given cr. for example
  // if cr is a > b + 5, return cr is
  //   a <= b + 5
  CODEREP* Complement_cr(CODEREP* cr)
  {
    Is_True(cr->Kind() == CK_OP, ("Unexpected CR kind %d", cr->Kind()));
    CODEREP* out = _vra->New_cr(OPCODE_make_op(Complement_opr(cr->Opr()),
                                               cr->Dtyp(), cr->Dsctyp()),
                                cr->Opnd(0), cr->Opnd(1));
    return out;
  }

  // canonicalize cr
  CODEREP* Canon_cr(CODEREP* expr, CODEREP* var)
  {
    CODEREP* out = NULL;
    Canon_cr(expr, var, out);
    return out;
  }
#if 0
  CODEREP* Create_loop_ub(CODEREP* lb, CODEREP* tripcount) {
    CODEREP* out = _vra->New_cr(OPCODE_make_op(OPR_ADD, lb->Dtyp(), MTYPE_V),
                                lb, tripcount, TRUE);
    return out;
  }

  CODEREP* Create_cmp_cr(OPERATOR opr, CODEREP* lhs, CODEREP* rhs) {
    CODEREP* out = _vra.New_cmp_cr(opr, lhs, rhs);
    return out;
  }
#endif
};

// =============================================================================
//
// Builder interface
// 
// =============================================================================

// traverse the DOM tree, check stmtreps and COMPGOTO/FALSEBR/TRUEBR,
// fill the value range of variable into VRA
//
// for value range of VAR, a stack for AUX_ID was maintained for all history
// value range of this AUX. For assignment of the AUX, the value range is the CR
// of its RHS. If this AUX is used in condition expr of COMPGOTO/FALSEBR/TRUEBR,
// its value range is the conjunction of top of the AUX's value range stack and
// the condition. For example:
//  x <- blah;     // X's value range is CR(blah), push CR(blah) into stack[x]
//  if (x > 5)
//    y <- x;      // X's value range is stack[x].Top() ^ (x > 5)
//                    push new value range into stack[x]
//    if ( x < 100 )
//      z <- x;    // X's value range is stack[x].Top() ^ (x < 10)
//                    push new value range into stack[x]
//      x <- blah; // X's value range is CR(blah). Push CR(blah) into stack[x]
// for value range of IVAR, so far we don't maintain the stack for current value
// range. So only conditional branch's successor's value range is created. For
// example:
//  x->a <- blah;  // No value range for x->a
//  if (x->a > 5)
//    y <- x;      // x->a's value range is (x->a > 5)
 
class VRA_BUILDER {
  friend class VRA;
private:
  typedef std::vector< std::stack<UINT32> > EXPR_STACK;
  
  VRA                  *_vra;
  COMP_UNIT            *_comp_unit;
  IPSA                 *_ipsa;
  CFG                  *_cfg;
  CODEMAP              *_htable;
  OPT_STAB             *_opt_stab;
  MEM_POOL              _loc_pool;
  EXPR_STACK            _stack;          // rename stack for each variable
  std::vector<bool>     _bb_visited;     // track if the BB is visited
  std::stack<CODEREP*>  _iv_stack;       // stack to keep the IV for loops
  std::vector<BB_NODE*> _to_be_resolved; // bb has loop info but no IV or TRIP COUNT

  // disable copy constructor
  VRA_BUILDER(const VRA_BUILDER&);
  // disable assign operator
  VRA_BUILDER& operator=(const VRA_BUILDER&);

private:
  // constructor, can only be called by VRA
  VRA_BUILDER(VRA* analyzer, COMP_UNIT* cu, IPSA* ipsa)
   : _vra(analyzer), _comp_unit(cu), _ipsa(ipsa),
     _cfg(cu->Cfg()), _htable(cu->Htable()), _opt_stab(cu->Opt_stab())
  {
  }

private:
  // create value range for equivalent crs in bb
  // b1 = a1;
  // c1 = b1;
  // if (c1 == 5) {
  //   ... blahblah  // a1, b1 and c1 equals to 5 in this bb
  // TODO: here is a workaround:
  //       if b1/c1 is in cond_crs, don't create value range for them
  void New_value_range(CODEREP* cr,  UINT32 bb_id, UINT32 expr_id,
                       hash_set<CODEREP*>& cond_crs, BOOL replace = FALSE)
  {
    // create value range for <cr, bb_id> at first
    Is_True(cr->Kind() == CK_VAR ||
            (VSA_Vra_Ivar && cr->Kind() == CK_IVAR), ("cr kind is not VAR/IVAR %d", cr->Kind()));
    _vra->New_value_range(cr, bb_id, expr_id, replace);
    Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
             (TFile, "VRA-BLD: BB%d New_value_range(cr%d:bb%d)->expr%d\n",
                     bb_id, cr->Coderep_id(), bb_id, expr_id));
    while (cr != NULL && cr->Kind() == CK_VAR) {
      // traverse cr's defstmt
      if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
          cr->Is_flag_set(CF_DEF_BY_CHI) ||
          cr->Is_flag_set(CF_DEF_BY_PHI) ||
          cr->Defstmt() == NULL)
        break;
      Is_True(cr->Defstmt()->Lhs() == cr, ("cr is not stmtrep lhs"));
      cr = cr->Defstmt()->Rhs();
      if (cr == NULL || cr->Kind() != CK_VAR)
        break;
      // only add cr when it's not used in cond expression
      if (cond_crs.find(cr) == cond_crs.end()) {
        cond_crs.insert(cr);
        _vra->New_value_range(cr, bb_id, expr_id, replace);
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-BLD: BB%d New_value_range(cr%d:bb%d)->expr%d\n",
                         bb_id, cr->Coderep_id(), bb_id, expr_id));
      }
    }
  }

  // New_value_expr(UINT32 expr, OPERATOR opr, CODEREP* cr)
  // new_expr = expr opr cr. opr is +/-
  UINT32 New_value_expr(UINT32 expr, IDTYPE bb, OPERATOR opr, CODEREP* var, CODEREP* opnd)
  {
    const VRA_EXPR& e = _vra->Expr(expr);
    UINT32 ret = expr;
    switch (e.Opr()) {
    case OPR_CODEREP:
    case OPR_PLACEHOLDER:
      {
        CODEREP* cr = e.Cr();
        CODEREP* val;
        if (cr->Kind() == CK_OP) {
          if (cr->Kid_count() != 2 || cr->Opnd(0)->Kind() != CK_VAR) {
            Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                     (TFile, "VRA-BLD: use IV expr %d for unsupported cr:\n", expr));
            Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG), cr->Print(TFile));
            return expr;
          }
          CODEREP* rhs = _vra->New_cr(OPCODE_make_op(opr, var->Dtyp(), MTYPE_V),
                                      cr->Opnd(1), opnd, TRUE);
          val = _vra->New_cr(cr->Op(), var, rhs, FALSE);
        }
        else {
          val = _vra->New_cr(OPCODE_make_op(opr, cr->Dtyp(), MTYPE_V),
                             cr, opnd, TRUE);
        }
        ret = _vra->New_coderep_expr(val, bb, FALSE);
      }
      break;

    case OPR_TOP:
    case OPR_BOTTOM:
    case OPR_EMPTY:
      break;

    case OPR_CONJUNCTION:
    case OPR_DISJUNCTION:
      {
        UINT32 op0 = New_value_expr(e.Op0(), bb, opr, var, opnd);
        UINT32 op1 = New_value_expr(e.Op1(), bb, opr, var, opnd);
        ret = _vra->New_expr(e.Opr(), bb, op0, op1);
      }
      break;

    default:
      Is_True(FALSE, ("unknown expr opr"));
      break;
    }
    return ret;
  }

  // check if sym is already in BB's phi list
  BOOL Var_in_phi_list(BB_NODE *bb, AUX_ID sym)
  {
    PHI_LIST *list = bb->Phi_list();
    if (list == NULL)
      return FALSE;

    PHI_NODE *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_NODE(phi, phi_iter, Init(list)) {
      // don't check if phi is live. ignore creating vra for dead phi
      // because dead phi means no use of phi result
      //if (!phi->Live())
      //  continue;
      if (phi->Aux_id() == sym)
        return TRUE;
    }
    return FALSE;
  }

  // check if cr contains LDID of aux_id
  BOOL Contains(CODEREP* cr, AUX_ID aux) {
    INT i;
    switch (cr->Kind()) {
    case CK_VAR:
      return cr->Aux_id() == aux;
    case CK_OP:
      for (i = 0; i < cr->Kid_count(); ++ i)
        if (Contains(cr->Opnd(i), aux))
          return TRUE;
      return FALSE;
    default:
      return FALSE;
    }
  }

  // check if bb is unreachable
  void Mark_reachable(UINT32 expr, IDTYPE bb) {
    if (expr == VRA::VR_EMPTY ||
        _vra->Expr(expr).Opr() == OPR_EMPTY) {
      _cfg->Get_bb(bb)->Set_unreachable();
    }
  }

  // find step op which increasing/decreasing iv
  CODEREP* Find_step_op(BB_NODE* bb, AUX_ID aux) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (OPERATOR_is_scalar_store(stmt->Opr()) &&
          stmt->Lhs()->Aux_id() == aux)
        return stmt->Rhs();
    }
    return NULL;
  }

  // find def call stmt with special DNA flag
  STMTREP* Find_def_call(CODEREP* cr, UINT32 &flag) {
    if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
        cr->Is_flag_set(CF_DEF_BY_PHI))
      return NULL;

    if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
      // check def chi
      STMTREP* stmt = cr->Defstmt();
      if (stmt->Opr() == OPR_CALL) {
        DNA_NODE* dna = _ipsa->Get_dna(_comp_unit->Dna()->File_idx(),
                                       stmt->St());
        if (dna != NULL) {
          flag = dna->Flags();
          return stmt;
        }
      }
    }
    else {
      // check def stmt
      STMTREP* stmt = cr->Defstmt();
      if (stmt != NULL && stmt->Rhs()->Kind() == CK_VAR)
        return Find_def_call(stmt->Rhs(), flag);
    }
    return NULL;
  }

  // Find java array length coderep by INTRN_NEW_OBJ_ARR
  CODEREP* Find_array_len(CODEREP *cr) {
    Is_True_Ret(PU_java_lang(Get_Current_PU()),
                ("Find_array_len only for java"), NULL);
    Is_True_Ret(cr->Kind() == CK_IVAR,
                ("Find_array_len: java array len should be iload"), NULL);
    CODEREP *base = cr->Ilod_base();
    if(base == NULL || base->Kind() != CK_VAR) {
      return NULL;
    }
    TY_IDX base_ty = base->object_ty();
    if(TY_kind(base_ty) == KIND_POINTER &&
       TY_is_array_class(TY_pointed(base_ty)) &&
       cr->I_field_id() == JAVA_ARR_LEN_FLDID) {
      STMTREP *defstmt = base->Defstmt();
      while(defstmt && defstmt->Rhs() && defstmt->Rhs()->Kind() == CK_VAR) {
        defstmt = defstmt->Rhs()->Defstmt();
      }
      if(defstmt &&
         defstmt->Opr() == OPR_INTRINSIC_CALL) {
        INTRINSIC intrn = defstmt->Rhs()->Intrinsic();
        if(intrn == INTRN_NEW_OBJ_ARR) {
          CODEREP *len_cr = defstmt->Rhs()->Opnd(0);
          Is_True_Ret(len_cr && len_cr->Opr() == OPR_PARM, ("invalid new obj array"), NULL);
          return len_cr->Ilod_base();
        } else if(intrn == INTRN_NEW_PRIM_ARR) {
          CODEREP *len_cr = defstmt->Rhs()->Opnd(1);
          Is_True_Ret(len_cr && len_cr->Opr() == OPR_PARM, ("invalid new prim array"), NULL);
          return len_cr->Ilod_base();
        }
      }
    }
    return NULL;
  }

  // traverse bb to collect value range information
  void Process_bb(BB_NODE *bb);
  // set value range for COMPGOTO/TRUEBR/FALSEBR's successors
  void Process_succ(BB_NODE *bb, hash_set<CODEREP*>& vars);
  // collect value range from compgoto
  void Process_compgoto(BB_NODE *bb, STMTREP *compgoto, hash_set<CODEREP*>& vars);
  // propagate predecessors' value range to current bb
  void Process_pred(BB_NODE *bb, BOOL fwd);
  // process value range for phi
  void Process_phi(BB_NODE *bb, BOOL fwd);
  // update phi's operands' value range if phi node was processed before
  void Update_phi(BB_NODE *bb, BB_NODE *succ, const hash_set<CODEREP*>& vars);
  // forward traverse stmt in bb to collect value range
  void Process_stmt_fwd(BB_NODE *bb);
  // backward traverse stmt in bb to collect 
  void Process_stmt_rev(BB_NODE *bb);
  // process stmtrep to collect value range
  void Process_stmt(STMTREP *sr, BB_NODE *bb, BOOL fwd);
  // process chi to collect value range
  void Process_chi(STMTREP *sr, BB_NODE *bb, BOOL fwd);
  // generate value range for xvsa range/compare call
  void Process_range_call(STMTREP *call, BB_NODE *bb, BOOL fwd);
  // initialize value range builder
  void Initialize();
  // finalize value range
  void Finalize();

  // For loop without IV
  // generate special value range for call
  UINT32 Process_retv_from_call(STMTREP *call, CODEREP* cr, BB_NODE *bb);
  // analyze all defs in loop body
  BOOL Analyze_def(BB_NODE_SET* body_set, BB_NODE* bb, CODEREP* rhs);
  // analyze coderep in stmt in loop body
  BOOL Analyze_op(BB_NODE_SET* body_set, BB_NODE* bb, CODEREP* rhs);
  // analyze value ranges for varaibles in loop without IV
  BOOL Analyze_loop_without_iv(BB_NODE* bb, BB_LOOP* loop);

  // Main entry to build value range
  void Build()
  {
    // initialize stack
    Initialize();

    // process dom tree
    Process_bb(_cfg->Entry_bb());

    // finalize value range table
    Finalize();
  }
};


// =============================================================================
//
//  Builder implementation
// 
// =============================================================================

// traverse bb to collect value range information
void
VRA_BUILDER::Process_bb(BB_NODE *bb)
{
  Is_True(_bb_visited[bb->Id()] == false, ("BB %d already visited", bb->Id()));
  _bb_visited[bb->Id()] = true;

  Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
           (TFile, "VRA-BLD: BB%d enter Process_bb.\n", bb->Id()));

  BOOL multi_pred = FALSE;
  if (bb->Pred() != NULL) {
    // process value range inherited from predecessors and push to stack
    Process_pred(bb, TRUE);
    //multi_pred = bb->Pred()->Multiple_bbs();
    if (bb->Phi_list()) {
      // process phi value range and push to stack
      Process_phi(bb, TRUE);
    }
  }

  // forward traverse stmts in bb and push to stack
  Process_stmt_fwd(bb);

  if (bb != _cfg->Fake_entry_bb() && bb->Succ() != NULL) {
    BB_LIST *succ = bb->Succ();
    // propagate value range to successor
    hash_set<CODEREP*> vars;
    if (succ->Multiple_bbs())
      Process_succ(bb, vars);
    // if successor is visited before, update its phi's operands
    if (_bb_visited[succ->Node()->Id()] == true)
      Update_phi(bb, succ->Node(), vars);
  }

  Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
           (TFile, "VRA-BLD: BB%d visit dominated BBs.\n", bb->Id()));

  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    // traverse all children in DOM tree
    Process_bb(dom_bb);
  }

  Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
           (TFile, "VRA-BLD: BB%d done all dominated BBs.\n", bb->Id()));

  // backward traverse stmts in bb and pop from stack
  Process_stmt_rev(bb);

  if (bb->Pred() != NULL) {
    if (bb->Phi_list()) {
      // process phi and pop from stack
      Process_phi(bb, FALSE);
    }
    // process value range inherit from predecessors and pop from stack
    Process_pred(bb, FALSE);
  }

  Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
           (TFile, "VRA-BLD: BB%d exit Process_bb.\n", bb->Id()));
}

// set TRUEBR/FALSEBR/COMPGOTO's successors' value range
void
VRA_BUILDER::Process_succ(BB_NODE *bb, hash_set<CODEREP*>& vars)
{
  STMTREP *stmt = bb->Last_stmtrep();
  Is_True(stmt != NULL, ("Null last stmt in bb"));
  OPERATOR opr = stmt->Opr();
  if (opr == OPR_AGOTO) {
    return;  // ignore AGOTO
  }
  else if (opr == OPR_COMPGOTO) {
    Is_True(bb->Kind() == BB_VARGOTO, ("BB kind %d not VARGOTO", bb->Kind()));
    // process value range for COMPGOTO
    return Process_compgoto(bb, stmt, vars);
  }

  // only TRUEBR and FALSEBR is allowed below
  Is_True(opr == OPR_TRUEBR || opr == OPR_FALSEBR,
          ("TODO: support %s", OPERATOR_name(opr)));
  if (opr != OPR_TRUEBR && opr != OPR_FALSEBR)
    return;

  CODEREP* cond_cr = stmt->Rhs();
  Is_True(cond_cr != NULL, ("invalid TRUEBR/FALSEBR stmtrep"));
  CODEREP_ANALYZER cra(_vra);
  // collect all scalars occurs in condition expression
  BOOL ret = cra.Analyze_cond_vars(stmt->Rhs(), vars);
  if (ret == FALSE || vars.size() == 0) {
    vars.clear();
    return;
  }

  UINT32 bbs[2];   // TRUEBR/FALSEBR only have 2 successors,
                   // one is the branch target in TRUEBR/FALSEBR
                   // the other is the fall-through BB
  UINT32 pdom_bb_id = 0;      // check if succ pdom bb
  BB_NODE *succ;
  BB_LIST_ITER succ_iter;
  FOR_ALL_ELEM(succ, succ_iter, Init(bb->Succ())) {
    if (bb->Dominates(succ) && succ->Postdominates(bb))
      pdom_bb_id = succ->Id();// set pdom_bb to this succ
    if (succ == bb->Next())
      bbs[0] = succ->Id();    // put fall-through into bbs[0]
    else
      bbs[1] = succ->Id();    // put branch target into bbs[1]
  }
  if (opr == OPR_TRUEBR) {
    // adjust two successors for TRUEBR to make sure
    // bbs[0] is alwys taken when cond_expr is TRUE
    UINT32 bb = bbs[0];
    bbs[0] = bbs[1];
    bbs[1] = bb;
  }

  if (cond_cr->Kind() == CK_VAR ||
      cond_cr->Kind() == CK_IVAR) {
    // convert if (a) into if (a != 0)
    cond_cr = _vra->New_cmp_cr(OPR_NE, cond_cr, (INT64)0);
  }

  BB_LOOP* loop_info = bb->Loop();
  CODEREP* iv = NULL;
  CODEREP* tc = NULL;
  IDTYPE tail_bb = 0;
  if (loop_info != NULL) {
    STMTREP* sr = loop_info->Trip_count_stmt();
    tc = sr ? sr->Rhs() : loop_info->Trip_count_expr();
    if (tc != NULL)
      iv = loop_info->Iv();
    if (loop_info->Tail())
      tail_bb = loop_info->Tail()->Id();
  }

  if (!_cfg->Get_bb(bbs[0])->Pred()->Multiple_bbs())
    _vra->Set_bb_cond(bbs[0], cond_cr, stmt);
  if (!_cfg->Get_bb(bbs[1])->Pred()->Multiple_bbs())
    _vra->Set_bb_cond(bbs[1], cra.Complement_cr(cond_cr), stmt);

  hash_set<CODEREP*> val_vars = vars;
  for (hash_set<CODEREP*>::iterator it = vars.begin();
       it != vars.end();
       ++it) {
    CODEREP* cr = (CODEREP*)*it;
    AUX_ID st = cr->Kind() == CK_IVAR ? ILLEGAL_AUX_ID : cr->Aux_id();
    if (!VSA_Vra_Ivar && st == ILLEGAL_AUX_ID)
      continue;

    // ignore iv's range. it's handled specially
    if (iv != NULL && st == iv->Aux_id() &&
        !cr->Is_flag_set(CF_DEF_BY_PHI) &&
        (bbs[0] == tail_bb || bbs[1] == tail_bb)) {
      // get UB
      CODEREP* canon_cr = cra.Canon_cr(cond_cr, cr);
      if (canon_cr == NULL)
        continue;
      Is_True(canon_cr->Kind() == CK_OP && canon_cr->Opnd(0) == cr,
              ("invalid canon_cr"));

      CODEREP* ub = canon_cr->Opnd(1);
      // get STEP
      STMTREP* sr = cr->Defstmt();
      Is_True(sr != NULL && sr->Opr() == OPR_STID, ("invalid step stmt"));
      CODEREP* step_cr = sr->Rhs();
      Is_True(step_cr->Kind() == CK_OP &&
              (step_cr->Opr() == OPR_ADD || step_cr->Opr() == OPR_SUB),
              ("invalid step cr"));
      CODEREP* step = step_cr->Opnd(0) == iv ? step_cr->Opnd(1) :
                                               step_cr->Opnd(0);
      BOOL increasing = step_cr->Opr() == OPR_ADD &&
                        (step->Kind() != CK_CONST || step->Const_val() > 0);
      // calculate the new upper/lower bound
      if (step->Kind() != CK_CONST || step->Const_val() != 1)
        ub = _vra->New_cr(OPCODE_make_op(step_cr->Opr(), cr->Dtyp(), MTYPE_V),
                          ub, step, TRUE);
      OPERATOR opr = !loop_info->Exit_early() ? OPR_EQ :
                       increasing ? OPR_LE : OPR_GE;
      CODEREP* eq = _vra->New_cmp_cr(opr, cr, ub);
      UINT32 expr = _vra->New_coderep_expr(eq, tail_bb, FALSE);
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-BLD: new range for iv sym%dv%d cr%d at tail BB%d: ",
                       iv->Aux_id(), iv->Version(), iv->Coderep_id(), tail_bb));
      Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                   eq->Print(TFile));
      Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                   _vra->Print_expr(expr, TFile));
      _vra->New_value_range(cr, tail_bb, expr);
      continue;
    }

    // TODO: hack for incorrect zero version, revise the back
    //Is_True(!_stack[st].empty(), ("stack for sym%d is empty", st));

    UINT32 left = (st == ILLEGAL_AUX_ID || _stack[st].empty()) ? VRA::VR_BOTTOM
                                                               : _stack[st].top();
    if (left != VRA::VR_BOTTOM) {
      // for pattern like
      // b = a;
      // if (a > 0) {
      //   if (b < 10) { /* b's left is CR a. Try to get a's range expr */
      //     ... <- b;
      const VRA_EXPR& left_expr = _vra->Expr(left);
      if (left_expr.Opr() == OPR_CODEREP &&
          left_expr.Cr()->Kind() == CK_VAR) {
        UINT32 cr_expr = _vra->Value_range(left_expr.Cr(), bb->Id());
        if (cr_expr > left) // new expr generated
          left = cr_expr;
      }
    }

    CODEREP* canon_cr = cra.Canon_cr(cond_cr, cr);  // ca
    if (canon_cr == NULL) {
      // fail to analyze, set to range before the condition
      New_value_range(cr, bbs[0], left, val_vars);
      New_value_range(cr, bbs[1], left, val_vars);
      continue;
    }
    if (iv && Contains(canon_cr->Opnd(1), iv->Aux_id())) // do not generate value range if compared with iv
      continue;

    // check if cr comes from is_null/is_not_null check result and
    // set value range for its arguments
    CODEREP* checked_cr = NULL;
    BOOL same_range = FALSE;
    if (cr->Kind() == CK_VAR &&
        canon_cr->Opnd(1)->Kind() == CK_CONST) {
      UINT32 flag = 0;
      STMTREP* stmt = Find_def_call(cr, flag);
      if (stmt != NULL &&
          (flag & (DNA_CHECK_ZERO | DNA_CHECK_NOT_ZERO)) &&
          stmt->Rhs()->Kid_count() > 0) {
        Is_True(stmt->Rhs()->Opnd(0)->Kind() == CK_IVAR &&
                stmt->Rhs()->Opnd(0)->Opr() == OPR_PARM, ("not param opr"));
        checked_cr = stmt->Rhs()->Opnd(0)->Ilod_base();
        same_range = (flag & DNA_CHECK_NOT_ZERO) != 0;
      }
    }

    // signed variable used in unsigned comparison
    // if ((unsigned) x < N) {
    //   0 <= x < N
    // } else {
    //   x < 0 || x >= N
    // }
    // if ((unsigned) x > N) {
    //   x > N || x < 0
    // } else {
    //   0 <= x <= N
    // }
    BOOL unsign_cvt = MTYPE_is_unsigned(cond_cr->Dsctyp()) &&
                      MTYPE_is_signed(cr->Dsctyp());

    // check if then/else bb pdom bb. if then/else bb pdom bb, value
    // range for the cr isn't impacted by the condition. For example:
    // if (x > 0) {
    //    ....
    // }
    // ... <- x;  // x's value range isn't impacted by 'x > 0'.
    if (bbs[0] != pdom_bb_id) {
      UINT32 right = _vra->New_coderep_expr(canon_cr, bbs[0], FALSE);
      // set flag for checked_cr
      if (checked_cr && checked_cr->Kind() == CK_VAR) {
        if (same_range)
          New_value_range(checked_cr, bbs[0], right, val_vars);
        else if (bbs[1] != pdom_bb_id)
          New_value_range(checked_cr, bbs[1], right, val_vars);
      }
      // check unsigned cvt
      if (unsign_cvt == TRUE &&
          (canon_cr->Opr() == OPR_LT || canon_cr->Opr() == OPR_LE)) {
        CODEREP *gt0_cr = _vra->New_cmp_cr(OPR_GE, cr,
                                           _vra->New_cr(MTYPE_I8, 0));
        UINT32 gt0_expr = _vra->New_coderep_expr(gt0_cr, bbs[0], FALSE);
        right = _vra->New_conj_expr(gt0_expr, right, bbs[0]);
      }
      // make a conjunction over previous value range and the condition
      UINT32 expr = _vra->New_conj_expr(left, right, bbs[0]);
      New_value_range(cr, bbs[0], expr, val_vars);
      Mark_reachable(expr, bbs[0]);
    }

    if (bbs[1] != pdom_bb_id) {
      CODEREP* cpl_cr = cra.Complement_cr(canon_cr);
      UINT32 right = _vra->New_coderep_expr(cpl_cr, bbs[1], FALSE);
      // set flag for checked_cr
      if (checked_cr && checked_cr->Kind() == CK_VAR) {
        if (same_range)
          New_value_range(checked_cr, bbs[1], right, val_vars);
        else if (bbs[0] != pdom_bb_id)
          New_value_range(checked_cr, bbs[0], right, val_vars);
      }
      // check unsigned cvt
      if (unsign_cvt == TRUE &&
          (canon_cr->Opr() == OPR_GT || canon_cr->Opr() == OPR_GE)) {
        CODEREP *gt0_cr = _vra->New_cmp_cr(OPR_GE, cr,
                                           _vra->New_cr(MTYPE_I8, 0));
        UINT32 gt0_expr = _vra->New_coderep_expr(gt0_cr, bbs[1], FALSE);
        right = _vra->New_conj_expr(gt0_expr, right, bbs[1]);
      }
      // make a conjunction over previous value range and the complement condition
      UINT32 expr = _vra->New_conj_expr(left, right, bbs[1]);
      New_value_range(cr, bbs[1], expr, val_vars);
      Mark_reachable(expr, bbs[1]);
    }
  }
}

// set COMPGOTO's successors' value range
void
VRA_BUILDER::Process_compgoto(BB_NODE *bb, STMTREP* compgoto, hash_set<CODEREP*>& vars)
{
  BB_SWITCH* switch_info = bb->Switchinfo();
  Is_True(switch_info != NULL, ("BB Switchinfo is NULL"));
  CODEREP* rhs = compgoto->Rhs();
  Is_True(rhs != NULL, ("COMPGOTO rhs is NULL"));
  CODEREP* var;
  INT64 adjust;
  if (rhs->Kind() == CK_OP && rhs->Opr() == OPR_CVT)
    rhs = rhs->Opnd(0);
  if (rhs->Kind() == CK_OP) {
    if (rhs->Opr() == OPR_SUB &&
        rhs->Opnd(1)->Kind() == CK_CONST) {
      // not handle SUB now. this can be enhanced later
      Is_True(FALSE, ("TODO: handle SUB constant"));
      return;
    }
    if (rhs->Opr() != OPR_ADD ||
        rhs->Opnd(1)->Kind() != CK_CONST)
      return; // not able to handle this case

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
    if (var->Kind() != CK_VAR) {
      Is_True(var->Kind() == CK_IVAR || var->Kind() == CK_OP,
              ("Unexpected COMPGOTO OP rhs opnd0 kind %d.", var->Kind()));
      return;  // not able to handle this case. TODO: IVAR value range
    }
  }
  else if (rhs->Kind() == CK_VAR) {
    adjust = 0;
    var = rhs;
  }
  else {
    Is_True(rhs->Kind() == CK_IVAR, ("RHS Kind %d not CK_IVAR", rhs->Kind()));
    return;  // not able to handle this case. TODO: IVAR value range
  }

  BB_NODE* def_targ = bb->Switchdefault();
  // var > ub
  CODEREP* def_gt = _vra->New_cmp_cr(OPR_GT, var, (INT64)bb->Switchentries() - 1 - adjust);
  UINT32 def_expr = _vra->New_coderep_expr(def_gt, def_targ->Id(), FALSE);
  if (MTYPE_is_unsigned(var->Dtyp()) && adjust < 0) {
    // var >= 0
    CODEREP* ge0 = _vra->New_cmp_cr(OPR_GE, var, (INT64)0);
    UINT32 ge0_expr = _vra->New_coderep_expr(ge0, def_targ->Id(), FALSE);
    // var < lb
    CODEREP* lta = _vra->New_cmp_cr(OPR_LT, var, -adjust);
    UINT32 lta_expr = _vra->New_coderep_expr(lta, def_targ->Id(), FALSE);
    // (var >= 0 && var < lb)
    UINT32 lb_expr = _vra->New_conj_expr(ge0_expr, lta_expr, def_targ->Id());
    // (var >= 0 && var < lb) || (var > ub)
    def_expr = _vra->New_disj_expr(lb_expr, def_expr, def_targ->Id());
  }
  else if (MTYPE_is_signed(var->Dtyp())) {
    // var < lb
    CODEREP* lta = _vra->New_cmp_cr(OPR_LT, var, -adjust);
    UINT32 lta_expr = _vra->New_coderep_expr(lta, def_targ->Id(), FALSE);
    // (var < lb) || (var > ub)
    def_expr = _vra->New_disj_expr(def_expr, lta_expr, def_targ->Id());
  }

  // traverse all switch entries and add them to targets
  // key in targets is the BB and value is the "value" in "case" statement
  // multiple "case" values may jump to the same target BB
  std::multimap<BB_NODE*, INT32> targets;
  for (INT32 i = 0; i < bb->Switchentries(); ++i) {
    BB_NODE* targ = bb->Switchcase(i);
    targets.insert(std::pair<BB_NODE*, INT32>(targ, i)); 
  }

  // the condition only contains var
  vars.insert(var);

  // traverse the multimap to generate value range for each target
  std::multimap<BB_NODE*, INT32>::iterator it = targets.begin();
  BB_NODE* prev_bb = NULL;
  INT32 prev_expr = -1;
  for (; it != targets.end(); ++it) {
    BB_NODE* targ = it->first;
    INT32 val = it->second;
    CODEREP* eq = _vra->New_cmp_cr(OPR_EQ, var, (INT64)val - adjust);
    if (targ == def_targ) {
      Is_True(def_expr != -1, ("No expr for default target"));
      // generate expr for eq
      UINT32 eq_expr = _vra->New_coderep_expr(eq, def_targ->Id(), FALSE);
      // generate disj over previous value range for default target
      def_expr = _vra->New_disj_expr(def_expr, eq_expr, def_targ->Id());
      continue;
    }

    if (prev_bb == NULL) {
      prev_bb = targ;
    }
    else if (prev_bb != targ) {
      Is_True(prev_expr != -1, ("No prev expr for prev BB"));
      // jump to a new target
      New_value_range(var, prev_bb->Id(), prev_expr, vars);
      prev_expr = -1;
      prev_bb = targ;
    }

    // generate expr for eq
    UINT32 eq_expr = _vra->New_coderep_expr(eq, targ->Id(), FALSE);
    if (prev_expr == -1) {
      // this is the first entry in the map
      prev_expr = eq_expr;
    }
    else {
      // not the first entry, make a disjunction
      prev_expr = _vra->New_disj_expr(prev_expr, eq_expr, targ->Id());
    }
  }

  if (prev_expr != -1) {
    Is_True(prev_bb != NULL, ("Prev BB is NULL"));
    // generate value range for the last target
    New_value_range(var, prev_bb->Id(), prev_expr, vars);
  }

  Is_True(def_expr != -1, ("No expr for default target"));
  // generate value range for default target
  New_value_range(var, def_targ->Id(), def_expr, vars);
}

// propagate predecessor's value range to current bb and push/pop stack
void
VRA_BUILDER::Process_pred(BB_NODE *bb, BOOL fwd)
{
  Is_True(bb->Pred() != NULL,
          ("bb must have multiple pred"));
  // process different value range from pred for variables without phi
  // for case 1 below:
  //   bb1 (x < a)
  //   | \
  //   |  bb2 (x > b)
  //   |  / \
  //   | /   \
  //   bb3   bb4
  // case 2:
  //   bb1 (x < a)
  //   | \
  //   |  bb2 (y < b)
  //   |  / \
  //   | /   \
  //   bb3   bb4
  // there is no phi node on bb3 for x. for case 1, x should have a disjunction of
  // its predecessor's value range. for case 2, x should have the same value range
  // before the first cond
  BB_NODE *pred = NULL;
  CODEREP* iv = _iv_stack.empty() ? NULL : _iv_stack.top();
  BB_LIST_ITER bb_iter;
  hash_set<CODEREP*> cr_set;
  vector< hash_set<CODEREP*> > bb_cr_set;
  bb_cr_set.reserve(8);
  BOOL unreachable = TRUE;

  FOR_ALL_ELEM(pred, bb_iter, Init(bb->Pred())) {
    // even for not visited pred, we still need to check their vars and push
    // a version to stack. otherwise in bottom-up phase, the vars in stack
    // will be mismatch
    bb_cr_set.push_back(hash_set<CODEREP*>());

    if (pred->Unreachable() && unreachable)
      unreachable = FALSE;
    //if (_bb_visited[pred->Id()] == false)
    //  continue;

    STMTREP *stmt = pred->Last_stmtrep();
    // no stmt
    if (stmt == NULL)
      continue;
    // not TRUEBR or FALSEBR
    OPERATOR opr = stmt->Opr();
    if (opr != OPR_TRUEBR && opr != OPR_FALSEBR)
      continue;

    // not create value range for IV here
    if (iv && Contains(stmt->Rhs(), iv->Aux_id()))
      continue;

    hash_set<CODEREP*>& vars = bb_cr_set.back();
    CODEREP* cond_cr = stmt->Rhs();
    Is_True(cond_cr != NULL, ("invalid TRUEBR/FALSEBR stmtrep"));
    CODEREP_ANALYZER cra(_vra);
    // collect all scalars used in condition expression
    BOOL ret = cra.Analyze_cond_vars(stmt->Rhs(), vars);
    if (ret == FALSE) {
      vars.clear();
      continue;
    }

    cr_set.insert(vars.begin(), vars.end());
  }

  if (pred != NULL && unreachable) {
    bb->Set_unreachable();
  }

  if (cr_set.empty())
    return;

  // traverse the set to generate value range for each variable and push/pop stack
  hash_set<CODEREP*>::iterator sit;
  for (sit = cr_set.begin(); sit != cr_set.end(); ++sit) {
    CODEREP* cr = (CODEREP*)*sit;
    if (!VSA_Vra_Ivar && cr->Kind() == CK_IVAR)
      continue;
    AUX_ID st = cr->Kind() == CK_IVAR ? ILLEGAL_AUX_ID : cr->Aux_id();
    if (st != ILLEGAL_AUX_ID && cr->Is_var_volatile())
      continue;
    if (st != ILLEGAL_AUX_ID && Var_in_phi_list(bb, st))
      continue;  // handled by Process_phi
    if (fwd) {
      UINT32 expr = _vra->Value_range(cr, bb->Id());
      UINT32 old_expr = expr;
      // traverse all preds to update the value range in this bb
      INT pred_idx = 0;
      FOR_ALL_ELEM(pred, bb_iter, Init(bb->Pred())) {
        hash_set<CODEREP*>& vars = bb_cr_set[pred_idx];
        ++pred_idx;
        if (vars.find(cr) != vars.end())  // this is taken care by Process_Succ
          continue;
        if (pred->Unreachable())
          continue;
        UINT32 var_expr = _vra->Value_range(cr, pred->Id());
        if (var_expr != VRA::VR_NOT_FOUND) {
          // generate a disjunction
          if (expr == VRA::VR_NOT_FOUND || expr == var_expr)
            expr = var_expr;
          else
            expr = _vra->Disjunction_in_place(expr, var_expr, bb->Id());
        }
      }
      if (old_expr == VRA::VR_NOT_FOUND) {
        if (expr == VRA::VR_NOT_FOUND)
          expr = _vra->New_coderep_expr(cr, bb->Id(), TRUE);  // create a placeholder
        _vra->New_value_range(cr, bb->Id(), expr);
      }
      Is_True(expr != VRA::VR_NOT_FOUND,
              ("Not find expr for sym%d at bb%d", st, bb->Id()));
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-BLD: BB%d Process_pred push sym%d cr%d:expr%d to stack\n",
                       bb->Id(), st, cr->Coderep_id(), expr));
      if (st != ILLEGAL_AUX_ID)
        _stack[st].push(expr);
    }
    else if (st != ILLEGAL_AUX_ID) {
      UINT32 expr = _vra->Value_range(cr, bb->Id());
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-BLD: BB%d Process_pred pop sym%d cr%d:expr%d from stack\n",
                       bb->Id(), st, cr->Coderep_id(), expr));
      UINT32 etop = _stack[st].top();
      // allow the expr id mismatch but the new expr must be a disjunction
      Is_True(expr == etop ||
              _vra->Expr(expr).Opr() == OPR_BOTTOM ||
              _vra->Expr(expr).Opr() == OPR_CONJUNCTION ||
              _vra->Expr(expr).Opr() == OPR_DISJUNCTION ||
              _vra->Expr(expr).Opr() == OPR_PLACEHOLDER ||
              _vra->Expr(etop).Opr() == OPR_PLACEHOLDER,
              ("stack for sym%d:cr%d:expr%d:top%d mismatch in BB%d",
               st, cr->Coderep_id(), expr, _stack[st].top(), bb->Id()));
      _stack[st].pop();
    }
  }
}

// traverse phi to generate value range and push/pop stack
void
VRA_BUILDER::Process_phi(BB_NODE *bb, BOOL fwd)
{
  Is_True(bb->Pred() != NULL && bb->Phi_list() != NULL,
          ("Should be multiple pred bbs"));

  // get IV and trip count from loop info
  CODEREP* iv = NULL;
  CODEREP* trip_count = NULL;
  BB_LOOP* loop_info = bb->Loop();
  INT32   loop_pre_index = -1;
  if (loop_info != NULL && loop_info->Iv() != NULL) {
    iv = loop_info->Iv();
    STMTREP* sr = loop_info->Trip_count_stmt();
    trip_count = sr ? sr->Rhs() : loop_info->Trip_count_expr();
    if (trip_count != NULL) {
      if (fwd) {
        _iv_stack.push(iv);  // push iv to stack
      }
      else {
        Is_True(iv == _iv_stack.top(), ("iv stack mismatch"));
        _iv_stack.pop();     // pop iv from stak
      }
    }
    else {
      // reset iv because there is no trip count
      iv = NULL;
    }
  }

  if (loop_info != NULL && iv == NULL && fwd) {
    // add to _to_be_resolved for loop without iv
    Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
             (TFile, "VRA-BLD: BB%d has loop info but no iv\n", bb->Id()));
    _to_be_resolved.push_back(bb);
  }

  Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               _vra->Print_bb_loop(bb, TFile));

  // collect predecessor's BB id into a vector
  std::vector<INT32> pred_vec;
  BB_NODE* pred;
  BB_LIST_ITER pred_iter;
  FOR_ALL_ELEM(pred, pred_iter, Init(bb->Pred())) {
    if (loop_info != NULL && loop_info->Preheader() == pred) {
      Is_True(loop_pre_index == -1, ("found more than 1 preheader for loop"));
      loop_pre_index = pred_vec.size();
    }
    pred_vec.push_back(pred->Id());
  }
  UINT32 pred_count = pred_vec.size();
  CODEREP_ANALYZER cra(_vra);

  // process phi to generate value range
  PHI_NODE* phi;
  PHI_LIST_ITER phi_iter;
  FOR_ALL_NODE(phi, phi_iter, Init(bb->Phi_list())) {
    if (!phi->Live())
      continue;
    CODEREP* phi_res = phi->RESULT();
    if (phi_res->Is_flag_set(CF_IS_ZERO_VERSION))
      continue;
    if (phi_res->Is_var_volatile())
      continue;
    if (MTYPE_is_m(phi_res->Dtyp())) // ignore MTYPE_M
      continue;
    if (phi_res->Aux_id() == _opt_stab->Default_vsym() ||
        phi_res->Aux_id() == _opt_stab->Return_vsym())
      continue;
    if (fwd) {
      UINT32 expr = VRA::VR_NOT_FOUND;
      if (iv != NULL && iv->Aux_id() == phi->Aux_id()) {
        // for loop with iv, use iv's initial value and trip count as
        // its value range in the loop
        Is_True(loop_pre_index == -1 ||
                _bb_visited[pred_vec[loop_pre_index]] == true,
                ("preheader of loop is not visited"));
        CODEREP *opnd = loop_pre_index != -1 ? phi->OPND(loop_pre_index) :
                                               phi->RESULT();
        Is_True(opnd != NULL && opnd->Kind() == CK_VAR,
                ("invalid phi opnd(%d)", loop_pre_index));
        Is_True(opnd->Is_flag_set(CF_DEF_BY_CHI) ||
                opnd->Is_flag_set(CF_DEF_BY_PHI) ||
                opnd->Defstmt() != NULL,
                ("iv in preheader does not have a defstmt"));
        BOOL increasing = TRUE;
        CODEREP *step_op = Find_step_op(loop_info->Step(), iv->Aux_id());
        if (step_op != NULL && step_op->Kind() == CK_OP &&
            (step_op->Opr() == OPR_ADD || step_op->Opr() == OPR_SUB)) {
          CODEREP *step_cr = step_op->Opnd(1);
          if (step_cr->Kind() == CK_VAR && step_cr->Aux_id() == iv->Aux_id())
            step_cr = step_op->Opnd(0);
          increasing = step_op->Opr() == OPR_ADD &&
                          (step_cr->Kind() != CK_CONST || step_cr->Const_val() > 0);
        }

        // use initial value before loop as lower bound
        CODEREP *lb = (opnd->Is_flag_set(CF_DEF_BY_CHI) ||
                       opnd->Is_flag_set(CF_DEF_BY_PHI)) ? opnd
                                                         : opnd->Defstmt()->Rhs();
        Is_True(lb != NULL, ("does not find iv lb expr"));
        // use lb + trip count as upper bound
        CODEREP *ub = _vra->New_cr(OPCODE_make_op(increasing ? OPR_ADD : OPR_SUB,
                                                  lb->Dtyp(), MTYPE_V),
                                   lb, trip_count, TRUE);
        // generate the value range which is (iv >= lb && iv < ub) if increasing
        // (iv <= lb && iv > ub) if not increasing
        CODEREP *gle = _vra->New_cmp_cr(increasing ? OPR_GE : OPR_LE, phi_res, lb);
        CODEREP *lgt = _vra->New_cmp_cr(increasing ? OPR_LT : OPR_GT, phi_res, ub);
        UINT32 left = _vra->New_coderep_expr(gle, bb->Id(), FALSE);
        UINT32 right = _vra->New_coderep_expr(lgt, bb->Id(), FALSE);
        expr = _vra->New_conj_expr(left, right, bb->Id());
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG), 
                 (TFile, "VRA-BLD: BB%d create conj expr %d = (%d, %d) for iv cr%d sym%dv%d %s\n",
                         bb->Id(), expr, left, right,
                         iv->Coderep_id(), iv->Aux_id(), iv->Version(), _vra->Var_name(iv)));
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "         left: "));
        Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                     _vra->Print_coderep(gle, TFile));
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "\n         right: "));
        Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                     _vra->Print_coderep(lgt, TFile));
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "\n"));
      }
      else {
        for (int i = 0; i < pred_count; ++i) {
          CODEREP *opnd = phi->OPND(i);
          Is_True(opnd != NULL, ("Invalid phi opnd(%d)", i));
          UINT32 opnd_expr = _vra->Value_range(opnd, bb->Id());
          if (opnd_expr == VRA::VR_NOT_FOUND) {
            if (opnd == phi_res) {
              continue;
            }
            if (_cfg->Get_bb(pred_vec[i])->Unreachable()) {
              continue;
            }
            opnd_expr = _vra->Value_range_rec(opnd, pred_vec[i]);
            if (opnd_expr == VRA::VR_NOT_FOUND) {
              // generate a placeholder if no 
              Is_True(_bb_visited[pred_vec[i]] == false ||
                      pred_vec[i] == bb->Id() ||
                      opnd->Is_flag_set(CF_DEF_BY_PHI),
                      ("no value range available for bb"));
              opnd_expr = _vra->New_coderep_expr(opnd, bb->Id(), TRUE);
              _vra->New_value_range(opnd, bb->Id(), opnd_expr);
            }
            Is_True(opnd_expr != VRA::VR_NOT_FOUND,
                    ("Not able to finx va expr for opnd(%d, BB%d) in BB%d",
                     i, pred_vec[i], bb->Id()));
          }
          expr = (expr == VRA::VR_NOT_FOUND || expr == opnd_expr) ?
                    opnd_expr : _vra->New_disj_expr(expr, opnd_expr, bb->Id());
        }
      }
      // generate value range and push to stack
      expr = _vra->New_value_range(phi_res, bb->Id(), expr);
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-BLD: BB%d Process_phi push sym%d cr%d:expr%d to stack\n",
                       bb->Id(), phi->Aux_id(), phi_res->Coderep_id(), expr));
      _stack[phi->Aux_id()].push(expr);
    }
    else {
#ifdef Is_True_On
      UINT32 expr = _vra->Value_range(phi_res, bb->Id());
      // allow the expr id mismatch but the new expr must be a disjunction
      Is_True(expr == _stack[phi->Aux_id()].top() ||
              _vra->Expr(expr).Opr() == OPR_DISJUNCTION,
              ("stack for sym%d:cr%d:expr%d:top%d mismatch in BB%d",
               phi->Aux_id(), phi_res->Coderep_id(), expr,
               _stack[phi->Aux_id()].top(), bb->Id()));
#endif
      // pop stack
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-BLD: BB%d Process_phi pop sym%d cr%d:expr%d from stack\n",
                       bb->Id(), phi->Aux_id(),
                       phi_res->Coderep_id(), _stack[phi->Aux_id()].top()));
      _stack[phi->Aux_id()].pop();
    }
  }
}

// update phi's operand's value range if the phi node is visited before its predecessor
void
VRA_BUILDER::Update_phi(BB_NODE *bb, BB_NODE* succ, const hash_set<CODEREP*>& vars)
{
  Is_True(succ->Pred() != NULL && succ->Pred()->Multiple_bbs(),
          ("succ should be multiple pred bbs"));
  INT32 pos = succ->Pred()->Pos(bb);
  Is_True(pos != -1,
          ("not find bb in succ pred list"));

  CODEREP* iv = _iv_stack.empty() ? NULL : _iv_stack.top();
  PHI_NODE* phi;
  PHI_LIST_ITER phi_iter;
  FOR_ALL_NODE(phi, phi_iter, Init(succ->Phi_list())) {
    if (!phi->Live())
      continue;
    if (phi->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION))
      continue;
    if (phi->RESULT()->Aux_id() == _opt_stab->Default_vsym() ||
        phi->RESULT()->Aux_id() == _opt_stab->Return_vsym())
      continue;
    // TODO: check this again. skip iv's value range
    if (iv != NULL && iv->Aux_id() == phi->Aux_id())
      continue;
    CODEREP* opnd = phi->OPND(pos);
    Is_True(opnd != NULL, ("phi opnd %d null", pos));
    if (opnd->Is_flag_set(CF_IS_ZERO_VERSION) || opnd == phi->RESULT())
      continue;
    if (MTYPE_is_m(opnd->Dtyp())) // ignore MTYPE_M
      continue;
    if (vars.find(opnd) != vars.end())
      continue;  // already handled in Process_succ

    UINT32 id = _vra->Value_range(opnd, succ->Id());
    VRA_EXPR& expr = _vra->Expr(id);
    if (id == VRA::VR_NOT_FOUND) {
      // happens for IV's phi operand
      id = _vra->Value_range_rec(opnd, bb->Id());
      Is_True(id != VRA::VR_NOT_FOUND,
              ("can not find expr for cr%d at bb%d",
               opnd->Coderep_id(), bb->Id()));
      _vra->New_value_range(opnd, succ->Id(), id);
      continue;
    }
    if (expr.Opr() == OPR_BOTTOM) {
      // already simplified to BOTTOM, give up
      continue;
    }
    else if (expr.Opr() == OPR_PLACEHOLDER) {
      Is_True(expr.Cr() == opnd, ("invalid placeholder"));
      // pseudo expr generated when phi is visited, overwritten it
      UINT32 new_id = _vra->Value_range_rec(opnd, bb->Id());
      Is_True(new_id != VRA::VR_NOT_FOUND,
              ("can not find expr for cr%d at bb%d",
               opnd->Coderep_id(), bb->Id()));
      _vra->Copy_in_place(id, new_id, bb->Id());
    }
    else {
      // make a disjunction over existing range
      // res = phi(v0, v1, v1)
      // v1 may have two different range in this bb
      UINT32 pred_id = _vra->Value_range_rec(opnd, bb->Id());
      Is_True(pred_id != VRA::VR_NOT_FOUND,
              ("can not find expr for cr%d at bb%d",
               opnd->Coderep_id(), bb->Id()));
      //UINT32 disj = _vra->New_disj_expr(id, pred_id, succ->Id());
      //_vra->New_value_range(opnd, succ->Id(), disj);
      _vra->New_value_range(opnd, succ->Id(), pred_id);
    }
  }
}

// forward processing stmts in BB
void
VRA_BUILDER::Process_stmt_fwd(BB_NODE *bb)
{
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    // process stmt and push to stack
    Process_stmt(stmt, bb, TRUE);
    // process chi and push to stack
    Process_chi(stmt, bb, TRUE);
  }
}

// backward processing stmts in BB
void
VRA_BUILDER::Process_stmt_rev(BB_NODE* bb)
{
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
    // process chi and pop from stack
    Process_chi(stmt, bb, FALSE);
    // process stmt and pop from stack
    Process_stmt(stmt, bb, FALSE);
  }
}

// process single stmt
void
VRA_BUILDER::Process_stmt(STMTREP *sr, BB_NODE *bb, BOOL fwd)
{
  UINT32 expr = VRA::VR_NOT_FOUND;
  CODEREP *lhs = sr->Lhs();
  if (lhs != NULL && lhs->Kind() == CK_VAR) {
    CODEREP* iv = _iv_stack.empty() ? NULL : _iv_stack.top();
    if (fwd) {
      CODEREP* rhs = sr->Rhs();
      if (rhs != NULL) {
        if (iv != NULL && iv->Aux_id() == lhs->Aux_id() &&
            rhs->Contains(iv)) {
          // assign to iv, assume the same range as [lb, lb + trip count)
          expr = _vra->Value_range_rec(iv, bb->Id());
          if (lhs != iv &&
              rhs->Kind() == CK_OP &&
              (rhs->Opr() == OPR_ADD || rhs->Opr() == OPR_SUB)) {
            CODEREP* opnd = rhs->Opnd(1);
            if (opnd->Contains(iv))
              opnd = rhs->Opnd(0);
            expr = New_value_expr(expr, bb->Id(), rhs->Opr(), lhs, opnd);
          }
        }
        else {
          if (rhs->Kind() == CK_VAR) {
            AUX_STAB_ENTRY* sym = _opt_stab->Aux_stab_entry(rhs->Aux_id());
            if (sym->Is_dedicated_preg() &&
                Is_Return_Preg(sym->St_ofst())) {
              // handle call's return value
              STMTREP *call = sr->Prev();
#ifdef TARG_X8664
              Is_True(call != NULL || Is_Formal_Preg(sym->St_ofst()) ||
                      sym->St_ofst() == RAX,
                      ("TODO: search the call stmt?"));
#endif
              if (call != NULL &&
                  (call->Opr() == OPR_CALL || call->Opr() == OPR_INTRINSIC_CALL)) {
                expr = Process_retv_from_call(call, lhs, bb);
              }
            }
            else {
              // prop flags for better path info
              if (rhs->Value_invalid_addr())
                lhs->Set_value_invalid_addr();
              if (rhs->Value_maydangling())
                lhs->Set_value_maydangling();
            }
          } else if(rhs->Kind() == CK_IVAR) {
            if(PU_java_lang(Get_Current_PU())) {
              CODEREP *len_cr = Find_array_len(rhs);
              if(len_cr != NULL) {
                CODEREP* eq = _vra->New_cr(OPCODE_make_op(OPR_EQ, MTYPE_I4, rhs->Dtyp()),
                                           rhs, len_cr);
                expr = _vra->New_coderep_expr(eq, bb->Id(), FALSE);
              }
            }
          }
          if (expr == VRA::VR_NOT_FOUND) {
            expr = _vra->New_coderep_expr(rhs, bb->Id(), FALSE);
          }
        }
        Is_True(expr != VRA::VR_NOT_FOUND, ("Invalid range expr"));
      }
      // generate value range and push to stack
      expr = _vra->New_value_range(lhs, bb->Id(), expr);
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-BLD: BB%d Process_stmt push sym%d cr%d:expr%d to stack\n",
                       bb->Id(), lhs->Aux_id(), lhs->Coderep_id(), expr));
      _stack[lhs->Aux_id()].push(expr);
    }
    else {
#ifdef Is_True_On
      UINT32 id = _stack[lhs->Aux_id()].top();
      if (id != VRA::VR_BOTTOM) {
        if (iv != NULL && iv->Aux_id() == lhs->Aux_id() &&
            sr->Rhs()->Contains(iv)) {
          Is_True(iv != lhs || id == _vra->Value_range_rec(iv, bb->Id()),
                  ("expr id mismatch for iv"));
        }
        else {
          CODEREP* rhs = sr->Rhs();
          BOOL is_call_ret = FALSE;
          if (rhs != NULL && rhs->Kind() == CK_VAR) {
            AUX_STAB_ENTRY* sym = _opt_stab->Aux_stab_entry(rhs->Aux_id());
            if (sym->Is_dedicated_preg() &&
                Is_Return_Preg(sym->St_ofst())) {
              // process call return value
              STMTREP *call = sr->Prev();
#ifdef TARG_X8664
              Is_True(call != NULL || Is_Formal_Preg(sym->St_ofst()) ||
                      sym->St_ofst() == RAX,
                      ("TODO: search the call stmt?"));
#endif
              if (call != NULL &&
                  (call->Opr() == OPR_CALL || call->Opr() == OPR_INTRINSIC_CALL)) {
                is_call_ret = TRUE;
              }
            }
          }
          if (rhs != NULL && !is_call_ret) {
            const VRA_EXPR& expr = _vra->Expr(id);
            Is_True(expr.Bb_id() == bb->Id(), ("expr bb id mismatch"));
            // make sure expr matches cr, or converted into disjunction
            Is_True(expr.Opr() == OPR_DISJUNCTION ||
                    (expr.Opr() == OPR_CODEREP &&
                     expr.Cr()->Contains(rhs)), ("expr opr mismatch"));
          }
          // TODO: verify call return value
        }
      }
#endif
      // pop from stack
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-BLD: BB%d Process_stmt pop sym%d cr%d:expr%d from stack\n",
                       bb->Id(), lhs->Aux_id(),
                       lhs->Coderep_id(), _stack[lhs->Aux_id()].top()));
      _stack[lhs->Aux_id()].pop();
    }
  }
  else if (sr->Opr() == OPR_CALL) {
    Process_range_call(sr, bb, fwd);
  }
}

// process stmt's chi list
void
VRA_BUILDER::Process_chi(STMTREP *sr, BB_NODE *bb, BOOL fwd)
{
  DNA_NODE* dna = _comp_unit->Dna();
  Is_True(dna != NULL, ("dna is null"));
  BOOL is_entry = (dna && sr->Opr() == OPR_OPT_CHI && bb->Kind() == BB_ENTRY);

  // processing CHI_LIST
  if (sr->Chi_list() != NULL) {
    CHI_NODE *chi;
    CHI_LIST_ITER chi_iter;
    // for OPT_CHI, set expr to TOP, otherwise to BOTTOM
    UINT32 top_expr = _vra->New_special_expr(OPR_TOP, bb->Id());
    UINT32 btm_expr = _vra->New_special_expr(OPR_BOTTOM, bb->Id());
    FOR_ALL_NODE(chi, chi_iter, Init(sr->Chi_list())) {
      if (!chi->Live())
        continue;

      CODEREP* cr = chi->RESULT();
      if (cr->Is_flag_set(CF_IS_ZERO_VERSION)) //TODO: return or create a BOTTOM expr???
        continue;                              // phase ordering??? before or after dna/rns???
      if (cr->Is_var_volatile())
        continue;
      if (MTYPE_is_m(cr->Dtyp())) // ignore MTYPE_M
        continue;

      if (cr->Aux_id() == _opt_stab->Default_vsym() ||
          cr->Aux_id() == _opt_stab->Return_vsym())
        continue;

      if (fwd) {
        Is_True(sr->Opr() != OPR_OPT_CHI || _stack[cr->Aux_id()].empty(),
                ("stack is not empty for OPT_CHI"));
        IDTYPE parm;
        UINT32 vr_expr = VRA::VR_NOT_FOUND;;
        if (is_entry &&
            _opt_stab->Aux_stab_entry(cr->Aux_id())->Is_real_var() &&
            (parm = dna->Is_param(cr)) != INVALID_VAR_IDX) {
          hash_set<int64_t> vals;
          INT i;
          BOOL value_zero = FALSE;
          for(i = VAR_INIT_ID; i < dna->Clby_list()->size(); i++) {
            RNA_NODE *rna = (*dna->Clby_list())[i];
            Is_True(rna != NULL, ("rna is null"));
            CODEREP* cr = rna->Get_arg(parm);
            if (cr != NULL && cr->Kind() == CK_CONST) {
              vals.insert(cr->Const_val());
              if (value_zero == FALSE && cr->Const_val() == 0)
                value_zero = TRUE;
            }
            else
              break;
          }
          if (i < dna->Clby_list()->size() ||
              vals.size() == 0 || vals.size() > 3) {
            vr_expr = top_expr;
          }
          else {
            if (value_zero && vals.size() == 1) {
              cr->Set_value_invalid_addr();
              cr->Set_value_maydangling();
            }
            for (hash_set<int64_t>::iterator vi = vals.begin();
                 vi != vals.end(); ++vi) {
              CODEREP* eq_cr = _vra->New_cmp_cr(OPR_EQ, cr, *vi);
              UINT32   eq_expr = _vra->New_coderep_expr(eq_cr, bb->Id(), FALSE);
              if (vr_expr == VRA::VR_NOT_FOUND)
                vr_expr = eq_expr;
              else
                vr_expr = _vra->New_disj_expr(vr_expr, eq_expr, bb->Id());
            }
          }
        }
        else {
          vr_expr = btm_expr;
        }
        UINT32 vr = _vra->New_value_range(cr, bb->Id(), vr_expr);
        // push to stack
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-BLD: BB%d Process_chi push sym%d cr%d:expr%d to stack\n",
                         bb->Id(), cr->Aux_id(), cr->Coderep_id(), vr));
        _stack[cr->Aux_id()].push(vr);
      }
      else {
        UINT32 vr = _vra->Value_range(cr, bb->Id());
        Is_True(!_stack[cr->Aux_id()].empty(), ("stack is empty"));
        Is_True(_stack[cr->Aux_id()].top() == vr,
                ("stack top mismatch"));
        // pop from stack
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-BLD: BB%d Process_chi pop sym%d cr%d:expr%d from stack\n",
                         bb->Id(), cr->Aux_id(), cr->Coderep_id(), vr));
        _stack[cr->Aux_id()].pop();
        Is_True(sr->Opr() != OPR_OPT_CHI || _stack[cr->Aux_id()].empty(),
                ("stack is not empty for OPT_CHI"));
      }
    }
  }
}

// generate value range for xvsa range/compare call
void
VRA_BUILDER::Process_range_call(STMTREP *sr, BB_NODE *bb, BOOL fwd)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_INTRINSIC_CALL,
          ("Not call stmt"));

  ST *st = sr->St();
  // handle __builtin_xvsa_range
  if (strcmp(ST_name(st), BUILTIN_XVSA_RANGE) == 0) {
    Is_True(sr->Rhs()->Kid_count() == 3, ("__builtin_xvsa_range not 3 param"));
    CODEREP *var = sr->Rhs()->Opnd(0)->Ilod_base();
    if (var == NULL ||
        (var->Kind() != CK_VAR && var->Kind() != CK_IVAR))
      return;
    CODEREP *lb = sr->Rhs()->Opnd(1)->Ilod_base();
    CODEREP *ub = sr->Rhs()->Opnd(2)->Ilod_base();
    if (var != NULL && lb != NULL && ub != NULL) {
      if (fwd) {
        CODEREP* ge = _vra->New_cr(OPCODE_make_op(OPR_GE, MTYPE_I4, var->Dtyp()),
                                   var, lb);
        CODEREP* lt = _vra->New_cr(OPCODE_make_op(OPR_LT, MTYPE_I4, var->Dtyp()),
                                   var, ub);
        // generate a conj
        UINT32 lbe = _vra->New_coderep_expr(ge, bb->Id(), FALSE);
        UINT32 ube = _vra->New_coderep_expr(lt, bb->Id(), FALSE);
        UINT32 expr = _vra->New_conj_expr(lbe, ube, bb->Id());
        hash_set<CODEREP*> val_vars;
        New_value_range(var, bb->Id(), expr, val_vars, TRUE);
        // push to stack
        if (var->Kind() == CK_VAR) {
          Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                   (TFile, "VRA-BLD: BB%d Process_chi push sym%d cr%d:expr%d to stack\n",
                           bb->Id(), var->Aux_id(), var->Coderep_id(), expr));
          _stack[var->Aux_id()].push(expr);
        }
      }
      else {
        if (var->Kind() == CK_VAR) {
          UINT32 expr = _vra->Value_range(var, bb->Id());
          Is_True(!_stack[var->Aux_id()].empty(), ("stack is empty"));
          Is_True(_stack[var->Aux_id()].top() == expr,
                  ("stack top mismatch"));
          // pop from stack
          Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                   (TFile, "VRA-BLD: BB%d Process_chi pop sym%d cr%d:expr%d from stack\n",
                           bb->Id(), var->Aux_id(), var->Coderep_id(), expr));
          _stack[var->Aux_id()].pop();
        }
      }
    }
    return;
  }

  OPERATOR opr = OPERATOR_UNKNOWN;
  CODEREP *var = NULL;
  CODEREP *val = NULL;
  //__builtin_xvsa_ne
  if (strcmp(ST_name(st), BUILTIN_XVSA_NE) == 0) {
    Is_True(sr->Rhs()->Kid_count() == 2, ("__builtin_xvsa_ne not 2 param"));
    var = sr->Rhs()->Opnd(0)->Ilod_base();
    val = sr->Rhs()->Opnd(1)->Ilod_base();
    opr = OPR_NE;
  }
  //__builtin_xvsa_compare
  else if (strcmp(ST_name(st), BUILTIN_XVSA_COMPARE) == 0) {
    Is_True(sr->Rhs()->Kid_count() == 3, ("__builtin_xvsa_compare not 3 param"));
    var = sr->Rhs()->Opnd(0)->Ilod_base();
    val = sr->Rhs()->Opnd(2)->Ilod_base();
    // operator
    CODEREP *cmp = sr->Rhs()->Opnd(1)->Ilod_base();
    if (cmp && cmp->Kind() == CK_LDA) {
      ST *st = cmp->Lda_base_st();
      if (ST_class(st) == CLASS_CONST) {
        const char* str = Index_to_char_array(TCON_str_idx(ST_tcon_val(st)));
        opr = Get_opr_from_char(str);
      }
    }
  }

  if (opr == OPERATOR_UNKNOWN || var == NULL || val == NULL ||
      (var->Kind() != CK_VAR && var->Kind() != CK_IVAR))
    return;

  if (fwd) {
    CODEREP* cmp = _vra->New_cr(OPCODE_make_op(opr, MTYPE_I4, var->Dtyp()),
                               var, val);
    UINT32 expr = _vra->New_coderep_expr(cmp, bb->Id(), FALSE);
    // merge with current value range
    UINT32 cur_vr = var->Kind() == CK_VAR
                       ? _stack[var->Aux_id()].top()
                       : _vra->Value_range_rec(var, bb->Id());
    if (cur_vr != VRA::VR_NOT_FOUND &&
        cur_vr != VRA::VR_BOTTOM &&
        cur_vr != VRA::VR_TOP) {
      const VRA_EXPR& cur_expr = _vra->Expr(cur_vr);
      if (cur_expr.Opr() == OPR_CODEREP ||
          cur_expr.Opr() == OPR_CONJUNCTION ||
          cur_expr.Opr() == OPR_DISJUNCTION)
        expr = _vra->New_conj_expr(expr, cur_vr, bb->Id());
    }

    hash_set<CODEREP*> val_vars;
    New_value_range(var, bb->Id(), expr, val_vars, TRUE);
    if (var->Kind() == CK_VAR) {
      // push to stack
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-BLD: BB%d Process_chi push sym%d cr%d:expr%d to stack\n",
                       bb->Id(), var->Aux_id(), var->Coderep_id(), expr));
      _stack[var->Aux_id()].push(expr);
    }
  }
  else {
    if (var->Kind() == CK_VAR) {
      UINT32 expr = _vra->Value_range(var, bb->Id());
      Is_True(!_stack[var->Aux_id()].empty(), ("stack is empty"));
      Is_True(_stack[var->Aux_id()].top() == expr,
              ("stack top mismatch"));
      // pop from stack
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-BLD: BB%d Process_chi pop sym%d cr%d:expr%d from stack\n",
                       bb->Id(), var->Aux_id(), var->Coderep_id(), expr));
      _stack[var->Aux_id()].pop();
    }
  }
}

// a temporary solution to process calls, should be replaced by API model later
UINT32
VRA_BUILDER::Process_retv_from_call(STMTREP *sr, CODEREP* cr, BB_NODE* bb)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_INTRINSIC_CALL,
          ("Not call stmt"));

  // handle intrinsic call here
  if (sr->Opr() == OPR_INTRINSIC_CALL) {
    if (sr->Rhs()->Intrinsic() == INTRN_ALLOC_OBJ) {
      // Assume INTRN_ALLOC_OBJ never fails according to Java Spec
      CODEREP* zero = _vra->New_cr(MTYPE_I8, 0);
      CODEREP* ne = _vra->New_cr(OPCODE_make_op(OPR_NE, MTYPE_I4, cr->Dtyp()),
                                 cr, zero);
      UINT32 expr = _vra->New_coderep_expr(ne, bb->Id(), FALSE);
      return expr;
    }
    return VRA::VR_NOT_FOUND;
  }

  // handle call
  ST* st = sr->St();
  // handle read/recv/recvfrom
  if (strcmp(ST_name(st), "read") == 0 ||
      strcmp(ST_name(st), "recv") == 0 ||
      strcmp(ST_name(st), "recvfrom") == 0) {
    // just handle "read(2)" so far
    CODEREP* rhs = sr->Rhs();
    Is_True(rhs->Kind() == CK_OP && rhs->Opr() == OPR_CALL,
            ("Not call expr"));
    if (rhs->Kid_count() < 3)
      return VRA::VR_BOTTOM;
    CODEREP_ANALYZER cra(_vra);
    CODEREP* opnd_2 = rhs->Opnd(2);
    Is_True(opnd_2->Kind() == CK_IVAR && opnd_2->Opr() == OPR_PARM, ("Not parm"));
    // read(2) returns -1 or size (>= 0) it reads
    CODEREP* neg_one = _vra->New_cr(MTYPE_I8, -1); //TODO: check sign???
    CODEREP* lb = _vra->New_cr(OPCODE_make_op(OPR_GE, MTYPE_I4, opnd_2->Dtyp()),
                               cr, neg_one);
    // read(2) reads bytes no more than the value in "opnd_2"
    CODEREP* ub = _vra->New_cr(OPCODE_make_op(OPR_LE, MTYPE_I4, opnd_2->Dtyp()),
                               cr, opnd_2->Ilod_base());
    // generate a conj
    UINT32 lbe = _vra->New_coderep_expr(lb, bb->Id(), FALSE);
    UINT32 ube = _vra->New_coderep_expr(ub, bb->Id(), FALSE);
    return _vra->New_conj_expr(lbe, ube, bb->Id());
  }

  if (sr->Callee_returns_new_heap_memory()) {
    // if malloc size <= 0, return cr == 0
    CODEREP *size = Vsa_malloc_size_opnd(sr, cr, _comp_unit);
    if(size != NULL && size->Kind() == CK_CONST &&
       size->Const_val() <= 0) {
      CODEREP* zero = _vra->New_cr(MTYPE_I8, 0);
      CODEREP* eq = _vra->New_cr(OPCODE_make_op(OPR_EQ, MTYPE_I4, cr->Dtyp()),
                                 cr, zero);
      UINT32 expr = _vra->New_coderep_expr(eq, bb->Id(), FALSE);
      return expr;
    }
  }

  DNA_NODE* dna = _comp_unit->Dna();
  if (dna == NULL)
    return VRA::VR_BOTTOM;

  IDTYPE which_arg;
  if (!dna->Non_functional() &&
      _comp_unit->Vsa()->Callee_may_taint_arg(sr, cr, &which_arg))
    return _vra->New_coderep_expr(sr->Rhs(), bb->Id(), FALSE);

  RNA_NODE *rna = dna->Get_callsite_rna(sr);
  if (rna == NULL)
    return VRA::VR_BOTTOM;

  // only process INTCONST so far
  hash_set<int64_t> vals;
  BOOL value_zero = FALSE;
  for (CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
       it != rna->Callee_list().end(); it++) {
    DNA_NODE* callee = _ipsa->Get_dna(it->Callee());
    if (callee->Non_functional())
      continue;
    for (INT i = VAR_INIT_ID; i < callee->Retv_list()->size(); ++ i) {
      PDV_NODE *pdv = (*callee->Retv_list())[i];
      if (pdv->Kind() & BY_RETURNSTMT) {
        STMTREP *stmt = pdv->Stmt();
        if (stmt && stmt->Opr() == OPR_STID && stmt->Rhs()->Kind() == CK_CONST) {
          vals.insert(stmt->Rhs()->Const_val());
          if (value_zero == FALSE && stmt->Rhs()->Const_val() == 0)
            value_zero = TRUE;
        }
        else {
          return VRA::VR_BOTTOM;
        }
      }
    }
  }
  if (vals.size() == 0 || vals.size() > 3)
    return VRA::VR_BOTTOM;

  if (value_zero && vals.size() == 1) {
    cr->Set_value_invalid_addr();
    cr->Set_value_maydangling();
  }

  UINT32 expr = VRA::VR_NOT_FOUND;
  for (hash_set<int64_t>::iterator vi = vals.begin(); vi != vals.end(); ++vi) {
    CODEREP* eq_cr = _vra->New_cmp_cr(OPR_EQ, cr, *vi);
    UINT32   eq_expr = _vra->New_coderep_expr(eq_cr, bb->Id(), FALSE);
    if (expr == VRA::VR_NOT_FOUND)
      expr = eq_expr;
    else
      expr = _vra->New_disj_expr(expr, eq_expr, bb->Id());
  }
  return expr;
}

// initialize the builder
void
VRA_BUILDER::Initialize()
{
  // resize two vectors, all values will be initialized to 0
  _stack.resize(_opt_stab->Lastidx() + 1);
  _bb_visited.resize(_cfg->Total_bb_count());
}

// finalize the builder
void
VRA_BUILDER::Finalize()
{
  // TODO: resolve value range for loop without IV
  //VALRANGE_RESOLVER resolver(_vra, _comp_unit);
  //resolver.Initialize();
  std::vector<BB_NODE*>::reverse_iterator it = _to_be_resolved.rbegin();
  for (; it != _to_be_resolved.rend(); ++it) {
    BB_NODE* bb = *it;
    BB_LOOP* loop_info = bb->Loop();
    BOOL     iv_updated = FALSE;     // is iv's value range updated
    BOOL     iv_ub_included = FALSE; // is iv's ub value included (<=, >=) or excluded (<, >)
    CODEREP *trip_count = NULL;
    UINT32 tail_bb = 0;
    if (loop_info != NULL && loop_info->Tail() != NULL)
      tail_bb = loop_info->Tail()->Id();
    STMTREP* loop_back = NULL;
    if (loop_info != NULL && loop_info->Loopback() != NULL)
      loop_back = loop_info->Loopback()->Last_stmtrep();
    //Analyze_loop_without_iv(bb, loop);
    PHI_NODE     *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_NODE(phi, phi_iter, Init(bb->Phi_list())) {
      if (!phi->Live())
        continue;
      CODEREP* cr = phi->RESULT();
      UINT32 id = _vra->Value_range(cr, bb->Id());
      if (id <= VRA::VR_EMPTY)
        continue;
      VRA_EXPR& expr = _vra->Expr(id);
      if (expr.Opr() != OPR_DISJUNCTION)
        continue;
      const VRA_EXPR* lb = &_vra->Expr(expr.Op0());
      const VRA_EXPR* ub = &_vra->Expr(expr.Op1());
      if (lb->Opr() == OPR_CODEREP && ub->Opr() == OPR_CODEREP &&
          loop_back != NULL &&
          (iv_updated || loop_back->Opr() == OPR_GOTO)) {
        // iv_uppdated:
        // no comparison on this cr but the IV's range is updated, turn
        // the range of cr to [lb, lb + trip_count] or [lb - trip_count,
        // lb] according to ucr's operator
        // OPT_GOTO:
        // loop back is unconditional, turn the value range of cr
        // from lu: x = blah1 ub: x+=blah2 into x >= blah1
        CODEREP* lcr = lb->Cr();
        CODEREP* ucr = ub->Cr();
        if (lcr->Kind() != CK_OP && ucr->Kind() == CK_OP &&
            ucr->Contains(cr) &&
            (ucr->Opr() == OPR_ADD || ucr->Opr() == OPR_SUB)) {
          OPERATOR opr = ucr->Opr() == OPR_ADD ? OPR_GE : OPR_LE;
          if (iv_updated) {
            CODEREP *lb_cmp = _vra->New_cmp_cr(opr, cr, lcr);
            UINT32   new_lb = _vra->New_coderep_expr(lb_cmp, bb->Id(), FALSE);
            CODEREP *new_ucr = _vra->New_cr(OPCODE_make_op(ucr->Opr(), cr->Dtyp(), MTYPE_V),
                                      lcr, trip_count, TRUE);
            OPERATOR ub_opr = opr == OPR_GE
                                ? (iv_ub_included ? OPR_LE : OPR_LT)
                                : (iv_ub_included ? OPR_GE : OPR_GT);
            CODEREP *ub_cmp = _vra->New_cmp_cr(ub_opr, cr, new_ucr);
            UINT32   new_ub = _vra->New_coderep_expr(ub_cmp, bb->Id(), FALSE);
            // rewrite expr at loop header
            expr.Init(OPR_CONJUNCTION, bb->Id(), new_lb, new_ub);
          }
          else {
            CODEREP *cmp = _vra->New_cmp_cr(opr, cr, lcr);
            expr.Init(cmp, bb->Id(), FALSE);
          }
        }
        continue;
      }
      else if (lb->Opr() == OPR_CONJUNCTION && ub->Opr() == OPR_CODEREP) {
        const VRA_EXPR* tmp = lb;
        lb = ub;
        ub = tmp;
      }
      else if (lb->Opr() != OPR_CODEREP || ub->Opr() != OPR_CONJUNCTION)
        continue;
      const VRA_EXPR* incr = &_vra->Expr(ub->Op0());
      const VRA_EXPR* top  = &_vra->Expr(ub->Op1());
      UINT32 new_ub = ub->Op1();
      if (incr->Opr() != OPR_CODEREP || top->Opr() != OPR_CODEREP)
        continue;
      if (incr->Cr()->Kind() != CK_OP || top->Cr()->Kind() != CK_OP)
        continue;
      // only handle ADD/SUB so far
      if (top->Cr()->Opr() == OPR_ADD || top->Cr()->Opr() == OPR_SUB) {
        const VRA_EXPR* tmp = incr;
        incr = top;
        top = incr;
        new_ub = ub->Op0();
      }
      else if (incr->Cr()->Opr() != OPR_ADD && incr->Cr()->Opr() != OPR_SUB)
        continue;
      OPERATOR opr = top->Cr()->Opr();
      if (incr->Cr()->Opr() == OPR_ADD &&
          opr != OPR_LT && opr != OPR_LE)
        continue;
      if (incr->Cr()->Opr() == OPR_SUB &&
          opr != OPR_GT && opr != OPR_GE)
        continue;
      opr = incr->Cr()->Opr() == OPR_ADD ? OPR_GE : OPR_LE;
      CODEREP* new_lb_cr = _vra->New_cmp_cr(opr, cr, lb->Cr());
      UINT32   new_lb = _vra->New_coderep_expr(new_lb_cr, bb->Id(), FALSE);
      //UINT32   new_vr = _vra->New_conj_expr(new_lb, top, bb->Id());
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "VRA-BLD: Finalize DISJ for phi cr%d in BB%d:\n", cr->Coderep_id(), bb->Id()));
      Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                   _vra->Print_expr(expr.Op0(), TFile));
      Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                   _vra->Print_expr(ub->Op0(), TFile));
      Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                   _vra->Print_expr(ub->Op1(), TFile));
      Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
               (TFile, "         New CONJ expr:\n"));
      Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                   _vra->Print_expr(new_lb, TFile));
      Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                   _vra->Print_expr(ub->Op1(), TFile));
      // rewrite expr at loop header
      expr.Init(OPR_CONJUNCTION, bb->Id(), new_lb, new_ub);

      if (tail_bb == 0)
        continue;
      // rewrite expr at loop tail
      CODEREP* incr_cr = incr->Cr();
      CODEREP* step = incr_cr->Opnd(0) == cr ? incr_cr->Opnd(1) :
                        incr_cr->Opnd(1) == cr ? incr_cr->Opnd(0) : NULL;
      if (step == NULL)
        continue;
      CODEREP* top0 = top->Cr()->Opnd(0);
      CODEREP* top1 = top->Cr()->Opnd(1);
      CODEREP* ub_cr = (top0->Kind() == CK_VAR &&
                        top0->Aux_id() == cr->Aux_id()) ? top1 :
                          (top1->Kind() == CK_VAR &&
                           top1->Aux_id() == cr->Aux_id()) ? top0 : NULL;
      if (ub_cr == NULL)
        continue;
      CODEREP* tail_var = ub_cr == top1 ? top0 : top1;
      // calculate new lower/upper bound
      if (step->Kind() != CK_CONST || step->Const_val() != 1)
        ub_cr = _vra->New_cr(OPCODE_make_op(incr_cr->Opr(), cr->Dtyp(), MTYPE_V),
                            ub_cr, step, TRUE);
      CODEREP* tail_cr = NULL;
      if (!loop_info->Exit_early()) {
        // if no early exit, value range in tail bb equals to ub. otherwise value
        // in tail bb is the same is loop header/body
        tail_cr = _vra->New_cmp_cr(OPR_EQ, tail_var, ub_cr);
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-BLD: Finalize new range for sym%dv%d cr%d at tail BB%d: ",
                         tail_var->Aux_id(), tail_var->Version(), tail_var->Coderep_id(),
                         tail_bb));
        Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                     tail_cr->Print(TFile));
      }
      else {
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-BLD: Finalize new range for sym%dv%d cr%d at tail BB%d to expr %d\n",
                         tail_var->Aux_id(), tail_var->Version(), tail_var->Coderep_id(),
                         tail_bb, id));
      }
      UINT32 tail_id = _vra->Value_range(tail_var, tail_bb);
      if (tail_id <= VRA::VR_EMPTY) {
        UINT32 tail_expr = tail_cr ? _vra->New_coderep_expr(tail_cr, tail_bb, FALSE)
                                   : id;
        tail_id = _vra->New_value_range(tail_var, tail_bb, tail_expr);
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-BLD: new range %d in BB%d: ",
                         tail_expr, tail_bb));
        Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                     _vra->Print_expr(tail_expr, TFile));
      }
      else {
        VRA_EXPR& tail_expr = _vra->Expr(tail_id);
        tail_cr ? tail_expr.Init(tail_cr, tail_bb, FALSE)
                : tail_expr.Init(OPR_CONJUNCTION, bb->Id(), new_lb, new_ub);
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-BLD: replace range %d with in tail BB%d tail_cr.\n",
                         tail_id, tail_bb));
      }
      // mark iv_updated if cr is IV
      if (cr == loop_info->Iv()) {
        trip_count = _vra->New_cr(OPCODE_make_op(OPR_SUB, cr->Dtyp(), MTYPE_V),
                                  ub_cr, lb->Cr(), TRUE);
        loop_info->Set_trip_count_expr(trip_count);
        iv_ub_included = (top->Cr()->Opr() == OPR_GE ||
                          top->Cr()->Opr() == OPR_LE);
        iv_updated = TRUE;
      }
      // check next, which may contain a phi of this value range
      if (loop_info->Tail()->Succ() == NULL ||
          loop_info->Tail()->Succ()->Next() != NULL)
        continue;
      BB_NODE* next = loop_info->Tail()->Succ()->Node();
      if (next->Phi_list() == NULL)
        continue;
      PHI_NODE* succ_phi = next->Phi_list()->Search_phi_node(tail_var->Aux_id());
      if (succ_phi == NULL || !succ_phi->Live())
        continue;
      UINT32 succ_id = _vra->Value_range(succ_phi->RESULT(), next->Id());
      if (succ_id <= VRA::VR_EMPTY)
        continue;
      VRA_EXPR& succ_expr = _vra->Expr(succ_id);
      if (succ_expr.Opr() != OPR_DISJUNCTION)
        continue;
      const VRA_EXPR& succ_op0 = _vra->Expr(succ_expr.Op0());
      if (_vra->Expr(succ_expr.Op0()).Bb_id() == tail_bb) {
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-BLD: replace succ sym%dv%d cr%d in BB%d DISJ(%d, %d) to (%d, %d).\n",
                        succ_phi->RESULT()->Aux_id(), succ_phi->RESULT()->Version(),
                        succ_phi->RESULT()->Coderep_id(), next->Id(),
                        succ_expr.Op0(), succ_expr.Op1(), tail_id, succ_expr.Op1()));
        succ_expr.Init(OPR_DISJUNCTION, next->Id(), tail_id, succ_expr.Op1());
      }
      else if (_vra->Expr(succ_expr.Op1()).Bb_id() == tail_bb) {
        Is_Trace(Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG),
                 (TFile, "VRA-BLD: replace succ sym%dv%d cr%d in BB%d DISJ(%d, %d) to (%d, %d).\n",
                        succ_phi->RESULT()->Aux_id(), succ_phi->RESULT()->Version(),
                        succ_phi->RESULT()->Coderep_id(), next->Id(),
                        succ_expr.Op0(), succ_expr.Op1(), succ_expr.Op0(), tail_id));
        succ_expr.Init(OPR_DISJUNCTION, next->Id(), succ_expr.Op0(), tail_id);
      }
    }
  }  
}

// my_log_2
static INT
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

// helper function to create and simplify the div
CODEREP*
VRA::New_div_cr(CODEREP* dividend, INT64 divisor) const
{
  switch (dividend->Kind()) {
  case CK_LDA:
  case CK_VAR:
  case CK_IVAR:
    return NULL;

  case CK_RCONST:
    Is_True(FALSE, ("TODO: rcont / const"));
    return NULL;

  case CK_CONST:
    // disable this assertion till the offset is canonicalized
    //Is_True(dividend->Const_val() % divisor == 0,
    //        ("fix me, hit %d/%d", dividend->Const_val(), divisor));
    return New_cr(dividend->Dtyp(), dividend->Const_val() / divisor);

  case CK_OP:
    if (dividend->Opr() == OPR_MPY) {
      if (dividend->Opnd(1)->Kind() == CK_CONST &&
          dividend->Opnd(1)->Const_val() % divisor == 0) {
        INT val = dividend->Opnd(1)->Const_val() / divisor;
        return val == 1 ? dividend->Opnd(0)
                        : New_cr(dividend->Op(), dividend->Opnd(0),
                                 New_cr(MTYPE_I8, val), TRUE);
      }
      CODEREP* op0 = New_div_cr(dividend->Opnd(0), divisor);
      if (op0 != NULL)
        return New_cr(dividend->Op(), op0, dividend->Opnd(1), TRUE);
      CODEREP* op1 = New_div_cr(dividend->Opnd(1), divisor);
      return op1 == NULL ? NULL :
                New_cr(dividend->Op(), dividend->Opnd(0), op1, TRUE);
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
                 New_cr(dividend->Op(), op0, op1, TRUE);
    }
    return NULL;

  default:
    Is_True(FALSE, ("unknown cr kind %d", dividend->Kind()));
  }
  return NULL;
}

// helper function to create cmp coderep
CODEREP*
VRA::Complement_cr(CODEREP* cr) const
{
  CODEREP_ANALYZER cra(this);
  return cra.Complement_cr(cr);
}

// analyze coderep (CK_OP) and get all scalars it used (CK_VAR)
BOOL
VRA::Analyze_coderep_vars(CODEREP* cr, hash_set<CODEREP*>& st) const
{
  CODEREP_ANALYZER cra(this);
  return cra.Analyze_cond_vars(cr, st);
}

// canonicalize cond expr coderep according to the var coderep
BOOL
VRA::Canonicalize_coderep(CODEREP* expr, CODEREP* var, CODEREP* &out) const
{
  CODEREP_ANALYZER cra(this);
  CONTEXT_SWITCH ctx(_comp_unit->Dna());
  BOOL ret = cra.Canon_cr(expr, var, out);
  return ret;
}

// build value range expression table
void
VRA::Build_table(IPSA* ipsa_mgr)
{
  VRA_BUILDER bld(this, _comp_unit, ipsa_mgr);
  bld.Build();
}

