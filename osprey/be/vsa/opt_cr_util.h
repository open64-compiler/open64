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
// opt_cr_util.h
//
// utilities to manipulate coderep
// ====================================================================
#ifndef opt_cr_util_INCLUDED
#define opt_cr_util_INCLUDED

#include "defs.h"
#include "erglob.h"
#include "symtab.h"
#include "const.h"
#include "opt_htable.h"
#include "opt_fold.h"
#include "opt_main.h"
#include <ext/hash_map>

class VSYM_OBJ_REP;

typedef __gnu_cxx::hash_map<IDTYPE, CODEREP *> CODEREP_MAP;
typedef __gnu_cxx::hash_map<IDTYPE, VSYM_OBJ_REP *> VOR_MAP;

// ====================================================================
// CR_UTIL
// utilities to manipulate coderep
// ====================================================================
class CR_UTIL {
private:
  COMP_UNIT *_cu;    // COMP_UNIT

private:
  // add new coderep into htable
  CODEREP *Enter_cr(CODEREP *cr, BOOL fold) const {
    CODEREP *ret = NULL;
    if (fold) {
      FOLD ftmp;
      ret = ftmp.Fold_Expr(cr);
    }
    return ret ? ret : _cu->Htable()->Rehash(cr);
  }

public:
  // constructor
  CR_UTIL(COMP_UNIT *cu) : _cu(cu) { }

public:
  // create integer constant coderep
  static void Init_const(CODEREP *cr, TYPE_ID dtyp, INT64 val) {
    cr->Init_const(MTYPE_is_signed(dtyp) ? MTYPE_I8 : dtyp, val);
  }

  // create float constant coderep
  static void Init_const(CODEREP *cr, TYPE_ID dtyp, double fval) {
    TCON tc = Host_To_Targ_Float(dtyp, fval);
    ST* st = New_Const_Sym(Enter_tcon(tc), MTYPE_To_TY(dtyp));
    cr->Init_rconst(dtyp, st);
  }

  // get complement operator
  static OPERATOR Complement_opr(OPERATOR opr) {
    switch (opr) {
    case OPR_EQ:   return OPR_NE;
    case OPR_NE:   return OPR_EQ;
    case OPR_LT:   return OPR_GE;
    case OPR_LE:   return OPR_GT;
    case OPR_GT:   return OPR_LE;
    case OPR_GE:   return OPR_LT;
    case OPR_ADD:  return OPR_SUB;
    case OPR_SUB:  return OPR_ADD;
    default:
      return OPERATOR_UNKNOWN;
    }
  }

  // get new operator if the lhs and rhs are exchanged
  static OPERATOR Exchange_opr(OPERATOR opr) {
    switch (opr) {
    case OPR_ADD:
    case OPR_NE:
    case OPR_EQ:  return opr;
    case OPR_GE:  return OPR_LE;
    case OPR_GT:  return OPR_LT;
    case OPR_LE:  return OPR_GE;
    case OPR_LT:  return OPR_GT;
    default:
      return OPERATOR_UNKNOWN;
    }
  }

  // find given var in expr and place it in lhs, the rest in rhs
  static BOOL Pickup_opnd(CODEREP* expr, CODEREP* var, CODEREP* &lhs, CODEREP* &rhs) {
    Is_True(expr->Kind() == CK_OP &&
            (expr->Opr() == OPR_ADD || expr->Opr() == OPR_SUB),
            ("only support ADD or SUB expr"));
    if (expr->Opr() == OPR_ADD && expr->Opnd(1) == var) {
      lhs = expr->Opnd(1);
      rhs = expr->Opnd(0);
      return TRUE;
    }
    else {
      lhs = expr->Opnd(0);
      rhs = expr->Opnd(1);
      return var == expr->Opnd(0);
    }
  }

  // find the stmt which assigns to aux in given bb and return the stmt rhs coderep
  static CODEREP *Find_assign(BB_NODE *bb, AUX_ID aux) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (OPERATOR_is_scalar_store(stmt->Opr()) &&
          stmt->Lhs()->Aux_id() == aux)
        return stmt->Rhs();
    }
    return NULL;
  }

  // analyze the coderep to find the var and vor used in it
  void     Analyze_cr(CODEREP *cr, CODEREP_MAP &var_map, VOR_MAP &vor_map, BOOL ivar_kid) const;

  // analyze the cmp cr and find the var and vor used in lhs and rhs respectively
  void     Analyze_cmp_cr(CODEREP *cr, CODEREP_MAP &lhs_var, VOR_MAP &lhs_vor,
                          CODEREP_MAP &rhs_var, VOR_MAP &rhs_vor) const;

  // transform unary op to put var in first kid in cr
  CODEREP *Transform_unary_op(CODEREP *cr, CODEREP *var) const;

  // transform binary op to put var in first kid in cr
  CODEREP *Transform_binary_op(CODEREP *cr, CODEREP *var) const;

  // transform cr to put var in first kid in cr
  CODEREP *Transform_cr(CODEREP *cr, CODEREP *var) const {
    if (cr == var)
      return cr;
    switch (cr->Kind()) {
    case CK_LDA:
    case CK_CONST:
    case CK_RCONST:
    case CK_VAR:
    case CK_IVAR:
      return NULL;
    case CK_OP:
      if (cr->Kid_count() == 0)
        return Transform_unary_op(cr, var);
      else if (cr->Kid_count() == 1)
        return Transform_binary_op(cr, var);
      Is_True(FALSE, ("TODO: cr with %d kids", cr->Kid_count()));
      return NULL;
    default:
      Is_True(FALSE, ("Unknown cr kind %d", cr->Kind()));
      return NULL;
    }
  }

public:
  // create integer constant cr
  CODEREP *New_const_cr(TYPE_ID dtyp, INT64 val) const {
    CODEREP *cr = Alloc_stack_cr(0);
    Init_const(cr, dtyp, val);
    return _cu->Htable()->Rehash(cr);
  }

  // create unary cr with given opc and operand
  CODEREP *New_unary_cr(OPCODE opc, CODEREP* op0, BOOL fold = FALSE) const {
    CODEREP* cr = Alloc_stack_cr(1);
    cr->Init_op(opc, 1);
    cr->Set_opnd(0, op0);
    return Enter_cr(cr, fold);
  }

  // create binary cr with given opc and operands
  CODEREP *New_binary_cr(OPCODE opc, CODEREP* op0, CODEREP* op1, BOOL fold = FALSE) const {
    CODEREP* cr = Alloc_stack_cr(2);
    cr->Init_op(opc, 2);
    cr->Set_opnd(0, op0);
    cr->Set_opnd(1, op1);
    return Enter_cr(cr, fold);
  }

  // create binary cr with given opc and operands in coderep and integer value
  CODEREP *New_binary_cr(OPCODE opc, CODEREP* op0, INT64 op1) const {
    CODEREP *cst = New_const_cr(op0->Dtyp(), op1);
    return New_binary_cr(opc, op0, cst);
  }

  // create comparison cr with given opr and operands
  CODEREP *New_cmp_cr(OPERATOR opr, CODEREP* op0, CODEREP* op1) const {
    Is_True(opr == OPR_EQ || opr == OPR_NE || opr == OPR_GT ||
            opr == OPR_GE || opr == OPR_LT || opr == OPR_LE,
            ("invalid opr for cmp cr"));
    return New_binary_cr(OPCODE_make_op(opr, Boolean_type, op0->Dtyp()),
                         op0, op1);
  }

  // create comparison cr with given opr and operands in coderep and integer value
  CODEREP *New_cmp_cr(OPERATOR opr, CODEREP* op0, INT64 op1) const {
    Is_True(MTYPE_is_integral(op0->Dtyp()), ("invalid dtyp"));
    CODEREP *cst = New_const_cr(op0->Dtyp(), op1);
    return New_cmp_cr(opr, op0, cst);
  }

  // create coderep with complement operator
  CODEREP *Complement_cr(CODEREP* cr) const {
    Is_True(cr->Kind() == CK_OP, ("Unexpected CR kind %d", cr->Kind()));
    OPERATOR opr = Complement_opr(cr->Opr());
    Is_True(opr != OPERATOR_UNKNOWN, ("no complement opr"));
    return New_binary_cr(OPCODE_make_op(opr, cr->Dtyp(), cr->Dsctyp()),
                         cr->Opnd(0), cr->Opnd(1));
  }

  // create division coderep with dividend in coderep and integer divisor
  CODEREP *New_div_cr(CODEREP* dividend, INT64 divisor) const;
};  // CR_UTIL


#endif /* opt_cr_util_INCLUDED */
