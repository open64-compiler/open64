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
#include "opt_vra.h"

// simplify conjunction over two coderep
UINT32
VRA::Simplify_conj_expr(UINT32 lid, CODEREP* lexp, UINT32 rid, CODEREP* rexp, UINT32 bb_id)
{
  if (lexp->Kind() == CK_OP && lexp->Kid_count() > 1 &&
      lexp->Opnd(1)->Kind() == CK_CONST &&
      rexp->Kind() == CK_OP && rexp->Kid_count() > 1 &&
      rexp->Opnd(1)->Kind() == CK_CONST &&
      (Is_assign(lexp->Opnd(0),rexp->Opnd(0)) ||
       Is_assign(rexp->Opnd(0), lexp->Opnd(0)))) {
    // both lexp and rexp are constants
    INT64 lval = lexp->Opnd(1)->Const_val();
    INT64 rval = rexp->Opnd(1)->Const_val();
    if (lexp->Opr() == OPR_EQ && rexp->Opr() == OPR_EQ) {
      return lval == rval ? lid : VR_EMPTY;
    }
    else if (lexp->Opr() == OPR_EQ && rexp->Opr() == OPR_NE) {
      return lval != rval ? lid : VR_EMPTY;
    }
    else if (lexp->Opr() == OPR_EQ && rexp->Opr() == OPR_GT) {
      return lval > rval ? lid : VR_EMPTY;
    }
    else if (lexp->Opr() == OPR_EQ && rexp->Opr() == OPR_GE) {
      return lval >= rval ? lid : VR_EMPTY;
    }
    else if (lexp->Opr() == OPR_EQ && rexp->Opr() == OPR_LT) {
      return lval < rval ? lid : VR_EMPTY;
    }
    else if (lexp->Opr() == OPR_EQ && rexp->Opr() == OPR_LE) {
      return lval <= rval ? lid : VR_EMPTY;
    }
    // OPR_NE
    else if (lexp->Opr() == OPR_NE && rexp->Opr() == OPR_GE) {
      if (lval == rval) {
        CODEREP *new_rexp = New_cr(OPCODE_make_op(OPR_GE, MTYPE_I4, rexp->Dtyp()),
                                   rexp->Opnd(0), New_cr(rexp->Dtyp(), rval+1));
        UINT32 new_rid = New_coderep_expr(new_rexp, bb_id, FALSE);
        return new_rid;
      }
    }
    // TODO
    else if (lexp->Opr() == OPR_GT && rexp->Opr() == OPR_GE) {
      return lval >= rval ? lid : rid;
    }
    else if (lexp->Opr() == OPR_GE && rexp->Opr() == OPR_GT) {
      return rval >= lval ? rid : lid;
    }
    else {
      //TODO: other possible simplifications???
      return VR_NOT_FOUND;
    }
  }
  return VR_NOT_FOUND;
}

// simplify conjunction over a coderep and a value range expression
UINT32
VRA::Simplify_conj_expr(UINT lid, CODEREP* lexp, UINT32 rid, UINT32 bb_id) {
  const VRA_EXPR& rhs = Expr(rid);
  if (rhs.Opr() == OPR_CODEREP) {
    // the value range expression is also a coderep
    CODEREP* rexp = rhs.Cr();
    return Simplify_conj_expr(lid, lexp, rid, rexp, bb_id);
  }
  else if (rhs.Opr() == OPR_CONJUNCTION) {
    // the value range expression is a conjunction
    UINT32 kid0 = rhs.Op0();
    UINT32 kid1 = rhs.Op1();
    UINT32 ret;
    if (!Visited(kid0) &&
        (ret = Simplify_conj_expr(lid, lexp, kid0, bb_id)) != VR_NOT_FOUND) {
      UINT32 ret1 = Simplify_conj_expr(ret, kid1, bb_id);
      if (ret1 != VR_NOT_FOUND)
        return ret1;
      else
        return New_expr(OPR_CONJUNCTION, bb_id, ret, kid1);
    }
    else if (!Visited(kid1) &&
             (ret = Simplify_conj_expr(lid, lexp, kid1, bb_id)) != VR_NOT_FOUND) {
      UINT32 ret1 = Simplify_conj_expr(ret, kid0, bb_id);
      if (ret1 != VR_NOT_FOUND)
        return ret1;
      else
        return New_expr(OPR_CONJUNCTION, bb_id, kid0, ret);
    }
    return VR_NOT_FOUND;
  }
  else {
    //TODO: other possible simplifications??
    return VR_NOT_FOUND;
  }
}

// simplify conjunction over two value range expressions
UINT32
VRA::Simplify_conj_expr(UINT lid, UINT32 rid, UINT32 bb_id)
{
  const VRA_EXPR& lhs = Expr(lid);
  const VRA_EXPR& rhs = Expr(rid);
  // TODO: handle empty
  if (lhs.Opr() == OPR_TOP || lhs.Opr() == OPR_BOTTOM) {
    // special cases for TOP/BOTTOM
    return rhs.Bb_id() == bb_id ? rid :
             rhs.Opr() == OPR_BOTTOM ? rid :
                          Clone_expr(rhs, bb_id);
  }
  else if (rhs.Opr() == OPR_TOP || rhs.Opr() == OPR_BOTTOM) {
    // special cases for TOP/BOTTOM
    return lhs.Bb_id() == bb_id ? lid :
             lhs.Opr() == OPR_BOTTOM ? lid :
                         Clone_expr(lhs, bb_id);
  }
  if (lhs.Opr() == OPR_CODEREP) {
    // lhs is a coderep
    CODEREP* lexp = lhs.Cr();
    if (rhs.Opr() == OPR_CODEREP) {
      CODEREP* rexp = rhs.Cr();
      return Simplify_conj_expr(lid, lexp, rid, rexp, bb_id);
    }
    else if (rhs.Opr() == OPR_CONJUNCTION) {
      return Simplify_conj_expr(lid, lexp, rid, bb_id);
    }
    else {
      // TODO: other possible simplifications???
      return VR_NOT_FOUND;
    }
  }
  else if (lhs.Opr() == OPR_CONJUNCTION) {
    // lhs is a conjunction
    if (rhs.Opr() == OPR_CODEREP) {
      CODEREP* rexp = rhs.Cr();
      return Simplify_conj_expr(rid, rexp, lid, bb_id);
    }
    else if (rhs.Opr() == OPR_CONJUNCTION) {
      // TODO: other possible simplifications???
      return VR_NOT_FOUND;
    }
    else {
      // TODO: other possible simplifications???
      return VR_NOT_FOUND;
    }
  }
  else {
    // TODO: other possible simplifications???
    return VR_NOT_FOUND;
  }
}

// simplify disjunction over two coderep
UINT32
VRA::Simplify_disj_expr(UINT32 lid, CODEREP* lexp, UINT32 rid, CODEREP* rexp, UINT32 bb_id)
{
  return VR_NOT_FOUND;
}

// simplify disjunction over a coderep and a value range expression
UINT32
VRA::Simplify_disj_expr(UINT lid, CODEREP* lexp, UINT32 rid, UINT32 bb_id)
{
  return VR_NOT_FOUND;
}

// simplify disjunction over two value range expressions
UINT32
VRA::Simplify_disj_expr(UINT lid, UINT32 rid, UINT32 bb_id)
{
  const VRA_EXPR& lhs = Expr(lid);
  const VRA_EXPR& rhs = Expr(rid);
  if (lhs.Opr() == OPR_BOTTOM) {
    return lid;
  }
  else if (lhs.Opr() == OPR_EMPTY) {
    return rhs.Bb_id() == bb_id ? rid :
             rhs.Opr() == OPR_BOTTOM ? rid :
                          Clone_expr(rhs, bb_id);
  }
  if (rhs.Opr() == OPR_BOTTOM) {
    return rid;
  }
  else if (rhs.Opr() == OPR_EMPTY) {
    return lhs.Bb_id() == bb_id ? lid :
             lhs.Opr() == OPR_BOTTOM ? lid :
                          Clone_expr(lhs, bb_id);
  }
  // TODO: other simplification
  return VR_NOT_FOUND;
}

