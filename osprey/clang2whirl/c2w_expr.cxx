/*
  Copyright (C) 2019-2020 XC5 Limited, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  http://www.xcalibyte.com

  For more information, see:
  http://github.com/open64-compiler/open64
  http://gitee.com/open64-compiler/open64

*/

#include <clang/AST/VTableBuilder.h>
#include "c2w_expr.h"
#include "c2w_builder.h"
#include "c2w_stmt.h"
#include "c2w_tracer.h"
#include "c2w_cxxabi.h"
#include "c2w_utils.h"
// clang header files
#include "clanginc.h"
// open64 header files
#include "open64inc.h"

using namespace clang;

namespace wgen {

static inline TYPE_ID
Widen_mtype(TYPE_ID t) {
  if (MTYPE_is_m(t))
    return t;
  if (MTYPE_is_void(t) || t == MTYPE_BS) {
    Is_True(false, ("Widen_mtype: for MTYPE_V or MTYPE_BS"));
    return t;
  }
  if (MTYPE_byte_size(t) >= 4)
    return t;
  return Mtype_TransferSize(MTYPE_I4, t);
}

// Get real whirl node
// 1. for UnaryOperator whose opcode is
// UO_PreInc/UO_PreDec/UO_PostInc/UO_PostDec
// such as: j = i < 0 ? ++i : --i;
// 2. for CXXConstructExpr
// such as: j ? A(1) : A(0);
// first get whirl node which is stored into current block
// and then create RCOMMA/COMMA for it
static WN *
Get_real_wn(WN *wn, const Expr *expr, TY_IDX ty) {
  // NOT NEEDED any more, should be removed later.
  return wn;
#if 0
  bool handle_wn = false;
  bool post_fix = false;
  OPERATOR opr = WN_operator(wn);

  expr = expr->IgnoreParenImpCasts();

  if (isa<CXXConstructExpr>(expr) ||
      isa<CXXBindTemporaryExpr>(expr) &&
      isa<CXXConstructExpr>(cast<CXXBindTemporaryExpr>(expr)->getSubExpr()))
    handle_wn = true;

  // get real call for CallExpr,
  // ignore builtin calls that may generate
  // OPR_INTCONST/OPR_CONST/OPR_INTRINSIC_OP
  if (isa<CallExpr>(expr) && opr != OPR_CALL && opr != OPR_COMMA &&
      opr != OPR_INTCONST && opr != OPR_CONST &&
      opr != OPR_INTRINSIC_OP)
    handle_wn = true;

  if (isa<UnaryOperator>(expr)) {
    UnaryOperatorKind opcode = cast<UnaryOperator>(expr)->getOpcode();
    if (opcode == UO_PreInc  ||
        opcode == UO_PreDec ||
        opcode == UO_PostInc ||
        opcode == UO_PostDec)
      handle_wn = true;
    if (cast<UnaryOperator>(expr)->isPostfix())
      post_fix = true;
  }

  if (!handle_wn)
    return wn;

  WN *ret = wn;
  WN *cur_blk = WhirlBlockUtil::getCurrentBlock();
  Is_True(opr == OPR_LDID ||
          opr == OPR_ILOAD ||
          opr == OPR_LDA ||
          MTYPE_is_complex(TY_mtype(ty)) && opr == OPR_PAIR ||
          (opr == OPR_CVT || opr == OPR_CVTL || opr == OPR_TAS),
          ("invalid whirl node"));

  if (WN_operator(wn) == OPR_ILOAD ||
      (WN_operator(wn) == OPR_CVT &&
       WN_operator(WN_kid0(wn)) == OPR_ILOAD))
    wn = WN_COPY_Tree(wn);
  else if (WN_operator(wn) == OPR_LDA ||
           (WN_operator(wn) == OPR_CVT &&
            WN_operator(WN_kid0(wn)) == OPR_LDA)) {
    WN *tmp = wn;
    bool has_cvt = false;
    if (WN_operator(wn) == OPR_CVT) {
      tmp = WN_kid0(wn);
      has_cvt = true;
    }
    TY_IDX ty = TY_pointed(WN_ty(tmp));
    tmp = WN_Ldid(TY_mtype(ty), 0, WN_st(tmp), ty);;
    if (has_cvt)
      WN_kid0(wn) = tmp;
    else
      wn = tmp;
  }

  WN *base = WN_CreateBlock();
  WN *ld = WN_COPY_Tree(WN_last(cur_blk));
  WN_INSERT_BlockLast(base, ld);
  WN_DELETE_FromBlock(cur_blk, WN_last(cur_blk));

  if (post_fix) {
    WN_INSERT_BlockBefore(base, ld, WN_COPY_Tree(WN_last(cur_blk)));
    WN_DELETE_FromBlock(cur_blk, WN_last(cur_blk));
  }

  Is_True(WN_operator(WN_last(base)) == OPR_STID   ||
          WN_operator(WN_last(base)) == OPR_ISTORE ||
          WN_operator(WN_last(base)) == OPR_CALL, ("invalid whirl node"));

  ret = WGEN_CreateComma(WN_rtype(wn), base, wn);

  return ret;
#endif
}

// check whether the WHIRL operator has subsumed cvtl in its semantics
// (intended only for integer operations)
bool
Has_subsumed_cvtl(OPERATOR opr)
{
  if (OPERATOR_is_load(opr) || OPERATOR_is_leaf(opr))
    return TRUE;
  if (opr == OPR_CVT || opr == OPR_CVTL || opr == OPR_TAS)
    return TRUE;
  if (opr == OPR_EQ || opr == OPR_NE ||
      opr == OPR_GE || opr == OPR_GT ||
      opr == OPR_LE || opr == OPR_LT ||
      opr == OPR_LNOT || opr == OPR_LAND || opr == OPR_LIOR ||
      opr == OPR_CAND || opr == OPR_CIOR)
    return TRUE;
  return FALSE;
}

RV
WhirlExprBuilder::GetRV(const Expr *expr, BOOL retv) {
  if (!retv || expr->getType()->isVoidType())
    return R_NVALUE;
  return expr->isGLValue() ? R_LVALUE : R_RVALUE;
}

Result
WhirlExprBuilder::ConvertArrayInitLoopExpr(const ArrayInitLoopExpr *expr, Result dest) {
  Is_True(!dest.isNone() &&
          (dest.IsDot() || dest.IsRef()) &&
          dest.FieldId() > 0,
          ("invalid dest?"));
  TY_IDX ty = _builder->TB().ConvertType(expr->getType());
  Is_True(TY_kind(ty) == KIND_ARRAY, ("not array type"));
  SRCPOS spos = SetSrcPos(getLocation(expr));
#ifdef Is_True_On
  const OpaqueValueExpr *common = expr->getCommonExpr();
  const Expr *source = common->getSourceExpr();
  if (isa<DeclRefExpr>(source)) {
    Is_True(_builder->LambdaHelper().IsInLambdaConstruct(),
            ("not in lambda construct?"));
    const DeclRefExpr *decl_ref = cast<DeclRefExpr>(source);
    const FieldDecl *fld = _builder->LambdaHelper().GetCapFld(decl_ref->getDecl());
    Is_True(fld == NULL || fld->getFieldIndex() + 1 == dest.FieldId(),
            ("field id mismatch"));
  }
  else if (isa<MemberExpr>(source)) {
    const ValueDecl *decl = cast<MemberExpr>(source)->getMemberDecl();
    Is_True(decl && isa<FieldDecl>(decl), ("not field decl"));
    Is_True(_builder->TB().GetFieldIDFromDecl(decl) == dest.FieldId(),
            ("field id mismatch"));
  }
  else {
    Is_True(FALSE, ("TODO: handle me"));
  }
#endif

  // check multi-dimension array
  const Expr *sub_expr = expr->getSubExpr();
  uint64_t elem = expr->getArraySize().getZExtValue();
  unsigned level = 0;
  while (isa<ArrayInitLoopExpr>(sub_expr)) {
    const ArrayInitLoopExpr* init_expr = cast<ArrayInitLoopExpr>(sub_expr);
    elem *= init_expr->getArraySize().getZExtValue();
    ++ level;
    sub_expr = init_expr->getSubExpr();
  }

  // check sub expr
  const Expr *array_expr = NULL;
  const CXXConstructorDecl *ctor_decl = NULL;
  if (isa<CXXConstructExpr>(sub_expr)) {
    const CXXConstructExpr *ctor_expr = cast<CXXConstructExpr>(sub_expr);
    if (!ctor_expr->getConstructor()->isTrivial()) {
      ctor_decl = ctor_expr->getConstructor();
      Is_True(ctor_decl->isCopyOrMoveConstructor(), ("not copy/move ctor"));
    }
    Is_True(ctor_expr->getConstructionKind() == CXXConstructExpr::CK_Complete,
            ("not C1?"));
    Is_True(ctor_expr->getNumArgs() == 1, ("more than 1 arg?"));
    array_expr = ctor_expr->getArg(0);
  }
  else {
    Is_True(isa<ImplicitCastExpr>(sub_expr) &&
            cast<ImplicitCastExpr>(sub_expr)->getCastKind() == CK_LValueToRValue,
            ("not lvalue to rvalue cast?"));
    array_expr = cast<ImplicitCastExpr>(sub_expr)->getSubExpr();
  }

  Is_True(isa<ArraySubscriptExpr>(array_expr),
          ("not ArraySubscriptExpr for ArrayInitLoopExpr"));
  TY_IDX ele_ty = _builder->TB().ConvertType(array_expr->getType());
  TY_IDX ptr_ty = Make_Pointer_Type(ele_ty);

  while (level > 0) {
    const ArraySubscriptExpr *subs_expr = cast<ArraySubscriptExpr>(array_expr);
    Is_True(subs_expr->getLHS() && isa<ImplicitCastExpr>(subs_expr->getLHS()),
            ("lhs is not ImplicitCastExpr"));
    Is_True(subs_expr->getRHS() && isa<ArrayInitIndexExpr>(subs_expr->getRHS()),
            ("rhs is not ArrayInitIndexExpr"));
    const ImplicitCastExpr *lhs = cast<ImplicitCastExpr>(subs_expr->getLHS());
    Is_True(lhs->getCastKind() == CK_ArrayToPointerDecay, ("not array to ptr"));
    const Expr *lhs_sub = lhs->getSubExpr();
    Is_True(lhs && isa<OpaqueValueExpr>(lhs_sub), ("not OpaqueValueExpr"));
    const Expr *src_expr = cast<OpaqueValueExpr>(lhs_sub)->getSourceExpr();
    Is_True(src_expr && isa<ArraySubscriptExpr>(src_expr), ("not array subs expr"));
    array_expr = cast<ArraySubscriptExpr>(src_expr);
    -- level;
  }
  Is_True(isa<ArraySubscriptExpr>(array_expr),
          ("not ArraySubscriptExpr for ArrayInitLoopExpr"));

  Result r = ConvertExpr(array_expr);
  WN *rhs = r.GetLValue();
  WN *lhs = dest.GetLValue();
  if (ctor_decl == NULL) {
    // not call ctor, do a bitwised-copy
    WN *mload = WN_CreateMload(0, ptr_ty, rhs, WN_Intconst(MTYPE_I4, TY_size(ty)));
    WN *mstore = WN_CreateMstore(0, ptr_ty, mload, lhs, WN_Intconst(MTYPE_I4, TY_size(ty)));
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), mstore);
    WN_Set_Linenum(mstore, spos);
    return Result::nwNone();
  }

  // generate a loop to construct each element
  // save lhs & rhs into iter
  WN *parm_dst = WGEN_StidTemp(ptr_ty, lhs, ".lhs.iter");
  WN_Set_Linenum(parm_dst, spos);
  WN *parm_src = WGEN_StidTemp(ptr_ty, rhs, ".rhs.iter");
  WN_Set_Linenum(parm_src, spos);
  WN *blk = WhirlBlockUtil::getCurrentBlock();
  WN_INSERT_BlockLast(blk, parm_dst);
  WN_INSERT_BlockLast(blk, parm_src);

  // get constructor st and create call
  GlobalDecl gd(ctor_decl, CXXCtorType::Ctor_Complete);
  ST_IDX st_idx = _builder->Get_func_st(gd);
  Is_True(st_idx, ("wrong ctor st_idx"));
  WN *ctor_call = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 2);
  WN_Set_Linenum(ctor_call, spos);
  WN_st_idx(ctor_call) = st_idx;
  WN_Set_Call_Default_Flags(ctor_call);
  if (!_builder->DeclBuilder().Call_nothrow(ctor_decl))
    Mark_call_region(ctor_call);
  WN_kid(ctor_call, 0) = WGEN_CreateParm(Pointer_Mtype,
                                         WN_Ldid(Pointer_Mtype, WN_offset(parm_dst),
                                                 WN_st(parm_dst), ptr_ty),
                                         ptr_ty);
  WN_kid(ctor_call, 1) = WGEN_CreateParm(Pointer_Mtype,
                                         WN_Ldid(Pointer_Mtype, WN_offset(parm_src),
                                                 WN_st(parm_src), ptr_ty),
                                         ptr_ty);
  WN *bound = WN_Intconst(elem > INT_MAX ? MTYPE_I8 : MTYPE_I4, elem);
  WN *parm[2] = { parm_dst, parm_src };
  WN *loop = ConvertConstantArray(ele_ty, bound, ctor_call, parm, 2);
  WN_INSERT_BlockLast(blk, loop);
  return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertArraySubscriptExpr(const ArraySubscriptExpr *expr) {
  TRACE_FUNC();
  const Expr *base = expr->getBase();
  const Expr *index = expr->getIdx();

  Result base_r = ConvertExpr(base);
  Is_True(base->isGLValue() || TY_kind(base_r.Ty()) == KIND_POINTER ||
          TY_kind(base_r.Ty()) == KIND_ARRAY,
          ("base is not pointer"));
  if (isa<ArrayInitIndexExpr>(index)) {
    if (base->isGLValue())
      base_r.SetLValue();
    return base_r;
  }

  Result index_r = ConvertExpr(index);
  WN *wn0 = base->isGLValue() ? base_r.GetLValue() : base_r.GetRValue();   // ARRAY base
  WN *wn1 = NULL;                  // ARRAY elem count
  WN *wn2 = index_r.GetRValue();   // ARRAY index

  TY_IDX ty = _builder->TB().ConvertType(expr->getType());
  TY_IDX base_ty = _builder->TB().ConvertType(base->getType());

  TY_IDX obj_ty = base_r.Ty();
  if (TY_kind(obj_ty) == KIND_POINTER) {
    obj_ty = TY_pointed(obj_ty);
    if (base_r.isNode() && WN_operator(base_r.Node()) == OPR_LDID) {
      ST *st = WN_st(base_r.Node());
      if (WN_field_id(base_r.Node()) == 0)
        obj_ty = TY_pointed(ST_type(*st));
      else {
        TY_IDX st_ty_idx = ST_type(*st);
        UINT cur_field_id = 0;
        FLD_HANDLE fld = FLD_get_to_field(st_ty_idx, WN_field_id(base_r.Node()), cur_field_id);
        obj_ty = TY_pointed(FLD_type(fld));
      }
    }
  }

  if (base_r.FieldId() > 0) {
    Is_True(TY_kind(obj_ty) == KIND_STRUCT, ("base not struct"));
    UINT fld_id = 0;
    UINT64 ofst = 0;
    FLD_HANDLE fh = get_fld_and_offset(obj_ty, base_r.FieldId(), fld_id, ofst);
    Is_True(!fh.Is_Null(), ("not find the field"));
    obj_ty = FLD_type(fh);
  }

  TY_IDX ret_ty = base_ty;
  // handle vector type
  ExprObjectKind ok = expr->getObjectKind();
  if (ok == OK_VectorComponent) {
    ret_ty = Make_Pointer_Type(ty);
  }

  // handle non-array and vector type
  if (TY_kind(obj_ty) != KIND_ARRAY ||
      TY_kind(ty) == KIND_ARRAY && TY_size(ty) >= TY_size(obj_ty)) {
    // generate wn0 + wn2 * scale
    WN *ofst;
    TYPE_ID rtyp = WN_rtype(wn2);
    if (WN_operator(wn2) == OPR_INTCONST)
      ofst = WN_Intconst(rtyp, WN_const_val(wn2) * TY_size(obj_ty));
    else
      ofst = WN_Mpy(rtyp, wn2, WN_Intconst(rtyp, TY_size(obj_ty)));
    WN* ret = WN_Add(Pointer_Mtype, wn0, ofst);
    Result r = Result::nwNode(ret, ret_ty);
    r.SetArray();
    return r;
  }

  // handle array
  Is_True(TY_kind(obj_ty) == KIND_ARRAY, ("should be KIND_ARRAY"));
  ARB_HANDLE arb = TY_arb(obj_ty);
  if (ARB_const_ubnd(arb))
    wn1 = WN_Intconst(MTYPE_I4, ARB_ubnd_val(arb) - ARB_lbnd_val(arb) + 1);
  else {
    // assumed to have one element for incomplete array type
    if (TY_is_incomplete(obj_ty)) {
      wn1 = WN_Intconst(MTYPE_I4, 0);
    } else {
      Is_True(ARB_ubnd_var(arb),
              ("no variable that stores the non-constant upper bound"));
      ST* var_st = &St_Table[ARB_ubnd_var(arb)];
      TY_IDX ty = ST_type(var_st);
      wn1 = WN_Add(TY_mtype(ty),
                   WN_Ldid(TY_mtype(ty), 0, var_st, ty),
                   WN_Intconst(TY_mtype(ty), 1));
    }
  }

  WN *array_wn;
  // check if current dimensions could be expanded
  bool expand_dimensions = false;
  if (WN_operator(wn0) == OPR_ARRAY && base_r.FieldId() == 0) {
    WN *wn0_base = WN_array_base(wn0);
    while (WN_operator(wn0_base) == OPR_ADD)
      wn0_base = WN_kid0(wn0_base);
    if (WN_operator(wn0_base) == OPR_LDA || WN_operator(wn0_base) == OPR_LDID) {
      TY_IDX wn0_base_ty = TY_pointed(WN_ty(wn0_base));
      if (WN_field_id(wn0_base)) {
        Is_True(TY_kind(wn0_base_ty) == KIND_STRUCT, ("base not struct"));
        UINT fld_id = 0;
        UINT64 ofst = 0;
        FLD_HANDLE fh = get_fld_and_offset(wn0_base_ty, WN_field_id(wn0_base),
                                           fld_id, ofst);
        Is_True(!fh.Is_Null(), ("not find the field"));
        wn0_base_ty = FLD_type(fh);
      }
      Is_True(TY_kind(wn0_base_ty) == KIND_ARRAY, ("should be KIND_ARRAY"));
      while (TY_kind(wn0_base_ty) == KIND_ARRAY) {
        wn0_base_ty = TY_etype(wn0_base_ty);
        if (wn0_base_ty == TY_etype(obj_ty)) {
          expand_dimensions = true;
          break;
        }
      }
    }
  }
  // Expand the current dimension by growing the array just expanded.
  if (expand_dimensions) {
    int old_kid_count = WN_kid_count(wn0);
    int new_kid_count = old_kid_count + 2;
    array_wn = WN_Create(OPR_ARRAY, Pointer_Mtype, MTYPE_V, new_kid_count);
    for (int kid = 0; kid < (old_kid_count >> 1); kid++) {
      WN_kid(array_wn, kid + 1) = WN_kid(wn0, kid + 1);
      WN_kid(array_wn, (new_kid_count >> 1) + kid + 1) =
        WN_kid(wn0, (old_kid_count >> 1) + kid + 1);
    }
    WN_kid(array_wn, 0) = WN_kid(wn0, 0);
    WN_kid(array_wn, new_kid_count >> 1) = wn1;
    WN_kid(array_wn, new_kid_count - 1) = wn2;
    WN_Delete(wn0);
  }
  else
    array_wn = WN_Ternary(OPR_ARRAY, Pointer_Mtype, wn0, wn1, wn2);

  TY_IDX ety = TY_etype(obj_ty);
  WN_element_size(array_wn) = TY_size(ety);
  if (TY_align(obj_ty) < TY_align(ety))
    Set_TY_align(ety, TY_align(obj_ty));

  Result r = Result::nwNode(array_wn, ret_ty);
  r.SetArray();
  return r;
}

Result
WhirlExprBuilder::ConvertBinaryConditionalOperator(const BinaryConditionalOperator *expr) {
  TRACE_FUNC();
  Expr *cond_expr = expr->getCond();
  Expr *true_expr = expr->getTrueExpr();
  Expr *false_expr = expr->getFalseExpr();

  WN *cond, *true_wn, *false_wn;
  cond = ConvertToNode(cond_expr);

  TY_IDX ty = _builder->TB().ConvertType(expr->getType());
  // If cond operator is not OPR_CONST or load,
  // save expr node cond, and use LDID of that stored address as cond
  if (WN_operator(cond) != OPR_CONST &&
      !OPCODE_is_load(WN_opcode(cond))) {
    TY_IDX cond_ty = _builder->TB().ConvertType(cond_expr->getType());
    ST *tmp_st = Gen_Temp_Symbol(cond_ty, "__save_expr");
    WN *st_wn = WN_Stid(TY_mtype(cond_ty), 0, tmp_st, cond_ty, cond);
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
    WN_Set_Linenum(st_wn, GetSrcPos());
    cond = WN_Ldid(TY_mtype(cond_ty), 0, tmp_st, cond_ty);
  }

  if (true_expr == cond_expr)
    true_wn = WN_COPY_Tree(cond);
  else
    true_wn = ConvertToNode(true_expr);

  false_wn = ConvertToNode(false_expr);


  // handle cond wn
  cond = Handle_cond_wn(cond);
  WN *ret = WGEN_CreateCselect(TY_mtype(ty), cond, true_wn, false_wn);

  return Result::nwNode(ret, ty);
}

static bool WN_hasReturnValueLDID(WN *wn) {
  if (wn == NULL)
    return false;
  if (WN_operator(wn) == OPR_LDID)
    return WN_offset(wn) == -1;
  if (WN_operator(wn) == OPR_COMMA)
    return WN_hasReturnValueLDID(WN_kid1(wn));
  if (WN_operator(wn) == OPR_RCOMMA)
    return WN_hasReturnValueLDID(WN_kid0(wn));
  INT kidno;
  for (kidno = 0; kidno < WN_kid_count(wn); kidno++) {
    if (WN_hasReturnValueLDID(WN_kid(wn, kidno)))
      return true;
  }
  return false;
}

Result
WhirlExprBuilder::EmitAssignNode(Result lhs, Result rhs, TY_IDX ty, RV rv) {
  Is_True(lhs.isNode() || lhs.isSym(),
          ("invalid lhs for assignment operator"));
  Is_True(!rhs.isNone(),
          ("invalid rhs"));
  Is_True(ty != (TY_IDX)0,
          ("TY for assignment operator can not be NULL"));

  WN* rhs_wn = rhs.GetRValue();
  WN* block;
  TYPE_ID rtype = WN_rtype(rhs_wn);
  if (rv != R_NVALUE) {
    block = WN_CreateBlock();
    rhs_wn = Handle_expr_for_copy(block, rhs_wn, ty, GetSrcPos());
  }
  else {
    block = WhirlBlockUtil::getCurrentBlock();
  }

  WN* ret = NULL;
  if (lhs.isNode()) {
    if (!lhs.IsRValue() ||
        lhs.IsArray() || lhs.IsArrow() || lhs.IsDeref() || lhs.IsRef()) {
      TY_IDX pty = lhs.Ty();
      Is_True(TY_kind(pty) == KIND_POINTER, ("not pointer type"));
      TY_IDX lty = TY_pointed(pty);
      UINT64 ofst = 0;
      UINT32 fid_id = lhs.FieldId();
      if (fid_id) {
        IDTYPE fld = 0;
        Is_True(TY_kind(lty) == KIND_STRUCT, ("not struct type"));
        FLD_HANDLE fh = get_fld_and_offset(lty, fid_id, fld, ofst);
        Is_True(!fh.Is_Null(), ("not find the field"));
        if (FLD_is_bit_field(fh)) {
          ty = MTYPE_To_TY(MTYPE_BS);
          ofst = 0;
        }
      }
      if (WN_hasReturnValueLDID(lhs.Node())) {
        WN *lhs_wn = Handle_expr_for_copy(block, lhs.Node(), pty, GetSrcPos());
        if (lhs_wn != lhs.Node()) {
          lhs = Result::nwNode(lhs_wn, pty);
          lhs.SetFieldId(fid_id);
        }
      }

      ret = WN_Istore(TY_mtype(ty), ofst,
                      pty, lhs.Node(), rhs_wn, lhs.FieldId());
    }
    else {
      Is_True(false, ("lhs not deref"));
      return Result::nwNone();
    }
  }
  else if (lhs.isSym()) {
    Is_True(!lhs.IsAddrOf(), ("invalid lhs with '&'"));
    if (lhs.IsDeref() || lhs.IsRef()) {
      TY_IDX pty = lhs.Ty();
      Is_True(TY_kind(pty) == KIND_POINTER, ("not pointer type"));
      TY_IDX lty = TY_pointed(pty);
      WN* addr = lhs.GetLValue();
      ret = WN_Istore(TY_mtype(ty), 0, pty, addr, rhs_wn);
    }
    else {
      UINT64 ofst = 0;
      if (lhs.FieldId()) {
        IDTYPE fld = 0;
        FLD_HANDLE fh = get_fld_and_offset(lhs.Ty(), lhs.FieldId(), fld, ofst);
        Is_True(!fh.Is_Null(), ("not find the field"));
        if (FLD_is_bit_field(fh)) {
          ty = MTYPE_To_TY(MTYPE_BS);
          ofst = 0;
        }
      }
      ret = WN_Stid(TY_mtype(ty), ofst, ST_ptr(lhs.Sym()), lhs.Ty(), rhs_wn, lhs.FieldId());
    }
  }

  WN_Set_Linenum(ret, GetSrcPos());

  if (rv == R_LVALUE) {
    WN_INSERT_BlockLast(block, ret);
    WN *lhs_wn = WN_COPY_Tree(lhs.GetLValue());
    WN *comma = WGEN_CreateComma(WN_rtype(lhs_wn), block, lhs_wn);
    rhs = Result::nwNode(comma, Make_Pointer_Type(ty));
  }
  else if (rv != R_NVALUE) {
    WN_INSERT_BlockLast(block, ret);
    lhs = lhs.ConvertToRValue(ty);
    WN *lhs_wn = WN_COPY_Tree_Without_Commas(lhs.GetRValue());
    WN *comma = WGEN_CreateComma(rtype, block, lhs_wn);
    rhs = Result::nwNode(comma, ty);
    rhs.SetRValue();
  }
  else {
    return Result::nwNode(ret, 0);
  }
  return rhs;

#if 0
  WN *ret = NULL;
  WN *ld_wn = NULL;
  // expr->getLHS() is OPR_ARRAY
  if (WN_operator(lhs) == OPR_ARRAY) {
    ret = WN_Istore(TY_mtype(ty), 0, Make_Pointer_Type(ty), lhs, rhs, 0);
    ld_wn = WN_Iload(TY_mtype(ty), 0, ty, lhs, 0);
  } else {
    UINT32 field_id = WN_field_id(lhs) ? WN_field_id(lhs) : 0;
    UINT64 ofst_value = WN_offset(lhs);
    ld_wn = lhs;
    if (WN_operator(lhs) == OPR_ILOAD) {
      ret = WN_CreateIstore(OPR_ISTORE, MTYPE_V, WN_desc(lhs),
                            ofst_value, WN_load_addr_ty(lhs), rhs,
                            WN_kid0(lhs), field_id);
    } else {
      OPERATOR opr = WN_operator(lhs);
      if (opr == OPR_CVT || opr == OPR_CVTL || opr == OPR_TAS)
        lhs = WN_kid0(lhs);
      ret = WN_Stid(TY_mtype(ty), ofst_value, WN_st(lhs), WN_ty(lhs), rhs, field_id);
    }
  }

  WN_Set_Linenum(ret, GetSrcPos());
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);
  return ld_wn;
#endif
}

// generate wn for BO_LT/BO_LE/BO_GT/BO_GE/BO_EQ/BO_NE
// and check if conversion is needed
static WN *
Gen_wn_for_cmp(OPERATOR opr, TYPE_ID mtyp, TYPE_ID lhs_mtyp, TYPE_ID rhs_mtyp,
               WN *lhs, WN *rhs) {
  WN *ret = NULL;

  if (MTYPE_size_min(rhs_mtyp) > MTYPE_size_min(lhs_mtyp) &&
      ! Has_subsumed_cvtl(WN_operator(lhs)))
    lhs = WN_CreateCvtl(OPR_CVTL, Widen_mtype(lhs_mtyp), MTYPE_V,
                        MTYPE_size_min(lhs_mtyp), lhs);
  if (MTYPE_size_min(lhs_mtyp) > MTYPE_size_min(rhs_mtyp) &&
      ! Has_subsumed_cvtl(WN_operator(rhs)))
    rhs = WN_CreateCvtl(OPR_CVTL, Widen_mtype(rhs_mtyp), MTYPE_V,
                        MTYPE_size_min(rhs_mtyp), rhs);

  if (MTYPE_is_integral(mtyp) && MTYPE_is_integral(lhs_mtyp) &&
      MTYPE_size_min(mtyp) > MTYPE_size_min(lhs_mtyp))
    lhs = WN_CreateCvtl(OPR_CVTL, Widen_mtype(lhs_mtyp), MTYPE_V,
                        MTYPE_size_min(lhs_mtyp), lhs);

  if (MTYPE_is_integral(mtyp) && MTYPE_is_integral(rhs_mtyp) &&
      MTYPE_size_min(mtyp) > MTYPE_size_min(rhs_mtyp))
    rhs = WN_CreateCvtl(OPR_CVTL, Widen_mtype(rhs_mtyp), MTYPE_V,
                        MTYPE_size_min(rhs_mtyp), rhs);

  ret = WN_CreateExp2(opr, Widen_mtype(mtyp),
                      Widen_mtype(lhs_mtyp), lhs, rhs);
  return ret;
}

// generate wn for BO_Add/BO_Sub/BO_AddAssign/BO_SubAssign
static WN *
Gen_wn_for_add_and_sub(BinaryOperatorKind opcode, TY_IDX ty_idx, TY_IDX lhs_ty, TY_IDX rhs_ty,
                       WN *lhs, WN *rhs) {
  Is_True((lhs != NULL && rhs != NULL),
          ("WN for Gen_wn_for_add_and_sub can not be NULL"));
  Is_True(ty_idx != (TY_IDX)0 && lhs_ty != (TY_IDX)0 && rhs_ty != (TY_IDX)0,
          ("TY for Gen_wn_for_add_and_sub can not be NULL"));
  Is_True(opcode == BO_Add || opcode == BO_AddAssign ||
          opcode == BO_Sub || opcode == BO_SubAssign,
          ("invalid opcode for Gen_wn_for_add_and_sub"));

  WN *ret = NULL;

  bool is_add = false;
  if (opcode == BO_Add || opcode == BO_AddAssign)
    is_add = true;

  // computing the difference between two pointers
  if (!is_add && TY_kind(lhs_ty) == KIND_POINTER &&
                 TY_kind(rhs_ty) == KIND_POINTER &&
                 TY_kind(ty_idx) == KIND_SCALAR) {
    TY_IDX ty = TY_pointed(lhs_ty);
    Is_True(TY_IDX_index(ty) == TY_IDX_index(TY_pointed(rhs_ty)),
            ("should point to the same type"));

    TYPE_ID mtyp = TY_mtype(ty_idx);
    WN *diff = WN_Sub(mtyp, lhs, rhs);
    // don't generate DIV for void ty
    if (TY_size(ty) == 0)
      return diff;

    WN *size = WN_Intconst(mtyp, TY_size(ty));
    return WN_Div(mtyp, diff, size);
  }

  // adjust offset for pointer
  if (TY_kind(ty_idx) == KIND_POINTER) {
    WN *base, *adjust;
    if (TY_kind(lhs_ty) == KIND_SCALAR) {
      adjust = lhs;
      base = rhs;
    } else if (TY_kind(rhs_ty) == KIND_SCALAR) {
      adjust = rhs;
      base = lhs;
    } else
      Is_True(false, ("unsupported whirl node"));

    Is_True(OPERATOR_is_expression(WN_operator(base)) &&
            MTYPE_is_integral(WN_rtype(base)) &&
            MTYPE_byte_size(WN_rtype(base)) == MTYPE_byte_size(Pointer_Mtype),
            ("bad whirl node or rtype"));

    TY_IDX pointed_ty = TY_pointed(ty_idx);
    // a fix for void pointed type: such as: (void *)c + 4
    UINT64 ty_size = TY_mtype(pointed_ty) == MTYPE_V ? 1 : TY_size(pointed_ty);
    if (WN_operator(adjust) != OPR_INTCONST) {
      WN *ofst = ty_size == 1 ? adjust
                              : WN_Mpy(WN_rtype(base), adjust,
                                       WN_Intconst(WN_rtype(base), ty_size));
      lhs = base;
      rhs = ofst;
      // fall through to generate binary operator
    } else {
      UINT64 ofst_value = WN_const_val(adjust) * ty_size;
      lhs = base;
      rhs = WN_Intconst(WN_rtype(base), ofst_value);
      // fall through to generate binary operator
    }
  }

  ret = is_add ?
        WN_Add(WN_rtype(lhs), lhs, rhs) :
        WN_Sub(WN_rtype(lhs), lhs, rhs);

  return ret;
}

Result
WhirlExprBuilder::ConvertCXXMFPFromFA(const UnaryOperator* expr, INT64 ofst) {
  const MemberPointerType *mpt = expr->getType()->getAs<MemberPointerType>();
  Is_True(mpt != NULL && mpt->isMemberFunctionPointer(),
          ("not member function pointer"));
  Is_True(expr->getOpcode() == UO_AddrOf &&
          isa<DeclRefExpr>(expr->getSubExpr()),
          ("not function decl ref"));
  Result r = ConvertExpr(expr);
  Is_True(r.isSym() && r.IsAddrOf() &&
          TY_kind(r.Ty()) == KIND_FUNCTION,
          ("not function type"));
  const Decl *decl = cast<DeclRefExpr>(expr->getSubExpr())->getDecl();
  Is_True(decl != NULL && isa<CXXMethodDecl>(decl),
          ("not cxx method decl"));
  ST *tmp_st = New_ST(CURRENT_SYMTAB);
  TY_IDX ty = _builder->TB().ConvertType(expr->getType());
  // TODO: have a unique mfp ty_idx
  Is_True(TY_kind(ty) == KIND_STRUCT &&
          TY_size(ty) == 2 * MTYPE_byte_size(Pointer_Mtype),
          ("not member function type"));
  ST_Init(tmp_st, Save_Str(".mpt.init"), CLASS_VAR,
          CURRENT_SYMTAB == GLOBAL_SYMTAB ? SCLASS_FSTATIC : SCLASS_AUTO,
          EXPORT_LOCAL, ty);
  const CXXMethodDecl *method = cast<CXXMethodDecl>(decl);
  WN *stid;
  // function address or offset in vtable
  if (method->isVirtual()) {
    // virtual function, store offset in vtable + 1 into first field
    ASTContext *ast_c = _builder->Context();
    ItaniumVTableContext *vt_ctx
        = cast<ItaniumVTableContext>(ast_c->getVTableContext());
    const clang::CXXDestructorDecl* dtor
        = dyn_cast<clang::CXXDestructorDecl>(method);
    UINT64 vofst;
    if (dtor)
      vofst = vt_ctx->getMethodVTableIndex(GlobalDecl(dtor, CXXDtorType::Dtor_Complete));
    else
      vofst = vt_ctx->getMethodVTableIndex(GlobalDecl(method));
    vofst *= MTYPE_byte_size(Pointer_Mtype);
    stid = WN_Stid(Pointer_Mtype, 0, tmp_st, ty,
                   WN_Intconst(Pointer_Mtype, vofst + 1), 1);
  }
  else {
    // non-virtual function, store function address to first field
    stid = WN_Stid(Pointer_Mtype, 0, tmp_st, ty,
                   r.GetRValue(), 1);
  }
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
  WN_Set_Linenum(stid, GetSrcPos());
  // store offset to this to second field
  stid = WN_Stid(MTYPE_I8, 8, tmp_st, ty,
                 WN_Intconst(MTYPE_I8, ofst), 2);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
  WN_Set_Linenum(stid, GetSrcPos());
  // generate mldid
  WN *mldid = WN_Ldid(MTYPE_M, 0, tmp_st, ty);
  return Result::nwNode(mldid, ty);
}

Result
WhirlExprBuilder::ConvertCXXMFPFromWhirl(WN *value, TY_IDX ty, INT64 ofst) {
  // TODO: have a unique mfp ty_idx
  Is_True(TY_kind(ty) == KIND_STRUCT &&
          TY_size(ty) == 2 * MTYPE_byte_size(Pointer_Mtype),
          ("not member function type"));
  // create a temp
  ST *tmp_st = New_ST(CURRENT_SYMTAB);
  ST_Init(tmp_st, Save_Str(".mpt.temp"), CLASS_VAR,
          CURRENT_SYMTAB == GLOBAL_SYMTAB ? SCLASS_FSTATIC : SCLASS_AUTO,
          EXPORT_LOCAL, ty);
  Set_ST_is_temp_var(tmp_st);
  // store to temp
  WN *stid = WN_Stid(MTYPE_M, 0, tmp_st, ty, value, 0);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
  WN_Set_Linenum(stid, GetSrcPos());
  // adjust offset
  WN *ldid = WN_Ldid(MTYPE_I8, 8, tmp_st, ty, 2);
  WN *add = WN_Add(MTYPE_I8,
                   WN_Ldid(MTYPE_I8, 8, tmp_st, ty, 2),
                   WN_Intconst(MTYPE_I8, ofst));
  stid = WN_Stid(MTYPE_I8, 8, tmp_st, ty, add, 2);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
  WN_Set_Linenum(stid, GetSrcPos());
  // generate mldid
  WN *mldid = WN_Ldid(MTYPE_M, 0, tmp_st, ty);
  return Result::nwNode(mldid, ty);
}

// compare member function pointer
Result
HandleBinaryOperatorForMemberFunctionPointer(BinaryOperatorKind opcode,
                                             TY_IDX ty_idx, TY_IDX lhs_ty, TY_IDX rhs_ty,
                                             WN *lhs, WN *rhs, SRCPOS spos) {
  Is_True(opcode == BO_EQ || opcode == BO_NE,
          ("wrong opcode"));
  Is_True(TY_kind(lhs_ty) == KIND_STRUCT &&
          TY_kind(rhs_ty) == KIND_STRUCT,
          ("ty is not struct"));
  Is_True(WN_rtype(lhs) == MTYPE_M &&
          WN_rtype(rhs) == MTYPE_M,
          ("not mtype m"));
  TYPE_ID mtyp = TY_mtype(ty_idx);
  WN *ret = NULL;

  if ((WN_operator(lhs) == OPR_LDID ||
       WN_operator(lhs) == OPR_ILOAD) &&
      (WN_operator(rhs) == OPR_LDID ||
       WN_operator(rhs) == OPR_ILOAD)) {
    // get the first element of lhs
    TYPE_ID lhs_mtyp = Pointer_Mtype;
    WN_set_rtype(lhs, Pointer_Mtype);
    WN_set_desc(lhs, Pointer_Mtype);
    WN_set_field_id(lhs, WN_field_id(lhs) + 1);
    // get the first element of rhs
    TYPE_ID rhs_mtyp = Pointer_Mtype;
    WN_set_rtype(rhs, Pointer_Mtype);
    WN_set_desc(rhs, Pointer_Mtype);
    WN_set_field_id(rhs, WN_field_id(rhs) + 1);

    // only compare the first element
    if (opcode == BO_EQ)
      ret = Gen_wn_for_cmp(OPR_EQ, mtyp,
                           lhs_mtyp, rhs_mtyp, lhs, rhs);
    else
      ret = Gen_wn_for_cmp(OPR_NE, mtyp,
                           lhs_mtyp, rhs_mtyp, lhs, rhs);
  } else {
    Is_True(WN_operator(lhs) == OPR_COMMA ||
            WN_operator(rhs) == OPR_COMMA,
            ("invalid wn"));
    WN *blk = WN_CreateBlock();
    // store lhs comma to temp
    if (WN_operator(lhs) == OPR_COMMA) {
      ST *temp = Gen_Temp_Symbol(lhs_ty, ".temp_save");
      WN *stid = WN_Stid(TY_mtype(lhs_ty), 0, temp, lhs_ty, lhs);
      WN_Set_Linenum(stid, spos);
      WN_INSERT_BlockLast(blk, stid);
      lhs = WN_Ldid(TY_mtype(lhs_ty), 0, temp, lhs_ty);
    }
    // store rhs comma to temp
    if (WN_operator(rhs) == OPR_COMMA) {
      ST *temp = Gen_Temp_Symbol(rhs_ty, ".temp_save");
      WN *stid = WN_Stid(TY_mtype(rhs_ty), 0, temp, rhs_ty, rhs);
      WN_Set_Linenum(stid, spos);
      WN_INSERT_BlockLast(blk, stid);
      rhs = WN_Ldid(TY_mtype(rhs_ty), 0, temp, rhs_ty);
    }
    TYPE_ID fld1_mtyp = Pointer_Mtype;
    TYPE_ID fld2_mtyp = MTYPE_I8;
    // get the first element of lhs
    WN *lhs_kid1 = WN_COPY_Tree(lhs);
    WN_set_rtype(lhs_kid1, fld1_mtyp);
    WN_set_desc(lhs_kid1, fld1_mtyp);
    WN_set_field_id(lhs_kid1, WN_field_id(lhs_kid1) + 1);
    // get the second element of lhs
    WN *lhs_kid2 = WN_COPY_Tree(lhs);
    WN_set_rtype(lhs_kid2, fld2_mtyp);
    WN_set_desc(lhs_kid2, fld2_mtyp);
    WN_set_field_id(lhs_kid2, WN_field_id(lhs_kid2) + 2);
    // get the first element of rhs
    WN *rhs_kid1 = WN_COPY_Tree(rhs);
    WN_set_rtype(rhs_kid1, fld1_mtyp);
    WN_set_desc(rhs_kid1, fld1_mtyp);
    WN_set_field_id(rhs_kid1, WN_field_id(rhs_kid1) + 1);
    // get the second element of rhs
    WN *rhs_kid2 = WN_COPY_Tree(rhs);
    WN_set_rtype(rhs_kid2, fld2_mtyp);
    WN_set_desc(rhs_kid2, fld2_mtyp);
    WN_set_field_id(rhs_kid2, WN_field_id(rhs_kid2) + 2);

    WN *wn = NULL;
    if (opcode == BO_EQ) {
      // check L1 == R2 && (L2 == R2 || L1 == 0)
      WN *cmp1 = WN_EQ(MTYPE_U4, lhs_kid1, rhs_kid1);
      WN *cmp2 = WN_EQ(MTYPE_U4, lhs_kid2, rhs_kid2);
      wn = WN_CAND(cmp1, WN_CIOR(cmp2,
                                 WN_EQ(MTYPE_U4, WN_COPY_Tree(lhs_kid1),
                                       WN_Intconst(fld1_mtyp, 0))));
    } else {
      // check L1 != R2 || (L2 != R2 && L1 != 0)
      WN *cmp1 = WN_NE(MTYPE_U4, lhs_kid1, rhs_kid1);
      WN *cmp2 = WN_NE(MTYPE_U4, lhs_kid2, rhs_kid2);
      wn = WN_CIOR(cmp1, WN_CAND(cmp2,
                                 WN_NE(MTYPE_U4, WN_COPY_Tree(lhs_kid1),
                                       WN_Intconst(fld1_mtyp, 0))));
    }
    ret = WN_CreateComma(OPR_COMMA, WN_rtype(wn), MTYPE_V, blk, wn);
  }
  return Result::nwNode(ret, ty_idx);
}

// update incomplete pointee ty
void
WhirlExprBuilder::Update_pointee_ty(QualType type, TY_IDX ty_idx) {
  if (TY_kind(ty_idx) == KIND_POINTER &&
      TY_is_incomplete(TY_pointed(ty_idx))) {
    if (type->isPointerType() || type->isReferenceType())
      _builder->TB().ConvertType(type->getPointeeType());
    else
      _builder->TB().ConvertType(type);
  }
}

Result
WhirlExprBuilder::ConvertBinaryOperator(const BinaryOperator *expr, BOOL retv) {
  TRACE_FUNC();
  ST *st;
  WN *wn, *ret;

  Expr *lhs_expr = expr->getLHS();
  Expr *rhs_expr = expr->getRHS();

  QualType lhs_type = lhs_expr->getType();
  QualType rhs_type = rhs_expr->getType();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  TY_IDX lhs_ty = _builder->TB().ConvertType(lhs_type);
  TY_IDX rhs_ty = _builder->TB().ConvertType(rhs_type);

  TYPE_ID mtyp = TY_mtype(ty_idx);
  TYPE_ID lhs_mtyp = TY_mtype(lhs_ty);
  TYPE_ID rhs_mtyp = TY_mtype(rhs_ty);
  RV rv = GetRV(expr, retv);
  SRCPOS spos = SetSrcPos(getLocation(expr));

  BinaryOperatorKind opcode = expr->getOpcode();
  if (opcode == clang::BO_Assign) {
    // assign is special, handle it here
    Result lhs_r = ConvertExpr(lhs_expr);
    Result rhs_r = Result::nwNone();
    SetSrcPos(getLocation(expr));
    if (lhs_type->isMemberPointerType() &&
        lhs_type->getAs<MemberPointerType>()->isMemberFunctionPointer() &&
        isa<UnaryOperator>(rhs_expr) &&
        cast<UnaryOperator>(rhs_expr)->getOpcode() == UO_AddrOf) {
      Is_True(rhs_type->isMemberPointerType() &&
              rhs_type->getAs<MemberPointerType>()->isMemberFunctionPointer(),
              ("rhs not member function pointer"));
      rhs_r = ConvertCXXMFPFromFA(cast<UnaryOperator>(rhs_expr), 0);
    }
    else {
      rhs_r = ConvertExpr(rhs_expr);
    }
    Result asgn = EmitAssignNode(lhs_r, rhs_r, ty_idx, rv);
    return asgn;
  }

  BOOL lhs_retv = opcode == clang::BO_Comma ? FALSE : TRUE;
  WN *lhs = ConvertToNode(lhs_expr, Result::nwNone(), lhs_retv);
  lhs = Get_real_wn(lhs, lhs_expr, lhs_ty);
  WN *rhs = ConvertToNode(rhs_expr, Result::nwNone(), TRUE);
  rhs = Get_real_wn(rhs, rhs_expr, rhs_ty);
  if (expr->isGLValue())
    ty_idx = Make_Pointer_Type(ty_idx);

  // handle complex type
  if (expr->getType()->isComplexType()) {
    if (!lhs_type->isComplexType()) {
      // get zero value
      WN *zero = Gen_null_const(lhs_ty);
      lhs = WN_Binary(OPR_PAIR, mtyp, lhs, zero);
    } else if (!rhs_type->isComplexType()) {
      WN *zero = Gen_null_const(rhs_ty);
      rhs = WN_Binary(OPR_PAIR, mtyp, rhs, zero);
    }
  }
  if (lhs_type->isMemberPointerType() &&
      lhs_type->getAs<MemberPointerType>()->isMemberFunctionPointer() &&
      (opcode == clang::BO_EQ || opcode == clang::BO_NE)) {
    Is_True(rhs_type->isMemberPointerType(), ("not member pointer"));
    // compare member function pointer
    return HandleBinaryOperatorForMemberFunctionPointer(opcode, ty_idx,
                                                        lhs_ty, rhs_ty,
                                                        lhs, rhs, spos);
  }

  switch (opcode) {
    case clang::BO_Add:
    case clang::BO_Sub:
    {
      Update_pointee_ty(lhs_type, lhs_ty);
      Update_pointee_ty(rhs_type, rhs_ty);
      ret = Gen_wn_for_add_and_sub(opcode, ty_idx, lhs_ty,
                                   rhs_ty, lhs, rhs);
      break;
    }
    case clang::BO_Mul:
      ret = WN_Mpy(mtyp, lhs, rhs);
      break;
    case clang::BO_Div:
      ret = WN_Div(mtyp, lhs, rhs);
      break;
    case clang::BO_Rem:
      ret = WN_Binary(OPR_REM, mtyp, lhs, rhs);
      break;
    case clang::BO_Shl:
      ret = WN_Shl(mtyp, lhs, rhs);
      break;
    case clang::BO_Shr:
      if (MTYPE_signed(mtyp))
        ret = WN_Ashr(mtyp, lhs, rhs);
      else
        ret = WN_Lshr(mtyp, lhs, rhs);
      break;
    case clang::BO_LT:
      ret = Gen_wn_for_cmp(OPR_LT, mtyp,
                           lhs_mtyp, rhs_mtyp, lhs, rhs);
      break;
    case clang::BO_GT:
      ret = Gen_wn_for_cmp(OPR_GT, mtyp,
                           lhs_mtyp, rhs_mtyp, lhs, rhs);
      break;
    case clang::BO_LE:
      ret = Gen_wn_for_cmp(OPR_LE, mtyp,
                           lhs_mtyp, rhs_mtyp, lhs, rhs);
      break;
    case clang::BO_GE:
      ret = Gen_wn_for_cmp(OPR_GE, mtyp,
                           lhs_mtyp, rhs_mtyp, lhs, rhs);
      break;
    case clang::BO_EQ:
      ret = Gen_wn_for_cmp(OPR_EQ, mtyp,
                           lhs_mtyp, rhs_mtyp, lhs, rhs);
      break;
    case clang::BO_NE:
      ret = Gen_wn_for_cmp(OPR_NE, mtyp,
                           lhs_mtyp, rhs_mtyp, lhs, rhs);
      break;
    case clang::BO_And:
      ret = WN_Band(mtyp, lhs, rhs);
      break;
    case clang::BO_Xor:
      ret = WN_Bxor(mtyp, lhs, rhs);
      break;
    case clang::BO_Or:
      ret = WN_Bior(mtyp, lhs, rhs);
      break;
    case clang::BO_LAnd:
    case clang::BO_LOr:
    {
      lhs = Handle_cond_wn(lhs);
      rhs = Handle_cond_wn(rhs);

      if (opcode == BO_LAnd)
        ret = WN_CAND(lhs, rhs);
      else
        ret = WN_CIOR(lhs, rhs);
      if (WN_operator(ret) != OPR_CAND &&
          WN_operator(ret) != OPR_CIOR) {
        TYPE_ID ret_mtyp = WN_rtype(ret);
        ret = WN_NE(ret_mtyp, ret,
                    Gen_null_const(MTYPE_To_TY(ret_mtyp)));
      }
      break;
    }
    case clang::BO_Comma:
      // insert lhs to current block
      if (lhs) {
        if (WN_operator(lhs) == OPR_COMMA)
          lhs = WN_kid0(lhs);
        else if (OPERATOR_is_expression(WN_operator(lhs))) {
          lhs = WN_CreateEval(lhs);
          WN_Set_Linenum(lhs, GetSrcPos());
        }
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), lhs);
      }
      if (!rhs)
        return Result::nwNone();
      // insert rhs stmt to current block
      if (!OPERATOR_is_expression(WN_operator(rhs))) {
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(),
                            rhs);
        return Result::nwNone();
      }
      ret = rhs;
      break;
    case clang::BO_Cmp:
      Is_True(false, ("TODO: unsupported BO_Cmp/BO_PtrMemD"));
    case clang::BO_PtrMemD:
      {
        Is_True(TY_kind(ty_idx) == KIND_POINTER, ("not pointer type"));
        ret = WN_Add(Pointer_Mtype, lhs, rhs);
        Result r = Result::nwNode(ret, ty_idx);
        r.SetDeref();
        return r;
      }
      break;
    case clang::BO_PtrMemI:
      Is_True(TY_kind(ty_idx) == KIND_POINTER, ("not pointer type"));
      ret = WN_Add(Pointer_Mtype, lhs, rhs);
      break;
    default:
      Is_True(false, ("unsupport opcode for BinaryOperator"));
  }
  Result r = Result::nwNode(ret, ty_idx);
  return r;
}

void
WhirlExprBuilder::SetCallParm(const clang::CallExpr *expr, WN *wn, int &start_offset) {
  int parm_idx = start_offset;
  const FunctionDecl *callee = expr->getDirectCallee();
  for (const Expr *args : expr->arguments()) {
    int i = parm_idx - start_offset;
    WN *arg_wn = ConvertToNode(args);
    TY_IDX arg_ty_idx = _builder->TB().ConvertType(args->getType());
    if (args->isGLValue())
      arg_ty_idx = Make_Pointer_Type(arg_ty_idx);

    // if parm is a function call, insert call wn into current block
    // and then generate ldid wn from WN_kid0 of call wn.
    if (WN_operator(arg_wn) == OPR_CALL) {
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), arg_wn);
      WN *node = WN_kid0(WN_kid0(arg_wn));
      Is_True(WN_operator(node) == OPR_LDA, ("invalid whirl node"));
      ST *tmp_st = WN_st(node);
      TY_IDX tmp_ty = ST_type(tmp_st);
      arg_wn = WN_Ldid(TY_mtype(tmp_ty), 0, tmp_st, tmp_ty);
    } else if (WN_operator(arg_wn) == OPR_ICALL) {
      WN *blk = WN_CreateBlock();
      WN_INSERT_BlockLast(blk, arg_wn);
      WN *ret = WN_Ldid(TY_mtype(arg_ty_idx), -1, Return_Val_Preg, arg_ty_idx);
      arg_wn = WGEN_CreateComma(WN_rtype(ret), blk, ret);
    }

    Is_True(arg_ty_idx != (TY_IDX) 0 && arg_wn != NULL, ("arg is null"));

    TYPE_ID arg_mtype = TY_mtype(arg_ty_idx);
    if (arg_mtype == MTYPE_M)
      arg_mtype = WN_rtype(arg_wn);
    WN *parm = WGEN_CreateParm(Mtype_comparison(arg_mtype), arg_wn, arg_ty_idx);
    WN_kid(wn, parm_idx++) = parm;
  }
  start_offset = parm_idx;
}

// handle Base of MemberExpr
// this function will be called by ConvertCallExpr
void
WhirlExprBuilder::EmitCallee(const Expr *expr) {
  expr = expr->IgnoreParens();
  if (const ImplicitCastExpr *cast_expr = dyn_cast<ImplicitCastExpr>(expr)) {
    if (cast_expr->getCastKind() == CK_FunctionToPointerDecay ||
        cast_expr->getCastKind() == CK_BuiltinFnToFnPtr)
      EmitCallee(cast_expr->getSubExpr());
  } else if (const MemberExpr *mem_expr = dyn_cast<MemberExpr>(expr)) {
    if (isa<FunctionDecl>(mem_expr->getMemberDecl())) {
      // emit code to compute the specified expression, ignoring the result
      WN *ret = ConvertToNode(mem_expr->getBase());
      if (OPERATOR_is_expression(WN_operator(ret)))
        ret = WN_CreateEval(ret);
      WN_Set_Linenum(ret, GetSrcPos());
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);
    }
  }
}

Result
WhirlExprBuilder::ConvertCallExpr(const CallExpr *expr, BOOL retv) {
  TRACE_FUNC();
  const FunctionDecl *callee = expr->getDirectCallee();
  WN *call_wn = NULL;
  WN *ld_wn = NULL;
  TY_IDX ret_ty_idx;

  // generate parm for implicit object
  const auto *cxx_call_expr = dyn_cast<CXXMemberCallExpr>(expr);
  const Expr *impl_obj = cxx_call_expr ? cxx_call_expr->getImplicitObjectArgument() : NULL;
  UINT num_args = expr->getNumArgs();
  num_args = impl_obj ? num_args + 1 : num_args;

  // check if MemberExpr is Arrow
  bool mem_expr_is_arrow = false;
  if (cxx_call_expr) {
    const MemberExpr *mem_expr =
      dyn_cast<MemberExpr>(cxx_call_expr->getCallee());
    Is_True(mem_expr, ("invalid MemberExpr"));
    if (mem_expr->isArrow())
      mem_expr_is_arrow = true;
  } else {
    EmitCallee(expr->getCallee());
  }

  bool indirect_call = false;
  if (callee != NULL) {
    // defer method expr if not exist
    if (isa<CXXMethodDecl>(callee)) {
      const CXXMethodDecl *cxx_callee = cast<CXXMethodDecl>(callee);
      if (cxx_callee->isTrivial() &&
          (cxx_callee->isCopyAssignmentOperator() ||
           cxx_callee->isMoveAssignmentOperator())) {
        Is_True(impl_obj && num_args == 2,
                ("wrong arg num for copy/move assignment"));
        Result lhs = ConvertExpr(impl_obj);
        Result rhs = ConvertExpr(expr->getArg(0));
        TY_IDX ty = _builder->TB().ConvertType(expr->getType());
        // the following codes are needed for copy/move assignment:
        // set deref flag for lhs if it is RValue
        // and get rvalue for rhs
        if (isa<CXXMemberCallExpr>(expr)) {
          if (lhs.IsRValue())
            lhs.SetDeref();
          rhs = rhs.ConvertToRValue(ty);
        }

        RV rv = GetRV(expr, retv);
        return EmitAssignNode(lhs, rhs, ty, rv);
      }
      else if (cxx_callee->isTrivial() && isa<CXXDestructorDecl>(callee))
        return Result::nwNone();
      Is_True(!cxx_callee->isTrivial(), ("callee is trivial"));
    }

    unsigned builtin_id = callee->getBuiltinID();
    if (builtin_id != 0) {
      Result r = ConvertBuiltinExpr(expr, callee, builtin_id, retv);
      if (r.isNode())
        return r;
      // fall through to generate normal call
    }

    if (const CXXDestructorDecl *dtor = dyn_cast<CXXDestructorDecl>(callee)) {
      if (dtor->getParent()->hasTrivialDestructor())
        return Result::nwNone();  // do nothing for trivial dtor
    }

    GlobalDecl gd = GetGlobalDecl(callee);
    ST_IDX ce_st = _builder->Get_func_st(gd);
    Is_True(ce_st != ST_IDX_ZERO, ("bad st"));

    // get right return type from callee type
    ret_ty_idx = TY_ret_type(ST_pu_type(ce_st));
    call_wn = WN_Create(OPR_CALL, retv ? TY_mtype(ret_ty_idx) : MTYPE_V, MTYPE_V, num_args);
    WN_Set_Linenum(call_wn, SetSrcPos(getLocation(expr)));
    WN_st_idx(call_wn) = ce_st;
    WN_Set_Call_Default_Flags(call_wn);
    if (!_builder->DeclBuilder().Call_nothrow(callee))
      Mark_call_region(call_wn);
  } else {
    indirect_call = true;
    const Expr *ind_callee = expr->getCallee();
    ld_wn = ConvertToNode(ind_callee);
    if (ld_wn == NULL) {
      Is_True(ind_callee->getStmtClass() == Expr::CXXPseudoDestructorExprClass,
              ("not CXXPseudoDestructorExpr"));
      return Result::nwNone();
    }
    Is_True(OPERATOR_is_expression(WN_operator(ld_wn)) &&
            WN_rtype(ld_wn) == Pointer_Mtype, ("invalid wn"));

    QualType func_type;
    if (auto ptrType = ind_callee->getType()->getAs<PointerType>()) {
      func_type = ptrType->getPointeeType();
    } else {
      func_type = ind_callee->getType();
    }
    Is_True(func_type->isFunctionType(), ("invalid function type"));
    TY_IDX rty = _builder->TB().ConvertType(func_type);
    ret_ty_idx = _builder->TB().ConvertType(expr->getType());
    if (expr->isGLValue())
      ret_ty_idx = Make_Pointer_Type(ret_ty_idx);
    call_wn = WN_Icall(retv ? TY_mtype(ret_ty_idx) : MTYPE_V, MTYPE_V,
                       expr->getNumArgs() + 1, rty);
    WN_Set_Linenum(call_wn, SetSrcPos(getLocation(expr)));
    WN_Set_Call_Default_Flags(call_wn);
    if (!_builder->DeclBuilder().Call_nothrow(expr->getCalleeDecl()))
      Mark_call_region(call_wn);
  }
  Is_True(call_wn != NULL, ("missing call whirl node"));

  int  parm_idx = 0;
  if(impl_obj) {
    Result r = ConvertExpr(impl_obj);
    WN *impl_node = r.GetLValue();
    TY_IDX ty_idx = _builder->TB().ConvertType(impl_obj->getType());
    if (TY_kind(ty_idx) != KIND_POINTER) {
      Is_True(TY_kind(ty_idx) == KIND_STRUCT, ("ty is not struct"));
      ty_idx = Make_Pointer_Type(ty_idx);
    }
    else {
      Is_True(TY_kind(TY_pointed(ty_idx)) == KIND_STRUCT, ("ty pointee is not struct"));
    }
    WN *impl_parm = WGEN_CreateParm(Mtype_comparison(TY_mtype(ty_idx)), impl_node, ty_idx);
    WN_kid(call_wn, parm_idx++) = impl_parm;
  }

  SetCallParm(expr, call_wn, parm_idx);

  // handle indirect call
  if (indirect_call) {
    WN_kid(call_wn, parm_idx) = ld_wn;
  }

  if (retv && !expr->getType()->isVoidType()) {
    ret_ty_idx = _builder->TB().ConvertType(expr->getType());
    TY_IDX ret_obj_idx = ret_ty_idx;
    if (expr->isGLValue())
      ret_ty_idx = Make_Pointer_Type(ret_ty_idx);
    TYPE_ID ret_ty = TY_mtype(ret_ty_idx);
    WN *blk = WN_CreateBlock();
    WN_INSERT_BlockLast(blk, call_wn);
    WN *ret = WN_Ldid(TY_mtype(ret_ty_idx), -1, Return_Val_Preg, ret_ty_idx);
    WN *comma = WGEN_CreateComma(WN_rtype(ret), blk, ret);

    Result r = Result::nwNode(comma, ret_ty_idx);
    if (!expr->isGLValue())
      r.SetRValue();
    else
      r.SetRef();
    return r;
  }
  else {
    return Result::nwNode(call_wn, 0);
  }
}

Result
WhirlExprBuilder::ConvertChooseExpr(const ChooseExpr *expr) {
  TRACE_FUNC();
  return ConvertExpr(expr->getChosenSubExpr());
}

Result
WhirlExprBuilder::ConvertConvertVectorExpr(const ConvertVectorExpr *expr) {
  WN *src = ConvertToNode(expr->getSrcExpr());
  Is_True(src && OPERATOR_is_expression(WN_operator(src)),
          ("bad src wn"));
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  // TODO: support convert vector in backend
  WN *dst = WN_Tas(TY_mtype(ty_idx), ty_idx, src);
  return Result::nwNode(dst, ty_idx);
}

Result
WhirlExprBuilder::ConvertCompoundAssignOperator(const CompoundAssignOperator *expr, BOOL retv) {
  TRACE_FUNC();

  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  TYPE_ID mtyp = TY_mtype(ty_idx);

  TY_IDX compute_ty = _builder->TB().ConvertType(expr->getComputationLHSType());
  TYPE_ID comp_mtyp = TY_mtype(compute_ty);

  TY_IDX result_ty = _builder->TB().ConvertType(expr->getComputationResultType());

  Result lhs_r = ConvertExpr(expr->getLHS());
  if (lhs_r.isNode() && WN_hasReturnValueLDID(lhs_r.Node())) {
    WN *lhs_wn = Handle_expr_for_copy(WhirlBlockUtil::getCurrentBlock(),
                                      lhs_r.Node(), lhs_r.Ty(), GetSrcPos());
    if (lhs_wn != lhs_r.Node()) {
      UINT32 fid_id = lhs_r.FieldId();
      lhs_r = Result::nwNode(lhs_wn, lhs_r.Ty());
      lhs_r.SetFieldId(fid_id);
     }
  }
  Result lhs_rhs_r = lhs_r.ConvertToRValue(ty_idx);
  WN *lhs = lhs_rhs_r.GetRValue();
  WN *rhs = ConvertToNode(expr->getRHS());

  TY_IDX lhs_ty = _builder->TB().ConvertType(expr->getLHS()->getType());
  TY_IDX rhs_ty = _builder->TB().ConvertType(expr->getRHS()->getType());

  // check if need convert for lhs
  WN *cvt_lhs;
  if (!lhs_r.isNode()) {
    cvt_lhs = WN_Type_Conversion(WN_COPY_Tree(lhs), comp_mtyp);
  } else {
    cvt_lhs = WN_Type_Conversion(lhs, comp_mtyp);
    lhs_r.SetNode(WN_COPY_Tree_Without_Commas(lhs_r.Node()));
  }

  // check if need convert for rhs
  TYPE_ID rhs_mtyp = TY_mtype(rhs_ty);
  if (MTYPE_is_complex(comp_mtyp) && !MTYPE_is_complex(rhs_mtyp)) {
    // check if scalar -> complex convert
    Is_True(Mtype_complex_to_real(comp_mtyp) == rhs_mtyp,
            ("invalid scalar to complex convert"));
    rhs = WN_Binary(OPR_PAIR, comp_mtyp,
                    rhs,
                    WN_Floatconst(rhs_mtyp, 0.0));
  }
  else {
    rhs = WN_Type_Conversion(rhs, comp_mtyp);
  }

  WN *ret = NULL, *wn = NULL;
  BinaryOperatorKind opcode = expr->getOpcode();

  switch (opcode) {
  case BO_MulAssign:
    wn = WN_Mpy(comp_mtyp, cvt_lhs, rhs);
    break;
  case BO_DivAssign:
    wn = WN_Div(comp_mtyp, cvt_lhs, rhs);
    break;
  case BO_RemAssign: {
    wn = WN_Binary(OPR_REM, comp_mtyp, cvt_lhs, rhs);
    break;
  }
  case BO_ShlAssign:
    wn = WN_Shl(comp_mtyp, cvt_lhs, rhs);
    break;
  case BO_ShrAssign:
    wn = WN_Binary(MTYPE_signed(comp_mtyp)? OPR_ASHR : OPR_LSHR,
                   comp_mtyp, cvt_lhs, rhs);
    break;
  case BO_AddAssign:
  case BO_SubAssign:
  {
    Update_pointee_ty(expr->getLHS()->getType(), lhs_ty);
    Update_pointee_ty(expr->getRHS()->getType(), rhs_ty);
    wn = Gen_wn_for_add_and_sub(opcode, compute_ty, lhs_ty,
                                rhs_ty, cvt_lhs, rhs);
    break;
  }
  case BO_AndAssign:
    wn = WN_Band(comp_mtyp, cvt_lhs, rhs);
    break;
  case BO_XorAssign:
    wn = WN_Bxor(comp_mtyp, cvt_lhs, rhs);
    break;
  case BO_OrAssign:
    wn = WN_Bior(comp_mtyp, cvt_lhs, rhs);
    break;
  default:
    Is_True(false, ("invalid compound assignment type"));
  }

  wn = WN_Type_Conversion(wn, mtyp);
  SetSrcPos(getLocation(expr));
  RV rv = GetRV(expr, retv);
  Result asgn = EmitAssignNode(lhs_r, Result::nwNode(wn, rhs_ty), ty_idx, rv);
  return asgn;
}

// Generate initialized st for local var
static ST_IDX Gen_local_var_st(TY_IDX ty_idx) {
  Is_True(ty_idx != (TY_IDX)0, ("invalid ty idx"));
  ST *st = New_ST(CURRENT_SYMTAB);
  ST_Init(st, Save_Str(".init"), CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL, ty_idx);
  return ST_st_idx(st);
}

Result
WhirlExprBuilder::ConvertCompoundLiteralExpr(const CompoundLiteralExpr *expr, Result dest) {
  TRACE_FUNC();
  TY_IDX ty = _builder->TB().ConvertType(expr->getType());

  bool need_tmp_dest = false;
  if (dest.isNone()) {
    need_tmp_dest = true;
  } else {
    Is_True(dest.isSym(), ("invalid dest"));
    if (dest.Ty() != ty)
      need_tmp_dest = true;
  }

  if (need_tmp_dest) {
    TY_IDX ty = _builder->TB().ConvertType(expr->getType());
    ST_IDX st_idx = Gen_local_var_st(ty);
    dest = Result::nwSym(st_idx, ty);
  }

  Result ret = ConvertExpr(expr->getInitializer(), dest);
  return need_tmp_dest && ret.isNone() ? dest : ret;
}

Result
WhirlExprBuilder::ConvertConditionalOperator(const ConditionalOperator *expr, BOOL retv) {
  TRACE_FUNC();
  const Expr *cond_expr = expr->getCond();
  const Expr *true_expr = expr->getTrueExpr();
  const Expr *false_expr = expr->getFalseExpr();
  TY_IDX true_ty = _builder->TB().ConvertType(true_expr->getType());
  TY_IDX false_ty = _builder->TB().ConvertType(false_expr->getType());

  // convert cond
  WN *cond = ConvertToNode(cond_expr);
  cond = Handle_cond_wn(cond);
  Is_True(cond != NULL &&
          OPCODE_is_expression(WN_opcode(cond)) &&
          MTYPE_is_integral(WN_rtype(cond)), ("bad cond"));

  // convert true expr
  WN *true_blk = WhirlBlockUtil::nwBlock();
  WN *true_wn = ConvertToNode(true_expr);
  true_wn = Get_real_wn(true_wn, true_expr, true_ty);
  WhirlBlockUtil::popCurrentBlock();  // pop true_blk

  // convert false expr
  WN *false_blk = WhirlBlockUtil::nwBlock();
  WN *false_wn = ConvertToNode(false_expr);
  false_wn = Get_real_wn(false_wn, false_expr, false_ty);
  WhirlBlockUtil::popCurrentBlock();  // pop false_blk

  // convert type
  TY_IDX ty = _builder->TB().ConvertType(expr->getType());
  if (expr->isGLValue()) {
    Is_True(TY_mtype(ty) != MTYPE_V, ("void as lvalue"));
    ty = Make_Pointer_Type(ty);
  }

  // check if true_wn return the same type as ty
  if (TY_mtype(ty) != MTYPE_V && true_wn != NULL) {
    if (!OPCODE_is_expression(WN_opcode(true_wn))) {
      WN_INSERT_BlockLast(true_blk, true_wn);
      WN *ret = Gen_null_const(ty);
      true_wn = WGEN_CreateComma(WN_rtype(ret), true_blk, ret);
    } else if (WN_first(true_blk) != NULL) {
      WN *comma = WGEN_CreateComma(Mtype_comparison(TY_mtype(ty)), true_blk, true_wn);
      true_wn = comma;
    }
  }

  // check if false_wn return the same type as ty
  if (TY_mtype(ty) != MTYPE_V && false_wn != NULL) {
    if (!OPCODE_is_expression(WN_opcode(false_wn))) {
      WN_INSERT_BlockLast(false_blk, false_wn);
      WN *ret = Gen_null_const(ty);
      false_wn = WGEN_CreateComma(WN_rtype(ret), false_blk, ret);
    } else if (WN_first(false_blk) != NULL) {
      WN *comma = WGEN_CreateComma(Mtype_comparison(TY_mtype(ty)), false_blk, false_wn);
      false_wn = comma;
    }
  }

  true_wn = true_wn ? true_wn : true_blk;
  false_wn = false_wn ? false_wn : false_blk;

  Is_True(true_wn != NULL, ("invalid true wn"));
  Is_True(false_wn != NULL, ("invalid false wn"));

  WN *ret = NULL;
  // special handle of INTCONST
  if (WN_operator(cond) == OPR_INTCONST) {
    ret = WN_const_val(cond) ? true_wn : false_wn;
  }
  else if (TY_mtype(ty) != MTYPE_V) {
    // generate cselect
    ret = WGEN_CreateCselect(TY_mtype(ty), cond, true_wn, false_wn);
  }
  else {
    // generate if...then...else
    if (OPERATOR_is_expression(WN_operator(true_wn))) {
      true_wn = WN_CreateEval(true_wn);
      WN_Set_Linenum(true_wn, SetSrcPos(getEndLocation(true_expr)));
    }
    if (OPERATOR_is_expression(WN_operator(false_wn))) {
      false_wn = WN_CreateEval(false_wn);
      WN_Set_Linenum(false_wn, SetSrcPos(getEndLocation(false_expr)));
    }

    Is_True(!OPCODE_is_expression(WN_opcode(true_wn)) &&
            !OPCODE_is_expression(WN_opcode(false_wn)), ("bad stmt"));
    WN *then_blk = WN_CreateBlock();
    WN_INSERT_BlockLast(then_blk, true_wn);
    WN *else_blk = WN_CreateBlock();
    WN_INSERT_BlockLast(else_blk, false_wn);
    ret = WN_CreateIf(cond, then_blk, else_blk);
    WN_Set_Linenum(ret, SetSrcPos(getLocation(expr)));
  }
  Is_True(TY_mtype(ty) == MTYPE_V || !retv ||
          OPERATOR_is_expression(WN_operator(ret)),
          ("bad return expr"));

  if (!retv) {
    WN *blk = WhirlBlockUtil::getCurrentBlock();
    if (OPERATOR_is_expression(WN_operator(ret))) {
      ret = WN_CreateEval(ret);
      WN_Set_Linenum(ret, SetSrcPos(getLocation(expr)));
    }
    WN_INSERT_BlockLast(blk, ret);
    return Result::nwNone();
  }

  Result r = Result::nwNode(ret, ty);
  return r;
}

Result
WhirlExprBuilder::ConvertCStyleCastExpr(const CStyleCastExpr *expr, Result dest) {
  TRACE_FUNC();
  Result r = ConvertCastExpr(cast<CastExpr>(expr), dest);
  return r;
}

Result
WhirlExprBuilder::ConvertCXXBindTemporaryExpr(const CXXBindTemporaryExpr *expr, Result dest) {
  TRACE_FUNC();

  TY_IDX ty = _builder->TB().ConvertType(expr->getType());
  Is_True(TY_kind(ty) == KIND_STRUCT || TY_kind(ty) == KIND_ARRAY,
          ("not struct type"));
  TY_IDX pty = Make_Pointer_Type(ty);

  bool need_push_st_to_stack = false;
  BOOL is_dest_none = dest.isNone();
  if (is_dest_none) {
    SRCPOS spos = SetSrcPos(getLocation(expr));
    ST* st = Create_tmp_sym(ty, ".cxx.bind.", spos);
    dest = Result::nwSym(ST_st_idx(st), ty);
    if (expr->getType()->isRecordType()) {
      const CXXRecordDecl *record_decl = expr->getType()->getAsCXXRecordDecl();
      if (record_decl && !record_decl->hasTrivialDestructor())
        need_push_st_to_stack = true;
    }
  }

  Result ret = ConvertExpr(expr->getSubExpr(), dest);

  if (need_push_st_to_stack)
    Push_dtor_for_copy_ctor(expr->getType(), dest.Sym());

  if (ret.isNone() || ret.isSym())
    return ret;
  Is_True(ret.isNode(), ("should be whirl node"));

  if (is_dest_none) {
    WN *tmp_wn = ret.Node();
    if (OPERATOR_is_expression(WN_operator(tmp_wn))) {
      tmp_wn = WN_Stid(TY_mtype(ty), 0, ST_ptr(dest.Sym()), ty, tmp_wn);
    } else {
      if (OPERATOR_is_call(WN_operator(tmp_wn))) {
        Is_True(WN_kid0(tmp_wn) != NULL &&
                WN_kid0(WN_kid0(tmp_wn)) != NULL &&
                WN_operator(WN_kid0(WN_kid0(tmp_wn))) == OPR_LDA &&
                WN_st_idx(WN_kid0(WN_kid0(tmp_wn))) == dest.Sym() &&
                WN_lda_offset(WN_kid0(WN_kid0(tmp_wn))) == 0,
                ("not call with tmp as this?"));
      } else if (OPERATOR_is_store(WN_operator(tmp_wn))) {
        Is_True(WN_kid1(tmp_wn) != NULL &&
                WN_operator(WN_kid1(tmp_wn)) == OPR_LDA &&
                WN_st_idx(WN_kid1(tmp_wn)) == dest.Sym() &&
                WN_lda_offset(WN_kid1(tmp_wn)) == 0,
                ("not store into tmp_st?"));
      }
    }
    // insert into current block
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), tmp_wn);
    WN_Set_Linenum(tmp_wn, GetSrcPos());
    return dest;
  }

  return ret;
}

// generate a loop to convert constant array
//  bound is the loop upper bound
//  stmt is the stmt inserted into loop body
//  parms are STIDs for pointers referenced in loop, which will be increased/decreased
//  inside the loop
// idx = bound
// while (idx > -1) {
//  stmt;
//  parms += sizeof(ty_ele);
// }
WN *
WhirlExprBuilder::ConvertConstantArray(TY_IDX ty_ele, WN *bound, WN* stmt, WN **parm, INT cnt) {
  Is_True(bound && MTYPE_is_integral(WN_rtype(bound)), ("bad bound"));
  Is_True(stmt && !OPERATOR_is_expression(WN_operator(stmt)), ("bad body stmt"));
  INT i;
  for (i = 0; i < cnt; ++i) {
    Is_True(parm[i] &&
            WN_operator(parm[i]) == OPR_STID &&
            WN_desc(parm[i]) == Pointer_Mtype &&
            TY_kind(WN_ty(parm[i])) == KIND_POINTER, ("bad param"));
  }

  // create loop index
  TYPE_ID mtyp = WN_rtype(bound);
  TY_IDX idx_ty = MTYPE_To_TY(mtyp);
  ST *idx_st = MTYPE_To_PREG(mtyp);
  PREG_NUM idx_ofst = Create_Preg(mtyp, ".loop.idx");
  WN *block = WN_CreateBlock();
  WN *st_wn = WN_Stid(mtyp, idx_ofst, idx_st, idx_ty,
                      WN_Sub(mtyp, bound, WN_Intconst(mtyp, 1)));
  WN_INSERT_BlockLast(block, st_wn);
  WN_Set_Linenum(st_wn, GetSrcPos());

  // create loop body
  WN *body = WN_CreateBlock();
  WN_INSERT_BlockLast(body, stmt);

  // increase parm
  TY_IDX ptr_ty = Make_Pointer_Type(ty_ele);
  UINT64 ty_size = TY_size(ty_ele) ? TY_size(ty_ele) : 1;
  for (i = 0; i < cnt; ++i) {
    WN *ld_wn = WN_Ldid(Pointer_Mtype, WN_offset(parm[i]), WN_st(parm[i]), ptr_ty);
    WN *add_wn = WN_Add(Pointer_Mtype, ld_wn,
                        WN_Intconst(Pointer_Mtype, ty_size));
    st_wn = WN_Stid(Pointer_Mtype, WN_offset(parm[i]), WN_st(parm[i]), ptr_ty, add_wn);
    WN_INSERT_BlockLast(body, st_wn);
    WN_Set_Linenum(st_wn, GetSrcPos());
  }
  WN *sub_wn = WN_Sub(mtyp,
                      WN_Ldid(mtyp, idx_ofst, idx_st, idx_ty),
                      WN_Intconst(mtyp, 1));
  st_wn = WN_Stid(mtyp, idx_ofst, idx_st, idx_ty, sub_wn);
  WN_INSERT_BlockLast(body, st_wn);
  WN_Set_Linenum(st_wn, GetSrcPos());

  // create condition expr
  WN *cond_wn = WN_NE(mtyp,
                      WN_Ldid(mtyp, idx_ofst, idx_st, idx_ty),
                      WN_Intconst(mtyp, -1));

  // create loop
  WN *loop = WN_CreateWhileDo(cond_wn, body);
  WN_Set_Linenum(loop, GetSrcPos());
  WN_INSERT_BlockLast(block, loop);
  return block;
}

Result
WhirlExprBuilder::ConvertCXXNewExpr(const clang::CXXNewExpr *expr) {
  QualType type = expr->getAllocatedType();
  TY_IDX arr_ty = _builder->TB().ConvertType(type);
  SRCPOS spos = SetSrcPos(getLocation(expr));

  // calculate size to be allocated
  WN *elem_count = NULL;
  WN *size_wn = NULL;
  TYPE_ID machine_uint_ty = _builder->TB().GetUIntPtrMType();
  if (!expr->isArray()) {
    INT64 type_size = _builder->Context()->getTypeSizeInChars(type).getQuantity();
    if (type_size == 0)
      type_size = 1; // change size to 1 if it's 0
    size_wn = WN_Intconst(machine_uint_ty, type_size);
  } else {
    elem_count = ConvertToNode(getArraySize(expr));
    Is_True(elem_count, ("wrong array size"));
    TYPE_ID rtype = WN_rtype(elem_count);
    TY_IDX  sz_ty = MTYPE_To_TY(rtype);
    Is_True(OPERATOR_is_expression(WN_operator(elem_count)),
            ("not expr"));
    elem_count = Handle_expr_for_copy(WhirlBlockUtil::getCurrentBlock(),
                                      elem_count, sz_ty, spos, ".nam.size");
    size_wn = WN_COPY_Tree(elem_count);
    Is_True(TY_kind(arr_ty) == KIND_SCALAR ||
            TY_kind(arr_ty) == KIND_ARRAY ||
            TY_kind(arr_ty) == KIND_STRUCT ||
            TY_kind(arr_ty) == KIND_POINTER,
            ("unsupported arr_ty"));
    // change size to 1 if it's 0
    INT64 ty_size = TY_size(arr_ty) ? TY_size(arr_ty) : 1;
    size_wn = WN_Mpy(rtype, size_wn,
                     WN_Intconst(rtype, ty_size));
    WN *st_wn = WGEN_StidTemp(sz_ty, size_wn, ".anon.");
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
    WN_Set_Linenum(st_wn, spos);
    size_wn = WN_Ldid(rtype, WN_offset(st_wn), WN_st(st_wn), sz_ty);
  }

  // convert operator new
  FunctionDecl *nw_func = expr->getOperatorNew();
  ST_IDX func_sym_idx = _builder->Get_func_st(nw_func);
  Is_True(func_sym_idx, ("bad func st"));

  WN *call_blk = WN_CreateBlock();
  TY_IDX ret_ty_idx = _builder->TB().ConvertType(nw_func->getReturnType());

  // create wn to call new
  INT p_arg_n = expr->getNumPlacementArgs();
  WN *call_wn = WN_Create(OPR_CALL, TY_mtype(ret_ty_idx), MTYPE_V, 1 + p_arg_n);
  WN_Set_Linenum(call_wn, spos);
  WN_st_idx(call_wn) = func_sym_idx;
  WN_kid(call_wn, 0) = WGEN_CreateParm(machine_uint_ty, size_wn,
                                       MTYPE_To_TY(machine_uint_ty));
  if (!_builder->DeclBuilder().Call_nothrow(nw_func))
    Mark_call_region(call_wn);

  for (INT i = 0; i < p_arg_n; ++i) {
    const Expr *parg = expr->getPlacementArg(i);
    TY_IDX parg_ty = _builder->TB().ConvertType(parg->getType());
    WN *parg_wn = ConvertToNode(parg);
    if (parg->isGLValue())
      parg_ty = Make_Pointer_Type(parg_ty);
    WN_kid(call_wn, i + 1) = WGEN_CreateParm(WN_rtype(parg_wn), parg_wn, parg_ty);
  }
  WN_INSERT_BlockLast(call_blk, call_wn);
  WN_Set_Linenum(call_blk, spos);

  WN *ret = WN_Ldid(TY_mtype(ret_ty_idx), -1, Return_Val_Preg, ret_ty_idx);
  WN *comma = WGEN_CreateComma(WN_rtype(ret), call_blk, ret);

  // create a tmp for return value
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  Is_True(TY_kind(ty_idx) == KIND_POINTER, ("not ptr type"));
  ST *tmp_st = Create_tmp_sym(Make_Pointer_Type(arr_ty), ".nwm.mptr", spos);
  WN *st_wn = WN_Stid(TY_mtype(ty_idx), 0, tmp_st, ty_idx, comma);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
  WN_Set_Linenum(st_wn, spos);

  if (!expr->hasInitializer()) {
    // done, return st contains the new result
    return Result::nwSym(ST_st_idx(tmp_st), ty_idx);
  }

  const Expr *init = expr->getInitializer();
  if (!expr->isArray()) {
    if (!isa<CXXConstructExpr>(init)) {
      TY_IDX ty_idx = _builder->TB().ConvertType(init->getType());
      //TY_IDX ptr_ty_idx = Make_Pointer_Type(ty_idx);
      Result dest = Result::nwSym(ST_st_idx(tmp_st), ty_idx);
      Result tmp = ConvertExpr(init, dest);
      if (!tmp.isNone() &&
          (!tmp.isSym() || tmp.Sym() != ST_st_idx(tmp_st))) {
        WN *value = tmp.GetRValue();
        WN *ret_wn = WN_Istore(TY_mtype(ty_idx), 0, Make_Pointer_Type(ty_idx),
                               WN_Ldid(Pointer_Mtype, 0, tmp_st, ty_idx), value);
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret_wn);
        WN_Set_Linenum(ret_wn, spos);
      }
      return Result::nwSym(ST_st_idx(tmp_st), ty_idx);
    }

    const CXXConstructExpr *ctor_expr = cast<CXXConstructExpr>(init);
    const CXXConstructorDecl *ctor_decl = ctor_expr->getConstructor();
    if (!ctor_decl->isTrivial()) {
      WN *ldid = WN_Ldid(TY_mtype(ty_idx), 0, tmp_st, ty_idx);
      Result dest = Result::nwNode(ldid, ty_idx);
      Result ret = ConvertExpr(init, dest);
      if (ret.isNode()) {
        WN *ret_wn = ret.Node();
        if (WN_operator(ret_wn) == OPR_CALL) {
          Is_True(WN_operator(WN_kid0(ret_wn)) == OPR_PARM,
                  ("not parm"));
          Is_True(WN_operator(WN_kid0(WN_kid0(ret_wn))) == OPR_LDID &&
                  WN_st(WN_kid0(WN_kid0(ret_wn))) == tmp_st,
                  ("not the same st"));
        }
        if (ret_wn != ldid) {
          Is_True(OPERATOR_is_stmt(WN_operator(ret_wn)), ("not stmt"));
          WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret_wn);
          WN_Set_Linenum(ret_wn, spos);
        }
      }
      else if (ret.isSym()) {
        return ret;
      }
      else {
        Is_True(FALSE, ("invalid return wn for construct expr"));
      }
    }
    else if (ctor_expr->requiresZeroInitialization()) {
      WN *mstore = WN_CreateMstore(0, ty_idx, WN_Intconst(MTYPE_I4, 0),
                                   WN_Ldid(Pointer_Mtype, 0, tmp_st, ty_idx),
                                   WN_COPY_Tree(size_wn));
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), mstore);
      WN_Set_Linenum(mstore, spos);
    }
  }
  else {
    Is_True(elem_count != NULL && size_wn != NULL,
            ("elem count or size is null"));
    if (isa<InitListExpr>(init)) {
      Result dest = Result::nwSym(ST_st_idx(tmp_st), ty_idx);
      dest.SetRef();
      // if num inits of InitListExpr is zero, just return st
      const InitListExpr *init_expr = dyn_cast<InitListExpr>(init);
      if (!init_expr->getNumInits())
        return Result::nwSym(ST_st_idx(tmp_st), ty_idx);
      Result ret = ConvertInitListExpr(cast<InitListExpr>(init), dest);
      Is_True(ret.isNone(), ("result is not none"));
      return Result::nwNode(dest.GetLValue(), ty_idx);
    }
    else if (isa<ImplicitValueInitExpr>(init)) {
      WN *mstore = WN_CreateMstore(0, ty_idx, WN_Intconst(MTYPE_I4, 0),
                                   WN_Ldid(Pointer_Mtype, 0, tmp_st, ty_idx),
                                   WN_COPY_Tree(size_wn));
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), mstore);
      WN_Set_Linenum(mstore, spos);
      return Result::nwSym(ST_st_idx(tmp_st), ty_idx);
    }
    Is_True(isa<CXXConstructExpr>(init), ("init is not CXXConstructExpr"));
    const CXXConstructExpr *ctor_expr = cast<CXXConstructExpr>(init);
    const CXXConstructorDecl *ctor_decl = ctor_expr->getConstructor();
    if (!ctor_decl->isTrivial()) {
      WN *blk = WhirlBlockUtil::getCurrentBlock();
      // adjust allocation size to save the element count
      INT offset = MTYPE_byte_size(Pointer_Mtype);
      TYPE_ID rtype = WN_rtype(size_wn);
      size_wn = WN_Add(rtype, size_wn,
                       WN_Intconst(rtype, offset));
      WN_kid(call_wn, 0) = WGEN_CreateParm(machine_uint_ty, size_wn,
                                           MTYPE_To_TY(machine_uint_ty));
      // adjust return memory pointer
      WN *ldid = WN_Ldid(TY_mtype(ret_ty_idx), 0, tmp_st, ret_ty_idx);
      WN *add_wn = WN_Add(Pointer_Mtype, ldid, WN_Intconst(Pointer_Mtype, offset));
      WN *st_wn = WN_Stid(TY_mtype(ret_ty_idx), 0, tmp_st, ret_ty_idx, add_wn);
      WN_INSERT_BlockLast(blk, st_wn);
      WN_Set_Linenum(st_wn, spos);

      // store element count
      TY_IDX ptr_ty = Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype));
      ldid = WN_Ldid(TY_mtype(ret_ty_idx), 0, tmp_st, ret_ty_idx);
      WN *istr_wn = WN_Istore(Pointer_Mtype, -offset, ptr_ty, ldid,
                              WN_COPY_Tree(elem_count), 0);
      WN_INSERT_BlockLast(blk, istr_wn);
      WN_Set_Linenum(istr_wn, spos);
      // call ctor one-by-one
      GlobalDecl gd(ctor_decl, CXXCtorType::Ctor_Complete);
      ST_IDX st_idx = _builder->Get_func_st(gd);
      Is_True(st_idx, ("bad func st"));

      // generate call to ctor
      WN *ctor_call = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 1);
      WN_Set_Linenum(ctor_call, spos);
      WN_st_idx(ctor_call) = st_idx;
      WN_Set_Call_Default_Flags(ctor_call);
      if (!_builder->DeclBuilder().Call_nothrow(ctor_decl))
        Mark_call_region(ctor_call);

      WN *stid = WGEN_StidTemp(ty_idx,
                               WN_Ldid(TY_mtype(ty_idx), 0, tmp_st, ty_idx),
                               "nam.iter");
      WN_INSERT_BlockLast(blk, stid);
      WN_Set_Linenum(stid, spos);
      ldid = WN_Ldid(Pointer_Mtype, WN_offset(st_wn), WN_st(st_wn), ty_idx);
      WN_kid(ctor_call, 0) = WGEN_CreateParm(Pointer_Mtype,
                                             ldid, ty_idx);
      WN *parm[1] = { stid };
      WN *loop = ConvertConstantArray(TY_pointed(ty_idx), elem_count, ctor_call, parm, 1);
      WN_INSERT_BlockLast(blk, loop);
    }
    else if (ctor_expr->requiresZeroInitialization()) {
      WN *mstore = WN_CreateMstore(0, ty_idx, WN_Intconst(MTYPE_I4, 0),
                                   WN_Ldid(Pointer_Mtype, 0, tmp_st, ty_idx),
                                   WN_COPY_Tree(size_wn));
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), mstore);
      WN_Set_Linenum(mstore, spos);
    }
  }

  return Result::nwSym(ST_st_idx(tmp_st), ty_idx);
}

Result
WhirlExprBuilder::EmitCXXNewDeleteCall(const CallExpr *expr,
                                       bool is_delete) {
  const FunctionProtoType *pu_type =
    expr->getCallee()->getType()->castAs<FunctionProtoType>();

  ASTContext &Ctx = (*(_builder->Context()));
  DeclarationName Name = Ctx.DeclarationNames
    .getCXXOperatorName(is_delete? OO_Delete :OO_New);

  WN *size_wn;
  TYPE_ID machine_uint_ty = _builder->TB().GetUIntPtrMType();
  for (auto *Decl : Ctx.getTranslationUnitDecl()->lookup(Name))
    if (auto *func = dyn_cast<FunctionDecl>(Decl))
      if (Ctx.hasSameType(func->getType(), QualType(pu_type, 0))) {
        size_wn = WN_Intconst(machine_uint_ty, 1);
        ST_IDX func_sym_idx = _builder->Get_func_st(func);
        Is_True(func_sym_idx, ("bad func st"));
        TY_IDX ret_ty_idx = _builder->TB().ConvertType(func->getReturnType());
        TYPE_ID ret_mtype = TY_mtype(ret_ty_idx);

        WN *call_wn = WN_Create(OPR_CALL, ret_mtype, MTYPE_V, 1);
        WN_Set_Linenum(call_wn, SetSrcPos(getLocation(func)));
        WN_st_idx(call_wn) = func_sym_idx;
        if (!_builder->DeclBuilder().Call_nothrow(func))
          Mark_call_region(call_wn);

        WN_kid(call_wn, 0) = WGEN_CreateParm(machine_uint_ty, size_wn,
                                             MTYPE_To_TY(machine_uint_ty));
        if (ret_mtype == MTYPE_V)
          return Result::nwNode(call_wn, ret_ty_idx);

        WN *call_blk = WN_CreateBlock();
        WN_INSERT_BlockLast(call_blk, call_wn);
        WN *ret = WN_Ldid(ret_mtype, -1, Return_Val_Preg, ret_ty_idx);
        WN *comma = WGEN_CreateComma(ret_mtype, call_blk, ret);
        return Result::nwNode(comma, ret_ty_idx);
      }
}

Result
WhirlExprBuilder::ConvertCXXConstCastExpr(const CXXConstCastExpr *expr) {
  TRACE_FUNC();
  Result r = ConvertCastExpr(cast<CastExpr>(expr));
  return r;
}

Result
WhirlExprBuilder::ConvertCXXConstructExpr(const CXXConstructExpr *expr,
                                          Result dest, CXXCtorType ctor) {
  TRACE_FUNC();
  SRCPOS spos = SetSrcPos(getLocation(expr));
  const clang::CXXConstructorDecl *ctor_decl = expr->getConstructor();
  // Elide the constructor if we're constructing from a temporary.
  if (expr->isElidable()) {
    if (expr->getArg(0)->isTemporaryObject(*(_builder->Context()), ctor_decl->getParent())) {
      return ConvertExpr(expr->getArg(0), dest);
    }
  }

  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  Is_True(TY_kind(ty_idx) == KIND_STRUCT ||
          TY_kind(ty_idx) == KIND_ARRAY, ("shoule be struct or struct array"));
  TY_IDX ty_ptr_idx = Make_Pointer_Type(ty_idx);

  // create the first parm for *`this'
  if (dest.isNone()) {
    // create tmp local st
    ST *tmp_st = Create_tmp_sym(ty_idx, "ctor.tmp", spos);
    dest = Result::nwSym(ST_st_idx(tmp_st), ty_idx);
  }

  WN_VECTOR args;
  for (UINT i = 0; i < expr->getNumArgs(); ++i) {
    const Expr *arg = expr->getArg(i);
    WN *arg_wn = ConvertToNode(arg);
    QualType arg_type = arg->getType();
    TY_IDX arg_ty = _builder->TB().ConvertType(arg_type);
    if (arg->isGLValue())
      arg_ty = Make_Pointer_Type(arg_ty);
    args.push_back(pair<WN *, TY_IDX>(arg_wn, arg_ty));
  }

  WN *call_wn = EmitCXXConstructCall(ctor_decl, dest, ctor, args);
  Is_True(call_wn != NULL &&
          OPERATOR_is_stmt(WN_operator(call_wn)), ("bad call"));
  WN_Set_Linenum(call_wn, SetSrcPos(getLocation(expr)));

  const ConstantArrayType *cat =
    _builder->Context()->getAsConstantArrayType(expr->getType());
  if (cat && OPERATOR_is_call(WN_operator(call_wn))) {
    Is_True(TY_kind(ty_idx) == KIND_ARRAY, ("invalid ty"));
    unsigned num_elem = cat->getSize().getZExtValue();
    while (cat->getElementType()->isConstantArrayType()) {
      cat = _builder->Context()->getAsConstantArrayType(cat->getElementType());
      num_elem *= cat->getSize().getZExtValue();
    }
    TY_IDX ele_ty = _builder->TB().ConvertType(cat->getElementType());
    Is_True(TY_kind(ele_ty) != KIND_ARRAY, ("bad elem type"));
    TY_IDX ptr_ty = Make_Pointer_Type(ele_ty);

    WN *targ_wn = WN_kid0(WN_kid0(call_wn));
    WN *st_wn = WGEN_StidTemp(ptr_ty, targ_wn, (".targ.iter"));
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
    WN_Set_Linenum(st_wn, spos);

    WN *ld_wn = WN_Ldid(Pointer_Mtype, WN_offset(st_wn), WN_st(st_wn), ptr_ty);
    WN_kid0(WN_kid0(call_wn)) = ld_wn;
    WN *elem_wn = WN_Intconst(num_elem > INT_MAX ? MTYPE_I8 : MTYPE_I4,
                              num_elem);
    WN *parm[1] = { st_wn };
    call_wn = ConvertConstantArray(ele_ty, elem_wn, call_wn, parm, 1);
  }

  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), call_wn);
  return dest;
}

Result
WhirlExprBuilder::ConvertCXXDeleteExpr(const clang::CXXDeleteExpr *expr) {
  FunctionDecl *del_decl = expr->getOperatorDelete();
  TY_IDX ret_ty_idx = _builder->TB().ConvertType(del_decl->getReturnType());
  TY_IDX parm_ty =
    _builder->TB().ConvertType(del_decl->getParamDecl(0)->getType());

  // get operator delete
  ST_IDX del_func_sym_idx = _builder->Get_func_st(del_decl);
  Is_True(del_func_sym_idx, ("bad del func st"));

  SRCPOS spos = SetSrcPos(getLocation(expr));
  // convert pointer to be deleted
  Result r = ConvertExpr(expr->getArgument());
  TY_IDX ptr_ty = r.Ty();
  Is_True(TY_kind(r.Ty()) == KIND_POINTER, ("not ptr type"));

  WN *ldid_wn = r.GetRValue();
  Is_True(OPERATOR_is_expression(WN_operator(ldid_wn)),
          ("not expr"));

  INT offset = MTYPE_byte_size(Pointer_Mtype);
  ldid_wn = Handle_expr_for_copy(WhirlBlockUtil::getCurrentBlock(),
                                 ldid_wn, ptr_ty, spos, ".dl.ptr");

  // call destructor before convert delete expr
  const Type *type = expr->getDestroyedType().getTypePtr();
  const CXXRecordDecl *record_decl = type->getAsCXXRecordDecl();
  if (record_decl && !record_decl->hasTrivialDestructor()) {
    const CXXDestructorDecl *dtor_decl = record_decl->getDestructor();
    GlobalDecl gd(dtor_decl, CXXDtorType::Dtor_Complete);
    ST_IDX st_idx = _builder->Get_func_st(gd);
    Is_True(st_idx, ("bad dtor st"));

    // generate call to dtor
    WN *dtor_call = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 1);
    WN_Set_Linenum(dtor_call, SetSrcPos(getLocation(expr)));
    WN_st_idx(dtor_call) = st_idx;
    WN_Set_Call_Default_Flags(dtor_call);
    if (!_builder->DeclBuilder().Call_nothrow(dtor_decl))
      Mark_call_region(dtor_call);

    // get destroyed obj type
    TY_IDX obj_ty = _builder->TB().ConvertType(record_decl->getTypeForDecl());
    Is_True(TY_kind(obj_ty) == KIND_STRUCT, ("not struct type"));
    TY_IDX obj_ptr_ty = Make_Pointer_Type(obj_ty);

    if (expr->isArrayForm()) {
      // store first obj to tmp
      WN *st_wn = WGEN_StidTemp(ptr_ty, ldid_wn, ".da.iter");
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
      WN_Set_Linenum(st_wn, spos);

      // get element count if it is delete[]
      WN* ilod_wn = WN_Iload(Pointer_Mtype, -offset, ptr_ty,
                             WN_COPY_Tree(ldid_wn));
      WN *sz_wn = WGEN_StidTemp(ptr_ty, ilod_wn, ".da.size");
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), sz_wn);
      WN_Set_Linenum(sz_wn, spos);
      WN *elem = WN_Ldid(Pointer_Mtype, WN_offset(sz_wn), WN_st(sz_wn), ptr_ty);

      // adjust ldid wn for call to operator delete
      ldid_wn = WN_Sub(Pointer_Mtype, WN_COPY_Tree(ldid_wn),
                       WN_Intconst(Pointer_Mtype, offset));

      // generate a loop to destruct all elements
      WN *obj_wn = WN_Ldid(Pointer_Mtype, WN_offset(st_wn), WN_st(st_wn), ptr_ty);
      WN_kid(dtor_call, 0) = WGEN_CreateParm(Pointer_Mtype, obj_wn, obj_ptr_ty);
      WN *parm[1] = { st_wn };
      WN *loop = ConvertConstantArray(obj_ty, elem, dtor_call, parm, 1);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), loop);
    }
    else {
      // call dtor on this object
      WN_kid(dtor_call, 0) = WGEN_CreateParm(WN_rtype(ldid_wn),
                                             WN_COPY_Tree(ldid_wn),
                                             WN_ty(ldid_wn));
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), dtor_call);
    }
  }
  // call operator delete
  WN *call_wn = WN_Create(OPR_CALL, TY_mtype(ret_ty_idx), MTYPE_V, 1);
  WN_Set_Linenum(call_wn, SetSrcPos(getLocation(del_decl)));
  WN_st_idx(call_wn) = del_func_sym_idx;
  if (!_builder->DeclBuilder().Call_nothrow(del_decl))
    Mark_call_region(call_wn);

  WN_kid(call_wn, 0) = WGEN_CreateParm(TY_mtype(parm_ty), ldid_wn, parm_ty);
  return Result::nwNode(call_wn, 0);
}

Result
WhirlExprBuilder::ConvertCXXDefaultArgExpr(const CXXDefaultArgExpr *expr) {
  TRACE_FUNC();
  return ConvertExpr(expr->getExpr());
}

Result
WhirlExprBuilder::ConvertCXXDefaultInitExpr(const CXXDefaultInitExpr *expr, Result dest) {
  TRACE_FUNC();
  return ConvertExpr(expr->getExpr(), dest);
}

INT64
ComputeOffsetHind(ASTContext *ctx, const CXXRecordDecl *src, const CXXRecordDecl *dst) {
  if (src == NULL || dst == NULL)
    return -1;

  CXXBasePaths paths(true,    // FindAmbiguities
                     true,    // RecordPaths
                     false);  // DetectVirtual
  if (!dst->isDerivedFrom(src, paths))
    return -2;

  UINT  pub_paths = 0;
  INT64 ofst = 0;
  for (const CXXBasePath &p : paths) {
    if (p.Access != AS_public)  // ignore non-public inheritance
      continue;
    ++ pub_paths;

    for (const CXXBasePathElement& pe : p) {
      if (pe.Base->isVirtual())
        return -1;
      if (pub_paths > 1)
        continue;
      // accumulate the base class offset
      const ASTRecordLayout &l = ctx->getASTRecordLayout(pe.Class);
      const CXXRecordDecl *base = pe.Base->getType()->getAsCXXRecordDecl();
      ofst += l.getBaseClassOffset(base).getQuantity();
    }
  }

  if (pub_paths == 0)
    return -2;

  if (pub_paths > 1)
    return -3;

  return ofst;
}

Result
WhirlExprBuilder::ConvertBaseToDerivedMemberPointer(const CastExpr *expr) {
  const Expr *sub = expr->getSubExpr();
  const MemberPointerType *exp_mpt = expr->getType()->getAs<MemberPointerType>();
  const MemberPointerType *sub_mpt = sub->getType()->getAs<MemberPointerType>();
  Is_True(exp_mpt != NULL && sub_mpt != NULL,
          ("null expr mpt or sub mpt"));
  const CXXRecordDecl *to_decl = cast<CXXRecordDecl>(exp_mpt->getClass()->getAs<RecordType>()->getDecl());
  const CXXRecordDecl *from_decl = cast<CXXRecordDecl>(sub_mpt->getClass()->getAs<RecordType>()->getDecl());
  INT64 ofst = ComputeOffsetHind(_builder->Context(), from_decl, to_decl);
  Is_True(ofst >= 0, ("bad offset"));
  if (exp_mpt->isMemberDataPointer()) {
    Is_True(sub_mpt->isMemberDataPointer(), ("not member data"));
    Result sub_r = ConvertExpr(sub);
    if (ofst == 0)
      return sub_r;
    TY_IDX to_ty = _builder->TB().ConvertType(expr->getType());
    WN *sub_wn = sub_r.GetRValue();
    Is_True(OPERATOR_is_expression(WN_operator(sub_wn)) &&
            MTYPE_is_integral(WN_rtype(sub_wn)), ("bad sub wn"));
    WN *ret = WN_Add(WN_rtype(sub_wn), sub_wn, WN_Intconst(WN_rtype(sub_wn), ofst));
    return Result::nwNode(ret, to_ty);
  }
  else if (isa<UnaryOperator>(sub) &&
           cast<UnaryOperator>(sub)->getOpcode() == UO_AddrOf) {
    return ConvertCXXMFPFromFA(cast<UnaryOperator>(sub), ofst);
  }
  else {
    Result sub_r = ConvertExpr(sub);
    Is_True(!sub_r.isNone(), ("return none"));
    if (ofst == 0)
      return sub_r;
    WN *sub_wn = sub_r.GetRValue();
    // TODO: have a unique mfp ty_idx
    Is_True(OPERATOR_is_expression(WN_operator(sub_wn)) &&
            WN_rtype(sub_wn) == MTYPE_M &&
            TY_kind(sub_r.Ty()) == KIND_STRUCT &&
            TY_size(sub_r.Ty()) == 2 * MTYPE_byte_size(Pointer_Mtype),
            ("bad return whirl node"));
    return ConvertCXXMFPFromWhirl(sub_wn, sub_r.Ty(), ofst);
  }
}

Result
WhirlExprBuilder::ConvertCXXDynamicCastExpr(const CXXDynamicCastExpr *expr) {
  TRACE_FUNC();
  if (expr->isAlwaysNull()) {
    TY_IDX ty = MTYPE_To_TY(Pointer_Mtype);
    WN *ret = Gen_null_const(ty);
    return Result::nwNode(ret, ty);
  }
  QualType expr_ty = expr->getType();
  Is_True(expr->isGLValue() ||
          expr_ty->isPointerType() ||
          expr_ty->isReferenceType(),
          ("not pointer or reference type"));
  const Expr *sub = expr->getSubExpr();
  QualType sub_ty = sub->getType();
  Is_True(sub->isGLValue() ||
          sub_ty->isPointerType() ||
          sub_ty->isReferenceType(),
          ("not pointer or reference type"));

  // convert types
  TY_IDX to_ty   = _builder->TB().ConvertType(expr_ty);
  TY_IDX from_ty = _builder->TB().ConvertType(sub_ty);
  // canonical types
  if (expr->isGLValue() || expr_ty->isReferenceType())
    to_ty = Make_Pointer_Type(to_ty);
  else
    expr_ty = expr_ty->getPointeeType();
  if (sub->isGLValue() || sub_ty->isReferenceType())
    from_ty = Make_Pointer_Type(from_ty);
  else
    sub_ty = sub_ty->getPointeeType();
  Is_True(TY_kind(to_ty) == KIND_POINTER &&
          TY_kind(from_ty) == KIND_POINTER, ("not pointer type"));

  SRCPOS spos = SetSrcPos(getLocation(expr));
  // handle sub expr
  WN *sub_wn = ConvertToNode(sub);
  Is_True(sub_wn != NULL &&
          OPERATOR_is_expression(WN_operator(sub_wn)), ("not expr"));
  Is_True(WN_rtype(sub_wn) == Pointer_Mtype,
          ("not pointer mtype"));
  sub_wn = Handle_expr_for_copy(WhirlBlockUtil::getCurrentBlock(),
                                sub_wn, from_ty, GetSrcPos(), ".dyncast");

  // create call to __dynamic_cast
  // TODO: improve __dynamic_cast st later
  static ST* dyncast_st;
  if (dyncast_st == NULL) {
    /*
     * extern "C"
     * void* __dynamic_cast ( const void *sub,
     *                        const abi::__class_type_info *src,
     *                        const abi::__class_type_info *dst,
     *                        std::ptrdiff_t src2dst_offset);
     * sub: source address to be adjusted; nonnull, and since the
     *      source object is polymorphic, *(void**)sub is a virtual
     *      pointer.
     * src: static type of the source object.
     * dst: destination type (the "T" in "dynamic_cast<T>(v)").
     * src2dst_offset: a static hint about the location of the
     *    source subobject with respect to the complete object;
     *    special negative values are:
     *       -1: no hint
     *       -2: src is not a public base of dst
     *       -3: src is a multiple public base type but never a
     *           virtual base type
     *    otherwise, the src type is a unique public nonvirtual
     *    base type of dst at offset src2dst_offset from the
     *    origin of dst.
     */
    TY_IDX ptr_ty = Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype));
    dyncast_st = Create_function("__dynamic_cast",
                                 ptr_ty,
                                 ptr_ty,
                                 ptr_ty,
                                 ptr_ty,
                                 Pointer_Mtype,
                                 TY_IDX_ZERO);
  }
  WN *call_wn = WN_Create(OPR_CALL, Pointer_Mtype, MTYPE_V, 4);
  WN_st_idx(call_wn) = ST_st_idx(dyncast_st);
  WN_Set_Call_Default_Flags(call_wn);
  Mark_call_region(call_wn);

  // from ptr
  WN_kid(call_wn, 0) = WGEN_CreateParm(Pointer_Mtype,
                                       WN_COPY_Tree(sub_wn),
                                       from_ty);
  // from rtti
  ST_IDX from_rtti = _builder->TB().ConvertRTTIForType(sub_ty);
  WN *from_lda = WN_Lda(Pointer_Mtype, 0, ST_ptr(from_rtti));
  Set_ST_addr_saved(ST_ptr(from_rtti));
  Set_ST_addr_passed(ST_ptr(from_rtti));
  WN_kid(call_wn, 1) = WGEN_CreateParm(Pointer_Mtype,
                                       from_lda,
                                       WN_ty(from_lda));
  // to  rtti
  ST_IDX  to_rtti = _builder->TB().ConvertRTTIForType(expr_ty);
  WN *to_lda = WN_Lda(Pointer_Mtype, 0, ST_ptr(to_rtti));
  Set_ST_addr_saved(ST_ptr(to_rtti));
  Set_ST_addr_passed(ST_ptr(to_rtti));
  WN_kid(call_wn, 2) = WGEN_CreateParm(Pointer_Mtype,
                                       to_lda,
                                       WN_ty(to_lda));
  // offset hint
  const CXXRecordDecl *from_decl = sub_ty->getAsCXXRecordDecl();
  const CXXRecordDecl *to_decl = expr_ty->getAsCXXRecordDecl();
  INT64 ofst = ComputeOffsetHind(_builder->Context(), from_decl, to_decl);;
  WN_kid(call_wn, 3) = WGEN_CreateParm(MTYPE_I8,
                                       WN_Intconst(MTYPE_I4, ofst),
                                       MTYPE_To_TY(MTYPE_I4));

  // create comma for call
  WN *comma_blk = WN_CreateBlock();
  WN_INSERT_BlockLast(comma_blk, call_wn);
  WN_Set_Linenum(call_wn, spos);
  WN *call_ret = WN_Ldid(Pointer_Mtype, -1, Return_Val_Preg, to_ty);
  WN *comma = WGEN_CreateComma(Pointer_Mtype, comma_blk, call_ret);

  // create null for check fail
  WN *null_wn = Gen_null_const(MTYPE_To_TY(Pointer_Mtype));

  // create cond to check from ptr
  WN *cond = Handle_cond_wn(WN_COPY_Tree(sub_wn));

  // create CSELECT
  WN *cselect = WGEN_CreateCselect(Pointer_Mtype, cond, comma, null_wn);
  return Result::nwNode(cselect, to_ty);
}

Result
WhirlExprBuilder::ConvertCXXFunctionalCastExpr(const CXXFunctionalCastExpr *expr, Result dest) {
  TRACE_FUNC();
  Result r = ConvertCastExpr(cast<CastExpr>(expr), dest);
  return r;
}

// gen INITO for MemberFunPointer
static ST_IDX
GenINITOForCXXMFP(TY_IDX ty, INT64 ofst) {
  ST *tmp_st = New_ST(CURRENT_SYMTAB);
  ST_Init(tmp_st, Save_Str(".init"), CLASS_VAR,
          SCLASS_PSTATIC, EXPORT_LOCAL, ty);
  Set_ST_is_initialized(tmp_st);

  INITV_IDX inv_blk = New_INITV();
  INITV_Init_Block(inv_blk, INITV_Next_Idx());
  INITV_IDX pfn_iv = New_INITV();
  INITV_Init_Integer(pfn_iv, MTYPE_I8, ofst);
  Set_INITV_next(0, pfn_iv);

  INITV_IDX delta_iv = New_INITV();
  INITV_Init_Integer(delta_iv, MTYPE_I8, 0);
  Set_INITV_next(pfn_iv, delta_iv);

  New_INITO(tmp_st, inv_blk);
  return ST_st_idx(tmp_st);
}

Result
WhirlExprBuilder::ConvertCXXMFPToINITV(const Expr *expr, TY_IDX ty) {
  Is_True(TY_kind(ty) == KIND_STRUCT &&
          TY_size(ty) == 2 * MTYPE_byte_size(Pointer_Mtype),
          ("not member function type"));
  const UnaryOperator *unary_expr = cast<UnaryOperator>(expr);
  const Decl *decl = cast<DeclRefExpr>(unary_expr->getSubExpr())->getDecl();
  Is_True(decl != NULL && isa<CXXMethodDecl>(decl),
          ("not cxx method decl"));
  const CXXMethodDecl *method = cast<CXXMethodDecl>(decl);
  // function address or offset in vtable
  if (method->isVirtual()) {
    UINT64 vofst;
    ItaniumVTableContext *vt_ctx
        = cast<ItaniumVTableContext>(_builder->Context()->getVTableContext());
    const clang::CXXDestructorDecl* dtor
        = dyn_cast<clang::CXXDestructorDecl>(method);
    if (dtor)
      vofst = vt_ctx->getMethodVTableIndex(GlobalDecl(dtor, CXXDtorType::Dtor_Complete));
    else
      vofst = vt_ctx->getMethodVTableIndex(GlobalDecl(method));
    vofst *= MTYPE_byte_size(Pointer_Mtype);
    // gen INITO for MemberFunPointer
    ST_IDX tmp_st = GenINITOForCXXMFP(ty, vofst + 1);
    return Result::nwSym(tmp_st, ty);
  } else {
    return ConvertCXXMFPFromFA(unary_expr, 0);
  }
}

Result
WhirlExprBuilder::EmitCXXMemberPointerCall(const CXXMemberCallExpr *expr, BOOL retv) {
  const BinaryOperator *BO =
    cast<BinaryOperator>(expr->getCallee()->IgnoreParens());
  const Expr *BaseExpr = BO->getLHS();
  const Expr *MemFnExpr = BO->getRHS();

  const MemberPointerType *MPT =
    MemFnExpr->getType()->castAs<MemberPointerType>();

  const FunctionProtoType *FPT =
    MPT->getPointeeType()->castAs<FunctionProtoType>();
  const CXXRecordDecl *RD =
    cast<CXXRecordDecl>(MPT->getClass()->getAs<RecordType>()->getDecl());
  SRCPOS spos = SetSrcPos(getLocation(expr));

  Result base = ConvertExpr(BaseExpr);
  WN *addr = base.GetLValue();
  TY_IDX base_ty = _builder->TB().ConvertType(BaseExpr->getType());
  if (BaseExpr->isGLValue())
    base_ty = Make_Pointer_Type(base_ty);

  TY_IDX fty = _builder->TB().ConvertType(cast<FunctionType>(FPT), FALSE,
                                          RD->getTypeForDecl());

  TY_IDX ret_ty_idx = _builder->TB().ConvertType(expr->getType());
  if (expr->isGLValue())
    ret_ty_idx = Make_Pointer_Type(ret_ty_idx);

  // get the member function pointer
  WN *fun_idx, *fun_ptr;
  TY_IDX sub_ty = _builder->TB().ConvertType(MemFnExpr->getType());

  // Ignore parentheses and casts
  const Expr *sub = MemFnExpr->IgnoreParenCasts();
  Result ptr = Result::nwNone();
  if (!(MPT->isMemberFunctionPointer()) ||
      !(isa<UnaryOperator>(sub) &&
        cast<UnaryOperator>(sub)->getOpcode() == UO_AddrOf)) {
    ptr = ConvertExpr(sub);
  } else {
    ptr = ConvertCXXMFPToINITV(sub, sub_ty);
  }

  if (ptr.isNode() && !ptr.IsLValue()) {
    WN *node = ptr.GetRValue();
    // store init_wn into current block, get temp init st
    ST *temp_st = Gen_Temp_Symbol(sub_ty, ".init");
    WN *st_wn = WN_Stid(TY_mtype(sub_ty), 0,
                        temp_st, sub_ty, node);
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
    WN_Set_Linenum(st_wn, spos);
    ptr = Result::nwSym(ST_st_idx(temp_st), sub_ty);
  }

  // get field id
  UINT32 field_id = ptr.FieldId();

  // get field __pfn
  ptr.SetFieldId(field_id + 1);
  Result mem_fun_ptr = ptr.ConvertToRValue(Make_Pointer_Type(fty) /*sub_ty*/);
  Is_True(mem_fun_ptr.isNode(), ("invalid whirl node"));
  fun_ptr = mem_fun_ptr.Node();

  // get field __delta
  ptr.SetFieldId(field_id + 2);
  Result mem_fun_idx = ptr.ConvertToRValue(MTYPE_To_TY(Pointer_Mtype) /*sub_ty*/);
  Is_True(mem_fun_idx.isNode(), ("invalid whirl node"));
  fun_idx = mem_fun_idx.Node();

  WN *parm = WN_Add(WN_rtype(addr), addr, fun_idx);
  WN *icall_wn = WN_Icall(TY_mtype(ret_ty_idx), MTYPE_V, expr->getNumArgs() + 2, fty);
  WN_Set_Linenum(icall_wn, spos);
  if (!_builder->DeclBuilder().Call_nothrow(expr->getMethodDecl()))
    Mark_call_region(icall_wn);

  WN *parm_first = WGEN_CreateParm(WN_rtype(parm), parm, base_ty);
  int parm_idx = 0;
  WN_kid(icall_wn, parm_idx++) = parm_first;
  SetCallParm(expr, icall_wn, parm_idx);

  TYPE_ID mtyp = MTYPE_I8;
  WN *wn = WN_Binary(OPR_BAND, mtyp, fun_ptr,
                     WN_Intconst(mtyp, 1));
  wn = WN_CreateExp1(OPR_EXTRACT_BITS, MTYPE_U4, MTYPE_V, wn);
  WN_set_bit_offset_size(wn, 0, 1);

  TYPE_ID uintptr_ty = _builder->TB().GetUIntPtrMType();
  WN *ilod_vptr_wn = WN_Iload(uintptr_ty, 0, Make_Pointer_Type(fty),
                              WN_COPY_Tree(parm), 0);
  WN *tmp_wn = WN_Iload(uintptr_ty, 0, Make_Pointer_Type(fty),
                        WN_Add(uintptr_ty, ilod_vptr_wn,
                               WN_Sub(mtyp, WN_COPY_Tree(fun_ptr),
                                      WN_Intconst(mtyp, 1))));
  WN *parm3 = WGEN_CreateCselect(uintptr_ty, wn, tmp_wn,
                                 WN_COPY_Tree(fun_ptr));
  WN_kid(icall_wn, parm_idx++) = parm3;
  WN_Set_Call_Default_Flags(icall_wn);

  if (retv && !expr->getType()->isVoidType()) {
    WN *blk = WN_CreateBlock();
    WN_INSERT_BlockLast(blk, icall_wn);
    WN *ret = WN_Ldid(TY_mtype(ret_ty_idx), -1, Return_Val_Preg, ret_ty_idx);
    WN *comma = WGEN_CreateComma(WN_rtype(ret), blk, ret);
    Result r = Result::nwNode(comma, ret_ty_idx);
    r.SetRValue();
    return r;
  }

  return Result::nwNode(icall_wn, ret_ty_idx);
}

WN *
WhirlExprBuilder::EmitAdjustVirtualBase(WN *wn, TY_IDX ty_idx, const CXXRecordDecl *decl) {
  Is_True(wn != NULL, ("invalid wn"));
  Is_True(TY_kind(ty_idx) == KIND_POINTER &&
          TY_kind(TY_pointed(ty_idx)) == KIND_STRUCT,
          ("should be pointer type"));

  ASTContext *ast_c = _builder->Context();
  ItaniumVTableContext *vtable_c =
      cast<ItaniumVTableContext>(ast_c->getVTableContext());
  const VTableLayout &layout = vtable_c->getVTableLayout(decl);

  size_t idx = 0;
  for (unsigned j = 0; j < layout.getVTableSize(0); j++) {
    idx++;
    if (layout.vtable_components()[j].getKind() == VTableComponent::CK_RTTI)
      break;
  }
  TYPE_ID machine_uint_ty = _builder->TB().GetUIntPtrMType();
  UINT64 ofst = idx * MTYPE_byte_size(machine_uint_ty);

  // get vptr
  WN *vptr_wn = WN_Iload(machine_uint_ty, 0, TY_pointed(ty_idx), wn, 1);
  // get vtable address
  WN *adjust_vptr_wn = WN_Iload(machine_uint_ty, -ofst,
                                MTYPE_To_TY(MTYPE_I8), vptr_wn);
  WN *ret = WN_Add(machine_uint_ty, WN_COPY_Tree(wn), adjust_vptr_wn);
  return ret;
}

Result
WhirlExprBuilder::ConvertCXXMemberCallExpr(const CXXMemberCallExpr *expr, BOOL retv) {
  CXXMethodDecl *md = expr->getMethodDecl();
  const Expr *callee = expr->getCallee()->ignoreParenBaseCasts();
  if (isa<BinaryOperator>(callee)) {
    return EmitCXXMemberPointerCall(expr, retv);
  }
  SRCPOS spos = SetSrcPos(getLocation(expr));

  const MemberExpr *member_expr = cast<MemberExpr>(callee);
  // create ICALL if ImplicitObjectArgument is ReferenceType
  const Expr *impl_obj = expr->getImplicitObjectArgument();
  bool is_ref_obj = false;
  if (impl_obj && isa<DeclRefExpr>(impl_obj)) {
    const DeclRefExpr *sub_expr = cast<DeclRefExpr>(impl_obj);
    if (sub_expr->getDecl()->getType()->isReferenceType())
      is_ref_obj = true;
  }

  if (md->isVirtual() && (member_expr->isArrow() || is_ref_obj) &&
      !member_expr->hasQualifier()) {
    Expr *base = member_expr->getBase();
    Result base_r = ConvertExpr(base);
    TY_IDX base_ty_idx = _builder->TB().ConvertType(base->getType());
    if (TY_kind(base_ty_idx) != KIND_POINTER)
      base_ty_idx = Make_Pointer_Type(base_ty_idx);
    Is_True(TY_kind(base_ty_idx) == KIND_POINTER &&
            TY_kind(TY_pointed(base_ty_idx)) == KIND_STRUCT, ("wrong base type"));
#if 0
    // make sure vtable exists
    if (!TY_vtable(TY_pointed(base_ty_idx))) {
      Is_True(base->getType()->isPointerType(), ("not pointer type"));
      const CXXRecordDecl *decl = base->getType()->getPointeeCXXRecordDecl();
      Is_True(decl != NULL, ("null decl"));
      _builder->DeclBuilder().ConvertVTable(decl);
      Is_True(TY_vtable(TY_pointed(base_ty_idx)), ("no vtable for record"));
    }
#endif
    WN *ldid_base_wn = base_r.GetLValue();
    Is_True((WN_operator(ldid_base_wn) == OPR_LDID ||
             WN_operator(ldid_base_wn) == OPR_ILOAD ||
             WN_operator(ldid_base_wn) == OPR_ILDA ||
             WN_operator(ldid_base_wn) == OPR_COMMA ||
             WN_operator(ldid_base_wn) == OPR_ADD),
             ("Base wn is not ldid, iload or ilda."));
    TY_IDX ret_ty_idx = _builder->TB().ConvertType(expr->getType());
    if (expr->isGLValue())
      ret_ty_idx = Make_Pointer_Type(ret_ty_idx);
    TY_IDX func_ty_idx = _builder->TB().ConvertType(md->getType());
    WN *icall_wn = WN_Icall(TY_mtype(ret_ty_idx), MTYPE_V, expr->getNumArgs() + 2, func_ty_idx);
    WN_Set_Linenum(icall_wn, spos);
    WN_Set_Call_Default_Flags(icall_wn);
    WN_Set_Call_Is_Virtual(icall_wn);
    if (!_builder->DeclBuilder().Call_nothrow(md))
      Mark_call_region(icall_wn);

    WN *parm_first = WGEN_CreateParm(TY_mtype(base_ty_idx), ldid_base_wn, base_ty_idx);
    int parm_idx = 0;
    WN_kid(icall_wn, parm_idx++) = parm_first;
    SetCallParm(expr, icall_wn, parm_idx);
    Result base_r2 = ConvertExpr(base);
    WN *ldid_base_wn2 = base_r2.GetLValue();
    ASTContext *ast_c = _builder->Context();
    ItaniumVTableContext *vtable_c = cast<ItaniumVTableContext>(ast_c->getVTableContext());
    TY_IDX vtable_ty = _builder->TB().ConvertVTableType(md->getParent());
    Is_True(TY_kind(vtable_ty) == KIND_ARRAY, ("Invalid ty for VTable"));
    uint64_t offset;
    const clang::CXXDestructorDecl* dtor = dyn_cast<clang::CXXDestructorDecl>(md);
    if (dtor) {
      offset = vtable_c->getMethodVTableIndex(GlobalDecl(dtor, CXXDtorType::Dtor_Complete));
    }
    else {
      offset = vtable_c->getMethodVTableIndex(GlobalDecl(md));
    }

    offset *= TY_size(TY_etype(vtable_ty));
    TYPE_ID uintptr_ty = _builder->TB().GetUIntPtrMType();
    WN *ilod_vptr_wn = WN_Iload(uintptr_ty, 0, base_ty_idx, ldid_base_wn2, 0);
    WN *ilod_func_ptr_wn = WN_Iload(uintptr_ty, offset, func_ty_idx, ilod_vptr_wn, 0);
    WN_kid(icall_wn, expr->getNumArgs() + 1) = ilod_func_ptr_wn;
    if (retv && !expr->getType()->isVoidType()) {
      TYPE_ID ret_mty = TY_mtype(ret_ty_idx);
      WN *blk = WN_CreateBlock();
      WN_INSERT_BlockLast(blk, icall_wn);
      WN *ret = WN_Ldid(ret_mty, -1, Return_Val_Preg, ret_ty_idx);
      WN *comma = WGEN_CreateComma(WN_rtype(ret), blk, ret);
      Result r = Result::nwNode(comma, ret_ty_idx);
      r.SetRValue();
      return r;
    }
    return Result::nwNode(icall_wn, 0);
  }
  else {
    return ConvertCallExpr(cast<CallExpr>(expr), retv);
  }
}

Result
WhirlExprBuilder::ConvertCXXOperatorCallExpr(const CXXOperatorCallExpr *expr, BOOL retv) {
  TRACE_FUNC();
  const CXXMethodDecl *met_decl = dyn_cast_or_null<CXXMethodDecl>(expr->getCalleeDecl());
  // no need to convert for trivial operator call expression
  if (met_decl != NULL && met_decl->isTrivial()) {
    TY_IDX ty = _builder->TB().ConvertType(expr->getType());
    Result lhs = ConvertExpr(expr->getArg(0));
    Result rhs = ConvertExpr(expr->getArg(1));
    if (!expr->getArg(1)->isRValue())
      rhs = rhs.ConvertToRValue(ty);
    SetSrcPos(getLocation(expr));
    RV rv = GetRV(expr, retv);
    Result asgn = EmitAssignNode(lhs, rhs, ty, rv);
    return asgn;
  }
  return ConvertCallExpr(expr, retv);
}

Result
WhirlExprBuilder::ConvertCXXReinterpretCastExpr(const CXXReinterpretCastExpr *expr) {
  TRACE_FUNC();
  Result r = ConvertCastExpr(cast<CastExpr>(expr));
  return r;
}

Result
WhirlExprBuilder::ConvertCXXStaticCastExpr(const CXXStaticCastExpr *expr) {
  TRACE_FUNC();
  Result r = ConvertCastExpr(cast<CastExpr>(expr));
  return r;
}

Result
WhirlExprBuilder::ConvertCXXStdInitializerListExpr(const CXXStdInitializerListExpr *expr, Result dest) {
  SRCPOS spos = SetSrcPos(getLocation(expr));
  // get initializerlist type and field info
  TY_IDX init_ty = _builder->TB().ConvertType(expr->getType());
  Is_True(TY_kind(init_ty) == KIND_STRUCT, ("not struct kind"));
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(init_ty));
  Is_True(!FLD_last_field(fld_iter), ("last field?"));
  FLD_HANDLE addr_fld(fld_iter);
  Is_True(TY_kind(FLD_type(addr_fld)) == KIND_POINTER, ("not pointer type"));
  fld_iter++;
  Is_True(FLD_last_field(fld_iter), ("not last field"));
  FLD_HANDLE size_fld(fld_iter); 
  Is_True(MTYPE_is_integral(TY_mtype(FLD_type(size_fld))), ("not integer type"));

  ST *tmp_st = NULL;
  WN *tmp_lhs = NULL;
  TY_IDX init_ptr_ty = TY_IDX_ZERO;
  if (!dest.isNone()) {
    if (dest.isSym() && !dest.FieldId() && !dest.IsRef()) {
      Is_True(dest.Ty() == init_ty, ("ty mismatch"));
      tmp_st = ST_ptr(dest.Sym());
    }
    else {
      tmp_lhs = dest.GetLValue();
      init_ptr_ty = Make_Pointer_Type(init_ty);
      tmp_lhs = Handle_expr_for_copy(WhirlBlockUtil::getCurrentBlock(),
                                     tmp_lhs, init_ptr_ty, spos);
    }
  }
  else {
    tmp_st = Create_tmp_sym(init_ty, ".init.list", spos);
  }

  // get initializer address
  const Expr *sub_expr = expr->getSubExpr();
  Result sub_r = ConvertExpr(sub_expr, dest);
  WN *sub_wn = sub_r.GetLValue();

  // get initializer size
  const ConstantArrayType *cat =
    _builder->Context()->getAsConstantArrayType(sub_expr->getType());
  Is_True(cat != NULL, ("not constant array type"));
  unsigned num_elem = cat->getSize().getZExtValue();

  // create temporary symbol
  WN *stid;
  TY_IDX addr_ty = FLD_type(addr_fld);
  if (tmp_st) {
    stid = WN_Stid(TY_mtype(addr_ty), FLD_ofst(addr_fld), tmp_st, init_ty,
                   sub_wn, 1);
  }
  else {
    Is_True(tmp_lhs != NULL, ("lhs is NULL"));
    stid = WN_Istore(TY_mtype(addr_ty), FLD_ofst(addr_fld), init_ptr_ty,
                     tmp_lhs, sub_wn, 1);
  }
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
  WN_Set_Linenum(stid, spos);
  TY_IDX size_ty = FLD_type(size_fld);
  if (tmp_st) {
    stid = WN_Stid(TY_mtype(size_ty), FLD_ofst(size_fld), tmp_st, init_ty,
                   WN_Intconst(TY_mtype(size_ty), num_elem), 2);
  }
  else {
    Is_True(tmp_lhs != NULL, ("lhs is NULL"));
    stid = WN_Istore(TY_mtype(size_ty), FLD_ofst(size_fld), init_ptr_ty,
                     WN_COPY_Tree(tmp_lhs), WN_Intconst(TY_mtype(size_ty), num_elem), 2);
  }
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
  WN_Set_Linenum(stid, spos);
  if (tmp_st)
    return Result::nwSym(ST_st_idx(tmp_st), init_ty);
  else
    return dest;
}

Result
WhirlExprBuilder::ConvertCXXTemporaryObjectExpr(const CXXTemporaryObjectExpr *expr, Result dest) {
  TRACE_FUNC();
  return ConvertCXXConstructExpr(expr, dest, CXXCtorType::Ctor_Complete);
}

Result
WhirlExprBuilder::EmitCXXFreeException(WN *wn) {
  static ST *cxa_free_exception =
    Create_function("__cxa_free_exception",
                    MTYPE_To_TY(MTYPE_V),
                    Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)),
                    TY_IDX_ZERO);
  WN *call = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 1);
  WN_Set_Linenum(call, GetSrcPos());
  WN_st_idx(call) = ST_st_idx(cxa_free_exception);
  WN_Set_Call_Default_Flags(call);
  Mark_call_region(call);

  WN_kid0(call) = WGEN_CreateParm(Pointer_Mtype,
                                  wn, Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)));
  return Result::nwNode(call, 0);
}

Result
WhirlExprBuilder::EmitCXXAllocateException(QualType type, SRCPOS spos) {
  TRACE_FUNC();
  INT64 type_size =
      _builder->Context()->getTypeSizeInChars(type).getQuantity();
  WN *size_wn = WN_Intconst(Pointer_Mtype, type_size);

  static ST *cxa_allocate_exeption =
      Create_function("__cxa_allocate_exception",
                       Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)),
                       MTYPE_To_TY(Pointer_Mtype),
                       TY_IDX_ZERO);
  WN *alloc_excep_call = WN_Create(OPR_CALL, Pointer_Mtype, MTYPE_V, 1);
  WN_st_idx(alloc_excep_call) = ST_st_idx(cxa_allocate_exeption);
  WN_Set_Call_Default_Flags(alloc_excep_call);
  WN_kid0(alloc_excep_call) = WGEN_CreateParm(Pointer_Mtype,
                                              size_wn,
                                              MTYPE_To_TY(Pointer_Mtype));
  WN_Set_Linenum(alloc_excep_call, spos);

  WN *blk = WN_CreateBlock();
  WN_INSERT_BlockLast(blk, alloc_excep_call);
  WN_Set_Linenum(blk, spos);
  TY_IDX void_ptr = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
  WN *retv = WN_Ldid(Pointer_Mtype, -1, Return_Val_Preg, void_ptr);
  WN *comma = WGEN_CreateComma(Pointer_Mtype, blk, retv);

  ST *tmp_st = Gen_Temp_Symbol(void_ptr, ".anon");
  WN *st_wn = WN_Stid(Pointer_Mtype, 0, tmp_st, void_ptr, comma);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
  WN_Set_Linenum(st_wn, spos);
  return Result::nwSym(ST_st_idx(tmp_st), void_ptr);
}

WN *
WhirlExprBuilder::EmitCXXConstructCall(const CXXConstructorDecl *decl, Result dest,
                                       CXXCtorType ctor, const WN_VECTOR &args) {
  Is_True(!dest.isNone(), ("dest is none"));
  WN *targ_wn = dest.GetLValue();
  TY_IDX ty_idx = dest.Ty();
  TY_IDX ty_ptr_idx;
  if (TY_kind(ty_idx) == KIND_STRUCT || TY_kind(ty_idx) == KIND_ARRAY) {
    ty_ptr_idx = Make_Pointer_Type(ty_idx);
  }
  else {
    ty_ptr_idx = ty_idx;
    Is_True(ty_ptr_idx != TY_IDX_ZERO &&
            TY_kind(ty_ptr_idx) == KIND_POINTER &&
            (TY_kind(TY_pointed(ty_ptr_idx)) == KIND_STRUCT ||
             TY_kind(TY_pointed(ty_ptr_idx)) == KIND_ARRAY), ("bad dest ty"));
    ty_idx = TY_pointed(ty_ptr_idx);
  }

  // if constructor is trivial, constructor fun will not be called
  if (decl->isTrivial()) {
    WN *ret;
    if (decl->isDefaultConstructor()) {
      // default constructor
      // clear *this to 0
      Is_True(args.size() == 0, ("default constructor has argument"));
      ret = WN_CreateMstore(0, ty_ptr_idx, WN_Intconst(MTYPE_U4, 0),
                            targ_wn, WN_Intconst(MTYPE_U4, TY_size(ty_idx)));
    }
    else if (decl->isCopyConstructor() ||
             decl->isMoveConstructor()) {
      // copy constructor
      // copy *parm to *this
      Is_True(args.size() == 1, ("copy constructor has one argument"));
      Is_True(TY_kind(args[0].second) == KIND_POINTER &&
              TY_kind(TY_pointed(args[0].second)) == KIND_STRUCT, ("ty idx mismatch"));
      WN *rhs = WN_Iload(TY_mtype(ty_idx), 0, ty_idx, args[0].first);
      ret = WN_Istore(TY_mtype(ty_idx), 0, ty_ptr_idx, targ_wn, rhs);
    }
    else {
      Is_True(false, ("unsupport constructor in ConvertCXXConstructExpr"));
    }
    WN_Set_Linenum(ret, GetSrcPos());
    return ret;
  }

  // defer constructor if not exist
  GlobalDecl gd(decl, ctor);
  ST_IDX st_idx = _builder->Get_func_st(gd);
  Is_True(st_idx, ("bad ctor st"));

  UINT num_args = decl->getNumParams();
  Is_True(args.size() >= num_args, ("args less than params"));
  WN *call_wn = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, num_args + 1);
  WN_Set_Linenum(call_wn, GetSrcPos());
  WN_st_idx(call_wn) = st_idx;
  if (!_builder->DeclBuilder().Call_nothrow(gd.getDecl()))
    Mark_call_region(call_wn);

  // set `this' param
  WN_kid0(call_wn) = WGEN_CreateParm(Pointer_Mtype, targ_wn, ty_ptr_idx);

  // set rest params
  for (UINT i = 0; i < num_args; ++i) {
    WN    *arg_wn = args[i].first;
    TY_IDX arg_ty = args[i].second;
    WN_kid(call_wn, i + 1) = WGEN_CreateParm(WN_rtype(arg_wn), arg_wn,
                                             arg_ty);
  }
  WN_set_kid_count(call_wn, num_args + 1);
  WN_Set_Call_Default_Flags(call_wn);

  return call_wn;
}

Result
WhirlExprBuilder::ConvertCXXThrowExpr(const CXXThrowExpr *expr) {
  TRACE_FUNC();
  const Expr *sub_expr = expr->getSubExpr();
  SRCPOS spos = SetSrcPos(getLocation(expr));

  // emit __cxa_rethrow
  if (!sub_expr) {
    ST *cxa_rethrow = Create_function("__cxa_rethrow", MTYPE_To_TY(MTYPE_V),
                                                       TY_IDX_ZERO);
    WN *call = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 0);
    WN_Set_Linenum(call, spos);
    WN_st_idx(call) = ST_st_idx(cxa_rethrow);
    WN_Set_Call_Default_Flags(call);
    Mark_call_region(call);
    return Result::nwNode(call, 0);
  }

  QualType type = sub_expr->getType();
  TY_IDX ty_idx = _builder->TB().ConvertType(type);

  // allocate the exception object
  Result alloc = EmitCXXAllocateException(type, spos);
  Is_True(alloc.isSym(), ("invaid return type"));
  WN *alloc_wn = alloc.GetRValue();

  // convert sub_expr to WN Node
  WN *sub_wn = ConvertToNode(sub_expr);
  TY_IDX ptr_ty = Make_Pointer_Type(ty_idx);
  WN* store = WN_Istore(TY_mtype(ty_idx), 0, ptr_ty,
                        WN_COPY_Tree(alloc_wn), sub_wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), store);
  WN_Set_Linenum(store, spos);

  // get addr of RTTI descriptor
  ST_IDX rtti_st = _builder->TB().ConvertRTTIForType(type);
  Is_True(rtti_st != ST_IDX_ZERO, ("rtti st is zero"));
  WN *lda_wn = WN_Lda(Pointer_Mtype, 0, ST_ptr(rtti_st));
  Set_ST_addr_saved(ST_ptr(rtti_st));

  // pass the address of the destructor if the exception type
  // has non-trivial destructors, otherwise, pass a null value
  WN *addr_wn = NULL;
  TY_IDX addr_ty = TY_IDX_ZERO;
  const CXXRecordDecl *record_decl = type->getAsCXXRecordDecl();
  if (record_decl && !record_decl->hasTrivialDestructor()) {
    const CXXDestructorDecl *dtor_decl = record_decl->getDestructor();
    GlobalDecl gd(dtor_decl, CXXDtorType::Dtor_Complete);
    // defer destructor if not exist
    ST_IDX st_idx = _builder->Get_func_st(gd);
    Is_True(st_idx, ("bad dtor st"));

    addr_wn = WN_Lda(Pointer_Mtype, 0, ST_ptr(st_idx));
    Set_ST_addr_saved(ST_ptr(st_idx));
    addr_ty = WN_ty(addr_wn);
  } else {
    addr_wn = WN_Intconst(Pointer_Mtype, 0);
    addr_ty = MTYPE_To_TY(Pointer_Mtype);
  }

  static ST* cxa_throw;
  if (cxa_throw == NULL)
    cxa_throw = Create_function("__cxa_throw", MTYPE_To_TY(MTYPE_V),
                                               MTYPE_To_TY(Pointer_Mtype),
                                               MTYPE_To_TY(Pointer_Mtype),
                                               MTYPE_To_TY(Pointer_Mtype),
                                               TY_IDX_ZERO);
  WN *call = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 3);
  WN_Set_Linenum(call, spos);
  WN_st_idx(call) = ST_st_idx(cxa_throw);
  WN_Set_Call_Default_Flags(call);
  Mark_call_region(call);

  WN_kid0(call) = WGEN_CreateParm(Pointer_Mtype,
                                  alloc_wn,
                                  WN_ty(alloc_wn));
  WN_kid1(call) = WGEN_CreateParm(Pointer_Mtype,
                                  lda_wn,
                                  WN_ty(lda_wn));
  WN_kid2(call) = WGEN_CreateParm(Pointer_Mtype,
                                  addr_wn,
                                  addr_ty);

  // TODO: call "__cxa_free_exception" if necessary
#if 0
  Result free = EmitCXXFreeException(WN_COPY_Tree(alloc_wn));
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), free.Node());
#endif

  return Result::nwNode(call, 0);
}

Result
WhirlExprBuilder::EmitLoadRTTIFromVTable(const Expr *expr, TY_IDX ptr_ty) {
  Result r = ConvertExpr(expr);
  SRCPOS spos = SetSrcPos(getLocation(expr));
  if (r.IsDeref()) {
    TY_IDX obj_ty = _builder->TB().ConvertType(expr->getType());
    Is_True(TY_kind(obj_ty) == KIND_STRUCT, ("not struct type"));
    WN *base_ptr = r.GetLValue();
    WN* cmp = WN_NE(MTYPE_I4, base_ptr, WN_Intconst(Pointer_Mtype, 0));
#if 0
    // make sure vtable exists
    if (!TY_vtable(obj_ty)) {
      const CXXRecordDecl *decl = expr->getType()->getAsCXXRecordDecl();
      Is_True(decl != NULL, ("null decl"));
      _builder->DeclBuilder().ConvertVTable(decl);
      Is_True(TY_vtable(obj_ty), ("no vtable for record"));
    }
#endif
    // kid0
    WN *ilod_vptr_wn = WN_Iload(Pointer_Mtype, 0, obj_ty,
                                WN_COPY_Tree(base_ptr), 1);
    WN *ilod_rtti_wn = WN_Iload(Pointer_Mtype, -8, ptr_ty, ilod_vptr_wn, 0);
    // kid1
    static ST_IDX cxa_bad_typeid_st;  // TODO: improve later
    if (!cxa_bad_typeid_st)
      cxa_bad_typeid_st = ST_st_idx(Create_function("__cxa_bad_typeid",
                                                    MTYPE_To_TY(Pointer_Mtype),
                                                    TY_IDX_ZERO));
    WN *call = WN_Create(OPR_CALL, Pointer_Mtype, MTYPE_V, 0);
    WN_Set_Linenum(call, spos);
    WN_st_idx(call) = cxa_bad_typeid_st;
    WN_Set_Call_Default_Flags(call);
    Mark_call_region(call);

    WN *blk = WN_CreateBlock();
    WN_INSERT_BlockLast(blk, call);
    WN *retv = WN_Ldid(Pointer_Mtype, -1, Return_Val_Preg, ptr_ty);
    WN *comma = WGEN_CreateComma(Pointer_Mtype, blk, retv);
    // cselect
    WN *cselect = WGEN_CreateCselect(Pointer_Mtype, cmp, ilod_rtti_wn, comma);
    Result ret = Result::nwNode(cselect, ptr_ty);
    ret.SetRValue();
    return ret;
  }
  return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertCXXTypeidExpr(const CXXTypeidExpr *expr) {
  TRACE_FUNC();
  TY_IDX ptr_ty =
    Make_Pointer_Type(_builder->TB().ConvertType(expr->getType()));

  if (expr->isTypeOperand()) {
    QualType type = expr->getTypeOperand(*(_builder->Context()));
    ST_IDX st = _builder->TB().ConvertRTTIForType(type);
    Is_True(st != ST_IDX_ZERO, ("rtti st is zero"));
    Result rtti = Result::nwSym(st, TY_IDX_ZERO);
    rtti.SetAddrOf();
    return Result::nwNode(rtti.GetRValue(), ptr_ty);
  }

  if (expr->isPotentiallyEvaluated()) {
    const Expr *oper = expr->getExprOperand();
    Result r = EmitLoadRTTIFromVTable(expr->getExprOperand(), ptr_ty);
    if (!r.isNone())
      return r;
  }

  QualType OperandTy = expr->getExprOperand()->getType();
  ST_IDX st = _builder->TB().ConvertRTTIForType(OperandTy);
  Result rtti = Result::nwSym(st, TY_IDX_ZERO);
  rtti.SetAddrOf();
  return Result::nwNode(rtti.GetRValue(), ptr_ty);
}

Result
WhirlExprBuilder::ConvertCXXUuidofExpr(const CXXUuidofExpr *expr) {
  TRACE_FUNC();
  Is_True(false, ("TODO: unsupported ConvertCXXUuidofExpr"));
  return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertLambdaFieldRefExpr(const DeclRefExpr *expr) {
  TRACE_FUNC();

  const ValueDecl *value = expr->getDecl();
  const auto *var_decl = dyn_cast<VarDecl>(value->getCanonicalDecl());
  const FieldDecl *fld = var_decl ? _builder->LambdaHelper().GetCapFld(var_decl) : NULL;
  if(fld) {
    const CXXMethodDecl *decl = dyn_cast<CXXMethodDecl>(_builder->Scope().CurrentDecl());
    Is_True(decl != NULL, ("current scope should be lambda operator() fun"));
    ST_IDX this_st = _builder->Scope().Get_this();
    Is_True(this_st, ("unable to find lambda function this"));
    TY_IDX this_ty = ST_type(this_st);
    UINT32 fld_id = fld->getFieldIndex() + 1;
    WN *load_addr = WN_Ldid(Pointer_Mtype, 0, this_st, ST_type(this_st));
    if (isa<ReferenceType>(fld->getType())) {
      TY_IDX ty_idx = _builder->TB().ConvertType(value->getType());
      UINT32 cur_field_id = 0;
      UINT64 off = 0;
      FLD_HANDLE fh = get_fld_and_offset(this_ty, fld_id, cur_field_id, off);
      Is_True(!fh.Is_Null(), ("not find the field"));
      TY_IDX obj_ty = FLD_type(fh);
      WN *ilod = WN_Iload(TY_mtype(obj_ty), off, TY_pointed(this_ty), load_addr, fld_id);
      Result ret = Result::nwNode(ilod, obj_ty);
      ret.SetRef();
      return ret;
    }
    else {
      TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
      Result ret = Result::nwNode(load_addr, this_ty);
      ret.AddFieldId(fld_id);
      ret.SetRef();
      return ret;
    }
  }
  return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertDeclRefExpr(const DeclRefExpr *expr) {
  TRACE_FUNC();

  // convert declref in lambda
  if(_builder->LambdaHelper().IsInLambda()) {
    Result lambda_node = ConvertLambdaFieldRefExpr(expr);
    if(!lambda_node.isNone()) {
      return lambda_node;
    }
  }

  const ValueDecl *value = expr->getDecl();
  // get the canonical decl
  value = cast<ValueDecl>(value->getCanonicalDecl());

  // get field offset for FieldDecl
  if (isa<FieldDecl>(value)) {
    const FieldDecl *fld = cast<FieldDecl>(value);
    ASTContext *ast_c = _builder->Context();
    const ASTRecordLayout &layout = ast_c->getASTRecordLayout(fld->getParent());
    UINT64 ofst = layout.getFieldOffset(fld->getFieldIndex());
    ofst = ofst ? ofst / MTYPE_byte_size(Pointer_Mtype) : 0;
    return Result::nwNode(WN_Intconst(MTYPE_I4, ofst),
                          MTYPE_To_TY(MTYPE_I4));
  }

  // convert init expr for an enum constant
  if (isa<EnumConstantDecl>(value)) {
    const EnumConstantDecl *decl = dyn_cast<EnumConstantDecl>(value);
    llvm::APSInt val = decl->getInitVal();
    TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
    WN *ret = WN_Intconst(Mtype_comparison(TY_mtype(ty_idx)),
                          val.isSigned()? val.getSExtValue() : val.getZExtValue());
    return Result::nwNode(ret, ty_idx);
  }

  // handle IndirectFieldDecl
  if (isa<IndirectFieldDecl>(value)) {
    const IndirectFieldDecl *IFD = cast<IndirectFieldDecl>(value);
    UINT64 OffsetInBits = 0;
    ASTContext *ast_c = _builder->Context();
    for (const NamedDecl *ND : IFD->chain())
      OffsetInBits += ast_c->getFieldOffset(cast<FieldDecl>(ND));
    WN *ret = WN_Intconst(MTYPE_I8, OffsetInBits >> 3);
    return Result::nwNode(ret, MTYPE_To_TY(MTYPE_I8));
  }

  ST_IDX st_idx = _builder->Get_decl_st(value);
  Is_True(st_idx != ST_IDX_ZERO, ("bad st"));

  TY_IDX ty_idx = _builder->TB().ConvertType(value->getType());
  Result r = Result::nwSym(st_idx, ty_idx);
  if (value->getType()->isReferenceType())
    r.SetRef();
  return r;
#if 0
  if(_builder->LambdaHelper().IsInLambda()) {
    Result lambda_node = ConvertLambdaFieldRefExpr(expr);
    if(lambda_node.isNode()) {
      return lambda_node;
    }
  }
  WN *ret = NULL;
  TY_IDX field_ty_idx;
  bool is_struct_ptr = false;
  const ValueDecl *value = expr->getDecl();
  // get the canonical decl
  value = cast<ValueDecl>(value->getCanonicalDecl());

  // gen null const for FieldDecl
  if (isa<FieldDecl>(value)) {
    TY_IDX ty_idx = _builder->TB().ConvertType(value->getType());
    if (TY_kind(ty_idx) == KIND_STRUCT)
      return Result::nwNode(WN_Intconst(MTYPE_I8, 0), 0);

    WN *ret = Gen_null_const(ty_idx);
    return Result::nwNode(ret, ty_idx);
  }

  // convert init expr for an enum constant
  if (isa<EnumConstantDecl>(value)) {
    const EnumConstantDecl *decl = dyn_cast<EnumConstantDecl>(value);
    llvm::APSInt val = decl->getInitVal();
    TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
    return Result::nwNode(WN_Intconst(Mtype_comparison(TY_mtype(ty_idx)),
                                 val.isSigned()? val.getSExtValue() : val.getZExtValue()));
  }

  TY_IDX ty_idx = _builder->TB().ConvertType(value->getType());
  ST_IDX st_idx = _builder->SB().ConvertSymbol(value);

  UINT32 cur_field_id = 0;
  UINT64 offset = 0;
  if (field_id) {
    if (TY_kind(ty_idx) == KIND_POINTER &&
        TY_kind(TY_pointed(ty_idx)) == KIND_STRUCT) {
      is_struct_ptr = true;
      ty_idx = TY_pointed(ty_idx);
    }
    Is_True(TY_kind(ty_idx) == KIND_STRUCT, ("invalid ty, should be KIND_STRUCT"));
    FLD_HANDLE fld = get_fld_and_offset(ty_idx, field_id, cur_field_id, offset);
    Is_True (!fld.Is_Null(), ("Invalid field id %d for type 0x%x",
                              field_id, ty_idx));
    field_ty_idx = FLD_type(fld);
  }

  TY_IDX load_ty = field_id != 0 ? field_ty_idx : ty_idx;
  if (ST_base(ST_ptr(st_idx)) != ST_ptr(st_idx) || is_struct_ptr) {
    ST *base_st = ST_base(ST_ptr(st_idx));
    WN *load_addr = WN_Ldid(Pointer_Mtype, 0, base_st, ST_type(base_st));
    if (is_struct_ptr)
      ret = WN_Iload(TY_mtype(load_ty), offset, ty_idx, load_addr, field_id);
    else
      ret = WN_Iload(TY_mtype(load_ty), offset, load_ty, load_addr, field_id);
  } else {
    bool gen_lda = false;
    bool gen_iload = false;
    const Type *valueType = NULL;
    const Type *type = _builder->Context()->getCanonicalType(value->getType()).getTypePtr();
    const RecordType *recd_ty = dyn_cast_or_null<RecordType>(type);
    // generate lda for: lambda class var, array var, function
    if(recd_ty && recd_ty->getDecl()->isLambda() ||
       type->isArrayType() || TY_kind(load_ty) == KIND_ARRAY ||
       type->isFunctionType()) {
      gen_lda = true;
    } else if(isa<ReferenceType>(value->getType())) {
      gen_iload = true;
    }
    
    if (gen_iload) {
      // generate ILOAD if declaration is a ref type
      TY_IDX pointed_type = _builder->TB().ConvertType(expr->getType());
      WN *ld_value = WN_Ldid(TY_mtype(load_ty), offset, ST_ptr(st_idx), ty_idx, 0);
      ret = WN_Iload(TY_mtype(pointed_type), 0, pointed_type, ld_value, field_id);
    } else if (gen_lda){
      // generate lda for lambda class var, lambda field reference, and array var
      ret = WN_Lda(Pointer_Mtype, offset, ST_ptr(st_idx), field_id);
    } else {
      ret = WN_Ldid(TY_mtype(load_ty), offset, ST_ptr(st_idx), ty_idx, field_id);
    }
  }
  return Result::nwNode(ret);
#endif
}

Result
WhirlExprBuilder::ConvertExprWithCleanups(const ExprWithCleanups *expr, Result dest) {
  TRACE_FUNC();
  return ConvertExpr(expr->getSubExpr(), dest);
}

Result
WhirlExprBuilder::ConvertExtVectorElementExpr(const ExtVectorElementExpr *expr) {
  TRACE_FUNC();
  Is_True(false, ("TODO: unsupported ConvertExtVectorElementExpr"));
  return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertGenericSelectionExpr(const GenericSelectionExpr *expr) {
  TRACE_FUNC();
  return ConvertExpr(expr->getResultExpr());
}

Result
WhirlExprBuilder::ConvertGNUNullExpr(const clang::GNUNullExpr *expr) {
  WN* zero = WN_Intconst(Pointer_Mtype, 0);
  return Result::nwNode(zero, MTYPE_To_TY(Pointer_Mtype));
}

static inline bool isTypeConvert(const ImplicitCastExpr *expr) {
  CastKind k = expr->getCastKind();
  bool c = false;
  switch (k) {
    case CK_IntegralToPointer:
    case CK_PointerToIntegral:
    case CK_IntegralCast:
    case CK_IntegralToFloating:
    case CK_FloatingToIntegral:
    case CK_FloatingCast:
      c = true;
      break;
    default:
      break;
  }
  return c;
}

static const Expr*
Find_non_derived_to_base(const CastExpr* expr) {
  Is_True(expr->getCastKind() == CK_UncheckedDerivedToBase ||
          expr->getCastKind() == CK_DerivedToBase, ("invalid expr"));
  const Expr *sub = expr->getSubExpr();
  while (sub->getStmtClass() == Expr::ImplicitCastExprClass &&
         (cast<CastExpr>(sub)->getCastKind() == CK_UncheckedDerivedToBase ||
          cast<CastExpr>(sub)->getCastKind() == CK_DerivedToBase)) {
    sub = cast<CastExpr>(sub)->getSubExpr();
  }
  return sub;
}

Result
WhirlExprBuilder::ConvertCastExpr(const CastExpr *expr, Result dest) {
  TRACE_FUNC();
  const Expr *sub = expr->getSubExpr();
  TY_IDX from_ty = _builder->TB().ConvertType(sub->getType());
  TY_IDX to_ty = _builder->TB().ConvertType(expr->getType());
  SRCPOS spos = SetSrcPos(getLocation(expr));
  WN *from_wn = NULL, *to_wn = NULL;

  CastKind kind = expr->getCastKind();
  switch (kind) {
  case CK_ToVoid: {
    Result sub_expr = ConvertExpr(sub);
    if (!isa<CallExpr>(sub))
      return sub_expr;

#if 0
    WN *ret_wn;
    if (sub_expr.isSym()) {
      // for non-void return type
      WN *cur_blk = WhirlBlockUtil::getCurrentBlock();
      WN *wn = WN_last(cur_blk);
      WN *call_blk = WN_kid0(WN_kid0(wn));
      Is_True(WN_operator(call_blk) == OPR_BLOCK, ("unexpected whirl node"));
      ret_wn = WN_COPY_Tree(WN_last(call_blk));
      WN_DELETE_FromBlock(cur_blk, wn);
    } else if (sub_expr.isNode()) {
      ret_wn = sub_expr.Node();
    } else {
      Is_True(false, ("can't got to here"));
    }
#endif
    Is_True(sub_expr.isNode(), ("sub expr is not whirl node"));
    WN* ret_wn = sub_expr.Node();
    WN* call_wn = ret_wn;
    if (WN_operator(ret_wn) == OPR_COMMA) {
      ret_wn = WN_kid0(ret_wn);
      Is_True(WN_operator(ret_wn) == OPR_BLOCK, ("not block"));
      call_wn = WN_last(ret_wn);
    }
    // only set void rtype for call
    if (WN_operator(call_wn) == OPR_ICALL ||
        WN_operator(call_wn) == OPR_INTRINSIC_CALL ||
        WN_operator(call_wn) == OPR_CALL) {
      // set return type to void
      WN_set_rtype(call_wn, MTYPE_V);
    }
#if 0
    WN *ret_blk = WN_CreateBlock();
    WN_INSERT_BlockLast(ret_blk, ret_wn);
    return Result::nwNode(ret_blk);
#else
    return Result::nwNode(ret_wn, 0);
#endif
  }
  case CK_BitCast:
    from_wn = ConvertToNode(sub);
    Is_True(from_wn && OPERATOR_is_expression(WN_operator(from_wn)),
            ("invalid wn"));
    if (WN_operator(from_wn) == OPR_COMMA) {
      // apply TAS on comma kid1
      if (WN_operator(WN_kid1(from_wn)) == OPR_LDID)
        WN_set_ty(WN_kid1(from_wn), to_ty);
      else
        WN_kid1(from_wn) = WN_Tas(TY_mtype(to_ty), to_ty, WN_kid1(from_wn));
      to_wn = from_wn;
    }
    else {
      to_wn = WN_Tas(TY_mtype(to_ty), to_ty, from_wn);
    }

#if 0
    //This code is not right, should be improved later
    // need to reset some pointer type
    if (WN_operator(to_wn) != OPR_TAS &&
        WN_ty(to_wn) != to_ty &&
        (TY_kind(from_ty) == KIND_POINTER &&
         TY_kind(to_ty) == KIND_POINTER &&
         TY_pointed(from_ty) != TY_pointed(to_ty)))
      WN_set_ty(to_wn, to_ty);
#endif

    return Result::nwNode(to_wn, to_ty);

  case CK_ArrayToPointerDecay:
  case CK_FunctionToPointerDecay:
#if 0
    if (sub->getStmtClass() == Expr::DeclRefExprClass) {
      const DeclRefExpr *decl = cast<DeclRefExpr>(sub);
      const ValueDecl *value = decl->getDecl();
      TY_IDX ty_idx = _builder->TB().ConvertType(value->getType());
      ST_IDX st_idx = _builder->SB().ConvertSymbol(value);
      UINT64 offset = 0;
      if (field_id) {
        UINT32 cur_field_id = 0;
        FLD_HANDLE fh = get_fld_and_offset(ty_idx, field_id, cur_field_id, offset);
        Is_True(!fh.Is_Null(), ("not find the field"));
      }
      to_wn = WN_Lda(Pointer_Mtype, offset, ST_ptr(st_idx), field_id);
      return Result::nwNode(to_wn);
    } else
    if (sub->getStmtClass() == Expr::StringLiteralClass ||
               sub->getStmtClass() == Expr::CompoundLiteralExprClass) {
      return _builder->ConstBuilder().ConvertConst(expr);
    }
    else if (sub->getStmtClass() == Expr::MemberExprClass) {
       WN *sub_wn = ConvertToNode(sub, dest);
       if (WN_operator(sub_wn) == OPR_ILOAD) {
         to_wn = WN_Ilda(Pointer_Mtype, WN_offset(sub_wn), WN_kid0(sub_wn), WN_field_id(sub_wn));
       } else
         to_wn = sub_wn;
       return Result::nwNode(to_wn);
    }
#endif
  {
    Result ret = ConvertExpr(sub, dest);
    if (sub->getStmtClass() == Expr::ArraySubscriptExprClass ||
        TY_kind(ret.Ty()) == KIND_ARRAY)
      return ret;
    if (kind == CK_ArrayToPointerDecay) {
      Is_True(TY_kind(from_ty) == KIND_ARRAY, ("invalid sub ty"));
      if (sub->getStmtClass() == Expr::MemberExprClass &&
          ret.Ty() != from_ty)
      return Result::nwNode(ret.GetLValue(), from_ty);
    }
    return Result::nwNode(ret.GetLValue(), to_ty);
  }

  case CK_NullToPointer:
  {
    Result ret = Result::nwNode(WN_Intconst(TY_mtype(to_ty), 0), to_ty);
    return ret;
  }

  case CK_NullToMemberPointer:
  {
    const MemberPointerType *mpt =
      expr->getType()->getAs<MemberPointerType>();

    // a NULL pointer is represented as -1
    if (mpt->isMemberDataPointer())
      return Result::nwNode(WN_Intconst(MTYPE_I8, -1),
                       MTYPE_To_TY(MTYPE_I8));
    else if (mpt->isMemberFunctionPointer()) {
      // gen INITO for MemberFunPointer
      ST_IDX tmp_st = GenINITOForCXXMFP(to_ty, 0);
      Result sym = Result::nwSym(tmp_st, to_ty);
      return Result::nwNode(sym.GetRValue(), to_ty);
    } else
     Is_True(FALSE,
             ("unsupported for CK_NullToMemberPointer"));
  }

  case CK_PointerToBoolean:
  case CK_IntegralToBoolean:
  case CK_FloatingToBoolean:
    from_wn = ConvertToNode(sub);
    to_wn = WN_NE(WN_rtype(from_wn), from_wn, Gen_null_const(from_ty));
    return Result::nwNode(to_wn, to_ty);

  case CK_VectorSplat:
  {
    // TODO: fix me
    const Expr *args[1] = { sub };
    return GenerateCall("__vector_splat", to_ty, 1, args, TRUE);
  }

  case CK_IntegralToPointer:
  case CK_PointerToIntegral:
  case CK_IntegralCast:
  case CK_BooleanToSignedIntegral:
  case CK_IntegralToFloating:
  case CK_FloatingToIntegral:
  case CK_FloatingCast:
    from_wn = ConvertToNode(sub);
    to_wn = WN_Type_Conversion(from_wn, TY_mtype(to_ty));
    if (WN_operator(to_wn) == OPR_INTCONST) {
      WN_set_rtype(to_wn, Mtype_comparison(TY_mtype(to_ty)));
    } else if (OPCODE_is_load(WN_opcode(to_wn)) &&
      WN_rtype(to_wn) != TY_mtype(to_ty)) {
      to_wn = WN_Cvt(WN_rtype(to_wn), TY_mtype(to_ty), to_wn);
    }
    return Result::nwNode(to_wn, to_ty);

  case CK_FloatingComplexToReal:
  case CK_IntegralComplexToReal:
  {
    from_wn = ConvertToNode(sub);
    // convert complex to real part
    TYPE_ID from_mtyp = Mtype_complex_to_real(TY_mtype(from_ty));
    to_wn = WN_Unary(OPR_FIRSTPART, from_mtyp, WN_COPY_Tree(from_wn));
    return Result::nwNode(to_wn, to_ty);
  }

  case CK_FloatingComplexToBoolean:
  case CK_IntegralComplexToBoolean:
  case CK_IntegralRealToComplex:
  case CK_FloatingRealToComplex:
  case CK_FloatingComplexCast:
  case CK_FloatingComplexToIntegralComplex:
  case CK_IntegralComplexCast:
  case CK_IntegralComplexToFloatingComplex:
  {
    from_wn = ConvertToNode(sub);
    if (WN_operator(from_wn) != OPR_CONST &&
        !OPCODE_is_load(WN_opcode(from_wn))) {
      ST *tmp_st = Gen_Temp_Symbol(from_ty, "__save_expr");
      WN *st_wn = WN_Stid(TY_mtype(from_ty), 0,
                          tmp_st, from_ty, from_wn);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
      WN_Set_Linenum(st_wn, spos);

      from_wn = WN_Ldid(TY_mtype(from_ty), 0, tmp_st, from_ty);
    }

    WN *real, *imag;
    // handle CK_IntegralRealToComplex & CK_FloatingRealToComplex
    if (!MTYPE_is_complex(TY_mtype(from_ty))) {
      to_wn = WN_Binary(OPR_PAIR, TY_mtype(to_ty), from_wn,
                        Gen_null_const(from_ty));
    } else {
      TYPE_ID from_mtyp = Mtype_complex_to_real(TY_mtype(from_ty));
      real = WN_Unary(OPR_FIRSTPART, from_mtyp, WN_COPY_Tree(from_wn));
      imag = WN_Unary(OPR_SECONDPART, from_mtyp, WN_COPY_Tree(from_wn));

      if (!MTYPE_is_complex(TY_mtype(to_ty))) {
        // handle CK_FloatingComplexToBoolean & CK_IntegralComplexToBoolean
        to_wn = WN_CIOR(Handle_cond_wn(real), Handle_cond_wn(imag));
      } else {
        TYPE_ID to_mtyp = Mtype_complex_to_real(TY_mtype(to_ty));
        to_wn = WN_Binary(OPR_PAIR, TY_mtype(to_ty),
                          WN_Type_Conversion(real, to_mtyp),
                          WN_Type_Conversion(imag, to_mtyp));
      }
    }
    return Result::nwNode(to_wn, to_ty);
  }

  case CK_ReinterpretMemberPointer:
  case CK_DerivedToBaseMemberPointer:
  case CK_AnyPointerToBlockPointerCast:
  case CK_ARCProduceObject:
  case CK_ARCConsumeObject:
  case CK_ARCReclaimReturnedObject:
  case CK_ARCExtendBlockObject:
  case CK_CopyAndAutoreleaseBlockObject:
  case CK_AddressSpaceConversion:
  case CK_IntToOCLSampler:
    // TODO
    return ConvertExpr(sub);

  case CK_BaseToDerivedMemberPointer:
    return ConvertBaseToDerivedMemberPointer(expr);

  case CK_MemberPointerToBoolean:
  {
    const MemberPointerType *mpt =
      sub->getType()->getAs<MemberPointerType>();

    if (mpt->isMemberDataPointer()) {
      // for member data pointers, emit a check against -1
      Result mem_ptr = ConvertExpr(sub);
      Is_True(mem_ptr.isNode(), ("invalid return type"));
      WN *wn = mem_ptr.Node();
      return Result::nwNode(WN_NE(WN_rtype(wn), wn,
                       WN_Intconst(WN_rtype(wn), -1)), to_ty);
    } else if (mpt->isMemberFunctionPointer()) {
      // just check __pfn for Member function pointers
      TY_IDX sub_ty = _builder->TB().ConvertType(sub->getType());
      Is_True(TY_kind(sub_ty) == KIND_STRUCT, ("invalid ty"));
      TY_IDX pfn_ty = FLD_type(TY_fld(sub_ty));
      WN *node = ConvertToNode(sub);

      if (WN_operator(node) == OPR_COMMA) {
        WN *blk = WN_kid0(node);
        WN *ldid = WN_kid1(node);
        Is_True(WN_operator(ldid) == OPR_LDID, ("not ldid"));

        ST *temp = Gen_Temp_Symbol(sub_ty, ".temp_save");
        WN *stid = WN_Stid(TY_mtype(sub_ty), 0, temp, sub_ty, ldid);
        WN_INSERT_BlockLast(blk, stid);
        WN_Set_Linenum(stid, spos);

        ldid = WN_Ldid(TY_mtype(pfn_ty), 0, temp,
                       sub_ty, WN_field_id(node) + 1);
        WN_kid1(node) = ldid;
        WN_set_rtype(node, Pointer_Mtype);
      } else if (WN_operator(node) == OPR_LDID ||
                 WN_operator(node) == OPR_ILOAD) {
        WN_set_rtype(node, Pointer_Mtype);
        WN_set_desc(node, Pointer_Mtype);
        WN_set_field_id(node, WN_field_id(node) + 1);
      } else
        Is_True(false, ("invalid return type"));

      return Result::nwNode(WN_NE(WN_rtype(node), node,
                                  WN_Intconst(TY_mtype(pfn_ty), 0)),
                            to_ty);
    } else
     Is_True(FALSE,
             ("unsupported for CK_MemberPointerToBoolean"));
  }

  case CK_Dependent:
  case CK_BuiltinFnToFnPtr:
    // TODO
    Is_True(FALSE, ("unsupported CK_Dependent or CK_BuiltinFnToFnPtr"));

  case CK_NonAtomicToAtomic:
  case CK_AtomicToNonAtomic:
    // TODO
    Is_True(FALSE, ("unsupported CK_NonAtomicToAtomic or CK_AtomicToNonAtomic"));

  case CK_Dynamic:
    // TODO
    Is_True(FALSE, ("unsupported CK_Dynamic"));

  case CK_ConstructorConversion:
  case CK_UserDefinedConversion:
  case CK_CPointerToObjCPointerCast:
  case CK_BlockPointerToObjCPointerCast:
  case CK_LValueBitCast:
  case CK_NoOp:
    return ConvertExpr(sub, dest);

  case CK_LValueToRValue:
    return ConvertLValueToRValue(sub);

  case CK_UncheckedDerivedToBase:
  case CK_DerivedToBase:
  {
    bool is_virtual = false;
    UINT32 field_id = 0;
    CastExpr::path_const_iterator path_begin = expr->path_begin();
    CastExpr::path_const_iterator path_end = expr->path_end();
    Is_True(path_begin != path_end, ("path should not be empty"));

    if ((*path_begin)->isVirtual()) {
      is_virtual = true;
      const QualType base_type = (*path_begin)->getType();
      const CXXRecordDecl* base_decl = base_type->isPointerType()
                                         ? base_type->getPointeeCXXRecordDecl()
                                         : base_type->getAsCXXRecordDecl();
      const QualType derived_type = sub->getType();
      const CXXRecordDecl* derived_decl = derived_type->isPointerType()
                                         ? derived_type->getPointeeCXXRecordDecl()
                                         : derived_type->getAsCXXRecordDecl();
      Is_True(base_decl != NULL && derived_decl != NULL,
              ("base or derived is not cxx record"));
      const CXXBaseSpecifier* spec = Find_vbase(base_decl, derived_decl);
      Is_True(spec != NULL, ("not find the base spec"));
      field_id += _builder->TB().GetFieldIDFromDecl((const Decl*)spec);
      ++ path_begin;  // move to next path
    }

    for ( ; path_begin != path_end; ++ path_begin ) {
      Is_True(!(*path_begin)->isVirtual(), ("should no virtual here"));
      field_id += _builder->TB().GetFieldIDFromDecl((const Decl*)*path_begin);
    }

    Result r = ConvertExpr(sub);
    r.AddFieldId(field_id);  

    Is_True(r.isSym() || r.isNode(), ("invalid return type"));
    if (r.isSym())
      return r;

    WN *node = r.Node();
    TY_IDX derived_ty = r.Ty();

    // store OPR_COMMA to temp st
    if (WN_operator(node) == OPR_COMMA) {
      UINT32 fld_id = r.FieldId();
      Is_True(WN_rtype(node) == Pointer_Mtype &&
              TY_kind(derived_ty) == KIND_POINTER &&
              TY_kind(TY_pointed(derived_ty)) == KIND_STRUCT,
              ("not return M* for field access"));
      Update_pointee_ty(sub->getType(), derived_ty);
      ST *temp = Gen_Temp_Symbol(derived_ty, ".temp_save");
      WN *stid = WN_Stid(TY_mtype(derived_ty), 0, temp, derived_ty, node);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
      WN_Set_Linenum(stid, spos);
      node = WN_Ldid(TY_mtype(derived_ty), 0, temp, derived_ty);
      r = Result::nwNode(node, derived_ty);
      r.SetFieldId(fld_id);
    }

    // for non-virtual base, just return LValue
    if (!is_virtual) {
      r.SetArrow();
      node = r.GetLValue();
      return Result::nwNode(node, derived_ty);
    }

    QualType derived_type = sub->getType().getCanonicalType();
    if (derived_type->isReferenceType()) {
      derived_type = derived_type->getAs<ReferenceType>()->getPointeeType();
    } else if (derived_type->isPointerType()) {
      derived_type = derived_type->getAs<PointerType>()->getPointeeType();
    }
    const CXXRecordDecl *derived = derived_type->getAsCXXRecordDecl();
    // call-site adjustment 
    node = EmitAdjustVirtualBase(r.GetRValue(), r.Ty(), derived);
    return Result::nwNode(node, derived_ty);
  }

  case CK_ToUnion:
    // TODO
    return ConvertExpr(sub);

  case CK_BaseToDerived:
  {
    WN *wn = ConvertToNode(sub);
    Is_True(wn != NULL, ("bad wn"));
    if (WN_operator(wn) == OPR_ILOAD) {
      WN *stid = WGEN_StidTemp(to_ty, wn, ".cast.btod");
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
      WN_Set_Linenum(stid, spos);
      wn = WN_Ldid(TY_mtype(to_ty), WN_offset(stid), WN_st(stid), to_ty);
    }
    if (expr->isGLValue())
      to_ty = Make_Pointer_Type(to_ty);
    return Result::nwNode(wn, to_ty);
  }

#if 0
  case CK_LValueBitCast:
    // TODO
    Is_True(FALSE, ("unsupported CK_LValueBitCast"));
  case CK_ObjCObjectLValueCast:
  case CK_ZeroToOCLQueue:
    // TODO
    Is_True(FALSE, ("unsupported CK_LValueBitCast"));
#endif

  default:
    Is_True(FALSE, ("New cast kind?"));
  }
  return Result::nwNone();
}


Result
WhirlExprBuilder::ConvertImplicitCastExpr(const ImplicitCastExpr *expr, Result dest) {
  TRACE_FUNC();
  return ConvertCastExpr(cast<CastExpr>(expr), dest);
}

// For the specified symbol, generate a sequence of stores
static void
Emit_store_for_pad(WN *addr_wn, UINT pad, UINT current_offset, SRCPOS spos) {
  Is_True(addr_wn != NULL, ("invalid addr"));
  WN *zero_wn = WN_Intconst(MTYPE_U4, 0);
  WN *pad_wn = WN_Intconst(MTYPE_U4, pad);

  TY_IDX mstore_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_U1)); // char *
  WN *ret = WN_CreateMstore(current_offset, mstore_ty, zero_wn, addr_wn, pad_wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);
  WN_Set_Linenum(ret, spos); 
}

// Emit array initialization
Result
WhirlExprBuilder::EmitArrayInitialization(const clang::InitListExpr *expr, Result dest, UINT current_offset) {
  Is_True(!dest.isNone(), ("no dest for array init"));
  TY_IDX ty_arr = _builder->TB().ConvertType(expr->getType());
  const ConstantArrayType *cat = _builder->Context()->getAsConstantArrayType(expr->getType());
  Is_True(cat != NULL, ("InitListExpr is not ConstantArrayType"));
  unsigned num_init = expr->getNumInits();
  unsigned num_element = cat->getSize().getZExtValue();
  TY_IDX ty_ele = _builder->TB().ConvertType(cat->getElementType());
  unsigned num_set = std::min(num_init, num_element);
  UINT pad = 0;
  UINT i;

  SRCPOS spos = SetSrcPos(getLocation(expr));

  TYPE_ID idx_mty = num_element > INT_MAX ? MTYPE_I8 : MTYPE_I4;
  WN *num_wn = WN_Intconst(idx_mty, num_element);
  Is_True(!dest.isNone(), ("dest is none"));
  WN *lda_wn = dest.GetLValue();

  // handle constant array that has non trivial initializer
  int element = 0;
  if (const CXXRecordDecl *record =
        _builder->DeclBuilder().ConvertConstantArrayType(expr->getType(),
                                                           element)) {
    std::vector<const Expr*> elems;
    elems.reserve(num_element);
    _builder->DeclBuilder().CollectInitListItems(expr, elems);

    // only handle non trivial initializer here
    if (isa<CXXConstructExpr>(elems[0]) &&
        _builder->DeclBuilder().GetNonTrivialInitializer(elems[0])) {

      TY_IDX ele_pty = Make_Pointer_Type(ty_ele);
      ST *fst = Gen_Temp_Symbol(ele_pty, ".init.ptr");
      WN *st_wn = WN_Stid(MTYPE_U8, 0, fst, ele_pty, lda_wn);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
      WN_Set_Linenum(st_wn, spos);
  
      TY_IDX num_ty = MTYPE_To_TY(idx_mty);
      ST *num_st = MTYPE_To_PREG(idx_mty);
      PREG_NUM num_ofst = Create_Preg(idx_mty, ".init.idx");
      st_wn = WN_Stid(idx_mty, num_ofst, num_st, num_ty,
                      WN_Intconst(idx_mty, num_element -1));
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
      WN_Set_Linenum(st_wn, spos);
  
      const Expr *init_expr;
      for (UINT i = 0; i < elems.size(); ++i) {
        init_expr = elems[i];
        if (init_expr == NULL) {
          Is_True(FALSE, ("TODO")); // generate mstore with 0 to dest
          continue;
        }
  
        // convert init whirl node
        Result tmp = Result::nwSym(ST_st_idx(fst), ele_pty);
        tmp.SetDeref();
        WN *init_wn = ConvertToNode(init_expr, tmp);
        if (init_wn && !OPERATOR_is_expression(WN_operator(init_wn))) {
          WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), init_wn);
          WN_Set_Linenum(init_wn, spos);
        }
  
        // adjust offset for next initialize
        WN *ld = WN_Ldid(MTYPE_U8, 0, fst, ele_pty);
        WN *add_wn = WN_Add(WN_rtype(ld), ld,
                            WN_Intconst(MTYPE_U8,
                                        TY_size(ty_arr) / elems.size()));
        WN *st_wn = WN_Stid(WN_rtype(add_wn), 0, fst, ele_pty, add_wn);
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
        WN_Set_Linenum(st_wn, spos);
      }
      return Result::nwNone();

    }
  }

  // no need to create zero initialized for CXXConstructExpr 
  bool need_zero_initd = true;

  for (i = 0; i < num_element; ++i) {
    if (i >= num_init && !expr->hasArrayFiller())
      break;
    const Expr *init = i < num_init ? expr->getInit(i)
                                    : expr->getArrayFiller();

    // generate padding for ImplicitValueInitExpr
    if (isa<ImplicitValueInitExpr>(init)) {
      pad += TY_size(ty_ele);
      continue;
    }
    if (pad > 0) {
      Emit_store_for_pad(WN_COPY_Tree(lda_wn),
                         pad, i * TY_size(ty_ele) - pad,
                         spos);
      pad = 0;
    }

    // handle StringLiteral
    // such as: char fname[3][5] = {"f1", "f2", "f3");
    if (isa<StringLiteral>(init)) {
      TY_IDX ty = _builder->TB().ConvertType(init->getType());
      Is_True(TY_kind(ty) == KIND_ARRAY, ("invalid ty"));
      WN *ret_wn = ConvertToNode(init);
      UINT len = cast<StringLiteral>(init)->getByteLength();
      GenMstoreForString(lda_wn, ret_wn, ty, len,
                         i * TY_size(ty_ele), spos);
      need_zero_initd = false;
      continue;
    }

    WN *array_wn = WN_Ternary(OPR_ARRAY, Pointer_Mtype,
                              WN_COPY_Tree(lda_wn), WN_COPY_Tree(num_wn),
                              WN_Intconst(MTYPE_U4, i));
    WN_element_size(array_wn) = TY_size(ty_ele);

    if (isa<InitListExpr>(init)) {
      Result tmp = Result::nwNode(array_wn, ty_arr);
      tmp.SetArray();
      ConvertInitListExpr(cast<InitListExpr>(init),
                          tmp,
                          i * TY_size(ty_ele));
    } else if (isa<CXXConstructExpr>(init)) {
      // CXXConstructExpr have dafaul ctor
      // no need to be zero initialized
      need_zero_initd = false;

      // adjust offset
      TY_IDX pty = Make_Pointer_Type(ty_ele);
      ST *tmp_st = Gen_Temp_Symbol(pty, ".init.ptr");
      WN *ldid_wn = WN_Ldid(MTYPE_U8, 0, tmp_st, pty);
      WN *adjust_wn = WN_Add(MTYPE_U8, WN_COPY_Tree(lda_wn),
                       WN_Intconst(MTYPE_U8, i * TY_size(ty_ele)));
      WN *stid_wn = WN_Stid(MTYPE_U8, 0, tmp_st, pty, adjust_wn);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid_wn);
      WN_Set_Linenum(stid_wn, spos);

      Result tmp = Result::nwNode(ldid_wn, pty);
      WN *ret_wn = ConvertToNode(init, tmp);
      if (!OPERATOR_is_expression(WN_operator(ret_wn))) {
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret_wn);
        WN_Set_Linenum(ret_wn, spos);
      }
    } else {
      WN *ret_wn = ConvertToNode(init);
      Is_True(ret_wn != NULL, ("invaid return type"));

      if (OPERATOR_is_expression(WN_operator(ret_wn))) {
        ret_wn = WN_Istore(TY_mtype(ty_ele), 0,
                           Make_Pointer_Type(ty_ele),
                           array_wn, ret_wn);
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret_wn);
        WN_Set_Linenum(ret_wn, spos);
      }
    }
  }

  if (need_zero_initd && num_set < num_element) {
    unsigned pad_num = num_element - num_set;
    Emit_store_for_pad(WN_COPY_Tree(lda_wn),
                       pad_num * TY_size(ty_ele),
                       num_set * TY_size(ty_ele),
                       spos);
  }

  return Result::nwNone();
}

// Emit record initialization
Result
WhirlExprBuilder::EmitRecordInitialization(const clang::InitListExpr *expr, Result dest, UINT current_offset) {
  Is_True(!dest.isNone(), ("no dest for record init"));
  TY_IDX ty = _builder->TB().ConvertType(expr->getType());
  TY_IDX pty = Make_Pointer_Type(ty);
  RecordDecl *record = expr->getType()->getAs<RecordType>()->getDecl();
  TY_IDX fld_ty;
  FLD_HANDLE fld = TY_fld(ty);
  INT pad = 0;
  INT emitted_bytes = 0;
  UINT field_num = 0;
  UINT elem_num = 0;
  SRCPOS spos = SetSrcPos(getLocation(expr));

  // make a new dest based on current address
  dest = Result::nwNode(dest.GetLValue(), pty);
  dest.SetRef();
  WN *targ_wn = dest.GetLValue();

  for (RecordDecl::field_iterator field = record->field_begin(),
       fieldEnd = record->field_end();
       field != fieldEnd; ++field, ++field_num) {
    Is_True(!fld.Is_Null(), ("Invalid field id %d for type 0x%x",
                              field_num, ty));
    if (elem_num >= expr->getNumInits())
      break;

    if (record->isUnion() && expr->getInitializedFieldInUnion() != *field) {
      fld = FLD_next(fld);
      continue;
    }
    if (field->isUnnamedBitfield()) {
      fld = FLD_next(fld);
      continue;
    }

    pad = FLD_ofst(fld) - emitted_bytes;
    if (pad > 0) {
      Emit_store_for_pad(WN_COPY_Tree(targ_wn),
                         pad,
                         FLD_ofst(fld) - pad,
                         spos);
      emitted_bytes += pad;
    }

    const Expr *init = expr->getInit(elem_num++);
    BOOL is_bit_field = field->isBitField();
    fld_ty = is_bit_field ? MTYPE_To_TY(MTYPE_BS) : FLD_type(fld);

    // generate padding for ImplicitValueInitExpr
    if (isa<ImplicitValueInitExpr>(init)) {
      if (FLD_next(fld).Is_Null())
        break;
      if (elem_num < expr->getNumInits() &&
          !isa<ImplicitValueInitExpr>(expr->getInit(elem_num))) {
        fld = FLD_next(fld);
        Emit_store_for_pad(WN_COPY_Tree(targ_wn),
                           FLD_ofst(fld) - emitted_bytes,
                           FLD_ofst(fld),
                           spos);
        emitted_bytes += pad;
      } else {
        fld = FLD_next(fld);
      }
      continue;
    }

     // set field id
     UINT field_id = _builder->TB().GetFieldIDFromDecl(*field);
     Is_True(field_id != 0, ("failed to get field id"));
     dest.SetFieldId(field_id);

     if (isa<InitListExpr>(init)) {
       Result ret = ConvertInitListExpr(cast<InitListExpr>(init),
                                   dest,
                                   current_offset + emitted_bytes); 
       if (!ret.isNone()) {
         WN *st_wn = WN_Istore(TY_mtype(fld_ty), FLD_ofst(fld), pty,
                               WN_COPY_Tree(targ_wn), ret.GetRValue(),
                               field_id);
         WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
         WN_Set_Linenum(st_wn, spos);
       }

       emitted_bytes += TY_size(fld_ty);
       fld = FLD_next(fld);
       continue; 
     } else {
       Result ret = ConvertExpr(init, dest);
       INT size = TY_size(fld_ty);
       if (ret.isNone() || ret == dest) {
         emitted_bytes += size;
         fld = FLD_next(fld);
         continue;
       }

       WN *ret_wn = init->isGLValue() ? ret.GetLValue() : ret.GetRValue();
       Is_True(ret_wn != NULL, ("invalid return type"));
       
       if (isa<StringLiteral>(init) && TY_kind(fld_ty) == KIND_ARRAY) {
         UINT len = cast<StringLiteral>(init)->getByteLength();
         GenMstoreForString(targ_wn, ret_wn, fld_ty, len,
                            FLD_ofst(fld), spos);
         emitted_bytes += TY_size(fld_ty);
         fld = FLD_next(fld);
         continue;
       }

       WN *st_wn = WN_Istore(TY_mtype(fld_ty), is_bit_field ? 0 : FLD_ofst(fld),
                             pty, WN_COPY_Tree(targ_wn), ret_wn, field_id);
       WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
       WN_Set_Linenum(st_wn, spos);
   
       if (is_bit_field) {
         // find number of bytes to output
         INT num_of_bytes = ((FLD_bofst(fld) + FLD_bsize(fld) - 1) >> 3) + 1;
         // find number of bytes that have been output with previous bitfields
         INT bytes_out = emitted_bytes - FLD_ofst(fld);
         emitted_bytes += num_of_bytes - bytes_out;
       } else {
         emitted_bytes += size;
       }
       fld = FLD_next(fld);
     }
  }

  pad = TY_size(ty) - emitted_bytes;
  if (pad > 0)
    Emit_store_for_pad(WN_COPY_Tree(targ_wn),
                       pad,
                       emitted_bytes,
                       spos);

  return Result::nwNone();
}

Result
WhirlExprBuilder::EmitVectorInitialization(const InitListExpr *expr, Result dest, UINT current_offset) {
  TRACE_FUNC();
  Is_True(!dest.isNone(), ("no dest for vector init"));
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());

  const VectorType *VT = expr->getType()->getAs<VectorType> ();
  TY_IDX ty_ele = _builder->TB().ConvertType(VT->getElementType());

  unsigned num_init = expr->getNumInits();
  unsigned num_element = VT->getNumElements();
  unsigned num_set = std::min(num_init, num_element);

  SRCPOS spos = SetSrcPos(getLocation(expr));
  WN *targ_wn = dest.GetLValue();

  for (UINT i = 0; i != num_set; ++i) {
    const Expr *init = expr->getInit(i);
    Result ret = ConvertExpr(init);
    Is_True(!ret.isNone(), ("invalid init"));
    WN *wn = ret.GetRValue();
    INT offset = i * TY_size(ty_ele);
    WN *st_wn = WN_Istore(TY_mtype(ty_ele), offset,
                          Make_Pointer_Type(ty_ele),
                          WN_COPY_Tree(targ_wn), wn);
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
    WN_Set_Linenum(st_wn, spos);
    continue;
  }

  if (num_set < num_element) {
    unsigned pad = num_element - num_set;
    Emit_store_for_pad(WN_COPY_Tree(targ_wn),
                       pad * TY_size(ty_ele),
                       num_set * TY_size(ty_ele),
                       spos);
  }

  return Result::nwNone();
}

Result
WhirlExprBuilder::EmitComplexInitialization(const clang::InitListExpr *expr, Result dest, UINT current_offset) {
  TRACE_FUNC();
  Is_True(expr->getType()->isComplexType(), ("not complex type"));
  Is_True(expr->getNumInits() == 2, ("not 2 inits"));
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());

  WN *real = ConvertToNode(expr->getInit(0));
  Is_True(real != NULL, ("real wn is null"));
  WN *imag = ConvertToNode(expr->getInit(1));
  Is_True(imag != NULL, ("imag wn is null"));
  WN *pair = WN_Binary(OPR_PAIR, TY_mtype(ty_idx), real, imag);

  Result rhs = Result::nwNode(pair, ty_idx);
  if (dest.isNone())
    return rhs;

  EmitAssignNode(dest, rhs, ty_idx, R_NVALUE);
  return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertInitListExpr(const InitListExpr *expr, Result dest, UINT current_offset) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  SRCPOS spos = SetSrcPos(getLocation(expr));
  BOOL is_dest_none = dest.isNone();
  if (is_dest_none)
    dest = Result::nwSym(ST_st_idx(Create_tmp_sym(ty_idx, ".init", spos)), ty_idx);

  Result ret = Result::nwNone();
  if (expr->getType()->isArrayType()) {
    ret = EmitArrayInitialization(expr, dest, current_offset);
  }
  else if (expr->getType()->isRecordType()) {
    ret = EmitRecordInitialization(expr, dest, current_offset);
  }
  else if (expr->getType()->isVectorType()) {
    ret = EmitVectorInitialization(expr, dest, current_offset);
  }
  else if (expr->getType()->isComplexType()) {
    ret = EmitComplexInitialization(expr, dest, current_offset);
  }
  else if (expr->isTransparent()) {
    ret = ConvertExpr(expr->getInit(0), dest);
  }
  else if (expr->getNumInits() == 0) {
    ret = Result::nwNode(Gen_null_const(ty_idx), ty_idx);
    if (is_dest_none) {
      EmitAssignNode(dest, ret, ty_idx, R_NVALUE);
    }
  }
  else {
    Is_True(false, ("Can't go to here."));
  }
  return is_dest_none ? dest : ret;
}

Result
WhirlExprBuilder::ConvertIntegerLiteral(const IntegerLiteral *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  return Result::nwIntConst(MTYPE_is_unsigned(TY_mtype(ty_idx)) ?
                       expr->getValue().getZExtValue() : expr->getValue().getSExtValue(),
                       ty_idx);
}

Result
WhirlExprBuilder::ConvertFloatingLiteral(const FloatingLiteral *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  TCON_IDX tcon = Convert_float_to_tcon(ty_idx, expr->getValue());
  Result r = Result::nwTcon(tcon, ty_idx);
  r.SetType(ty_idx);
  return r;
}

Result
WhirlExprBuilder::ConvertLambdaExpr(const LambdaExpr *expr, Result dest) {
  TRACE_FUNC();
  LambdaPhase phase = _builder->LambdaHelper().SetLambdaPhase(IN_LAMBDA_CONSTRUCTOR);
  CXXRecordDecl *cxx_decl = expr->getLambdaClass();
  CXXRecordDecl::field_iterator cur_fld = cxx_decl->field_begin();
  int idx = 1;

  const VarDecl *lambda_var = _builder->LambdaHelper().GetLambdaVar(expr);
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  ST_IDX lambda_st = (dest.isSym() && dest.FieldId() == 0) ? dest.Sym() : (ST_IDX)0;
  SRCPOS spos = SetSrcPos(getLocation(expr));

  if(!lambda_var && lambda_st == (ST_IDX)0) {
    lambda_st = ST_st_idx(Create_tmp_sym(ty_idx, "lambda_var", spos));
  }
  if(expr->capture_size() > 0) {
    for (LambdaExpr::const_capture_init_iterator i = expr->capture_init_begin(),
                                                e = expr->capture_init_end();
        i != e; ++i, ++cur_fld, ++idx) {
      Result dest = Result::nwSym(lambda_st, ty_idx);
      dest.SetDot();
      dest.SetFieldId(idx);
      WN *init_wn = ConvertToNode(*i, dest);
      if (init_wn && OPERATOR_is_expression(WN_operator(init_wn)) &&
          !(WN_has_sym(init_wn) && ST_st_idx(WN_st(init_wn)) == lambda_st)) {
        TY_IDX fld_ty = _builder->TB().ConvertType(cur_fld->getType());
        UINT32 cur_field_id = 0;
        UINT64 ofst_value = 0;
        FLD_HANDLE fld = get_fld_and_offset(ty_idx, idx, cur_field_id, ofst_value);
        Is_True(!fld.Is_Null(), ("not find the field"));
        init_wn = WN_Stid(TY_mtype(fld_ty), ofst_value, ST_ptr(lambda_st), ty_idx, init_wn, idx);
        WN_Set_Linenum(init_wn, spos);
      }
      if (init_wn && !OPERATOR_is_expression(WN_operator(init_wn)))
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), init_wn);
    }
    Is_True(cur_fld == cxx_decl->field_end(), ("Lambda Fld/Var mismatch"));
  }

  _builder->LambdaHelper().SetLambdaPhase(phase);
  return Result::nwSym(lambda_st, ty_idx);
}

Result
WhirlExprBuilder::ConvertLValueToRValue(const clang::Expr* expr) {
  Result r = ConvertExpr(expr);
  Is_True(r.isNode() || r.isSym() || r.isIntConst(),
          ("invalid lvalue to rvalue convert"));

  TY_IDX rty = _builder->TB().ConvertType(expr->getType());
  return r.ConvertToRValue(rty);
}

Result
WhirlExprBuilder::ConvertMaterializeTemporaryExpr(const MaterializeTemporaryExpr *expr, Result dest) {
  TRACE_FUNC();
  BOOL is_ref = FALSE;
  const ValueDecl *decl = expr->getExtendingDecl();
  Is_True(decl == NULL || isa<VarDecl>(decl), ("not var decl"));
  if (decl && isa<VarDecl>(decl) && decl->getType()->isReferenceType()
                                 && !decl->getType()->isLValueReferenceType()) {
    is_ref = TRUE;
  }

  TY_IDX ty = _builder->TB().ConvertType(expr->getType());
  TY_IDX pty = Make_Pointer_Type(ty);
  BOOL is_dest_none = dest.isNone();
  SRCPOS spos = SetSrcPos(getLocation(expr));

  Result tmp_dest = dest;
  if (is_dest_none || is_ref) {
    // generate a temp in memory is dest is NULL or dest is a ref
    static int matl_temp_index;
    char tmp_name[256];
    snprintf(tmp_name, sizeof(tmp_name), ".material.%d", matl_temp_index++);
    SYMTAB_IDX symtab = CURRENT_SYMTAB;
    if (decl && isa<VarDecl>(decl) &&
        dyn_cast<VarDecl>(decl)->hasGlobalStorage())
      symtab = GLOBAL_SYMTAB;
    ST *tmp_st = New_ST(symtab);
    ST_Init(tmp_st, Save_Str(tmp_name), CLASS_VAR,
            symtab == GLOBAL_SYMTAB ? SCLASS_FSTATIC : SCLASS_AUTO,
            EXPORT_LOCAL, ty);
    tmp_dest = Result::nwSym(ST_st_idx(tmp_st), ty);
    // check if we need destruct materialize temporary obj
    if (expr->getType()->isRecordType()) {
      const CXXRecordDecl *record_decl = expr->getType()->getAsCXXRecordDecl();
      if (record_decl && !record_decl->hasTrivialDestructor()) {
        if (decl && isa<VarDecl>(decl))
          Push_dtor_call_stack(expr->getType(), ST_st_idx(tmp_st));
        else
          Push_dtor_for_copy_ctor(expr->getType(), ST_st_idx(tmp_st));
      }
    }
  }

  Result ret = ConvertExpr(getSubExpr(expr), tmp_dest);

  if (is_dest_none) {
    if (!ret.isNone() && ret != tmp_dest) {
      WN *rhs = ret.GetRValue();
      WN *stid = WN_Stid(TY_mtype(ty), 0,
                         ST_ptr(tmp_dest.Sym()), pty, rhs);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
      WN_Set_Linenum(stid, spos);
    }
    return tmp_dest;
  }

  if (ret.isSym()) {
    Is_True(dest.isSym(), ("invalid ret R"));
    if (dest.Sym() != ret.Sym()) {
      if (is_ref) {
        WN *rhs = ret.IsLValue() ? ret.GetLValue() : ret.GetRValue();
        WN *stid = WN_Istore(WN_rtype(rhs), 0, pty, dest.GetLValue(), rhs);
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
        WN_Set_Linenum(stid, spos);
        return dest;
      }
      return ret;
    }
    else {
      Is_True(dest.FieldId() == ret.FieldId(),
              ("invalid ret R"));
    }
  }
  else if (ret.isNode()) {
    WN *tmp_wn = ret.Node();
    if (OPERATOR_is_stmt(WN_operator(tmp_wn))) {
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), tmp_wn);
    }
    else if (OPERATOR_is_expression(WN_operator(tmp_wn))) {
      WN *lhs = dest.GetLValue();
      if (WN_has_sym(tmp_wn) && WN_has_sym(lhs) &&
          WN_st(tmp_wn) != WN_st(lhs)) {
        WN *store = WN_Istore(TY_mtype(ty), 0, pty, lhs, tmp_wn, 0);
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), store);
        WN_Set_Linenum(store, spos);
      }
      else if (!WN_has_sym(tmp_wn) && WN_has_sym(lhs)) {
        if (is_ref) {
          WN *tmp_stid = WN_Stid(TY_mtype(ty), 0, ST_ptr(tmp_dest.Sym()), ty, tmp_wn);
          WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), tmp_stid);
          WN_Set_Linenum(tmp_stid, spos);
          tmp_wn = tmp_dest.GetLValue();
        }
        WN *stid = WN_Stid(WN_rtype(tmp_wn), 0, WN_st(lhs), ty, tmp_wn);
        WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
        WN_Set_Linenum(stid, spos);
      }
    }
    else {
      Is_True(FALSE, ("not expr or stmt"));
    }
  }
  else if (!ret.isNone()) {
    WN *rhs = ret.GetRValue();
    WN *lhs = dest.GetLValue();
    if (is_ref) {
      WN *stid = WN_Stid(TY_mtype(ty), 0, ST_ptr(tmp_dest.Sym()), ty, rhs);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
      WN_Set_Linenum(stid, spos);

      Is_True(dest.isSym(), ("invalid ret R"));
      ST_IDX dest_st = dest.Sym();
      rhs = tmp_dest.GetLValue();
      stid = WN_Stid(WN_rtype(rhs), 0, ST_ptr(dest_st),
                     ST_type(dest_st), rhs);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
      WN_Set_Linenum(stid, spos);
      return dest;
    }
    // TODO: not use istore
    WN *store = WN_Istore(TY_mtype(ty), 0, pty, lhs, rhs, 0);
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), store);
    WN_Set_Linenum(store, spos);
  }

  return dest;
}

Result
WhirlExprBuilder::ConvertMemberExpr(const MemberExpr *expr, BOOL retv) {
  TRACE_FUNC();

  ValueDecl *decl = expr->getMemberDecl();
  const Expr *base = expr->getBase();

  EnumConstantDecl *enum_decl = dyn_cast<EnumConstantDecl>(decl);
  if (enum_decl) {
    llvm::APSInt val = enum_decl->getInitVal();
    TY_IDX ty = _builder->TB().ConvertType(expr->getType());
    WN *ret = WN_Intconst(Mtype_comparison(TY_mtype(ty)),
                          val.isSigned()? val.getSExtValue() : val.getZExtValue());
    return Result::nwNode(ret, ty);
  }

  VarDecl *var_decl = dyn_cast<VarDecl>(decl);
  if (var_decl) {
    // call ctor if exist
    ConvertExpr(base);
    TY_IDX mem_ty = _builder->TB().ConvertType(decl->getType());
    ST_IDX st = _builder->Get_var_st(var_decl);
    Is_True(st != ST_IDX_ZERO, ("bad st"));
    return Result::nwSym(st, mem_ty);
  }

  Result r = ConvertExpr(base);

  if (!retv)
    return r;

  if (expr->isArrow()) {
    // make sure record type is converted
    Is_True(base->getType()->isPointerType(), ("not ptr type"));
    _builder->TB().ConvertType(base->getType()->getPointeeType());
  } else
    _builder->TB().ConvertType(base->getType());


  if (r.isNode() && !r.IsRef() &&
      WN_operator(r.Node()) == OPR_COMMA) {
    WN* comma = r.Node();
    TY_IDX ty = _builder->TB().ConvertType(base->getType());
    ST *temp;
    if (!expr->isArrow() && !base->isGLValue()) {
      Is_True(WN_rtype(comma) == MTYPE_M &&
              TY_kind(ty) == KIND_STRUCT,
              ("not return M for field access"));
    }
    else if (base->isGLValue()) {
      Is_True(WN_rtype(comma) == Pointer_Mtype &&
              TY_kind(ty) == KIND_STRUCT,
              ("not return M& for field access"));
    }
    else {
      Is_True(WN_rtype(comma) == Pointer_Mtype &&
              TY_kind(ty) == KIND_POINTER &&
              TY_kind(TY_pointed(ty)) == KIND_STRUCT,
              ("not return M* for field access"));
    }
    temp = Gen_Temp_Symbol(ty, ".mcall.");
    WN *stid = WN_Stid(TY_mtype(ty), 0, temp, ty, comma);
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), stid);
    WN_Set_Linenum(stid, SetSrcPos(getLocation(expr)));
    r = Result::nwSym(ST_st_idx(temp), ty);
  }

  UINT32 fld_id = 0;
  BOOL   fld_ref = FALSE;
  if (isa<FieldDecl>(decl)) {
    FieldDecl *field = dyn_cast<FieldDecl>(decl);
    Is_True(field, ("FieldDecl should not be NULL"));
    fld_id = _builder->TB().GetFieldIDFromDecl(field);
    fld_ref = field->getType()->isReferenceType();
    // reset complete type
    TY_IDX base_ty = r.Ty();
    QualType qtype = base->getType();
    if (isa<DeclRefExpr>(base))
      qtype = cast<DeclRefExpr>(base)->getDecl()->getType();
    Update_pointee_ty(qtype, base_ty);
    r.SetType(base_ty);
  }

  if (expr->isArrow()) {
    if (r.IsAddrOf()) {
      r.ResetAddrOf();
      r.AddFieldId(fld_id);
    }
    else if (r.isSym()) {
      Is_True(!r.IsArrow() && !r.IsDeref() && !r.IsAddrOf(), ("TODO: arrow"));
      WN* ret = r.GetRValue();
      TY_IDX mem_ty = _builder->TB().ConvertType(base->getType());
      r = Result::nwNode(ret, mem_ty);
      r.AddFieldId(fld_id);
      r.SetArrow();
    }
    else if (r.isNode()) {
      Is_True(!r.IsDeref() && !r.IsAddrOf(), ("TODO: arrow"));
      if (r.FieldId() == 0) {
        TY_IDX mem_ty = _builder->TB().ConvertType(base->getType());
        // may be pointer to base/derived
        Is_True(TY_kind(mem_ty) == TY_kind(r.Ty()), ("ty mismatch?"));
        r.SetType(mem_ty);
      }
      r.AddFieldId(fld_id);
      r.SetArrow();
    }
  }
  else {
    r.AddFieldId(fld_id);
  }

  if (fld_ref) {
    TY_IDX ty = _builder->TB().ConvertType(expr->getType());
    TY_IDX pty = Make_Pointer_Type(ty);
    r = r.ConvertToRValue(pty);
    r.SetRef();
  }

  if (expr->isGLValue())
    r.SetLValue();

  return r;
}

#ifdef C2W_ENABLE_OBJC
Result
WhirlExprBuilder::ConvertObjCBridgedCastExpr(const ObjCBridgedCastExpr *expr)
{
    TRACE_FUNC();
    Is_True(false, ("unsupported ConvertObjCBridgedCastExpr"));
    return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertObjCEncodeExpr(const ObjCEncodeExpr *expr)
{
    TRACE_FUNC();
    Is_True(false, ("unsupported ConvertObjCEncodeExpr"));
    return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertObjCIsaExpr(const ObjCIsaExpr *expr)
{
    TRACE_FUNC();
    Is_True(false, ("unsupported ConvertObjCIsaExpr"));
    return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertObjCIvarRefExpr(const ObjCIvarRefExpr *expr)
{
    TRACE_FUNC();
    Is_True(false, ("unsupported ConvertObjCIvarRefExpr"));
    return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertObjCMessageExpr(const ObjCMessageExpr *expr)
{
    TRACE_FUNC();
    Is_True(false, ("unsupported ConvertObjCMessageExpr"));
    return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertObjCPropertyRefExpr(const ObjCPropertyRefExpr *expr)
{
    TRACE_FUNC();
    Is_True(false, ("unsupported ConvertObjCPropertyRefExpr"));
    return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertObjCSelectorExpr(const ObjCSelectorExpr *expr)
{
    TRACE_FUNC();
    Is_True(false, ("unsupported ConvertObjCSelectorExpr"));
    return Result::nwNone();
}
#endif

Result
WhirlExprBuilder::ConvertOpaqueValueExpr(const OpaqueValueExpr *expr) {
  TRACE_FUNC();
  return ConvertExpr(expr->getSourceExpr());
}

Result
WhirlExprBuilder::ConvertParenExpr(const ParenExpr *expr, Result dest, BOOL retv) {
  TRACE_FUNC();
  return ConvertExpr(expr->getSubExpr(), dest, retv);
}

#if LLVM_VERSION_MAJOR == 11
Result
WhirlExprBuilder::ConvertConstantExpr(const ConstantExpr *expr, Result dest, BOOL retv) {
  TRACE_FUNC();
  return ConvertExpr(expr->getSubExpr(), dest, retv);
}
#endif

Result
WhirlExprBuilder::ConvertPredefinedExpr(const PredefinedExpr *expr) {
  TRACE_FUNC();
  const StringLiteral *str = expr->getFunctionName();
  Is_True(str != nullptr, ("no StringLiteral name in PredefinedExpr"));
  return ConvertStringLiteral(str);
}

Result
WhirlExprBuilder::ConvertPseudoObjectExpr(const PseudoObjectExpr *expr) {
  TRACE_FUNC();
  Is_True(false, ("TODO: unsupported ConvertPseudoObjectExpr"));
  return Result::nwNone();
}

Result
WhirlExprBuilder::ConvertShuffleVectorExpr(const ShuffleVectorExpr *expr) {
  const Expr *args[MAX_PARM_CNT];
  Is_True(expr->getNumSubExprs() >= 2 && expr->getNumSubExprs() < MAX_PARM_CNT,
          ("invalid sub expr count"));
  INT cnt = expr->getNumSubExprs() <= MAX_PARM_CNT
              ? expr->getNumSubExprs() : MAX_PARM_CNT;
  TY_IDX ty = _builder->TB().ConvertType(expr->getType());

  // TODO: fully support this in backend. so far we generate a call
  for (INT i = 0; i < cnt; ++i)
    args[i] = expr->getExpr(i);

  return GenerateCall("__shuffle_vector", ty, cnt, args, TRUE);
}

Result
WhirlExprBuilder::ConvertStmtExpr(const StmtExpr *expr) {
  TRACE_FUNC();
  const CompoundStmt *stmt = expr->getSubStmt();
  QualType qtype = expr->getType();

  if (qtype->isVoidType()) {
    WhirlStmtBuilder stmt_bldr(_builder);
    WN *ret = stmt_bldr.ConvertStmt(stmt);
    if (ret)
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);
    return Result::nwNone();
  } else {
    // create tmp st
    TY_IDX ty_idx = _builder->TB().ConvertType(qtype);
    ST *tmp_st = Create_tmp_sym(ty_idx, ".anon", 0);
    Is_True(isa<CompoundStmt>(stmt), ("should be CompoundStmt"));

    WhirlStmtBuilder stmt_bldr(_builder);
    WN *ret = stmt_bldr.ConvertCompoundStmt(stmt, ST_st_idx(tmp_st));
    if (ret)
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), ret);
    return Result::nwSym(ST_st_idx(tmp_st), ty_idx);
  }
}

Result
WhirlExprBuilder::ConvertStringLiteral(const StringLiteral *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  TCON_IDX tcon = Convert_string_to_tcon(expr, ty_idx);
  ST *st = New_Const_Sym(tcon, ty_idx);
  WN *wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  Set_ST_addr_saved(st);
  return Result::nwNode(wn, ty_idx);
}

Result
WhirlExprBuilder::ConvertSubstNonTypeTemplateParmExpr(const SubstNonTypeTemplateParmExpr *expr) {
  TRACE_FUNC();
  return ConvertExpr(expr->getReplacement());
}

Result
WhirlExprBuilder::ConvertTypeTraitExpr(const TypeTraitExpr *expr) {
  TRACE_FUNC();
  bool val = expr->getValue();
  WN *ret = WN_Intconst(MTYPE_I4, val);
  return Result::nwNode(ret, MTYPE_To_TY(MTYPE_I4));
}

// This function is used to process
// UO_PostInc/UO_PostDec/UO_PreInc/UO_PreDec for UnaryOperator
// since this function may be called more than once.
// such as: --(i ? i : j)
Result
WhirlExprBuilder::ConvertPrePostIncDec(
  const UnaryOperator *expr, WN *wn_sub, TY_IDX dst_ty, RV rv) {
  UnaryOperatorKind opcode = expr->getOpcode();
  Is_True(opcode == UO_PostInc ||
          opcode == UO_PostDec ||
          opcode == UO_PreInc ||
          opcode == UO_PreDec,
          ("invalid UnaryOperator for ConvertPrePostIncDec"));

  // handle OPR_COMMA: such as: --(++i)
  WN *comma_blk = NULL;
  if (WN_operator(wn_sub) == OPR_COMMA) {
    comma_blk = WN_kid0(wn_sub);
    wn_sub = WN_last(wn_sub);
  }

  Is_True(wn_sub != NULL &&
          (WN_operator(wn_sub) == OPR_LDID ||
           WN_operator(wn_sub) == OPR_ILOAD),
          ("WN for ConvertPrePostIncDec can not be NULL"));
  Is_True(dst_ty != (TY_IDX)0,
          ("TY for ConvertPrePostIncDec can not be NULL"));

  UINT field_id = OPERATOR_has_field_id(WN_operator(wn_sub))
                    ? WN_field_id(wn_sub) : 0;
  INT64 offset = OPERATOR_has_offset(WN_operator(wn_sub))
                    ? WN_offset(wn_sub) : 0;
  TYPE_ID dtype = TY_mtype(dst_ty);
  TYPE_ID rtype = Mtype_comparison(dtype);

  // get block for internal statements
  WN* block = (rv != R_NVALUE) ? WN_CreateBlock() : WhirlBlockUtil::getCurrentBlock();
  if (comma_blk)
    block = comma_blk;

  SRCPOS spos = SetSrcPos(getLocation(expr));
  WN_Set_Linenum(block, spos);

  BOOL gen_istore = FALSE; // istore or stid
  WN *ret_value = NULL;    // return value
  WN *rhs = NULL;          // rhs for the store
  WN *addr = NULL;         // address to be load/store
  TY_IDX load_ty = TY_IDX_ZERO;  // ty for iload/istore
  ST *st = NULL;           // st to be load/store

  if (WN_operator(wn_sub) == OPR_ILOAD) {
    gen_istore = TRUE;
    load_ty = WN_load_addr_ty(wn_sub);
    // handle address to load/store
    addr = WN_kid0(wn_sub);
    Is_True(WN_rtype(addr) == Pointer_Mtype, ("not ptr mtype"));
    TY_IDX ptr_ty = Make_Pointer_Type(dst_ty);

#if 0  // creating pointer temporaries is bad for alias analysis
    // don't gen copy addr for 'this' symbol
    if (!WN_has_sym(addr)
        || strcmp(ST_name(WN_st(addr)), "this") != 0
        || TY_kind(TY_pointed(WN_type(addr))) != KIND_STRUCT) {
#else
    if (WN_hasReturnValueLDID(addr))
#endif
    {
      addr = Handle_expr_for_copy(block, addr, ptr_ty, GetSrcPos());
      if (addr != WN_kid0(wn_sub))
        WN_kid0(wn_sub) = addr;
    }
    addr = WN_COPY_Tree_Without_Commas(addr);
  }
  else {
    Is_True(WN_operator(wn_sub) == OPR_LDID, ("not ldid"));
    st = WN_st(wn_sub);
  }

  // handle return value for postfix incr/decr
  if (rv != R_NVALUE && expr->isPostfix()) {
    WN *st_wn = WGEN_StidTemp(dst_ty, wn_sub, ".anon.");
    WN_INSERT_BlockLast(block, st_wn);
    WN_Set_Linenum(st_wn, GetSrcPos());
    wn_sub = WN_CreateLdid(OPR_LDID, rtype, WN_desc(st_wn),
                           WN_offset(st_wn), WN_st(st_wn), dst_ty);
    ret_value = WN_COPY_Tree(wn_sub);
  }

  // handle bool types
  QualType type = expr->getSubExpr()->getType();
  if (type->isBooleanType()) {
    Is_True(expr->isIncrementOp(), ("only incr on boolean type"));
    rhs = WN_Intconst(rtype, 1);
  }
  else {
    WN *one = NULL;
    if (type->isIntegerType()) {
      one = WN_Intconst(rtype, 1);
    }
    else if (type->isRealFloatingType()) {
      one = WN_Floatconst(Mtype_complex_to_real(rtype), 1.0);
    }
    else if (const PointerType *ptr = type->getAs<PointerType>()) {
      QualType ptree = ptr->getPointeeType();
      if (const VariableArrayType *VAT =
            _builder->Context()->getAsVariableArrayType(ptree)) {
        // handle Array upper bound for VariableArrayType
        WN *length = ConvertToNode(VAT->getSizeExpr());
        TY_IDX rty = WN_type(length);
        TYPE_ID mtyp = TY_mtype(rty);
        ST *leng_st = Gen_Temp_Symbol(rty, "__save_expr");
        WN *st_wn = WN_Stid(mtyp, 0, leng_st, rty, length);
        WN_INSERT_BlockLast(block, st_wn);
        WN_Set_Linenum(st_wn, spos);

        TYPE_ID bound_type = Widen_mtype(mtyp);
        TY_IDX bound_ty = MTYPE_To_TY(bound_type);
        WN *ldid = WN_CreateLdid(OPR_LDID, bound_type, mtyp, 0, leng_st, rty);
        WN *sub = WN_Binary(OPR_SUB, TY_mtype(rty), ldid,
                            WN_Intconst(bound_type, 1));
        ST *bound_st = Gen_Temp_Symbol(bound_ty, "__vla_bound");
        WN *stid = WN_Stid(bound_type, 0, bound_st, bound_ty, sub);
        WN_INSERT_BlockLast(block, stid);
        WN_Set_Linenum(stid, spos);

        WN *pragma = WN_CreateXpragma(WN_PRAGMA_COPYIN_BOUND, (ST_IDX)NULL, 1);
        WN_kid0(pragma) = WN_Ldid(bound_type, 0, bound_st, bound_ty);
        WN_INSERT_BlockLast(block, pragma);
        WN_Set_Linenum(pragma, spos);

        one = WN_COPY_Tree(ldid);
      }
      else {
        Is_True(!type->isFunctionType(), ("func ptr ++?"));
        TY_IDX ptr_ty = _builder->TB().ConvertType(ptree);
        one = WN_Intconst(rtype, TY_size(ptr_ty));
      }
    }
    else {
      Is_True(FALSE, ("TODO"));
    }
    rhs = WN_Binary(expr->isIncrementOp() ? OPR_ADD : OPR_SUB,
                    rtype, wn_sub, one);
  }
  Is_True(rhs != NULL, ("rhs is NULL"));

  if (rv != R_NVALUE && expr->isPrefix()) {
    Is_True(ret_value == NULL, ("ret value already set"));
    ret_value = WN_COPY_Tree(wn_sub);
  }

  WN *store = NULL;
  if (gen_istore) {
    Is_True(addr != NULL, ("addr is NULL"));
    store = WN_CreateIstore(OPR_ISTORE, MTYPE_V, dtype,
                            offset, load_ty, rhs, addr, field_id);
  }
  else {
    Is_True(st != NULL, ("st is NULL"));
    store = WN_Stid(dtype, offset, st, ST_type(st), rhs, field_id);
  }
  WN_INSERT_BlockLast(block, store);
  WN_Set_Linenum(store, spos);

  if (rv != R_NVALUE) {
    Is_True(ret_value != NULL, ("ret value is null"));
    WN *comma = WGEN_CreateComma(rtype, block, ret_value);
    Result r = Result::nwNode(comma, dst_ty);
    r.SetRValue();
    return r;
  }
  else {
    return Result::nwNone();
  }
}

Result
WhirlExprBuilder::ConvertUnaryOperator(const UnaryOperator *expr, BOOL retv) {
  TRACE_FUNC();
  if (expr->getOpcode() == clang::UO_AddrOf) {
    Result r = ConvertExpr(expr->getSubExpr());
    if (r.IsDeref()) {
      r.ResetDeref();
      return r;
    }
    if (r.IsRef()) {
      Is_True(TY_kind(r.Ty()) == KIND_POINTER, ("not ptr type"));
      WN *ret = NULL;
      if (r.isNode())
        ret = WN_Iload(Pointer_Mtype, 0, r.Ty(), r.Node());
      else
        ret = r.GetLValue();
      return Result::nwNode(ret, r.Ty());
    }
    if (r.FieldId()) {
      Is_True(TY_kind(r.Ty()) == KIND_STRUCT ||
              (TY_kind(r.Ty()) == KIND_POINTER &&
               TY_kind(TY_pointed(r.Ty())) == KIND_STRUCT),
              ("not ptr or struct type"));
      TY_IDX ret_ty = r.isSym() ? Make_Pointer_Type(r.Ty()) : r.Ty();
      return Result::nwNode(r.GetLValue(), ret_ty);
    }
    r.SetAddrOf();
    return r;
  }
  else if (expr->getOpcode() == clang::UO_Deref) {
    Result r = ConvertExpr(expr->getSubExpr());
    TY_IDX ty = _builder->TB().ConvertType(expr->getSubExpr()->getType());
    if (r.IsRValue() && !expr->isRValue())
      r.ResetRValue();
    if (r.IsAddrOf()) {
      r.ResetAddrOf();
      return r;
    }
    if (r.FieldId() == 0)
      r.SetType(ty);
    r.SetDeref();
    return r;
  }

  WN *wn;
  TY_IDX dst_ty = _builder->TB().ConvertType(expr->getType());
  WN *wn_rt = NULL;
  if (expr->getOpcode() == UO_PostInc ||
      expr->getOpcode() == UO_PostDec ||
      expr->getOpcode() == UO_PreInc ||
      expr->getOpcode() == UO_PreDec) {
    Result r = ConvertExpr(expr->getSubExpr());
    r = r.ConvertToRValue(dst_ty);
    RV rv = GetRV(expr, retv);
    Is_True(r.isNode(), ("invalid r"));
    WN *wn_sub = r.Node();
    // fix up for OPR_CSELECT: such as: --(i ? i : j)
    if (WN_operator(wn_sub) == OPR_CSELECT) {
      WN *kid = NULL;
      int i = 1;
      for (i; i < WN_kid_count(wn_sub); i++) {
        kid = WN_COPY_Tree(WN_kid(wn_sub, i));
        Result ret = ConvertPrePostIncDec(expr, kid, dst_ty, rv);
        if (ret.isNode())
          kid = ret.Node();
        else if (ret.isSym()) {
          ST_IDX st = ret.Sym();
          kid = WN_Ldid(TY_mtype(ST_type(st)), 0, ST_ptr(st), ST_type(st));
        }
        kid = Get_real_wn(kid, expr, dst_ty);
        WN_kid(wn_sub, i) = kid;
      }
      return Result::nwNode(wn_sub, dst_ty);
    } else {
      return ConvertPrePostIncDec(expr, wn_sub, dst_ty, rv);
    }
  }

  WN *wn_sub = ConvertToNode(expr->getSubExpr());
  switch (expr->getOpcode()) {
    case clang::UO_Minus:
      wn_rt = WN_Unary(OPR_NEG, WN_rtype(wn_sub), wn_sub);
      break;
    case clang::UO_Not:
      wn_rt = WN_Unary(OPR_BNOT, WN_rtype(wn_sub), wn_sub);
      break;
    case clang::UO_LNot:
      wn_rt = WN_EQ(MTYPE_I4, Handle_cond_wn(wn_sub),
                    WN_Intconst(MTYPE_I4, 0));
      break;
    case clang::UO_Plus:
    case clang::UO_Extension:
      wn_rt = wn_sub;
      break;
    case clang::UO_Real:
    case clang::UO_Imag: {
      TYPE_ID mtype = TY_mtype(dst_ty);
      Is_True(MTYPE_float(mtype), ("invalid type"));
      Is_True(!expr->isGLValue() || WN_rtype(wn_sub) == Pointer_Mtype,
              ("invalid pointer mtype"));
      if (expr->getOpcode() == UO_Real)
        wn_rt = expr->isGLValue()
                  ? WN_Add(Pointer_Mtype, wn_sub, WN_Intconst(Pointer_Mtype, 0))
                  : WN_Unary(OPR_FIRSTPART, Widen_mtype(mtype), wn_sub);
      else
        wn_rt = expr->isGLValue()
                  ? WN_Add(Pointer_Mtype, wn_sub, WN_Intconst(Pointer_Mtype,
                                                              MTYPE_byte_size(mtype)))
                  : WN_Unary(OPR_SECONDPART, Widen_mtype(mtype), wn_sub);
      if (expr->isGLValue()) {
        TY_IDX pty = Make_Pointer_Type(dst_ty);
        Result r = Result::nwNode(wn_rt, pty);
        r.SetDeref();  // A temporary hack, how?
        return r;
      }
      break;
    }
    case clang::UO_Coawait:
      Is_True(false, ("unsupported UO_Coawait"));
    default:
      Is_True(false, ("Unsupported opcode for UnaryOperator"));
  }
  return Result::nwNode(wn_rt, dst_ty);
}

Result
WhirlExprBuilder::ConvertUserDefinedLiteral(const UserDefinedLiteral *expr, BOOL retv) {
  TRACE_FUNC();
  return ConvertCallExpr(cast<CallExpr>(expr), retv);
}

WN*
WhirlExprBuilder::ConvertToNode(const clang::Expr *expr, Result dest, BOOL retv) {
  Result r = ConvertExpr(expr, dest, retv);
  if (r.isNone()) {
    return NULL;
  }
  else if (!retv) {
    if (r.isNode()) {
      WN *node = r.Node();
      if (OPERATOR_is_expression(WN_operator(node)))
        node = WN_CreateEval(node);
      WN_Set_Linenum(node, GetSrcPos());
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), node);
    }
    return NULL;
  }
  else if (r.isSym() && expr->getType()->isMemberPointerType()) {
    if (expr->isGLValue())
      return r.GetLValue();;

    TY_IDX sub_ty = _builder->TB().ConvertType(expr->getType());
    const Expr *sub = expr->IgnoreParenCasts();
    const MemberPointerType *mpt =
        expr->getType()->getAs<MemberPointerType>();
    WN *init_blk = WhirlBlockUtil::nwBlock();
    Result ptr = Result::nwNone();
    if (!(mpt->isMemberFunctionPointer()) ||
        !(isa<UnaryOperator>(sub) &&
          cast<UnaryOperator>(sub)->getOpcode() == UO_AddrOf)) {
      ptr = ConvertExpr(sub);
    } else {
      ptr = ConvertCXXMFPToINITV(sub, sub_ty);
    }
    WhirlBlockUtil::popCurrentBlock();
    WN *ret = ptr.GetRValue();
    if (WN_first(init_blk))
      ret = WGEN_CreateComma(TY_mtype(sub_ty), init_blk, ret);
    return ret;
  }
  else if (expr->isGLValue()) {
    return r.GetLValue();
  }
  else if (expr->isRValue()) {
    return r.GetRValue();
  }
  else {
    return r.IsLValue() ? r.GetLValue() : r.GetRValue();
  }
}

// --------------------------------------------------------------------
// expand a VA_ARG_EXPR node for scalar type according to X86-64 ABI and
// return the WHIRL node that represents the address to be dereferenced;
// 'twice' is true is loading two consecutive parameters of the same type
// because they belong to a struct;
// currently, twice is TRUE only if isfloat is FALSE
// --------------------------------------------------------------------
static WN *
Gen_x8664_va_arg(WN *ap_wn, BOOL isfloat, TY_IDX ty_idx,
                 BOOL twice, SRCPOS spos) {

  // compare gp_offset with 48 or fp_offset with 176
  WN *wn0 = WN_Iload(MTYPE_I4, !isfloat ? 0 : 4, MTYPE_To_TY(MTYPE_I4),
                     WN_COPY_Tree(ap_wn));
  WN *wn1 = WN_Intconst(MTYPE_I4, (!isfloat ? 48 : 176) - (twice ? 8 : 0));
  WN *wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  LABEL_IDX lab1;
  New_LABEL (CURRENT_SYMTAB, lab1);
  WN *lab1_wn = WN_CreateLabel((ST_IDX) 0, lab1, 0, NULL);
  wn = WN_CreateTruebr(lab1, wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");
  /* compute reg_save_area+gp_offset/fp_offset and store to arg_temp_st */
  wn0 = WN_Iload(MTYPE_I4, !isfloat ? 0 : 4, MTYPE_To_TY(MTYPE_I4),
                 WN_COPY_Tree(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype),
                 WN_COPY_Tree(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  /* increment gp_offset by 8 or fp_offset by 16 */
  wn0 = WN_Iload(MTYPE_I4, !isfloat ? 0 : 4, MTYPE_To_TY(MTYPE_I4),
                 WN_COPY_Tree(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, (!isfloat ? 8 : 16) * ((INT)twice+1));
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, !isfloat ? 0 : 4,
                 Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)),
                 WN_COPY_Tree(ap_wn), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  /* branch around next part */
  LABEL_IDX lab2;
  New_LABEL(CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel ((ST_IDX) 0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), lab1_wn);
  WN_Set_Linenum(lab1_wn, spos);

  /* store overflow_arg_area pointer to arg_temp_st */
  wn0 = WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx),
                 WN_COPY_Tree(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  /* increment overflow_arg_area pointer by 8 */
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype),
                 WN_COPY_Tree(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, twice ? 16 : 8);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Istore(Pointer_Mtype, 8, Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
                 WN_COPY_Tree(ap_wn), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), lab2_wn);
  WN_Set_Linenum(lab1_wn, spos);

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}

// --------------------------------------------------------------------
// expand a VA_ARG_EXPR node for struct type being passed in 2 different classes
// of registers, according to X86-64 ABI and return the WHIRL node that
// represents the address to be dereferenced; this requires allocating a
// temporary for assembling the struct if passed in registers; isfloat0 is
// for the first 8-byte and isfloat1 is for the second 8-byte
// --------------------------------------------------------------------
static WN *
Gen_x8664_va_arg_2_mixed(WN *ap_wn, BOOL isfloat0, BOOL isfloat1,
                         TY_IDX ty_idx, SRCPOS spos)
{
  // compare gp_offset with 48
  WN *wn0 = WN_Iload(MTYPE_I4, 0, MTYPE_To_TY(MTYPE_I4), WN_COPY_Tree(ap_wn));
  WN *wn1 = WN_Intconst(MTYPE_I4, 48);
  WN *wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  LABEL_IDX lab1;
  New_LABEL(CURRENT_SYMTAB, lab1);
  WN *lab1_wn = WN_CreateLabel((ST_IDX) 0, lab1, 0, NULL);
  wn = WN_CreateTruebr(lab1, wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // compare fp_offset with 176
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_COPY_Tree(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 176);
  wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  wn = WN_CreateTruebr(lab1, wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // allocate a temporary location to assemble the structure value
  ST *struct_temp_st = Gen_Temp_Symbol(ty_idx, ".va_arg_struct");

  // compute reg_save_area+gp_offset and store dereferenced value to
  // struct_temp_st
  wn0 = WN_Iload(MTYPE_I4, 0, MTYPE_To_TY(MTYPE_I4), WN_COPY_Tree(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype),
                 WN_COPY_Tree(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_I8, 0, MTYPE_To_TY(MTYPE_I8), wn);
  wn = WN_Stid(MTYPE_I8, isfloat0 ? 8 : 0, struct_temp_st,
               MTYPE_To_TY(MTYPE_I8), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // compute reg_save_area+fp_offset and store dereferenced value to
  // struct_temp_st
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_COPY_Tree(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype),
                 WN_COPY_Tree(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_F8, 0, MTYPE_To_TY(MTYPE_F8), wn);
  wn = WN_Stid(MTYPE_F8, isfloat0 ? 0 : 8, struct_temp_st,
               MTYPE_To_TY(MTYPE_F8), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // increment gp_offset by 8
  wn0 = WN_Iload(MTYPE_I4, 0, MTYPE_To_TY(MTYPE_I4), WN_COPY_Tree(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 8);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 0, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)),
                 WN_COPY_Tree(ap_wn), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // increment fp_offset by 16
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_COPY_Tree(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 16);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 4, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)),
                 WN_COPY_Tree(ap_wn), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // put the address of struct_temp_st in arg_temp_st
  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");

  wn = WN_Lda(Pointer_Mtype, 0, struct_temp_st, 0);
  Set_ST_addr_saved(struct_temp_st);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // branch around next part
  LABEL_IDX lab2;
  New_LABEL(CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel((ST_IDX) 0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), lab1_wn);
  WN_Set_Linenum(lab1_wn, spos);

  // store overflow_arg_area pointer to arg_temp_st
  wn0 = WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx),
                 WN_COPY_Tree(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // increment overflow_arg_area pointer by 16
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype),
                 WN_COPY_Tree(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, 16);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Istore(Pointer_Mtype, 8,Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
                 WN_COPY_Tree(ap_wn), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), lab2_wn);
  WN_Set_Linenum(lab2_wn, spos);

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}

// expand a VA_ARG_EXPR node for struct type being passed in 2 float
// registers, according to X86-64 ABI and return the WHIRL node that
// represents the address to be dereferenced; this requires allocating a
// temporary for assembling the struct if passed in registers, because each
// float register is saved into 128 bit locations
static WN *
Gen_x8664_va_arg_2_float(WN *ap_wn, TY_IDX ty_idx, SRCPOS spos)
{
  LABEL_IDX lab1;
  New_LABEL(CURRENT_SYMTAB, lab1);
  WN *lab1_wn = WN_CreateLabel((ST_IDX) 0, lab1, 0, NULL);
  // compare fp_offset with 160 (176 - 16)
  WN *wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_COPY_Tree(ap_wn));
  WN *wn1 = WN_Intconst(MTYPE_I4, 160);
  WN *wn = WN_Relational(OPR_GE, MTYPE_I4, wn0, wn1);
  wn = WN_CreateTruebr(lab1, wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // allocate a temporary location to assemble the structure value
  ST *struct_temp_st = Gen_Temp_Symbol(ty_idx, ".va_arg_struct");

  // compute reg_save_area+fp_offset and store 1st dereferenced value to
  // struct_temp_st
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_COPY_Tree(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype),
                 WN_COPY_Tree(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_F8, 0, MTYPE_To_TY(MTYPE_F8), wn);
  wn = WN_Stid(MTYPE_F8, 0, struct_temp_st, MTYPE_To_TY(MTYPE_F8), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // compute reg_save_area+fp_offset and store 2nd dereferenced value to
  // struct_temp_st
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_COPY_Tree(ap_wn));
  wn1 = WN_Iload(Pointer_Mtype, 16, MTYPE_To_TY(Pointer_Mtype),
                 WN_COPY_Tree(ap_wn));
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Iload(MTYPE_F8, 16, MTYPE_To_TY(MTYPE_F8), wn);
  wn = WN_Stid(MTYPE_F8, 8, struct_temp_st, MTYPE_To_TY(MTYPE_F8), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // increment fp_offset by 32
  wn0 = WN_Iload(MTYPE_I4, 4, MTYPE_To_TY(MTYPE_I4), WN_COPY_Tree(ap_wn));
  wn1 = WN_Intconst(MTYPE_I4, 32);
  wn = WN_Binary(OPR_ADD, MTYPE_I4, wn0, wn1);
  wn = WN_Istore(MTYPE_I4, 4, Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4)),
                 WN_COPY_Tree(ap_wn), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // put the address of struct_temp_st in arg_temp_st
  ST *arg_temp_st = Gen_Temp_Symbol(Make_Pointer_Type(ty_idx), ".va_arg");

  wn = WN_Lda(Pointer_Mtype, 0, struct_temp_st, 0);
  Set_ST_addr_saved(struct_temp_st);
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // branch around next part
  LABEL_IDX lab2;
  New_LABEL(CURRENT_SYMTAB, lab2);
  WN *lab2_wn = WN_CreateLabel((ST_IDX) 0, lab2, 0, NULL);
  wn = WN_CreateGoto(lab2);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), lab1_wn);
  WN_Set_Linenum(lab1_wn, spos);

  // store overflow_arg_area pointer to arg_temp_st
  wn0 = WN_Iload(Pointer_Mtype, 8, Make_Pointer_Type(ty_idx),
                 WN_COPY_Tree(ap_wn));
  wn = WN_Stid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx), wn0);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // increment overflow_arg_area pointer by 16
  wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype),
                 WN_COPY_Tree(ap_wn));
  wn1 = WN_Intconst(MTYPE_U8, 16);
  wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
  wn = WN_Istore(Pointer_Mtype, 8,Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
                 WN_COPY_Tree(ap_wn), wn);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), lab2_wn);
  WN_Set_Linenum(lab2_wn, spos);

  return WN_Ldid(Pointer_Mtype, 0, arg_temp_st, Make_Pointer_Type(ty_idx));
}

Result
WhirlExprBuilder::ConvertVAArgExpr(const VAArgExpr *expr) {
  TRACE_FUNC();
  QualType type = expr->getType();
  WN *ap_wn, *wn, *wn0, *wn1;

  if (type->isVariablyModifiedType())
    Is_True(false, ("unsupported ConvertVAArgExpr"));

  SRCPOS spos = SetSrcPos(getLocation(expr));
  ap_wn = ConvertToNode(expr->getSubExpr());

  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  //TODO: Fix_TY_mtype here?
  TYPE_ID mtype = TY_mtype(ty_idx);
#ifdef TARG_X8664
  if (mtype != MTYPE_F10 && mtype != MTYPE_M &&
      !MTYPE_is_complex(mtype)) {
    wn = Gen_x8664_va_arg(ap_wn, MTYPE_float(mtype), ty_idx, FALSE, spos);
    wn = WN_CreateIload(OPR_ILOAD, Widen_mtype(mtype), mtype, 0,
                        ty_idx, Make_Pointer_Type(ty_idx), wn);
  }
  else if (MTYPE_is_complex(mtype)) {
    Is_True((mtype == MTYPE_C4 || mtype == MTYPE_C8 ||
             mtype == MTYPE_C10 || mtype == MTYPE_C16 ||
             mtype == MTYPE_CQ),
             ("unexpected complex type"));
    wn = Gen_x8664_va_arg(ap_wn, MTYPE_float(mtype), ty_idx, FALSE, spos);
    wn = WN_CreateIload(OPR_ILOAD, mtype, mtype, 0, ty_idx,
                        Make_Pointer_Type(ty_idx), wn);
  }
  else {
    enum X86_64_PARM_CLASS classes[MAX_CLASSES];
    INT n = Classify_Aggregate(ty_idx, classes);
    // handle X87 X87UP and COMPLEX_X87 cases
    if (n != 0 && (classes[0] == X86_64_X87_CLASS ||
                   classes[0] == X86_64_X87UP_CLASS ||
                   classes[0] == X86_64_COMPLEX_X87_CLASS)) {
       // x87, x87up and complex_x87 are passed in memory
       n = 0;
    }
    if (n == 0) {
      // can only pass in memory
      // increment overflow_arg_area pointer by 8
      INT delta = ((TY_size(ty_idx) + 7) / 8) * 8;
      wn0 = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype),
                     WN_COPY_Tree(ap_wn));
      wn1 = WN_Intconst(MTYPE_U8, delta);
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn0, wn1);
      wn = WN_Istore(Pointer_Mtype, 8,
                     Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype)),
                     WN_COPY_Tree(ap_wn), wn);
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
      WN_Set_Linenum(wn, spos);

      // load pointer to overflow_arg_area
      wn = WN_Iload(Pointer_Mtype, 8, MTYPE_To_TY(Pointer_Mtype),
                    WN_COPY_Tree(ap_wn));
      // adjust with the amount just incremented
      wn1 = WN_Intconst(MTYPE_I8, -delta);
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, WN_COPY_Tree(wn0), wn1);
    }
    else if (n == 1) {
      wn = Gen_x8664_va_arg(ap_wn, classes[0] == X86_64_SSE_CLASS,
                            ty_idx, FALSE, spos);
    }
    else if (n > 1) {
      // must be == 2
      if (classes[0] == classes[1]) {
        if (classes[0] == X86_64_INTEGER_CLASS)
          wn = Gen_x8664_va_arg(ap_wn, classes[0] == X86_64_SSE_CLASS,
                                ty_idx, TRUE/*twice*/, spos);
        else
          wn = Gen_x8664_va_arg_2_float(ap_wn, ty_idx, spos);
      }
      else {
        wn = Gen_x8664_va_arg_2_mixed(ap_wn,
                                      classes[0] == X86_64_SSE_CLASS,
                                      classes[1] == X86_64_SSE_CLASS,
                                      ty_idx, spos);
      }
    }

    if( mtype == MTYPE_F10 )
      wn = WN_CreateIload(OPR_ILOAD, Widen_mtype (mtype), mtype, 0,
                          ty_idx, Make_Pointer_Type(ty_idx), wn);
    else
      wn = WN_CreateIload(OPR_ILOAD, MTYPE_M, MTYPE_M, 0, ty_idx,
                          Make_Pointer_Type(ty_idx), wn);
  }
  return Result::nwNode(wn, ty_idx);
#else
  Is_True(false, ("TODO: not supported varg for non-x86 yet"));
  return Result::nwNone();
#endif
}

Result
WhirlExprBuilder::ConvertVAArgExpr32Bit(const VAArgExpr *expr) {
  SRCPOS spos = SetSrcPos(getLocation(expr));
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  TYPE_ID mtype = TY_mtype(ty_idx);
  INT64 rounded_size = ((TY_size(ty_idx) + 3) / 4) * 4;

  WN *ap_wn = ConvertToNode(expr->getSubExpr());
  Is_True(WN_operator(ap_wn) == OPR_LDID ||
          WN_operator(ap_wn) == OPR_ILOAD,
          ("unexpected ap_wn operator"));

  TY_IDX ap_ty_idx = _builder->TB().ConvertType(expr->getSubExpr()->getType());
  WN *ap_addr = NULL;
  TY_IDX ap_addr_ty = TY_IDX_ZERO;
  ST *ap_st = NULL;
  WN_OFFSET ap_offset = 0;
  UINT32 ap_field_id = 0;
  if (WN_operator(ap_wn) == OPR_LDID) {
    ap_st = WN_st(ap_wn);
    ap_offset = WN_offset(ap_wn);
    ap_field_id = WN_field_id(ap_wn);
  }
  else if (WN_operator(ap_wn) == OPR_ILOAD) {
    ap_offset = WN_offset(ap_wn);
    ap_field_id = WN_field_id(ap_wn);
    ap_addr = WN_COPY_Tree(WN_kid0(ap_wn));
    ap_addr_ty = WN_load_addr_ty(ap_wn);
  }
  else {
    Is_True(FALSE, ("unsupported ap_wn operator"));
    return Result::nwNone();
  }

  // set wn to the next slot
  WN *wn = WN_Binary(OPR_ADD, Pointer_Mtype, ap_wn,
                     WN_Intconst(Pointer_Mtype, rounded_size));

  // pad wn for vector types to 8-byte aligned
  if (MTYPE_is_vector(mtype)) {
    wn = WN_Add(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 7));
    wn = WN_Div(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 8));
    wn = WN_Mpy(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, 8));
  }

  // store wn to ad addr
  if (ap_st)
    wn = WN_Stid (Pointer_Mtype, ap_offset, ap_st, ap_ty_idx, wn, ap_field_id);
  else
    wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, Pointer_Mtype, ap_offset,
                         ap_addr_ty, wn,
                         WN_COPY_Tree(ap_wn), ap_field_id);
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
  WN_Set_Linenum(wn, spos);

  // load value from ap addr
  wn = WN_CreateIload(OPR_ILOAD, Mtype_comparison(mtype), mtype, -rounded_size,
                      ty_idx, Make_Pointer_Type(ty_idx, FALSE),
                      ap_wn);
  return Result::nwNode(wn, ty_idx);
}

Result
WhirlExprBuilder::ConvertCXXThisExpr(const CXXThisExpr *expr) {
  TRACE_FUNC();
  // convert ty
  ST_IDX st_idx = _builder->Scope().Get_this();
  Is_True(st_idx != (ST_IDX)0, ("symbol must have created"));
  TY_IDX ty_idx = ST_type(st_idx);
  Is_True(TY_kind(ty_idx) == KIND_POINTER && TY_kind(TY_pointed(ty_idx)) == KIND_STRUCT,
          ("invalid ty for CXXThisExpr"));
  // get this for current CXXMethodDecl
  
  if (_builder->LambdaHelper().IsInLambdaBody()) {
    // handled captured this in lambda
    const CXXMethodDecl *method = dyn_cast<CXXMethodDecl>(_builder->Scope().CurrentDecl());
    Is_True(method != NULL, ("not a CXXMethodDecl"));
    const CXXRecordDecl *record = method->getParent();
    Is_True(record != NULL, ("not find the CXXRecordDecl"));
    // get FieldDecl for captured "this" field
    const FieldDecl *field = _builder->LambdaHelper().GetCapFld(record);
    Is_True(field != NULL, ("this not captured in lambda"));
    UINT32 fld_id = field->getFieldIndex() + 1;
    TY_IDX record_ty = _builder->TB().ConvertType(expr->getType());
    Is_True(TY_kind(record_ty) == KIND_POINTER && TY_kind(TY_pointed(record_ty)) == KIND_STRUCT,
            ("invalid ty for CXXRecordDecl"));
    UINT32 cur_fld_id = 0;
    UINT64 ofst = 0;
    FLD_HANDLE fh = get_fld_and_offset(ty_idx, fld_id, cur_fld_id, ofst);
    Is_True(!fh.Is_Null(), ("not find the field"));
    WN *load_addr = WN_Ldid(Pointer_Mtype, 0, st_idx, ty_idx);
    WN *ret = WN_Iload(Pointer_Mtype, ofst, TY_pointed(ty_idx), load_addr, fld_id);
    return Result::nwNode(ret, record_ty);
  }
  else {
    // handle really this
    WN *ret = WN_Ldid(TY_mtype(ty_idx), 0, ST_ptr(st_idx), ty_idx, 0);
    return Result::nwNode(ret, ty_idx);
  }
}

Result
WhirlExprBuilder::ConvertImplicitValueInitExpr(const ImplicitValueInitExpr *expr, Result dest) {
  TRACE_FUNC();
  Is_True(!dest.isNone(), ("no dest for implicit value init"));
  QualType type = expr->getType();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  WN *wn = NULL;

  UINT value = 0;
  if (type->getAs<PointerType>()) {
    value = _builder->Context()->getTargetNullPointerValue(type);
  }

  if (const ConstantArrayType *cat =
      _builder->Context()->getAsConstantArrayType(type)) {
    SRCPOS srcpos = SetSrcPos(getLocation(expr));
    unsigned num_element = cat->getSize().getZExtValue();
    WN *num_wn = WN_Intconst(num_element > INT_MAX ? MTYPE_I8
                                                   : MTYPE_I4,
                             num_element);
    TY_IDX ty_ele = TY_etype(ty_idx);
    TY_IDX ty_ptr = Make_Pointer_Type(ty_ele);
    WN *targ_wn = dest.GetLValue();
    WN *st_wn = WGEN_StidTemp(ty_ptr, targ_wn, ".init.iter");
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
    WN_Set_Linenum(st_wn, srcpos);

    WN *ld_wn = WN_Ldid(Pointer_Mtype, WN_offset(st_wn), WN_st(st_wn), ty_ptr);
    WN *istr_wn = WN_Istore(TY_mtype(ty_ele), 0, ty_ptr, ld_wn, Gen_null_const(ty_ele));
    WN *parm[1] = { st_wn };
    wn = ConvertConstantArray(ty_ele, num_wn, istr_wn, parm, 1);
    Is_True(wn && WN_operator(wn) == OPR_BLOCK, ("bad wn"));
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
    return Result::nwNone();
  }
  else {
    wn = Gen_null_const(ty_idx);
  }
  return Result::nwNode(wn, ty_idx);
}

Result
WhirlExprBuilder::ConvertCharacterLiteral(const CharacterLiteral *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  return Result::nwIntConst(expr->getValue(), ty_idx);
}

// Return the size or alignment of the type of argument of the sizeof expression as an integer.
Result
WhirlExprBuilder::ConvertUnaryExprOrTypeTraitExpr(const UnaryExprOrTypeTraitExpr *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  if (expr->getKind() == UETT_SizeOf) {
    QualType type_to_size = expr->getTypeOfArgument();
    if (const VariableArrayType *VAT =
        _builder->Context()->getAsVariableArrayType(type_to_size)) {
      TY_IDX ety = _builder->TB().ConvertType(VAT->getElementType());
      WN *elem_cnt = ConvertToNode(VAT->getSizeExpr());
      INT64 ty_size = TY_size(ety) ? TY_size(ety) : 1;
      WN *size_cnt = WN_Mpy(TY_mtype(ty_idx), elem_cnt,
                            WN_Intconst(TY_mtype(ty_idx), ty_size));
      return Result::nwNode(size_cnt, ty_idx);
    }
  } else if (expr->getKind() == UETT_OpenMPRequiredSimdAlign) {
    Is_True(false, ("unsupported UETT_OpenMPRequiredSimdAlign for UnaryExprOrTypeTraitExpr"));
  }

  // The result must be constant, use the constant folding logic
  llvm::APSInt value = expr->EvaluateKnownConstInt(*(_builder->Context()));
  WN *wn = WN_Intconst(TY_mtype(ty_idx), value.getZExtValue());
  return Result::nwNode(wn, ty_idx);
}

Result
WhirlExprBuilder::ConvertCXXBoolLiteralExpr(const CXXBoolLiteralExpr *expr) {
  TRACE_FUNC();
  return Result::nwIntConst(expr->getValue(), MTYPE_To_TY(MTYPE_I4));
}

Result
WhirlExprBuilder::ConvertImaginaryLiteral(const ImaginaryLiteral *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  Result elem = ConvertExpr(expr->getSubExpr());

  TCON_IDX tcon = Convert_imaginary_to_tcon(expr, elem, ty_idx);
  return Result::nwTcon(tcon, ty_idx);
}

Result
WhirlExprBuilder::ConvertOffsetOfExpr(const OffsetOfExpr *expr) {
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  // fold the offsetof to a constant
  llvm::APSInt value;
  if (evaluateAsInt(expr, value, _builder->Context())) {
    return Result::nwIntConst(value.getZExtValue(), ty_idx);
  }

  // Loop over the components of the offsetof to compute the value
  unsigned n = expr->getNumComponents();
  QualType current_type = expr->getTypeSourceInfo()->getType();
  for (unsigned i = 0; i != n; ++i) {
    OffsetOfNode offset_node = expr->getComponent(i);
    switch (offset_node.getKind()) {
      case OffsetOfNode::Array: {
        break;
      }
      case OffsetOfNode::Field: {
        break;
      }
      case OffsetOfNode::Identifier:
        Is_True(false, ("dependent __builtin_offsetof"));
      case OffsetOfNode::Base: {
        if (offset_node.getBase()->isVirtual()) {
          Is_True(false, ("unsupported: virtual base in offsetof"));
          continue;
        }
        RecordDecl *record = current_type->getAs<RecordType>()->getDecl();
        const ASTRecordLayout &record_layout = _builder->Context()->getASTRecordLayout(record);

        // Save the element type.
        current_type = offset_node.getBase()->getType();

        // Compute the offset to the base.
        const RecordType *base_rt = current_type->getAs<RecordType>();
        CXXRecordDecl *base_rd = cast<CXXRecordDecl>(base_rt->getDecl());
        CharUnits offset_int = record_layout.getBaseClassOffset(base_rd);
        INT64 ofst_value = offset_int.getQuantity();
        TYPE_ID ofst_mty = (ofst_value >> 32) ? MTYPE_I8 : MTYPE_I4;
        Result offset = Result::nwIntConst(offset_int.getQuantity(), MTYPE_To_TY(ofst_mty));
        break;
      }
      default:
         Is_True(false, ("unsupported"));
    }

  }
  return Result::nwNone();
}


Result
WhirlExprBuilder::ConvertSizeOfPackExpr(const SizeOfPackExpr *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  WN *ret = WN_Intconst(TY_mtype(ty_idx), expr->getPackLength());
  return Result::nwNode(ret, ty_idx);
}

Result
WhirlExprBuilder::ConvertCXXScalarValueInitExpr(const CXXScalarValueInitExpr *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  WN *ret = Gen_null_const(ty_idx);
  return Result::nwNode(ret, ty_idx);
}

Result
WhirlExprBuilder::ConvertCXXNoexceptExpr(const CXXNoexceptExpr *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  return Result::nwNode(WN_Intconst(Mtype_comparison(TY_mtype(ty_idx)),
                                    expr->getValue()),
                        ty_idx);
}

Result
WhirlExprBuilder::ConvertCXXNullPtrLiteralExpr(const CXXNullPtrLiteralExpr *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  return Result::nwNode(WN_Intconst(TY_mtype(ty_idx), 0), ty_idx);
}

Result
WhirlExprBuilder::ConvertAddrLabelExpr(const AddrLabelExpr *expr) {
  TRACE_FUNC();
  // get label idx
  LABEL_IDX label_idx = _builder->Get_label_idx(expr->getLabel());
  WN *wn = WN_LdaLabel(Pointer_Mtype, label_idx);
  Set_LABEL_addr_saved(label_idx);

  // we should not prevent inlining of function explicitly marked
  // static inline just because a label therein had its address taken.
  if (ST_export(Get_Current_PU_ST()) != EXPORT_LOCAL)
    Set_PU_no_inline(Get_Current_PU());

  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  return Result::nwNode(wn, ty_idx);
}

Result
WhirlExprBuilder::ConvertExpr(const Expr *expr, Result dest, BOOL retv) {
  TRACE_FUNC();
  Result r = Result::nwNone();
  switch (expr->getStmtClass()) {
    case Expr::ArrayInitLoopExprClass:
      r = ConvertArrayInitLoopExpr(cast<ArrayInitLoopExpr>(expr), dest);
      break;
    case Expr::ArraySubscriptExprClass:
      r = ConvertArraySubscriptExpr(cast<ArraySubscriptExpr>(expr));
      break;
    case Expr::BinaryConditionalOperatorClass:
      r = ConvertBinaryConditionalOperator(cast<BinaryConditionalOperator>(expr));
      break;
    case Expr::BinaryOperatorClass:
      r = ConvertBinaryOperator(cast<BinaryOperator>(expr), retv);
      break;
    case Expr::CallExprClass:
      r = ConvertCallExpr(cast<CallExpr>(expr), retv);
      break;
    case Expr::CharacterLiteralClass:
      r = ConvertCharacterLiteral(cast<CharacterLiteral>(expr));
      break;
    case Expr::ChooseExprClass:
      r = ConvertChooseExpr(cast<ChooseExpr>(expr));
      break;
    case Expr::ConvertVectorExprClass:
      r = ConvertConvertVectorExpr(cast<ConvertVectorExpr>(expr));
      break;
    case Expr::CompoundAssignOperatorClass:
      r = ConvertCompoundAssignOperator(cast<CompoundAssignOperator>(expr), retv);
      break;
    case Expr::CompoundLiteralExprClass:
      r = ConvertCompoundLiteralExpr(cast<CompoundLiteralExpr>(expr), dest);
      break;
    case Expr::ConditionalOperatorClass:
      r = ConvertConditionalOperator(cast<ConditionalOperator>(expr), retv);
      break;
    case Expr::CStyleCastExprClass:
      r = ConvertCStyleCastExpr(cast<CStyleCastExpr>(expr), dest);
      break;
    case Expr::CXXBindTemporaryExprClass:
      r = ConvertCXXBindTemporaryExpr(cast<CXXBindTemporaryExpr>(expr), dest);
      break;
    case Expr::CXXNewExprClass:
      r = ConvertCXXNewExpr(cast<CXXNewExpr>(expr));
      break;
    case Expr::CXXConstCastExprClass:
      r = ConvertCXXConstCastExpr(cast<CXXConstCastExpr>(expr));
      break;
    case Expr::CXXConstructExprClass:
      r = ConvertCXXConstructExpr(cast<CXXConstructExpr>(expr),
                                  dest, CXXCtorType::Ctor_Complete);
      break;
    case Expr::CXXDeleteExprClass:
      r = ConvertCXXDeleteExpr(cast<CXXDeleteExpr>(expr));
      break;
    case Expr::CXXDefaultArgExprClass:
      r = ConvertCXXDefaultArgExpr(cast<CXXDefaultArgExpr>(expr));
      break;
    case Expr::CXXDefaultInitExprClass:
      r = ConvertCXXDefaultInitExpr(cast<CXXDefaultInitExpr>(expr), dest);
      break;
    case Expr::CXXDynamicCastExprClass:
      r = ConvertCXXDynamicCastExpr(cast<CXXDynamicCastExpr>(expr));
      break;
    case Expr::CXXFunctionalCastExprClass:
      r = ConvertCXXFunctionalCastExpr(cast<CXXFunctionalCastExpr>(expr), dest);
      break;
    case Expr::CXXMemberCallExprClass:
      r = ConvertCXXMemberCallExpr(cast<CXXMemberCallExpr>(expr), retv);
      break;
    case Expr::CXXOperatorCallExprClass:
      r = ConvertCXXOperatorCallExpr(cast<CXXOperatorCallExpr>(expr), retv);
      break;
    case Expr::CXXReinterpretCastExprClass:
      r = ConvertCXXReinterpretCastExpr(cast<CXXReinterpretCastExpr>(expr));
      break;
    case Expr::CXXStaticCastExprClass:
      r = ConvertCXXStaticCastExpr(cast<CXXStaticCastExpr>(expr));
      break;
    case Expr::CXXStdInitializerListExprClass:
      r = ConvertCXXStdInitializerListExpr(cast<CXXStdInitializerListExpr>(expr), dest);
      break;
    case Expr::CXXThisExprClass:
      r = ConvertCXXThisExpr(cast<CXXThisExpr>(expr));
      break;
    case Expr::CXXTemporaryObjectExprClass:
      r = ConvertCXXTemporaryObjectExpr(cast<CXXTemporaryObjectExpr>(expr), dest);
      break;
    case Expr::CXXThrowExprClass:
      r = ConvertCXXThrowExpr(cast<CXXThrowExpr>(expr));
      break;
    case Expr::CXXTypeidExprClass:
      r = ConvertCXXTypeidExpr(cast<CXXTypeidExpr>(expr));
      break;
    case Expr::CXXUuidofExprClass:
      r = ConvertCXXUuidofExpr(cast<CXXUuidofExpr>(expr));
      break;
    case Expr::DeclRefExprClass:
      r = ConvertDeclRefExpr(cast<DeclRefExpr>(expr));
      break;
    case Expr::ExprWithCleanupsClass:
      r = ConvertExprWithCleanups(cast<ExprWithCleanups>(expr), dest);
      break;
    case Expr::ExtVectorElementExprClass:
      r = ConvertExtVectorElementExpr(cast<ExtVectorElementExpr>(expr));
      break;
    case Expr::FloatingLiteralClass:
      r = ConvertFloatingLiteral(cast<FloatingLiteral>(expr));
      break;
    case Expr::GenericSelectionExprClass:
      r = ConvertGenericSelectionExpr(cast<GenericSelectionExpr>(expr));
      break;
    case Expr::GNUNullExprClass:
      r = ConvertGNUNullExpr(cast<GNUNullExpr>(expr));
      break;
    case Expr::ImplicitCastExprClass:
      r = ConvertImplicitCastExpr(cast<ImplicitCastExpr>(expr), dest);
      break;
    case Expr::ImplicitValueInitExprClass:
      r = ConvertImplicitValueInitExpr(cast<ImplicitValueInitExpr>(expr), dest);
      break;
    case Expr::InitListExprClass:
      r = ConvertInitListExpr(cast<InitListExpr>(expr), dest);
      break;
    case Expr::IntegerLiteralClass:
      r = ConvertIntegerLiteral(cast<IntegerLiteral>(expr));
      break;
    case Expr::LambdaExprClass:
      r = ConvertLambdaExpr(cast<LambdaExpr>(expr), dest);
      break;
    case Expr::MaterializeTemporaryExprClass:
      r = ConvertMaterializeTemporaryExpr(cast<MaterializeTemporaryExpr>(expr), dest);
      break;
    case Expr::MemberExprClass:
      r = ConvertMemberExpr(cast<MemberExpr>(expr), retv);
      break;
#ifdef C2W_ENABLE_OBJC
    case Expr::ObjCBridgedCastExprClass:
        r = ConvertObjCBridgedCastExpr(cast<ObjCBridgedCastExpr>(expr));
        break;
    case Expr::ObjCEncodeExprClass:
        r = ConvertObjCEncodeExpr(cast<ObjCEncodeExpr>(expr));
        break;
    case Expr::ObjCIsaExprClass:
        r = ConvertObjCIsaExpr(cast<ObjCIsaExpr>(expr));
        break;
    case Expr::ObjCIvarRefExprClass:
        r = ConvertObjCIvarRefExpr(cast<ObjCIvarRefExpr>(expr));
        break;
    case Expr::ObjCMessageExprClass:
        r = ConvertObjCMessageExpr(cast<ObjCMessageExpr>(expr));
        break;
    case Expr::ObjCPropertyRefExprClass:
        r = ConvertObjCPropertyRefExpr(cast<ObjCPropertyRefExpr>(expr));
        break;
    case Expr::ObjCSelectorExprClass:
        r = ConvertObjCSelectorExpr(cast<ObjCSelectorExpr>(expr));
        break;
#endif
    case Expr::OpaqueValueExprClass:
      r = ConvertOpaqueValueExpr(cast<OpaqueValueExpr>(expr));
      break;
    case Expr::ParenExprClass:
      r = ConvertParenExpr(cast<ParenExpr>(expr), dest, retv);
      break;
#if LLVM_VERSION_MAJOR == 11
    case Expr::ConstantExprClass:
      r = ConvertConstantExpr(cast<ConstantExpr>(expr), dest, retv);
      break;
#endif
    case Expr::PredefinedExprClass:
      r = ConvertPredefinedExpr(cast<PredefinedExpr>(expr));
      break;
    case Expr::CXXPseudoDestructorExprClass:
      // Do nothing
      break;
    case Expr::PseudoObjectExprClass:
      r = ConvertPseudoObjectExpr(cast<PseudoObjectExpr>(expr));
      break;
    case Expr::ShuffleVectorExprClass:
      r = ConvertShuffleVectorExpr(cast<ShuffleVectorExpr>(expr));
      break;
    case Expr::StmtExprClass:
      r = ConvertStmtExpr(cast<StmtExpr>(expr));
      break;
    case Expr::StringLiteralClass:
      r = ConvertStringLiteral(cast<StringLiteral>(expr));
      break;
    case Expr::SubstNonTypeTemplateParmExprClass:
      r = ConvertSubstNonTypeTemplateParmExpr(cast<SubstNonTypeTemplateParmExpr>(expr));
      break;
    case Expr::TypeTraitExprClass:
      r = ConvertTypeTraitExpr(cast<TypeTraitExpr>(expr));
      break;
    case Expr::UnaryOperatorClass:
      r = ConvertUnaryOperator(cast<UnaryOperator>(expr), retv);
      break;
    case Expr::UserDefinedLiteralClass:
      r = ConvertUserDefinedLiteral(cast<UserDefinedLiteral>(expr), retv);
      break;
    case Expr::VAArgExprClass:
      r = TARGET_64BIT ? ConvertVAArgExpr(cast<VAArgExpr>(expr))
                       : ConvertVAArgExpr32Bit(cast<VAArgExpr>(expr));
      break;
    case Expr::UnaryExprOrTypeTraitExprClass:
      r = ConvertUnaryExprOrTypeTraitExpr(cast<UnaryExprOrTypeTraitExpr>(expr));
      break;
    case Expr::CXXBoolLiteralExprClass:
      r = ConvertCXXBoolLiteralExpr(cast<CXXBoolLiteralExpr>(expr));
      break;
    case Expr::ImaginaryLiteralClass:
      r = ConvertImaginaryLiteral(cast<ImaginaryLiteral>(expr));
      break;
    case Expr::OffsetOfExprClass:
      r = ConvertOffsetOfExpr(cast<OffsetOfExpr>(expr));
      break;
    case Expr::AtomicExprClass:
      r = ConvertAtomicExpr(cast<AtomicExpr>(expr), retv);
      break;
    case Expr::SizeOfPackExprClass:
      r = ConvertSizeOfPackExpr(cast<SizeOfPackExpr>(expr));
      break;
    case Expr::CXXScalarValueInitExprClass:
      r = ConvertCXXScalarValueInitExpr(cast<CXXScalarValueInitExpr>(expr));
      break;
    case Expr::CXXNoexceptExprClass:
      r = ConvertCXXNoexceptExpr(cast<CXXNoexceptExpr>(expr));
      break;
    case Expr::CXXNullPtrLiteralExprClass:
      r = ConvertCXXNullPtrLiteralExpr(cast<CXXNullPtrLiteralExpr>(expr));
      break;
    case Expr::AddrLabelExprClass:
      r = ConvertAddrLabelExpr(cast<AddrLabelExpr>(expr));
      break;
    default:
      Is_True(false,
                 ("unsupport expr: %s", expr->getStmtClassName()));
  }
  return r;
}

} // namespace wgen
