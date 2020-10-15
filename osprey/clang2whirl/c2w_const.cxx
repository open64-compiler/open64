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

#include "c2w_const.h"
#include "c2w_builder.h"
#include "c2w_stmt.h"
#include "c2w_tracer.h"
#include "c2w_expr.h"
#include "c2w_utils.h"

// clang header files
#include "clanginc.h"
#include "clang/AST/RecordLayout.h"
#include "clang/AST/VTableBuilder.h"

using namespace clang;

// open64 header files
#include "open64inc.h"

namespace wgen {

TCON_VALUE_MAP _tcon_value_map;

WhirlConstBuilder::WhirlConstBuilder(WhirlBuilder *builder)
  : _builder(builder), _const_idx(0) {
}

WhirlConstBuilder::~WhirlConstBuilder() {
}

STR_IDX
WhirlConstBuilder::NewConstantName() {
  return Save_Str2i("<const>", "_", _const_idx++);
}

Result
WhirlConstBuilder::ConvertStringLiteral(const StringLiteral *expr) {
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  TCON_IDX tcon = Convert_string_to_tcon(expr, ty_idx);
  return Result::nwTcon(tcon, ty_idx);
}

Result
WhirlConstBuilder::ConvertFloatingConstant(const clang::FloatingLiteral *expr) {
  return ConvertFloatingConstant(expr, expr->getType());
}

Result
WhirlConstBuilder::ConvertFloatingConstant(const clang::FloatingLiteral *expr, clang::QualType type) {
  TY_IDX ty_idx = _builder->TB().ConvertType(type);
  const llvm::APFloat &init = expr->getValue();
  return Result::nwTcon(Convert_float_to_tcon(ty_idx, init), ty_idx);
}

Result
WhirlConstBuilder::ConvertIntegerConstant(const clang::IntegerLiteral *expr) {
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  return Result::nwIntConst(expr->getValue().getZExtValue(), ty_idx);
}

Result
WhirlConstBuilder::ConvertCharacterLiteral(const clang::CharacterLiteral *expr) {
  return ConvertCharacterLiteral(expr, expr->getType());
}

Result
WhirlConstBuilder::ConvertCharacterLiteral(const clang::CharacterLiteral *expr, clang::QualType type) {
  TY_IDX ty_idx = _builder->TB().ConvertType(type);
  return Result::nwIntConst(expr->getValue(), ty_idx);
}

// generate initialized st for local var
static ST_IDX
Gen_local_var_st(TY_IDX ty_idx, SRCPOS spos) {
  Is_True(ty_idx != (TY_IDX)0, ("invalid ty idx"));
  ST *st = New_ST(CURRENT_SYMTAB);
  ST_Init(st, Save_Str(".init"), CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL, ty_idx);
  Set_ST_is_initialized(st);
  Set_ST_Srcpos(*st, spos);
  return ST_st_idx(st);
}

Result
WhirlConstBuilder::ConvertCastExpr(const CastExpr *expr, QualType type) {
  const Expr *sub_expr = expr->getSubExpr();
  TY_IDX from_ty = _builder->TB().ConvertType(sub_expr->getType());;
  QualType to_type = expr->getType();
  if (!type.isNull())
    to_type = type;
  TY_IDX to_ty = _builder->TB().ConvertType(to_type);

  CastKind kind = expr->getCastKind();
  switch (kind) {
  case CK_FloatingCast:
  case CK_IntegralCast:
  case CK_FloatingRealToComplex:
  case CK_IntegralRealToComplex:
  case CK_FloatingComplexCast:
  case CK_FloatingComplexToIntegralComplex:
  case CK_IntegralComplexCast:
  case CK_IntegralComplexToFloatingComplex:
  case CK_FloatingComplexToReal:
  case CK_IntegralComplexToReal:
    if (isa<FloatingLiteral>(sub_expr))
      return ConvertFloatingConstant(cast<FloatingLiteral>(sub_expr), expr->getType());
    else if (isa<CharacterLiteral>(sub_expr))
      return ConvertCharacterLiteral(cast<CharacterLiteral>(sub_expr), expr->getType());
    else
      return ConvertConst(sub_expr, to_type);

  case CK_FloatingToIntegral:
    if (isa<FloatingLiteral>(sub_expr)) {
      INT64 val = cast<FloatingLiteral>(sub_expr)->getValue().convertToDouble();
      return Result::nwIntConst(val, to_ty);
    }
    else {
      Result r = Result::nwNone();
      if (isa<CharacterLiteral>(sub_expr))
        r = ConvertCharacterLiteral(cast<CharacterLiteral>(sub_expr), to_type);
      else
        r = ConvertConst(sub_expr, to_type);
      TCON &c = Tcon_Table[r.Tcon()];
      if (!MTYPE_is_float(TCON_ty(c)))
        return r;
      INT64 val = (TCON_ty(c) == MTYPE_F4) ? c.vals.fval : c.vals.dval;
      return Result::nwIntConst(val, to_ty);
    }

  case CK_FunctionToPointerDecay:
  case CK_ArrayToPointerDecay:
    {
      Result r = ConvertConst(sub_expr);
      Is_True(r.isTCon() || r.isInitV() || r.isSym(), ("invalid return type"));

      if (r.isTCon()) {
        // generate constant st
        ST *st = New_ST(GLOBAL_SYMTAB);
        ST_Init(st, NewConstantName(), CLASS_CONST, SCLASS_FSTATIC, EXPORT_LOCAL, from_ty);
        Set_ST_tcon(*st, r.Tcon());
        Set_ST_is_initialized(st);
        Result r = Result::nwSym(ST_st_idx(st), from_ty);
        r.SetConstSym();
        return r;
      } else if (r.isInitV()) {
        SRCPOS spos = _builder->SetSrcPos(getLocation(expr));
        ST_IDX st_idx = Gen_local_var_st(from_ty, spos);
        New_INITO(st_idx, r.InitV());
        return Result::nwSym(st_idx, from_ty);
      } else {
        ST *st = ST_ptr(r.Sym());
        if (CURRENT_SYMTAB != GLOBAL_SYMTAB && isa<CompoundLiteralExpr>(sub_expr)) {
          // need to make a copy so any potential store will not
          // overwrite the original
          ST *copy = New_ST(CURRENT_SYMTAB);
          ST_Init(copy, Save_Str(".cpfrominit"), CLASS_VAR, SCLASS_AUTO,
                  EXPORT_LOCAL, ST_type(st));
          WN *init_wn = WN_Ldid(MTYPE_M, 0, st, ST_type(st));
          WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(),
                              WN_Stid(MTYPE_M, 0, copy, ST_type(copy),
                                      init_wn));
          Set_ST_addr_saved(copy);
          return Result::nwNode(WN_Lda(Pointer_Mtype, ST_ofst(copy), copy), to_ty);
        } else
          return r;
      }
    }

  case CK_IntegralToFloating:
    {
      Result sub = ConvertConst(sub_expr);
      if (sub.isNode()) {
        WN *to_wn = WN_Type_Conversion(sub.Node(), TY_mtype(to_ty));
        return Result::nwNode(to_wn, to_ty);
      } else if (sub.isIntConst()) {
        TCON_IDX tcon;
        if (to_type->isComplexType())
          tcon = Gen_complex_tcon(to_ty, sub.IntConst(), 0);
        else
          tcon = createTconFromValue(to_ty, sub.IntConst());
        return Result::nwTcon(tcon, to_ty);
      } else
        Is_True(false, ("invalid return type"));
    }

  case CK_FloatingComplexToBoolean:
  case CK_IntegralComplexToBoolean:
    {
      Result sub = ConvertConst(sub_expr);
      Is_True(sub.isTCon(), ("invalid return type"));
      int val = _tcon_value_map[sub.Tcon()] ? 1 : 0;
      return Result::nwNode(WN_Intconst(MTYPE_U4, val), to_ty);
    }

  case CK_LValueToRValue:
    return ConvertConst(sub_expr, R_RVALUE);

  case CK_AtomicToNonAtomic:
  case CK_NonAtomicToAtomic:
  case CK_NoOp:
  case CK_ConstructorConversion:
  case CK_BitCast:
  case CK_IntegralToPointer:
  case CK_PointerToIntegral:
    return ConvertConst(sub_expr);

  case CK_NullToPointer: {
    Result sub = ConvertConst(sub_expr);
    sub.SetType(to_ty);
    return sub;
  }

  case CK_IntegralToBoolean:
    return ConvertConst(sub_expr);

  case CK_DerivedToBase:
    {
      const Expr *sub = expr->getSubExpr();
      Is_True(isa<UnaryOperator>(sub) &&
              cast<UnaryOperator>(sub)->getOpcode() == UO_AddrOf,
              ("not addr of object"));
      Result sub_r = ConvertConst(sub);
      Is_True(sub_r.isSym() && sub_r.IsAddrOf(),
              ("bad sub expr"));
      return sub_r;
    }

  case CK_BaseToDerivedMemberPointer:
    {
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
        Result sub_r = ConvertConst(sub);
        Is_True(sub_r.isIntConst(), ("not intconst"));
        return Result::nwIntConst(sub_r.IntConst() + ofst, sub_r.Ty());
      }
      else {
        Is_True(isa<UnaryOperator>(sub) &&
                cast<UnaryOperator>(sub)->getOpcode() == UO_AddrOf,
                ("not constant init"));
        Result sub_r = ConvertConst(sub);
        Is_True(sub_r.isSym() && sub_r.IsAddrOf() &&
                TY_kind(sub_r.Ty()) == KIND_FUNCTION,
                ("bad sub expr"));
        const Expr *decl_ref = cast<UnaryOperator>(sub)->getSubExpr();
        const Decl *decl = cast<DeclRefExpr>(decl_ref)->getDecl();
        Is_True(decl != NULL && isa<CXXMethodDecl>(decl),
                ("not cxx method decl"));
        INITV_IDX inv_blk = New_INITV();
        INITV_Init_Block(inv_blk, INITV_Next_Idx());
        INITV_IDX fptr_inv = New_INITV();
        const CXXMethodDecl *method = cast<CXXMethodDecl>(decl);
        if (method->isVirtual()) {
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
          INITV_Init_Integer(fptr_inv, MTYPE_I8, vofst);
        }
        else {
          Is_True(sub_r.FieldId() == 0, ("fld id is not zero"));
          ST *st = ST_ptr(sub_r.Sym());
          INITV_Init_Symoff(fptr_inv, st, 0);
        }
        INITV_IDX adj_inv = New_INITV();
        INITV_Init_Integer(adj_inv, MTYPE_I8, ofst);
        Set_INITV_next(fptr_inv, adj_inv);
        return Result::nwInitV(inv_blk, 0);
      }
    }

  default:
    Is_True(false, ("New cast kind?"));
  }
}

// Emit INITV from int const or tcon.
// This function should be called by array and record initialization.
INITV_IDX
WhirlConstBuilder::EmitInitV(Result init_const, QualType type) {
  Is_True(init_const.isIntConst() || init_const.isTCon() ||
          init_const.isInitV() || init_const.isSym(),
          ("invalid type for INITV"));
  if (init_const.isInitV())
    return init_const.InitV();
  INITV_IDX cur = New_INITV();
  if (init_const.isIntConst()) {
    TY_IDX ty_idx = _builder->TB().ConvertType(type);
    INITV_Init_Integer(cur, TY_mtype(ty_idx), init_const.IntConst());
  } else if (init_const.isTCon()) {
    INITV_Set_VAL(Initv_Table[cur], init_const.Tcon(), 1);
  } else if (init_const.isSym()) {
    INITV_Init_Symoff(cur, &St_Table[init_const.Sym()], 0);
  }
  return cur;
}

// Emit array initialization: generate initv and return initv block
Result
WhirlConstBuilder::EmitArrayInitialization(const clang::InitListExpr *expr) {
  TY_IDX ty_arr = _builder->TB().ConvertType(expr->getType());
  const ConstantArrayType *cat = _builder->Context()->getAsConstantArrayType(expr->getType());
  Is_True(cat != NULL, ("InitListExpr is not ConstantArrayType"));

  unsigned num_init = expr->getNumInits();
  unsigned num_element = cat->getSize().getZExtValue();
  TY_IDX ty_ele = _builder->TB().ConvertType(cat->getElementType());
  unsigned num_set = std::min(num_init, num_element);

  if (num_init == 0 && num_element == 0)
    return Result::nwNone();

  INITV_IDX inv_blk = New_INITV();
  INITV_Init_Block(inv_blk, 0);
  INT initv_table_size = INITV_Next_Idx();
  INITV_IDX last = 0;
  UINT emitted = 0;
  UINT pad = 0;
  UINT i;
  vector<pair<ST_IDX, const Expr*> > symoff_list;

  for (i = 0; i != num_element; ++i) {
    if (i >= num_init && !expr->hasArrayFiller())
      break;
    const Expr *init = i < num_init ? expr->getInit(i)
                                    : expr->getArrayFiller();
    if (isa<ImplicitValueInitExpr>(init)) {
      pad += TY_size(ty_ele);
    } else {
      Result ret = Result::nwNone();
      if (isa<CXXConstructExpr>(init)) {
        pad = TY_size(ty_ele);
      } else if (isa<CallExpr>(init)) {
        pad = TY_size(ty_ele);
      }
      else {
        if (const UnaryOperator *unary_expr = dyn_cast<UnaryOperator>(init)) {
          if (unary_expr->getOpcode() == clang::UO_AddrOf) {
            const Expr *sub_expr = unary_expr->getSubExpr()->IgnoreParenImpCasts();
            if (isa<CompoundLiteralExpr>(sub_expr)) {
              TY_IDX sub_ty = _builder->TB().ConvertType(sub_expr->getType());
              SRCPOS spos = _builder->SetSrcPos(getLocation(expr));
              ST_IDX st_idx = Gen_local_var_st(sub_ty, spos);
              symoff_list.push_back(std::make_pair(st_idx, init));
              ret = Result::nwSym(st_idx, sub_ty);
            }
          }
        }
        if (ret.isNone()) {
          ret = ConvertConst(init);
        }

        // Emit INITV
        INITV_IDX cur = EmitInitV(ret, init->getType());
        if (last != 0)
          Set_INITV_next(last, cur);
        last = cur;
        // check if pad is needed
        UINT sz = Get_INITV_Size(cur);
        emitted += sz;
        if (sz < TY_size(ty_ele)) {
          pad = TY_size(ty_ele) - sz;
        }
      }
    }
    // generate padding
    if (pad > 0) {
      if (num_set > 0 && i < num_set - 1 &&
          isa<ImplicitValueInitExpr>(expr->getInit(i+1)))
        continue;
      emitted += pad;
      INITV_IDX inv = New_INITV();
      INITV_Init_Pad (inv, pad);
      if (last != 0)
        Set_INITV_next(last, inv);
      last = inv;
      // reset pad
      pad = 0;
    }
  }

  // generate padding
  if (emitted < TY_size(ty_arr)) {
    unsigned pad = TY_size(ty_arr) - emitted;
    INITV_IDX cur = New_INITV();
    INITV_Init_Pad (cur, pad);
    if (last != 0)
      Set_INITV_next(last, cur);
  }

  // handle symoff_list
  for (int i = 0; i < symoff_list.size(); ++i) {
    const Expr *tmp_init = symoff_list[i].second;
    Is_True(tmp_init && isa<UnaryOperator>(tmp_init),
            ("invalid type"));
    Result ret = ConvertConst(tmp_init);
    INITV_IDX cur = EmitInitV(ret, tmp_init->getType());
    Is_True(cur != INITV_IDX_ZERO, ("null initv block"));
    New_INITO(symoff_list[i].first, cur);
  }

  if (initv_table_size != INITV_Next_Idx())
    INITV_Init_Block(inv_blk, initv_table_size);

  return Result::nwInitV(inv_blk, ty_arr);
}

// Emit record initialization: generate initv and return initv block
Result
WhirlConstBuilder::EmitRecordInitialization(const clang::InitListExpr *expr) {
  TY_IDX ty = _builder->TB().ConvertType(expr->getType());
  RecordDecl *record = expr->getType()->getAs<RecordType>()->getDecl();
  UINT field_num = 0;
  UINT elem_num = 0;
  bool gen_initv = true;
  ST_IDX st_idx = (ST_IDX)0;
  TY_IDX fld_ty;
  FLD_HANDLE fld = TY_fld(ty);

  INT pad = 0;
  UINT64 emitted_bytes = 0;
  INITV_IDX inv_blk = New_INITV();
  INITV_Init_Block(inv_blk, 0);
  INT initv_table_size = INITV_Next_Idx();
  INITV_IDX last = 0;
  vector<pair<ST_IDX, const Expr*> > symoff_list;
  for (RecordDecl::field_iterator field = record->field_begin(),
       fieldEnd = record->field_end();
       field != fieldEnd; ++field, ++field_num) {
    Is_True(!fld.Is_Null(), ("Invalid field id %d for type 0x%x",
                              field_num, ty));
    if (record->isUnion() && expr->getInitializedFieldInUnion() != *field) {
      fld = FLD_next(fld);
      continue;
    }
    if (field->isUnnamedBitfield()) {
      fld = FLD_next(fld);
      continue;
    }
    fld_ty = FLD_type(fld);
    if (!TY_is_incomplete(fld_ty) && TY_size(fld_ty) == 0) {
      fld = FLD_next(fld);
      elem_num++;
      continue;
    }
    pad = FLD_ofst(fld) - emitted_bytes;

    // Emit INITV
    Is_True(elem_num < expr->getNumInits(),
            ("initializer access out of range"));
    const Expr *init = expr->getInit(elem_num++);
    BOOL is_bit_field = FLD_is_bit_field(fld);
    fld_ty = is_bit_field ? MTYPE_To_TY(MTYPE_BS) : FLD_type(fld);

    if (pad > 0) {
      if (gen_initv) {
        INITV_IDX inv = New_INITV();
        INITV_Init_Pad(inv, pad);
        if (last != 0)
          Set_INITV_next(last, inv);
        last = inv;
      }
      emitted_bytes += pad;
    }

    // generate padding for ImplicitValueInitExpr
    if (isa<ImplicitValueInitExpr>(init)) {
      if (FLD_next(fld).Is_Null())
        break;
      if (elem_num < expr->getNumInits() &&
          !isa<ImplicitValueInitExpr>(expr->getInit(elem_num))) {
        fld = FLD_next(fld);
        INITV_IDX inv = New_INITV();
        pad = FLD_ofst(fld) - emitted_bytes;
        INITV_Init_Pad (inv, pad);
        if (last != 0)
          Set_INITV_next(last, inv);
        last = inv;
        emitted_bytes += pad;
      } else {
        fld = FLD_next(fld);
      }
      continue;
    }

    Result ret = Result::nwNone();
#if LLVM_VERSION_MAJOR == 11
    if (isa<ConstantExpr>(init))
      init = cast<ConstantExpr>(init)->getSubExpr();
#endif
    if (const UnaryOperator *unary_expr = dyn_cast<UnaryOperator>(init)) {
      if (unary_expr->getOpcode() == clang::UO_AddrOf) {
        const Expr *sub_expr = unary_expr->getSubExpr()->IgnoreParenImpCasts();
        if (isa<CompoundLiteralExpr>(sub_expr)) {
          TY_IDX sub_ty = _builder->TB().ConvertType(sub_expr->getType());
          SRCPOS spos = _builder->SetSrcPos(getLocation(expr));
          ST_IDX st_idx = Gen_local_var_st(sub_ty, spos);
          symoff_list.push_back(std::make_pair(st_idx, init));
          ret = Result::nwSym(st_idx, sub_ty);
        }
      }
    }
    if (ret.isNone()) {
      ret = ConvertConst(init);
      if (ret.isNone()) {
        fld = FLD_next(fld);
        continue;
      }
    }

    INT size = TY_size(fld_ty);

    if (is_bit_field) {
      Is_True(ret.isIntConst(),
        ("initialization value of bitfield expected to be integer"));
      AddBitfieldInitvForTree(ret.IntConst(), fld, last, emitted_bytes);
    } else {
      INITV_IDX cur = EmitInitV(ret, init->getType());
      if (last != 0)
        Set_INITV_next(last, cur);
      last = cur;

      // add padding for StringLiteral
      if (init->getStmtClass() == Stmt::StringLiteralClass) {
        const StringLiteral *str_expr = cast<StringLiteral>(init);
        INT val_size = str_expr->getByteLength() + 1;
        if (size > val_size) {
          INITV_IDX inv = New_INITV();
          INITV_Init_Pad(inv, size - val_size);
          Set_INITV_next(last, inv);
          last = inv;
        }
      }

      emitted_bytes += size;
    }
    fld = FLD_next(fld);
  }
  pad = TY_size(ty) - emitted_bytes;

  if (pad > 0) {
    if (gen_initv) {
      INITV_IDX inv = New_INITV();
      INITV_Init_Pad (inv, pad);
      if (last != 0)
        Set_INITV_next(last, inv);
    }
  }

  // handle symoff_list
  for (int i = 0; i < symoff_list.size(); ++i) {
    const Expr *tmp_init = symoff_list[i].second;
    Is_True(tmp_init && isa<UnaryOperator>(tmp_init),
            ("invalid type"));
    Result ret = ConvertConst(tmp_init);
    INITV_IDX cur = EmitInitV(ret, tmp_init->getType());
    Is_True(cur != INITV_IDX_ZERO, ("null initv block"));
    New_INITO(symoff_list[i].first, cur);
  }

  if (initv_table_size != INITV_Next_Idx())
    INITV_Init_Block(inv_blk, initv_table_size);

  return Result::nwInitV(inv_blk, ty);
}

Result
WhirlConstBuilder::EmitVectorInitialization(const InitListExpr *expr) {
  TRACE_FUNC();
  INITV_IDX inv_blk = New_INITV();
  INITV_Init_Block(inv_blk, 0);
  INT initv_table_size = INITV_Next_Idx();
  INITV_IDX last = 0;

  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());

  const VectorType *VT = expr->getType()->getAs<VectorType> ();
  TY_IDX ty_ele = _builder->TB().ConvertType(VT->getElementType());

  unsigned num_init = expr->getNumInits();
  unsigned num_element = VT->getNumElements();
  unsigned num_set = std::min(num_init, num_element);

  bool gen_tmp_st = false;
  ST_IDX st_idx = (ST_IDX)0;

  for (UINT i = 0; i != num_set; ++i) {
    const Expr *init = expr->getInit(i);
    Result ret = ConvertConst(init);

    // Emit INITV
    INITV_IDX cur = EmitInitV(ret, init->getType());
    if (last != 0) {
      Set_INITV_next(last, cur);
    }
    last = cur;
  }

  // generate padding
  if (num_set < num_element) {
    unsigned pad = num_element - num_set;

    TCON_IDX tcon;
    if (VT->getElementType()->isComplexType())
      tcon = Gen_complex_tcon(ty_ele, 0, 0);
    else
      tcon = createTconFromValue(ty_ele, 0);
    for (UINT j = 0; j != pad; ++j) {
      INITV_IDX cur = New_INITV();
      INITV_Set_VAL(Initv_Table[cur], tcon, 1);
      Set_INITV_next(last, cur);
      last = cur;
    }
  }

  if (initv_table_size != INITV_Next_Idx())
    INITV_Init_Block(inv_blk, initv_table_size);

  return Result::nwInitV(inv_blk, ty_idx);
}

Result
WhirlConstBuilder::ConvertInitListExpr(const InitListExpr *expr) {
  TRACE_FUNC();
  if (expr->getType()->isArrayType()) {
    return EmitArrayInitialization(expr);
  } else if (expr->getType()->isRecordType()) {
    return EmitRecordInitialization(expr);
  } else if (expr->getType()->isVectorType()) {
    return EmitVectorInitialization(expr);
  } else if (expr->isTransparent()) {
    // TODO
    return ConvertConst(expr->getInit(0));
  } else {
    Is_True(false, ("Can't go to here."));
  }
}

// TODO: ConvertUnaryOperator & ConvertBinaryOperator may need to be improved
Result
WhirlConstBuilder::ConvertUnaryOperator(const UnaryOperator *expr, QualType type) {
  TY_IDX ty_idx = _builder->TB().ConvertType(type);

  bool is_integer = expr->getType()->isIntegerType();
  UnaryOperatorKind opcode = expr->getOpcode();

  const Expr *sub_expr = expr->getSubExpr();
  Result sub = ConvertConst(sub_expr, type);

  if (opcode == UO_Extension) {
    return sub;
  }

  if (sub.isIntConst()) {
    UINT64 val = sub.IntConst();
    switch (opcode) {
      case UO_Plus: val = +val; break;
      case UO_Minus: val = -val; break;
      case UO_Not: val = ~val; break;
      case UO_LNot: val = !val; break;
      case UO_AddrOf: return sub;    // return constant for &(constant)
      default:
        Is_True(false, ("unsupported opcode for unary operator to int const"));
    }
    if (is_integer)
      return Result::nwIntConst(val, ty_idx);
    else
      return Result::nwTcon(createTconFromValue(ty_idx, val), ty_idx);
  } else if (sub.isTCon()) {
    long double val = _tcon_value_map[sub.Tcon()];
    switch (opcode) {
      case UO_Plus: val = +val; break;
      case UO_Minus: val = -val; break;
      case UO_Not: val = val; break;
      case UO_LNot: val = !val; break;
      default:
        Is_True(false, ("unsupported opcode for unary operator to tcon"));
    }
    if (is_integer)
      return Result::nwIntConst(val, ty_idx);
    else
      return Result::nwTcon(createTconFromValue(ty_idx, val), ty_idx);
  } else if (sub.isSym()) {
    if (opcode == UO_AddrOf)
      sub.SetAddrOf();
    return sub;
  } else if (sub.isInitV()) {
    return sub;
  } else {
    Is_True(false, ("unsupported return type for unary operator"));
  }
}

// generate init SYMOFF with offset
static Result
Gen_symoff_initv(ST_IDX st, UINT64 offset) {
  Is_True(st != (ST_IDX)0, ("invalid st"));
  TY_IDX ty = ST_type(st);

  // get right offset, if ty is an incomplete array type
  if (TY_is_incomplete(ty) && TY_kind(ty) == KIND_POINTER) {
    offset = offset * TY_size(TY_pointed(ty));
  } else if (TY_kind(ty) == KIND_ARRAY) {
    offset = offset * TY_size(TY_etype(ty));
  } else if (TY_kind(ty) == KIND_STRUCT) {
    offset = offset;
  } else
    Is_True(false, ("invalid ty"));

  INITV_IDX inv = New_INITV();
  INITV_Init_Symoff(inv, &St_Table[st], offset);
  return Result::nwInitV(inv, ty);
}

static inline
mTYPE_ID Size_To_MTYPE(UINT size) {
  switch (size) {
    case 1:  return MTYPE_I1;
    case 2:  return MTYPE_I2;
    case 4:  return MTYPE_I4;
    case 8:  return MTYPE_I8;
    default:
      Is_True(FALSE, ("unexpected size=%d", size));
      return MTYPE_UNKNOWN;
  }
} /* Size_To_MTYPE */

void
WhirlConstBuilder::ResetINITVLabel(BinaryOperatorKind opcode, Result lhs, Result rhs, UINT size, BOOL first_child, BOOL last_child)
{
  Is_True(opcode == BO_Add || opcode == BO_Sub,
          ("Only BO_Add and BO_Sub is allowed in Label Values"));

  if (lhs.isInitV() && INITV_kind(lhs.InitV()) == INITVKIND_LABEL) {
    Is_True(opcode == BO_Sub, ("only Label Substraction is allowed"));
    INT16 flags = first_child == TRUE ? INITVLABELFLAGS_VALUES_FIRST :
                                        INITVLABELFLAGS_VALUES_PLUS;
    Set_INITV_lab_flags(lhs.InitV(), flags);
    Set_INITV_lab_mtype(lhs.InitV(), Size_To_MTYPE(size));
  }
  else {
    ResetINITVLabel(opcode, lhs, rhs, size, first_child, FALSE);
  }

  if (rhs.isInitV() && INITV_kind(rhs.InitV()) == INITVKIND_LABEL) {
    Is_True(opcode == BO_Sub, ("only Label Substraction is allowed"));
    INT16 flags = last_child == TRUE ? INITVLABELFLAGS_VALUES_LAST :
                                        INITVLABELFLAGS_VALUES_MINUS;
    Set_INITV_lab_flags(rhs.InitV(), flags);
    Set_INITV_lab_mtype(rhs.InitV(), Size_To_MTYPE(size));
  }
  else {
    ResetINITVLabel(opcode, lhs, rhs, size, FALSE, last_child);
  }
}

Result
WhirlConstBuilder::ConvertBinaryOperator(const BinaryOperator *expr, QualType type) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(type);

  bool is_integer = expr->getType()->isIntegerType();
  BinaryOperatorKind opcode = expr->getOpcode();
  const Expr *lhs_expr = expr->getLHS();
  const Expr *rhs_expr = expr->getRHS();

  // TODO: need improve
  // gen tcon for complex type
  if ((isa<ImplicitCastExpr>(lhs_expr) &&
      cast<CastExpr>(lhs_expr)->getCastKind() == CK_IntegralRealToComplex) &&
      isa<ImaginaryLiteral>(rhs_expr)) {
    Result lhs = ConvertConst(lhs_expr);
    Result rhs = ConvertImaginaryLiteral(cast<ImaginaryLiteral>(rhs_expr), type);
    Is_True(lhs.isIntConst() && rhs.isTCon(), ("invalid return type for complex type"));
    long double imag_val = _tcon_value_map[rhs.Tcon()];
    Is_True(imag_val != 0, ("invalid imaginary value"));
    TCON_IDX tcon_idx = Gen_complex_tcon(ty_idx, lhs.IntConst(), imag_val);
    return Result::nwTcon(tcon_idx, ty_idx);
  }

  // handle AddrLabelExpr
  if (isa<AddrLabelExpr>(lhs_expr) || isa<AddrLabelExpr>(rhs_expr)) {
    INITV_IDX inv_blk = New_INITV();
    INITV_Init_Block(inv_blk, INITV_Next_Idx());
    Result lhs = ConvertConst(lhs_expr);
    Is_True(lhs.isInitV(), ("should be initv"));
    Result rhs = ConvertConst(rhs_expr);
    Is_True(rhs.isInitV(), ("should be initv"));
    Set_INITV_next(lhs.InitV(), rhs.InitV());
    ResetINITVLabel(opcode, lhs, rhs, TY_size(ty_idx));
    return Result::nwInitV(inv_blk, ty_idx);
  }

  Result lhs = ConvertConst(lhs_expr);
  Result rhs = ConvertConst(rhs_expr);

  if ((lhs.isIntConst() ||
       (lhs.isNode() && WN_operator(lhs.Node()) == OPR_INTCONST)) &&
      (rhs.isIntConst() ||
       (rhs.isNode() && WN_operator(rhs.Node()) == OPR_INTCONST))) {
    UINT64 lhs_val = lhs.isNode() ? WN_const_val(lhs.Node()) : lhs.IntConst();
    UINT64 rhs_val = rhs.isNode() ? WN_const_val(rhs.Node()) : rhs.IntConst();
    UINT64 val;
    switch (opcode) {
      case BO_Add: val = lhs_val + rhs_val; break;
      case BO_Sub: val = lhs_val - rhs_val; break;
      case BO_Mul: val = lhs_val * rhs_val; break;
      case BO_Div: val = rhs_val ? lhs_val / rhs_val : 0; break;
      case BO_Rem: val = rhs_val ? lhs_val % rhs_val : 0; break;
      case BO_Shl: val = lhs_val << rhs_val; break;
      case BO_Shr: val = lhs_val >> rhs_val; break;
      case BO_GT:  val = lhs_val > rhs_val; break;
      case BO_GE:  val = lhs_val >= rhs_val; break;
      case BO_LT:  val = lhs_val < rhs_val; break;
      case BO_LE:  val = lhs_val <= rhs_val; break;
      case BO_EQ:  val = lhs_val == rhs_val; break;
      case BO_NE:  val = lhs_val != rhs_val; break;
      case BO_And: val = lhs_val & rhs_val; break;
      case BO_Xor: val = lhs_val ^ rhs_val; break;
      case BO_Or:  val = lhs_val | rhs_val; break;
      case BO_LAnd:val = lhs_val && rhs_val; break;
      case BO_LOr: val = lhs_val || rhs_val; break;
      default:
         Is_True(false, ("unsupported opcode for binary operator to int const"));
    }
    if (is_integer)
      return Result::nwIntConst(val, ty_idx);
    else
      return Result::nwTcon(createTconFromValue(ty_idx, val), ty_idx);
  } else if (lhs.isTCon() && rhs.isTCon()) {
    long double lhs_val = _tcon_value_map[lhs.Tcon()];
    long double rhs_val = _tcon_value_map[rhs.Tcon()];
    long double val;
    switch (opcode) {
      case BO_Add: val = lhs_val + rhs_val; break;
      case BO_Sub: val = lhs_val - rhs_val; break;
      case BO_Mul: val = lhs_val * rhs_val; break;
      case BO_Div: val = lhs_val / rhs_val; break;
      case BO_GT:  val = lhs_val > rhs_val ? 1 : 0; break;
      case BO_GE:  val = lhs_val >= rhs_val ? 1 : 0; break;
      case BO_LT:  val = lhs_val < rhs_val ? 1 : 0; break;
      case BO_LE:  val = lhs_val <= rhs_val ? 1 : 0; break;
      case BO_EQ:  val = lhs_val == rhs_val ? 1 : 0; break;
      case BO_NE:  val = lhs_val != rhs_val ? 1 : 0; break;
      default:
         Is_True(false, ("unsupported opcode for binary operator to tcon"));
    }
    if (is_integer)
      return Result::nwIntConst(val, ty_idx);
    else
      return Result::nwTcon(createTconFromValue(ty_idx, val), ty_idx);
  } else if (lhs.isSym() || rhs.isSym() ||
             lhs.isInitV() || rhs.isInitV()) {
      // adjust sym offset
      Result base = Result::nwNone();
      Result adjust = Result::nwNone();
      if (lhs.isSym() || lhs.isInitV()) {
        base = lhs;
        adjust = rhs;
      } else {
        base = rhs;
        adjust = lhs;
      }
      Is_True(adjust.isIntConst(), ("should be int const"));
      UINT64 offset = adjust.IntConst();
      offset = opcode == BO_Add ? offset : -offset;
      if (base.isInitV()) {
        // adjust INITV offset
        Set_INITV_ofst(base.InitV(), offset);
        return base;
      } else {
        return Gen_symoff_initv(base.Sym(), offset);
      }
  } else {
    Is_True(false, ("unsupported return type for binary operator"));
  }
}

Result
WhirlConstBuilder::ConvertDeclRefExpr(const DeclRefExpr *expr, RV rv) {
  TRACE_FUNC();

  const ValueDecl *value = expr->getDecl();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());

  // convert enum constant
  if (isa<EnumConstantDecl>(value)) {
    const EnumConstantDecl *decl = dyn_cast<EnumConstantDecl>(value);
    llvm::APSInt val = decl->getInitVal();
    return Result::nwIntConst(val.isSigned()?
                         val.getSExtValue() : val.getZExtValue(), ty_idx);
  }

  // convert field decl
  if (isa<FieldDecl>(value)) {
    const FieldDecl* fld = cast<FieldDecl>(value);
    ASTContext *ast_c = _builder->Context();
    const ASTRecordLayout &layout = ast_c->getASTRecordLayout(fld->getParent());
    UINT64 ofst = layout.getFieldOffset(fld->getFieldIndex());
    return Result::nwIntConst(ofst, MTYPE_To_TY(MTYPE_U8));
  }

  // defer CXXMethodDecl if not exist
  ST_IDX sub_idx = _builder->Get_decl_st(value);
  Is_True(sub_idx != ST_IDX_ZERO, ("bad st"));

  if (rv != R_RVALUE) {
    return Result::nwSym(sub_idx, ty_idx);
  }

  Is_True(isa<VarDecl>(value), ("value is not vardecl"));
  const VarDecl *var = cast<VarDecl>(value)->getCanonicalDecl();
  Is_True(var->getInit() != NULL, ("value is not const initialized"));
  return ConvertConstForDecl(var);
}

Result
WhirlConstBuilder::ConvertParenExpr(const ParenExpr *expr, QualType type, RV rv) {
  TRACE_FUNC();
  return ConvertConst(expr->getSubExpr(), type, rv);
}

Result
WhirlConstBuilder::ConvertPredefinedExpr(const PredefinedExpr *expr) {
  return ConvertStringLiteral(expr->getFunctionName());
}

Result
WhirlConstBuilder::ConvertCXXBoolLiteralExpr(const CXXBoolLiteralExpr *expr) {
  return Result::nwIntConst(expr->getValue(), MTYPE_To_TY(MTYPE_I4));
}

Result
WhirlConstBuilder::ConvertCXXTypeidExpr(const CXXTypeidExpr *expr) {
  Is_True(expr->isGLValue(), ("not GLValue"));

  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  QualType type = expr->isTypeOperand()
                    ? expr->getTypeOperand(*(_builder->Context()))
                    : expr->getExprOperand()->getType();
  ST_IDX st = _builder->TB().ConvertRTTIForType(type);
  Is_True(st != ST_IDX_ZERO, ("rtti st is zero"));
  Result r = Result::nwSym(st, ty_idx);
  r.SetAddrOf();
  return r;
}

Result
WhirlConstBuilder::ConvertArraySubscriptExpr(const ArraySubscriptExpr *expr) {
  TRACE_FUNC();
  Result lhs = ConvertConst(expr->getLHS());
  Result rhs = ConvertConst(expr->getRHS());
  Is_True(lhs.isSym() && rhs.isIntConst(), ("invalid return type for ArraySubscriptExpr"));
  return Gen_symoff_initv(lhs.Sym(), rhs.IntConst());
}

// Convert ImaginaryLiteral, return TCON & val
Result
WhirlConstBuilder::ConvertImaginaryLiteral(const ImaginaryLiteral *expr, QualType type) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  Result elem = ConvertConst(expr->getSubExpr());

  TCON_IDX tcon = Convert_imaginary_to_tcon(expr, elem, ty_idx);
  return Result::nwTcon(tcon, ty_idx);
}

Result
WhirlConstBuilder::ConvertCompoundLiteralExpr(const CompoundLiteralExpr *expr) {
  return ConvertConst(expr->getInitializer());
}

Result
WhirlConstBuilder::ConvertExprWithCleanups(const ExprWithCleanups *expr) {
  if (!expr->cleanupsHaveSideEffects())
    return ConvertConst(expr->getSubExpr());
  return Result::nwNone();
}

Result
WhirlConstBuilder::ConvertConditionalOperator(const ConditionalOperator *expr, QualType type) {
  Result cond = ConvertConst(expr->getCond());
  Is_True(cond.isIntConst(), ("conditional expr should be int const"));
  if (cond.IntConst())
    return ConvertConst(expr->getTrueExpr(), type);
  else
    return ConvertConst(expr->getFalseExpr(), type);
}

Result
WhirlConstBuilder::ConvertUnaryExprOrTypeTraitExpr(const UnaryExprOrTypeTraitExpr *expr) {
  QualType TypeToSize = expr->getTypeOfArgument();
  TY_IDX ty_idx = _builder->TB().ConvertType(TypeToSize);
  if (expr->getKind() == UETT_SizeOf) {
    if (const VariableArrayType *vat =
          _builder->Context()->getAsVariableArrayType(TypeToSize)) {
      if (expr->isArgumentType()) {
        // TODO: sizeof(type) - make sure to emit the VLA size.
        Is_True(false, ("unsupported for ArgumentType"));
      } else {
        // C99 6.5.3.4p2: If the argument is an expression of type
        // VLA, it is evaluated.
        return ConvertConst(expr->getArgumentExpr());
      }
    }
  } else if (expr->getKind() == UETT_OpenMPRequiredSimdAlign) {
    auto Alignment =
      _builder->Context()
      ->toCharUnitsFromBits(_builder->Context()->getOpenMPDefaultSimdAlign(
          expr->getTypeOfArgument()->getPointeeType())).getQuantity();
    return Result::nwNode(WN_Intconst(Mtype_comparison(TY_mtype(ty_idx)), Alignment), ty_idx);
  }

  // If this isn't sizeof(vla), the result must be constant; use the constant
  // folding logic so we don't have to duplicate it here.
  return Result::nwIntConst(expr->EvaluateKnownConstInt(*(_builder->Context())).getZExtValue(), ty_idx);
}

Result
WhirlConstBuilder::ConvertMemberExpr(const MemberExpr *expr) {
  TRACE_FUNC();

  Result base = ConvertConst(expr->getBase());
  if (base.isSym()) {
    ST_IDX st_idx = base.Sym();
    TY_IDX ret_ty = ST_type(st_idx);
    UINT32 cur_field_id = 0;
    UINT64 ofst_value = 0;
    NamedDecl *name_decl = expr->getMemberDecl();
    if (auto *field = dyn_cast<FieldDecl>(name_decl)) {
      Is_True(field, ("FieldDecl should not be NULL"));
      UINT32 field_id = _builder->TB().GetFieldIDFromDecl(field);
      // get offset
      FLD_HANDLE fld = get_fld_and_offset(ret_ty, field_id, cur_field_id, ofst_value);
      Is_True(!fld.Is_Null(), ("not find the field"));
    }
    return Gen_symoff_initv(base.Sym(), ofst_value);
  } else if (base.isIntConst() && base.IntConst() == 0) {
    // special case to get offset of field within record
    UINT32 cur_field_id = 0;
    UINT64 ofst_value = 0;
    NamedDecl *name_decl = expr->getMemberDecl();
    if (auto *field = dyn_cast<FieldDecl>(name_decl)) {
      Is_True(field, ("FieldDecl should not be NULL"));
      UINT32 field_id = _builder->TB().GetFieldIDFromDecl(field);
      TY_IDX struct_ty = TY_pointed(base.Ty());
      // get offset
      FLD_HANDLE fld = get_fld_and_offset(struct_ty, field_id, cur_field_id, ofst_value);
      Is_True(!fld.Is_Null(), ("not find the field"));
    }
    Result r = Result::nwIntConst(ofst_value, base.Ty());
    return r;
  } else {
    Is_True(!base.isNode(), ("invalid return type"));
    return base;
  }
}

Result
WhirlConstBuilder::ConvertCXXNullPtrLiteralExpr(const CXXNullPtrLiteralExpr *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  Is_True(false, ("unsupported CXXNullPtrLiteralExpr"));
}

Result
WhirlConstBuilder::ConvertGNUNullExpr(const GNUNullExpr *expr) {
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  return Result::nwIntConst(0, ty_idx);
}

Result
WhirlConstBuilder::ConvertOffsetOfExpr(const OffsetOfExpr *expr) {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  // fold the offsetof to a constant
  llvm::APSInt value;
  if (evaluateAsInt(expr, value, _builder->Context())) {
    return Result::nwIntConst(value.getZExtValue(), ty_idx);
  }
  Is_True(false, ("unsupported ConvertOffsetOfExpr"));
#if 0
  TY_IDX ty_idx = _builder->TB().ConvertType(expr->getType());
  WhirlExprBuilder expr_bldr(_builder);
  WN *ret_wn = expr_bldr.ConvertToNode(expr);
  Is_True(ret_wn != NULL, ("invalid return type"));
  Is_True(WN_operator(ret_wn) == OPR_INTCONST, ("invalid whirl node"));
  return Result::nwIntConst(WN_const_val(ret_wn), ty_idx);
#endif
}

Result
WhirlConstBuilder::ConvertAddrLabelExpr(const AddrLabelExpr *expr) {
  TRACE_FUNC();

  // get label idx
  LABEL_IDX label_idx = _builder->Get_label_idx(expr->getLabel());

  // create INITV for label idx
  INITV_IDX inv = New_INITV();
  INT16 flag = INITVLABELFLAGS_UNUSED;
  mTYPE_ID mtype = MTYPE_UNKNOWN;
  INITV_Init_Label(inv, label_idx, 1, flag, mtype);
  Set_LABEL_addr_saved(label_idx);
  Set_PU_no_inline(Get_Current_PU());

  return Result::nwInitV(inv, 0);
}

Result
WhirlConstBuilder::ConvertMaterializeTemporaryExpr(const MaterializeTemporaryExpr* expr) {
  Is_True(expr->isGLValue(), ("not lvalue"));
  Result r = ConvertConst(getSubExpr(expr));
  Is_True(!r.isNone() && !r.isNode(), ("is none or node"));
  INITV_IDX inv = EmitInitV(r, expr->getType());
  ST *tmp_st = New_ST(GLOBAL_SYMTAB);
  static int suffix = 0;
  char name_buf[256];
  const char *sym_str = "TODO";
  snprintf(name_buf, sizeof(name_buf) - 1, "_ZGR%d%s%d",
           (int)strlen(sym_str), sym_str, suffix++);
  TY_IDX ty = _builder->TB().ConvertType(expr->getType());
  ST_Init(tmp_st, Save_Str(name_buf),
          CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, ty);
  Set_ST_is_initialized(tmp_st);
  New_INITO(ST_st_idx(tmp_st), inv);
  return Result::nwSym(ST_st_idx(tmp_st), ty);
}

Result
WhirlConstBuilder::ConvertConstForDecl(const clang::VarDecl *decl) {
  Is_True(decl->getInit() != NULL, ("no init for decl"));
  Result r = ConvertConst(decl->getInit());
  Is_True(!r.isNone(), ("no const init for r"));
  return r;
}

Result
WhirlConstBuilder::ConvertConst(const clang::Expr *expr, RV rv) {
  return ConvertConst(expr, expr->getType(), rv);
}

Result
WhirlConstBuilder::ConvertConst(const clang::Expr *expr, QualType type, RV rv) {
  TRACE_FUNC();
  Result r = Result::nwNone();
  switch (expr->getStmtClass()) {
    case Expr::StringLiteralClass:
      r = ConvertStringLiteral(cast<StringLiteral>(expr));
      break;
    case Expr::FloatingLiteralClass:
      r = ConvertFloatingConstant(cast<FloatingLiteral>(expr), type);
      break;
    case Expr::IntegerLiteralClass:
      r = ConvertIntegerConstant(cast<IntegerLiteral>(expr));
      break;
    case Expr::CharacterLiteralClass:
      r = ConvertCharacterLiteral(cast<CharacterLiteral>(expr));
      break;
    case Expr::ImplicitCastExprClass:
    case Expr::CStyleCastExprClass:
    case Expr::CXXFunctionalCastExprClass:
    case Expr::CXXStaticCastExprClass:
      r = ConvertCastExpr(cast<CastExpr>(expr), type);
      break;
    case Expr::InitListExprClass:
      r = ConvertInitListExpr(cast<InitListExpr>(expr));
      break;
    case Expr::UnaryOperatorClass:
      r = ConvertUnaryOperator(cast<UnaryOperator>(expr), type);
      break;
    case Expr::BinaryOperatorClass:
      r = ConvertBinaryOperator(cast<BinaryOperator>(expr), type);
      break;
    case Expr::DeclRefExprClass:
      r = ConvertDeclRefExpr(cast<DeclRefExpr>(expr), rv);
      break;
    case Expr::ParenExprClass:
      r = ConvertParenExpr(cast<ParenExpr>(expr), type, rv);
      break;
    case Expr::PredefinedExprClass:
      r = ConvertPredefinedExpr(cast<PredefinedExpr>(expr));
      break;
    case Expr::CXXBoolLiteralExprClass:
      r = ConvertCXXBoolLiteralExpr(cast<CXXBoolLiteralExpr>(expr));
      break;
    case Expr::CXXTypeidExprClass:
      r = ConvertCXXTypeidExpr(cast<CXXTypeidExpr>(expr));
      break;
    case Expr::ArraySubscriptExprClass:
      r = ConvertArraySubscriptExpr(cast<ArraySubscriptExpr>(expr));
      break;
    case Expr::ImaginaryLiteralClass:
      r = ConvertImaginaryLiteral(cast<ImaginaryLiteral>(expr), type);
      break;
    case Expr::CompoundLiteralExprClass:
      r = ConvertCompoundLiteralExpr(cast<CompoundLiteralExpr>(expr));
      break;
    case Expr::ExprWithCleanupsClass:
      r = ConvertExprWithCleanups(cast<ExprWithCleanups>(expr));
      break;
    case Expr::ConditionalOperatorClass:
      r = ConvertConditionalOperator(cast<ConditionalOperator>(expr), type);
      break;
    case Expr::UnaryExprOrTypeTraitExprClass:
      r = ConvertUnaryExprOrTypeTraitExpr(cast<UnaryExprOrTypeTraitExpr>(expr));
      break;
    case Expr::MemberExprClass:
      r = ConvertMemberExpr(cast<MemberExpr>(expr));
      break;
    case Expr::CXXNullPtrLiteralExprClass:
      r = ConvertCXXNullPtrLiteralExpr(cast<CXXNullPtrLiteralExpr>(expr));
      break;
    case Expr::GNUNullExprClass:
      r = ConvertGNUNullExpr(cast<GNUNullExpr>(expr));
      break;
    case Expr::OffsetOfExprClass:
      r = ConvertOffsetOfExpr(cast<OffsetOfExpr>(expr));
      break;
    case Expr::AddrLabelExprClass:
      r = ConvertAddrLabelExpr(cast<AddrLabelExpr>(expr));
      break;
    case Expr::MaterializeTemporaryExprClass:
      Is_True(expr->isGLValue(), ("only lvalue can be static init"));
      r = ConvertMaterializeTemporaryExpr(cast<MaterializeTemporaryExpr>(expr));
      break;
    case Expr::SubstNonTypeTemplateParmExprClass:
      r = ConvertConst(cast<SubstNonTypeTemplateParmExpr>(expr)->getReplacement());
      break;
#if LLVM_VERSION_MAJOR == 11
    case Expr::ConstantExprClass:
      r = ConvertConst(cast<ConstantExpr>(expr)->getSubExpr());
      break;
#endif
    default:
      Is_True(false,
              ("unsupport expr: %s", expr->getStmtClassName()));
  }
  return r;
}

} // namespace wgen
