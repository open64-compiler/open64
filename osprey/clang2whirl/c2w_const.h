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

#ifndef CLANG2WHIRL_CONST_H
#define CLANG2WHIRL_CONST_H

// forward clang declarations
#include "clangdecl.h"

// forward open64 declarations
#include "open64decl.h"

#include "c2w_map.h"
#include "c2w_result.h"
#include "clanginc.h"

namespace wgen {

class WhirlBuilder;

class WhirlConstBuilder {
private:
  WhirlBuilder *_builder;
  UINT32 _const_idx;
  
  STRING_TCON_MAP _string_tcon_map;
  STRCST_ST_MAP _string_st_map;
  //VAR_INITR_MAP _var_initr_map;

public:
  WhirlConstBuilder(WhirlBuilder *builder);
  
  ~WhirlConstBuilder();

private:
  STR_IDX NewConstantName();
  
  INITV_IDX EmitInitV(Result init_const, clang::QualType type);

  Result EmitArrayInitialization(const clang::InitListExpr *expr);

  Result EmitRecordInitialization(const clang::InitListExpr *expr);

  Result EmitVectorInitialization(const clang::InitListExpr *expr);

  void ResetINITVLabel(clang::BinaryOperatorKind opr, Result lhs, Result rhs, UINT size, BOOL first_child = TRUE, BOOL last_child = TRUE);

public:
  Result ConvertTargetConstant(const clang::StringLiteral *expr);
  
  Result ConvertStringLiteral(const clang::StringLiteral *expr);
  
  Result ConvertFloatingConstant(TY_IDX ty_idx, const llvm::APFloat init);

  Result ConvertFloatingConstant(const clang::FloatingLiteral *expr);
  
  Result ConvertFloatingConstant(const clang::FloatingLiteral *expr, clang::QualType type);
  
  Result ConvertIntegerConstant(const clang::IntegerLiteral *expr);
  
  Result ConvertCharacterLiteral(const clang::CharacterLiteral *expr);
  
  Result ConvertCharacterLiteral(const clang::CharacterLiteral *expr, clang::QualType type);

  Result ConvertCastExpr(const clang::CastExpr *expr, clang::QualType type);

  Result ConvertInitListExpr(const clang::InitListExpr *expr);

  Result ConvertUnaryOperator(const clang::UnaryOperator *expr, clang::QualType type);

  Result ConvertBinaryOperator(const clang::BinaryOperator *expr, clang::QualType type);

  Result ConvertDeclRefExpr(const clang::DeclRefExpr *expr, RV rv);

  Result ConvertParenExpr(const clang::ParenExpr *expr, clang::QualType type, RV rv = R_UNSPEC);

  Result ConvertPredefinedExpr(const clang::PredefinedExpr *expr);

  Result ConvertCXXBoolLiteralExpr(const clang::CXXBoolLiteralExpr *expr);

  Result ConvertCXXTypeidExpr(const clang::CXXTypeidExpr *expr);

  Result ConvertArraySubscriptExpr(const clang::ArraySubscriptExpr *expr);

  Result ConvertImaginaryLiteral(const clang::ImaginaryLiteral *expr, clang::QualType type);

  Result ConvertCompoundLiteralExpr(const clang::CompoundLiteralExpr *expr);

  Result ConvertExprWithCleanups(const clang::ExprWithCleanups *expr);

  Result ConvertConditionalOperator(const clang::ConditionalOperator *expr, clang::QualType type);

  Result ConvertUnaryExprOrTypeTraitExpr(const clang::UnaryExprOrTypeTraitExpr *exp);

  Result ConvertMemberExpr(const clang::MemberExpr *expr);

  Result ConvertCXXNullPtrLiteralExpr(const clang::CXXNullPtrLiteralExpr *expr);

  Result ConvertGNUNullExpr(const clang::GNUNullExpr *expr);

  Result ConvertCallExpr(const clang::CallExpr *expr);

  Result ConvertOffsetOfExpr(const clang::OffsetOfExpr *expr);

  // convert AddrLabelExpr to INITV
  Result ConvertAddrLabelExpr(const clang::AddrLabelExpr *expr);

  Result ConvertMaterializeTemporaryExpr(const clang::MaterializeTemporaryExpr *expr);

  Result ConvertConstForDecl(const clang::VarDecl *decl);

  Result ConvertConst(const clang::Expr *expr, RV rv = R_UNSPEC);

  Result ConvertConst(const clang::Expr *expr, clang::QualType type, RV rv = R_UNSPEC);
};

} // namespace wgen

#endif /* CLANG2WHIRL_CONST_H */
