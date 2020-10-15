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

#ifndef CLANG2WHIRL_EXPR_H
#define CLANG2WHIRL_EXPR_H

// forward clang declarations
#include <symtab_idx.h>
#include "clangdecl.h"

// forward open64 declarations
#include "open64decl.h"
#include "c2w_builder.h"
#include "c2w_result.h"
#include "c2w_tracer.h"
#include "clanginc.h"

namespace wgen {

class WhirlBuilder;

typedef std::vector< std::pair<WN *, TY_IDX> > WN_VECTOR;

class WhirlExprBuilder {
  // max builtin call param count
  enum { MAX_PARM_CNT = 80 };

private:
  // pointer to WhirlBuilder
  WhirlBuilder *_builder;

public:
  // build builtin table for intrinsics which is mapped to whirl intrinsic
  // op directly
  static void InitializeBuiltinTable();

  // constructor to convert expr into return value
  WhirlExprBuilder(WhirlBuilder *builder) : _builder(builder) { }

  // destructor
  ~WhirlExprBuilder() { }

private:
  RV     GetRV(const clang::Expr *expr, BOOL retv);

  void SetCallParm(const clang::CallExpr *expr, WN *wn, int &start_offset);

  WN    *EmitAdjustVirtualBase(WN *wn, TY_IDX from_ty, const clang::CXXRecordDecl *decl);

  Result EmitAssignNode(Result lhs, Result rhs, TY_IDX ty, RV rv);

  Result EmitArrayInitialization(const clang::InitListExpr *expr, Result dest, UINT current_offset = 0);

  Result EmitRecordInitialization(const clang::InitListExpr *expr, Result dest, UINT current_offset = 0);
  
  Result EmitVectorInitialization(const clang::InitListExpr *expr, Result dest, UINT current_offset = 0);

  Result EmitComplexInitialization(const clang::InitListExpr *expr, Result dest, UINT current_offset = 0);

  Result EmitCXXAllocateException(clang::QualType type, SRCPOS spos);

  Result EmitCXXFreeException(WN *wn);

  Result ConvertCXXMFPToINITV(const clang::Expr *expr, TY_IDX ty);

  Result EmitCXXMemberPointerCall(const clang::CXXMemberCallExpr *expr, BOOL retv);

  Result EmitCXXNewDeleteCall(const clang::CallExpr *expr, bool is_delete);

  Result EmitLoadRTTIFromVTable(const clang::Expr *expr, TY_IDX ptr_ty);

  Result GenerateCall(const char* fname, TY_IDX rtype, INT parm_cnt, const clang::Expr * const *parm, BOOL retv);

  Result GenerateIntrinsic(const clang::CallExpr *expr, TYPE_ID rtype, INTRINSIC iopc, BOOL is_op, BOOL retv);

private:
  Result ConvertArrayInitLoopExpr(const clang::ArrayInitLoopExpr *expr, Result dest);

  Result ConvertArraySubscriptExpr(const clang::ArraySubscriptExpr *expr);
  
  Result ConvertBinaryConditionalOperator(const clang::BinaryConditionalOperator *expr);

  void Update_pointee_ty(clang::QualType type, TY_IDX ty_idx);

  Result ConvertBinaryOperator(const clang::BinaryOperator *expr, BOOL retv);

  Result ConvertBuiltinExpr(const clang::CallExpr *expr, const clang::FunctionDecl *decl, unsigned builtin_id, BOOL retv);

  void EmitCallee(const clang::Expr *expr);

  Result ConvertCallExpr(const clang::CallExpr *expr, BOOL retv);
  
  Result ConvertChooseExpr(const clang::ChooseExpr *expr);

  Result ConvertConvertVectorExpr(const clang::ConvertVectorExpr *expr);
  
  Result ConvertCompoundAssignOperator(const clang::CompoundAssignOperator *expr, BOOL retv);
  
  Result ConvertCompoundLiteralExpr(const clang::CompoundLiteralExpr *expr, Result dest);
  
  Result ConvertConditionalOperator(const clang::ConditionalOperator *expr, BOOL retv);
  
  Result ConvertCStyleCastExpr(const clang::CStyleCastExpr *expr, Result dest);
  
  Result ConvertCXXBindTemporaryExpr(const clang::CXXBindTemporaryExpr *expr, Result dest);

  Result ConvertCXXNewExpr(const clang::CXXNewExpr *expr);
  
  Result ConvertCXXConstCastExpr(const clang::CXXConstCastExpr *expr);
  
  Result ConvertCXXDeleteExpr(const clang::CXXDeleteExpr *expr);
  
  Result ConvertCXXDefaultArgExpr(const clang::CXXDefaultArgExpr *expr);
  
  Result ConvertCXXDefaultInitExpr(const clang::CXXDefaultInitExpr *expr, Result dest);
  
  Result ConvertCXXDynamicCastExpr(const clang::CXXDynamicCastExpr *expr);
  
  Result ConvertCXXFunctionalCastExpr(const clang::CXXFunctionalCastExpr *expr, Result dest);
  
  Result ConvertCXXMFPFromFA(const clang::UnaryOperator* expr, INT64 ofst);

  Result ConvertCXXMFPFromWhirl(WN *value, TY_IDX ty, INT64 ofst);

  Result ConvertCXXMemberCallExpr(const clang::CXXMemberCallExpr *expr, BOOL retv);
  
  Result ConvertCXXOperatorCallExpr(const clang::CXXOperatorCallExpr *expr, BOOL retv);
  
  Result ConvertCXXReinterpretCastExpr(const clang::CXXReinterpretCastExpr *expr);
  
  Result ConvertCXXStaticCastExpr(const clang::CXXStaticCastExpr *expr);

  Result ConvertCXXStdInitializerListExpr(const clang::CXXStdInitializerListExpr *expr, Result dest);
  
  Result ConvertCXXTemporaryObjectExpr(const clang::CXXTemporaryObjectExpr *expr, Result dest);

  Result ConvertCXXThrowExpr(const clang::CXXThrowExpr *expr);
  
  Result ConvertCXXTypeidExpr(const clang::CXXTypeidExpr *expr);
  
  Result ConvertCXXUuidofExpr(const clang::CXXUuidofExpr *expr);

  Result ConvertLambdaFieldRefExpr(const clang::DeclRefExpr *expr);
  
  Result ConvertDeclRefExpr(const clang::DeclRefExpr *expr);
  
  Result ConvertExprWithCleanups(const clang::ExprWithCleanups *expr, Result dest);
  
  Result ConvertExtVectorElementExpr(const clang::ExtVectorElementExpr *expr);
  
  Result ConvertGenericSelectionExpr(const clang::GenericSelectionExpr *expr);
 
  Result ConvertGNUNullExpr(const clang::GNUNullExpr *expr);
 
  Result ConvertImplicitCastExpr(const clang::ImplicitCastExpr *expr, Result dest);

  Result ConvertInitListExpr(const clang::InitListExpr *expr, Result dest, UINT current_offset = 0);
  
  Result ConvertIntegerLiteral(const clang::IntegerLiteral *expr);
  
  Result ConvertFloatingLiteral(const clang::FloatingLiteral *expr);
  
  Result ConvertLambdaExpr(const clang::LambdaExpr *expr, Result dest);
  
  Result ConvertLValueToRValue(const clang::Expr* expr);

  Result ConvertMaterializeTemporaryExpr(const clang::MaterializeTemporaryExpr *expr, Result dest);
  
  Result ConvertMemberExpr(const clang::MemberExpr *expr, BOOL retv);

#ifdef C2W_ENABLE_OBJC
  Result ConvertObjCBridgedCastExpr(const clang::ObjCBridgedCastExpr *expr);
  Result ConvertObjCEncodeExpr(const clang::ObjCEncodeExpr *expr);
  Result ConvertObjCIsaExpr(const clang::ObjCIsaExpr *expr);
  Result ConvertObjCIvarRefExpr(const clang::ObjCIvarRefExpr *expr);
  Result ConvertObjCMessageExpr(const clang::ObjCMessageExpr *expr);
  Result ConvertObjCPropertyRefExpr(const clang::ObjCPropertyRefExpr *expr);
  Result ConvertObjCSelectorExpr(const clang::ObjCSelectorExpr *expr);
#endif
  
  Result ConvertOpaqueValueExpr(const clang::OpaqueValueExpr *expr);
  
  Result ConvertParenExpr(const clang::ParenExpr *expr, Result dest, BOOL retv);

#if LLVM_VERSION_MAJOR == 11
  Result ConvertConstantExpr(const clang::ConstantExpr *expr, Result dest, BOOL retv);
#endif
  
  Result ConvertPredefinedExpr(const clang::PredefinedExpr *expr);
  
  Result ConvertPseudoObjectExpr(const clang::PseudoObjectExpr *expr);

  Result ConvertShuffleVectorExpr(const clang::ShuffleVectorExpr *expr);
  
  Result ConvertStmtExpr(const clang::StmtExpr *expr);
  
  Result ConvertStringLiteral(const clang::StringLiteral *expr);
  
  Result ConvertSubstNonTypeTemplateParmExpr(const clang::SubstNonTypeTemplateParmExpr *expr);
  
  Result ConvertTargetBuiltinExpr(const clang::CallExpr *expr, const clang::FunctionDecl *decl, unsigned builtin_id);

  Result ConvertTypeTraitExpr(const clang::TypeTraitExpr *expr);

  Result ConvertPrePostIncDec(const clang::UnaryOperator *expr, WN *wn_sub, TY_IDX dst_ty, RV rv);

  Result ConvertUnaryOperator(const clang::UnaryOperator *expr, BOOL retv);
  
  Result ConvertUserDefinedLiteral(const clang::UserDefinedLiteral *expr, BOOL retv);
  
  Result ConvertVAArgExpr(const clang::VAArgExpr *expr);

  Result ConvertVAArgExpr32Bit(const clang::VAArgExpr *expr);

  Result ConvertCXXThisExpr(const clang::CXXThisExpr *expr);

  Result ConvertImplicitValueInitExpr(const clang::ImplicitValueInitExpr *expr, Result dest);

  Result ConvertCharacterLiteral(const clang::CharacterLiteral *expr);

  Result ConvertCastExpr(const clang::CastExpr *expr, Result dest = Result::nwNone());

  Result ConvertUnaryExprOrTypeTraitExpr(const clang::UnaryExprOrTypeTraitExpr *expr);

  Result ConvertCXXBoolLiteralExpr(const clang::CXXBoolLiteralExpr *expr);

  Result ConvertImaginaryLiteral(const clang::ImaginaryLiteral *expr);

  Result ConvertOffsetOfExpr(const clang::OffsetOfExpr *expr);

  Result ConvertAtomicExpr(const clang::AtomicExpr *expr, BOOL retv);

  Result ConvertSizeOfPackExpr(const clang::SizeOfPackExpr *expr);

  Result ConvertCXXScalarValueInitExpr(const clang::CXXScalarValueInitExpr *expr);

  Result ConvertCXXNoexceptExpr(const clang::CXXNoexceptExpr *expr);

  Result ConvertCXXNullPtrLiteralExpr(const clang::CXXNullPtrLiteralExpr *expr);

  Result ConvertAddrLabelExpr(const clang::AddrLabelExpr*);

  WN    *ConvertConstantArray(TY_IDX ty_ele, WN *bound, WN* body, WN **parm, INT cnt);

  SRCPOS SetSrcPos(clang::SourceLocation sl) { return _builder->SetSrcPos(sl); }
  SRCPOS GetSrcPos(void)                     { return _builder->GetSrcPos(); }

public:
  WN     *EmitCXXConstructCall(const clang::CXXConstructorDecl *decl, Result dest,
                              clang::CXXCtorType ctor, const WN_VECTOR &args);

  WN    *ConvertToNode(const clang::Expr *expr, Result dest = Result::nwNone(), BOOL retv = TRUE);

  Result ConvertExpr(const clang::Expr *expr, Result dest = Result::nwNone(), BOOL retv = TRUE);

  Result ConvertBaseToDerivedMemberPointer(const clang::CastExpr *expr);

  Result ConvertCXXConstructExpr(const clang::CXXConstructExpr *expr, Result dest, clang::CXXCtorType ctor);

};

}

#endif /* CLANG2WHIRL_EXPR_H */
