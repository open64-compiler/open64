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
#include "c2w_type.h"
#include "c2w_builder.h"
#include "c2w_tracer.h"

#include "clanginc.h"

using namespace clang;

// open64 header files
#include "open64inc.h"

namespace wgen {

TY_IDX VTableTypeConverter::Convert(const CXXRecordDecl *decl) {
  ASTContext *ast_c = B()->Context();
  ItaniumVTableContext *vtable_c = cast<ItaniumVTableContext>(ast_c->getVTableContext());
  const VTableLayout &layout = vtable_c->getVTableLayout(decl);
  size_t size = 0;
  TY_IDX pointer_type = Make_Pointer_Type(MTYPE_To_TY(MTYPE_I8));
  for (size_t i = 0; i < layout.getNumVTables(); i++) {
    size += layout.getVTableSize(i);
  }
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  TY_Init(ty, size * TY_size(pointer_type), KIND_ARRAY, MTYPE_M, B()->TB().getTempName());
  Set_TY_etype(ty, pointer_type);
  Set_TY_align(ty_idx, TY_align(pointer_type));
  ARB_HANDLE arb = New_ARB ();
  ARB_Init (arb, 0, 0, 0);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  Set_ARB_dimension (arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_stride_val(arb, TY_size(pointer_type));
  Set_ARB_const_lbnd (arb);
  Set_ARB_lbnd_val (arb, 0);
  Set_ARB_const_ubnd (arb);
  Set_ARB_ubnd_val (arb, size);
  Set_TY_arb (ty, arb);
  return ty_idx;
}

TY_IDX VTTTypeConverter::Convert(const CXXRecordDecl *decl) {
  ASTContext *ast_c = B()->Context();
  ItaniumVTableContext *vtable_c = cast<ItaniumVTableContext>(ast_c->getVTableContext());
  const VTableLayout &layout = vtable_c->getVTableLayout(decl);
  size_t size = layout.getNumVTables();
  TY_IDX pointer_type = Make_Pointer_Type(MTYPE_To_TY(MTYPE_I8));

  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  TY_Init(ty, size * TY_size(pointer_type), KIND_ARRAY, MTYPE_M, B()->TB().getTempName());
  Set_TY_etype(ty, pointer_type);
  Set_TY_align(ty_idx, TY_align(pointer_type));
  ARB_HANDLE arb = New_ARB ();
  ARB_Init (arb, 0, 0, 0);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  Set_ARB_dimension (arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_stride_val(arb, TY_size(pointer_type));
  Set_ARB_const_lbnd (arb);
  Set_ARB_lbnd_val (arb, 0);
  Set_ARB_const_ubnd (arb);
  Set_ARB_ubnd_val (arb, size);
  Set_TY_arb (ty, arb);
  return ty_idx;
}

UINT32 WhirlTypeBuilder::nameIdx = 0;

WhirlTypeBuilder::WhirlTypeBuilder(WhirlBuilder *builder)
  : _vtc(builder), _vtt_tc(builder), _builder(builder),
    _m_uintptr_ty(MTYPE_UNKNOWN) {
}

WhirlTypeBuilder::~WhirlTypeBuilder() {
}

void
WhirlTypeBuilder::Initialize() {
  _m_uintptr_ty = TY_mtype(ConvertType(_builder->Context()->VoidPtrTy));
}

void
WhirlTypeBuilder::Finalize() {
  TRACE_FUNC();
}

TY_IDX
WhirlTypeBuilder::ConvertAtomicType(const AtomicType *type) {
  TRACE_FUNC();
  TRACE_WARN(("AtomicType is not supported, discard atomic"));
  return ConvertType(type->getValueType());
}

TY_IDX
WhirlTypeBuilder::ConvertBlockPointerType(const BlockPointerType *type) {
  TRACE_FUNC();
  Is_True(false, ("unsupported ConvertBlockPointerType"));
  return (TY_IDX) 0;
}

TY_IDX
WhirlTypeBuilder::ConvertBuiltinType(const BuiltinType *type) {
  TRACE_FUNC();
  Is_True(type, ("NULL type"));
  TYPE_ID mtype = MTYPE_UNKNOWN;
  switch (type->getKind()) {
    case BuiltinType::Void:
#ifdef C2W_ENABLE_OBJC
      case BuiltinType::ObjCId:
      case BuiltinType::ObjCClass:
      case BuiltinType::ObjCSel:
          break;
#endif
      mtype = MTYPE_V;
      break;
    case BuiltinType::Bool:
      mtype = MTYPE_U1;
      break;
    case BuiltinType::Char_S:
    case BuiltinType::SChar:
      mtype = MTYPE_I1;
      break;
    case BuiltinType::Char_U:
    case BuiltinType::UChar:
      mtype = MTYPE_U1;
      break;
    case BuiltinType::Short:
      mtype = MTYPE_I2;
      break;
    case BuiltinType::UShort:
      mtype = MTYPE_U2;
      break;
    case BuiltinType::Int:
      mtype = MTYPE_I4;
      break;
    case BuiltinType::UInt:
      mtype = MTYPE_U4;
      break;
    case BuiltinType::Long:
      mtype = MTYPE_I8;
      break;
    case BuiltinType::ULong:
      mtype = MTYPE_U8;
      break;
    case BuiltinType::LongLong:
      mtype = MTYPE_I8;
      break;
    case BuiltinType::ULongLong:
      mtype = MTYPE_U8;
      break;
    case BuiltinType::WChar_S:
      mtype = MTYPE_I4;
      break;
    case BuiltinType::WChar_U:
      mtype = MTYPE_U4;
      break;
    case BuiltinType::Char16:
      mtype = MTYPE_U2;
      break;
    case BuiltinType::Char32:
      mtype = MTYPE_U4;
      break;
    case BuiltinType::Half: // F2
      TRACE_WARN(("F2 is not supported, promote to F4"));
      mtype = MTYPE_F4;
      break;
    case BuiltinType::Float:
      mtype = MTYPE_F4;
      break;
    case BuiltinType::Double:
      mtype = MTYPE_F8;
      break;
    case BuiltinType::LongDouble:
    case BuiltinType::Float128:
      mtype = MTYPE_F10;
      break;
    case BuiltinType::NullPtr:
      mtype = Pointer_Mtype;
      break;
    case BuiltinType::UInt128:
    case BuiltinType::Int128:
      TRACE_WARN(("I128 is not supported, lower to I8"));
      mtype = MTYPE_I8;
      break;
#ifdef C2W_ENABLE_OPENCL
    case BuiltinType::OCLImage1d:
    case BuiltinType::OCLImage1dArray:
    case BuiltinType::OCLImage1dBuffer:
    case BuiltinType::OCLImage2d:
    case BuiltinType::OCLImage2dArray:
    case BuiltinType::OCLImage3d:
    case BuiltinType::OCLSampler:
    case BuiltinType::OCLEvent:
        break;
#endif
    default:
      Is_True(false,
              ("Unsupported builtin type: %d", type->getKind()));
  }
  return MTYPE_To_TY(mtype);
}

TY_IDX
WhirlTypeBuilder::ConvertComplexType(const ComplexType *type) {
  TRACE_FUNC();
  TY_IDX elem_ty = ConvertType(type->getElementType());
  TYPE_ID mtype;
  INT64 ty_size = TY_size(elem_ty) * 2;
  switch (ty_size) {
    case 2:
      Is_True(false, ("unsupported complex integer"));
    case 4:
    case 8:
      mtype = MTYPE_C4; break;
    case 16:
      mtype = MTYPE_C8; break;
    // TODO: the correct way to get the type is from type mode
    // so it can support float_128
    case 24:
    case 32:
      mtype = MTYPE_C10;
      break;
    default:  Is_True(false, ("unexpected size"));
  }
  TY_IDX ty_idx = MTYPE_To_TY(mtype);      // use predefined type
  return ty_idx;
}

TY_IDX
WhirlTypeBuilder::ConvertConstantArrayType(const ConstantArrayType *type) {
  TRACE_FUNC();
  TY_IDX elem_ty = ConvertType(type->getElementType());
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  addTypeToMap(type, ty_idx);
  UINT ty_size = type->getSize().getZExtValue() * TY_size(elem_ty);
  TY_Init(ty, ty_size, KIND_ARRAY, MTYPE_M, (STR_IDX) 0);
  Set_TY_etype(ty, elem_ty);
  Set_TY_align(ty_idx, TY_align(elem_ty));
  Set_TY_anonymous(ty);
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, 0, 0, 0);
  Set_TY_arb(ty, arb);
  Set_ARB_first_dimen(arb);
  Set_ARB_last_dimen(arb);
  Set_ARB_dimension(arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_lbnd_val(arb, 0);
  //Set_ARB_stride_val(arb, type->getSize().getZExtValue());
  Set_ARB_ubnd_val(arb, type->getSize().getZExtValue() - 1);
  return (TY_IDX) ty_idx;
}

TY_IDX
WhirlTypeBuilder::ConvertElaboratedType(const ElaboratedType *type) {
  TRACE_FUNC();
  return ConvertType(type->getNamedType());
}

TY_IDX
WhirlTypeBuilder::ConvertEnumType(const EnumType *type) {
  TRACE_FUNC();
  const EnumDecl *enum_decl = cast<EnumType>(type)->getDecl();
  if (enum_decl->isCompleteDefinition() || enum_decl->isFixed())
    return ConvertType(enum_decl->getIntegerType());
  //isIntegerTy(32)
  return (TY_IDX) 0;
  //return llvm::Type::getInt32Ty(_builder->Context());
}

TY_IDX
WhirlTypeBuilder::EmitGlobalInitializerType() {
  TY_IDX ty_idx;
  TY &func = New_TY(ty_idx);
  TY_Init(func, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align(ty_idx, 1);
  // void return type
  TYLIST tylist_idx;
  TY_IDX rty = MTYPE_To_TY(MTYPE_V);
  Set_TYLIST_type(New_TYLIST(tylist_idx), rty);
  Set_TY_tylist(ty_idx, tylist_idx);
  // two param type
  TY_IDX pty = MTYPE_To_TY(MTYPE_I4);
  Set_TYLIST_type(New_TYLIST(tylist_idx), pty);
  Set_TYLIST_type(New_TYLIST(tylist_idx), pty);
  Set_TYLIST_type(New_TYLIST(tylist_idx), 0);
  return ty_idx;
}

// emit pu with void return type
TY_IDX
WhirlTypeBuilder::EmitCXXPureVirtualType() {
  TY_IDX ty_idx;
  TY &func = New_TY(ty_idx);
  TY_Init(func, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align(ty_idx, 1);
  // void return type
  TYLIST tylist_idx;
  TY_IDX rty = MTYPE_To_TY(MTYPE_V);
  Set_TYLIST_type(New_TYLIST(tylist_idx), rty);
  Set_TY_tylist(ty_idx, tylist_idx);
  Set_TYLIST_type(New_TYLIST(tylist_idx), 0);
  return ty_idx;
}

TY_IDX
WhirlTypeBuilder::ConvertFunctionType(const FunctionType *type, const Type *record_type) {
  TY_IDX ty_idx;
  Is_True(_builder->Lang_C() || _builder->Lang_CPP(), ("unknown language"));

  // function type
  TY &func = New_TY(ty_idx);
  addTypeToMap(type, ty_idx);
  TY_Init(func, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align(ty_idx, 1);

  // convert param type and add them into parm_types
  vector<TY_IDX> parm_types;
  CanQualType FTY = type->getCanonicalTypeUnqualified();
  if (isa<FunctionProtoType>(FTY)) {
    Set_TY_has_prototype(ty_idx);
    CanQual<FunctionProtoType> FPT = FTY.getAs<FunctionProtoType>();

    // set varargs
    if (FPT->isVariadic())
      Set_TY_is_varargs(ty_idx);

    for (int i = 0, e = FPT->getNumParams(); i < e; ++i) {
      TY_IDX pty = ConvertType(FPT->getParamType(i));
      parm_types.push_back(pty);
    }
  } else {
    Clear_TY_has_prototype(ty_idx);
  }

  // return type
  TYLIST tylist_idx;
  TY_IDX rty = ConvertType(type->getReturnType());
  Set_TYLIST_type(New_TYLIST(tylist_idx), rty);
  Set_TY_tylist(ty_idx, tylist_idx);

  if (record_type) {
    Is_True(_builder->Lang_CPP(), ("Invalid language"));
    TY_IDX rty = ConvertType(record_type);
    Set_TYLIST_type(New_TYLIST(tylist_idx), Make_Pointer_Type(rty));
  }

  // set param type into TYLIST
  for (int n = 0; n < parm_types.size(); ++n) {
    Set_TYLIST_type(New_TYLIST(tylist_idx), parm_types[n]);
  }

  // end of param
  Set_TYLIST_type(New_TYLIST(tylist_idx), 0);

  return ty_idx;
}

TY_IDX
WhirlTypeBuilder::ConvertIncompleteArrayType(const IncompleteArrayType *type) {
  TRACE_FUNC();
  TY_IDX elementType = ConvertType(type->getElementType());
  // assumed to have zero element for incomplete array type
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  addTypeToMap(type, ty_idx);
  // make a array type with one element
  TY_Init(ty, 0, KIND_ARRAY, MTYPE_M, (STR_IDX) 0);
  Set_TY_etype(ty, elementType);
  Set_TY_align(ty_idx, TY_align(elementType));
  Set_TY_anonymous(ty);
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, 0, 0, 0);
  Set_TY_arb(ty, arb);
  Set_ARB_first_dimen(arb);
  Set_ARB_last_dimen(arb);
  Set_ARB_dimension(arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_lbnd_val(arb, 0);
  Clear_ARB_const_ubnd (arb);
  Set_ARB_ubnd_val (arb, 0);
  // set incomplete flag
  Set_TY_is_incomplete(ty_idx);
  return ty_idx;
}

TY_IDX
WhirlTypeBuilder::ConvertMemberPointerType(const MemberPointerType *type) {
  TRACE_FUNC();
  if (type->isMemberDataPointer()) {
    return Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8));
  }

  Is_True(type->isMemberFunctionPointer(), ("invalid other type"));

  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  TY_Init(ty, 0, KIND_STRUCT, MTYPE_M, (STR_IDX)0);
  TY_IDX pointed_ty = ConvertType(type->getPointeeType());

  int first_field_idx = Fld_Table.Size();
  FLD_Init(New_FLD(), Save_Str("__pfn"), Make_Pointer_Type(pointed_ty), 0);
  FLD_Init(New_FLD(), Save_Str("__delta"), MTYPE_To_TY(MTYPE_I8), 8);
  Set_TY_size(ty_idx, 16);
  Set_TY_align(ty_idx, 8);
  FLD_IDX last_field_idx = Fld_Table.Size() - 1;
  Set_TY_fld(ty, FLD_HANDLE(first_field_idx));
  Set_FLD_last_field(FLD_HANDLE(last_field_idx));

  // set _fld_used for MemberPointerType
  _fld_used[type] = 2;

  return ty_idx;
}

TY_IDX
WhirlTypeBuilder::ConvertPointerType(const PointerType *type) {
  TRACE_FUNC();
  // allow incomplete pointee
  TY_IDX pointed_ty = ConvertType(type->getPointeeType(), TRUE);
  return Make_Pointer_Type(pointed_ty);
}

TY_IDX
WhirlTypeBuilder::ConvertReferenceType(const ReferenceType *type) {
  TRACE_FUNC();
  // allow incomplete pointee
  TY_IDX pointed_ty = ConvertType(type->getPointeeType(), TRUE);
  return Make_Pointer_Type(pointed_ty);
}

TY_IDX
WhirlTypeBuilder::ConvertVariableArrayType(const VariableArrayType *type) {
  TRACE_FUNC();
  TY_IDX elem_ty = ConvertType(type->getElementType());

  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  addTypeToMap(type, ty_idx);
  // make a array type with zero element
  TY_Init(ty, 0, KIND_ARRAY, MTYPE_M, (STR_IDX) 0);
  Set_TY_etype(ty, elem_ty);
  Set_TY_align(ty_idx, TY_align(elem_ty));
  Set_TY_anonymous(ty);
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, 0, 0, 0);
  Set_TY_arb(ty, arb);
  Set_ARB_first_dimen(arb);
  Set_ARB_last_dimen(arb);
  Set_ARB_dimension(arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_stride_val(arb, TY_size(elem_ty));
  Set_ARB_lbnd_val(arb, 0);
  Set_TY_is_incomplete(ty_idx);

  if (const Expr *size = type->getSizeExpr()) {
    if (const IntegerLiteral *int_size = dyn_cast<IntegerLiteral>(size)) {
      Set_ARB_const_ubnd(arb);
      Set_ARB_ubnd_val(arb, int_size->getValue().getZExtValue() - 1);
    } else {
      // get vla bound st for size expr
      ST_IDX ubnd_var_st = _builder->Get_vla_bound_st(size);
      WN *wn = WN_CreateXpragma(WN_PRAGMA_COPYIN_BOUND, (ST_IDX)NULL, 1);
      TY_IDX ty = ST_type(ubnd_var_st);
      WN_kid0(wn) = WN_Ldid(TY_mtype(ty), 0, ubnd_var_st, ty);
      WN_Set_Linenum(wn, ST_Srcpos(ubnd_var_st));
      WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), wn);
      Clear_ARB_const_ubnd(arb);
      Set_ARB_ubnd_var(arb, ubnd_var_st);
    }
    Clear_TY_is_incomplete(ty_idx);
  } else {
    Clear_ARB_const_ubnd(arb);
    Set_ARB_ubnd_val(arb, 0);
  }

  return ty_idx;
}

TY_IDX
WhirlTypeBuilder::ConvertParenType(const ParenType *type) {
  TRACE_FUNC();
  return ConvertType(type->getInnerType());
}

TY_IDX
WhirlTypeBuilder::ConvertVectorType(const VectorType *type) {
  TRACE_FUNC();
#ifdef TARG_UWASM
  Fail_Assertion(FALSE, "UWASM not support vector type");
  return MTYPE_To_TY(MTYPE_UNKNOWN);
#else
  TY_IDX elem_ty = ConvertType(type->getElementType());
  TYPE_ID elem_mtype = TY_mtype(elem_ty);

  int elem = type->getNumElements();
  TYPE_ID mtype = MTYPE_UNKNOWN;

  switch (elem) {
    case 1:
      if (elem_mtype == MTYPE_I8 ||
          elem_mtype == MTYPE_U8)
        mtype = MTYPE_V8I8;
    case 2:
      if (elem_mtype == MTYPE_I1 ||
          elem_mtype == MTYPE_U1)
        mtype = MTYPE_V8I1;
      else if (elem_mtype == MTYPE_I2 ||
          elem_mtype == MTYPE_U2)
        mtype = MTYPE_V8I2;
      else if (elem_mtype == MTYPE_I4 ||
          elem_mtype == MTYPE_U4)
        mtype = MTYPE_V8I4;
      else if (elem_mtype == MTYPE_F4)
        mtype = MTYPE_V8F4;
      else if (elem_mtype == MTYPE_I8 ||
               elem_mtype == MTYPE_U8)
        mtype = MTYPE_V16I8;
      else if (elem_mtype == MTYPE_F8)
        mtype = MTYPE_V16F8;
      break;
    case 4:
      if (elem_mtype == MTYPE_I1 ||
          elem_mtype == MTYPE_U1)
        mtype = MTYPE_V8I1;
      else if (elem_mtype == MTYPE_I2 ||
          elem_mtype == MTYPE_U2)
        mtype = MTYPE_V8I2;
      else if (elem_mtype == MTYPE_I4 ||
               elem_mtype == MTYPE_U4)
        mtype = MTYPE_V16I4;
      else if (elem_mtype == MTYPE_I8 ||
               elem_mtype == MTYPE_U8)
        mtype = MTYPE_V32I8;
      else if (elem_mtype == MTYPE_F4)
        mtype = MTYPE_V16F4;
      else if (elem_mtype == MTYPE_F8)
        mtype = MTYPE_V32F8;
      break;
    case 8:
      if (elem_mtype == MTYPE_I1 ||
          elem_mtype == MTYPE_U1)
        mtype = MTYPE_V8I1;
      else if (elem_mtype == MTYPE_I2 ||
               elem_mtype == MTYPE_U2)
        mtype = MTYPE_V16I2;
      else if (elem_mtype == MTYPE_I4 ||
               elem_mtype == MTYPE_U4)
        mtype = MTYPE_V32I4;
      else if (elem_mtype == MTYPE_F4)
        mtype = MTYPE_V32F4;
      // TODO: support AVX-512
      else if (elem_mtype == MTYPE_I8 ||
               elem_mtype == MTYPE_U8)
        mtype = MTYPE_V32I8;
      else if (elem_mtype == MTYPE_F8)
        mtype = MTYPE_V32F8;
      break;
    case 16:
      if (elem_mtype == MTYPE_I1 ||
          elem_mtype == MTYPE_U1)
        mtype = MTYPE_V16I1;
      else if (elem_mtype == MTYPE_I2 ||
               elem_mtype == MTYPE_U2)
        mtype = MTYPE_V32I2;
      // TODO: support AVX-512
      else if (elem_mtype == MTYPE_I4 ||
               elem_mtype == MTYPE_U4)
        mtype = MTYPE_V32I4;
      else if (elem_mtype == MTYPE_F4)
        mtype = MTYPE_V32F4;
      // TODO: support AVX-512
      else if (elem_mtype == MTYPE_I8 ||
               elem_mtype == MTYPE_U8)
        mtype = MTYPE_V32I8;
      else if (elem_mtype == MTYPE_F8)
        mtype = MTYPE_V32F8;
      break;
    case 32:
      if (elem_mtype == MTYPE_I1 ||
          elem_mtype == MTYPE_U1)
        mtype = MTYPE_V32I1;
      // TODO: support AVX-512
      else if (elem_mtype == MTYPE_I2 ||
               elem_mtype == MTYPE_U2)
        mtype = MTYPE_V32I2;
      // TODO: support AVX-512
      else if (elem_mtype == MTYPE_I4 ||
               elem_mtype == MTYPE_U4)
        mtype = MTYPE_V32I4;
      break;
    case 64:
      // TODO: support AVX-512
      if (elem_mtype == MTYPE_I1 ||
          elem_mtype == MTYPE_U1)
        mtype = MTYPE_V32I1;
      // TODO: support AVX-512
      else if (elem_mtype == MTYPE_I2 ||
               elem_mtype == MTYPE_U2)
        mtype = MTYPE_V32I2;
      // TODO: support AVX-512
      else if (elem_mtype == MTYPE_I4 ||
               elem_mtype == MTYPE_U4)
        mtype = MTYPE_V32I4;
      break;
    default:
      Is_True(false, ("unexpected vector type element count"));
  }

  Is_True(mtype != MTYPE_UNKNOWN,
          ("unexpected vector type element count"));

  return MTYPE_To_TY(mtype);
#endif
}

struct FieldLayoutInfo {
private:
  enum FieldKind {
    F_BASE,
    F_FIELD,
    F_VBASE,
  };
  union {
    const CXXBaseSpecifier *_base;
    const FieldDecl        *_field;
  }                         _u;
  long                      _offset;
  TY_IDX                    _ty;
  FieldKind                 _kind;

public:
  FieldLayoutInfo(const CXXBaseSpecifier* base, long ofst, TY_IDX ty)
  {
    _u._base = base;
    _offset = ofst * 8; // convert to bit
    _ty = ty;
    _kind = base->isVirtual() ? F_VBASE : F_BASE;
  }

  FieldLayoutInfo(const FieldDecl* field, long ofst, TY_IDX ty)
  {
    _u._field = field;
    _offset = ofst;
    _ty = ty;
    _kind = F_FIELD;
  }

  const CXXBaseSpecifier *Base() const    { return _u._base;  }
  const FieldDecl        *Field() const   { return _u._field; }
  long                    Offset() const  { return _offset;   }
  TY_IDX                  Ty() const      { return _ty;       }
  FieldKind               Kind() const    { return _kind;     }

  BOOL                    IsBase() const  { return _kind == F_BASE;  }
  BOOL                    IsVBase() const { return _kind == F_VBASE; }
  BOOL                    IsField() const { return _kind == F_FIELD; }

  std::string             Name() const
  {
    return IsField() ? Field()->getNameAsString()
                     : Base()->getType()->getAsCXXRecordDecl()->getNameAsString();
  }

  const char             *KindName() const
  {
    return IsBase() ? "_base."
                     : IsVBase() ? "_vbase."
                                  : IsField() ? "" : "_unk.";
  }
};

struct FieldLayoutInfoCmp {
  bool operator()(const FieldLayoutInfo& a, const FieldLayoutInfo& b) const {
    if (a.Offset() == b.Offset())
      return a.Kind() < b.Kind();
    return a.Offset() < b.Offset();
  }
};

TY_IDX
WhirlTypeBuilder::ConvertRecordType(const RecordType *type, BOOL incomplete, TY_IDX ty_idx) {
  TRACE_FUNC();
  RecordDecl *decl = type->getDecl();

  // init TY
  if (ty_idx == TY_IDX_ZERO) {
    TY& pty = New_TY(ty_idx);
    addTypeToMap(type, ty_idx);
    STR_IDX name_idx = STR_IDX_ZERO;
    if (isa<ClassTemplateSpecializationDecl>(decl)) {
      SmallString<128> name_buf;
      llvm::raw_svector_ostream os(name_buf);
      const LangOptions &opts = _builder->Context()->getLangOpts();
      // get same class name as GCC "class_name<template_arg>"
      decl->getNameForDiagnostic(os, PrintingPolicy(opts), true);
      name_idx = _builder->DeclBuilder().ConvertName(decl, os.str());
    }
    else if (!decl->getName().empty()) {
      std::string TypeName = decl->getQualifiedNameAsString();
      name_idx = _builder->EnterString(TypeName.c_str());
    }
    TY_Init(pty, 0, KIND_STRUCT, MTYPE_M, name_idx);
    if (decl->getName().empty() || decl->isAnonymousStructOrUnion())
      Set_TY_anonymous(pty);
    if (type->isUnionType())
      Set_TY_is_union(pty);
    Set_TY_is_incomplete(pty);
  }

  if (incomplete == FALSE &&
      type->isIncompleteType() &&
      decl->getDefinition())
    type = cast<RecordType>(decl->getDefinition()->getTypeForDecl());

  if (type->isIncompleteType())
    return ty_idx;

  // handle record layour
  ASTContext *ast_c = _builder->Context();
  const ASTRecordLayout &layout = ast_c->getASTRecordLayout(decl);
  const CXXRecordDecl *cxx_decl = dyn_cast<CXXRecordDecl>(decl);
  BOOL is_cstyle = (cxx_decl == NULL ||
                    cxx_decl->isPOD() || cxx_decl->isCLike());
  BOOL has_vptr = FALSE;
  BOOL is_empty = TRUE;
  std::vector<FieldLayoutInfo> fields;

  // process base class for c++
  if (!is_cstyle && cxx_decl->hasDefinition()) {
    has_vptr = layout.hasOwnVFPtr();
    for (const CXXBaseSpecifier &base : cxx_decl->bases()) {
      const RecordType *base_type = base.getType().getCanonicalType()->getAs<RecordType>();
      TY_IDX ty = ConvertType(base_type);
      const CXXRecordDecl *decl = base_type->getAsCXXRecordDecl();
      long ofst;
      if (base.isVirtual())
        ofst = layout.getVBaseClassOffset(decl).getQuantity();
      else
        ofst = layout.getBaseClassOffset(decl).getQuantity();
      fields.push_back(FieldLayoutInfo(&base, ofst, ty));
      if (is_empty)
        is_empty = FALSE;
    }
    for (const CXXBaseSpecifier &base : cxx_decl->vbases()) {
      const RecordType *base_type = base.getType().getCanonicalType()->getAs<RecordType>();
      TY_IDX ty = ConvertType(base_type);
      const CXXRecordDecl *decl = base_type->getAsCXXRecordDecl();
      long ofst = layout.getVBaseClassOffset(decl).getQuantity();
      fields.push_back(FieldLayoutInfo(&base, ofst, ty));
      if (is_empty)
        is_empty = FALSE;
    }
  }
  // process fields
  unsigned field_index = 0;
  for (RecordDecl::field_iterator iter = decl->field_begin();
       iter != decl->field_end(); iter++, field_index++) {
    FieldDecl *fld_decl = *iter;
    const Type *fld_type = fld_decl->getType().getCanonicalType().getTypePtr();
    TY_IDX ty = ConvertType(fld_type);
    long ofst = layout.getFieldOffset(field_index);
    fields.push_back(FieldLayoutInfo(fld_decl, ofst, ty));
    if (is_empty)
      is_empty = FALSE;
  }

  if (!has_vptr && is_empty) {
    Set_TY_size(ty_idx, 0);
    Set_TY_align(ty_idx, 1);
    Clear_TY_is_incomplete(ty_idx);
    return ty_idx;
  }

  // sort by offset
  std::stable_sort(fields.begin(), fields.end(), FieldLayoutInfoCmp());

  // process fields
  long ty_align = layout.getAlignment().getQuantity();
  long ty_size = layout.getSize().getQuantity();
  int first_field_idx = Fld_Table.Size();
  int next_field_id = 1;

  // generate vptr
  if (has_vptr) {
    ItaniumVTableContext *vtable_c = cast<ItaniumVTableContext>(ast_c->getVTableContext());
    const VTableLayout &vt_layout = vtable_c->getVTableLayout(cxx_decl);
    TY_IDX vty = Make_Pointer_Type(ty_idx);
    FLD_HANDLE fld = New_FLD();
    STR_IDX fld_name = Save_Str2("_vptr.", TY_name(ty_idx));
    FLD_Init(fld, fld_name, vty, 0);
    next_field_id++;
  }

  // handle bases and fields
  TY_IDX last_ty   = TY_IDX_ZERO;
  long   last_ofst = 0;
  long   last_field_id = 0;
  for (unsigned fld_idx = 0; fld_idx < fields.size(); ++fld_idx) {
    FieldLayoutInfo& info = fields[fld_idx];
    TY_IDX ty = info.Ty();
    long ofst = info.Offset();
    // check if base already processed for this vbase
    if (info.IsVBase() &&
        ty == last_ty && ofst == last_ofst) {
      Is_True(_fld_map[info.Field()] == 0 ||
              _fld_map[info.Field()] == last_field_id,
              ("field ID already set"));
      _fld_map[info.Field()] = last_field_id;
      continue;
    }
    last_ty = ty;
    last_ofst = ofst;
    last_field_id = next_field_id;

    // generate new FLD
    const char* pfx = info.KindName();
    std::string str = info.Name();
    STR_IDX fld_name = Save_Str2(pfx, str.c_str());
    FLD_HANDLE fld = New_FLD();
    FLD_Init(fld, fld_name, ty, ofst/8);
    const Type *fld_type;
    if (info.IsField()) {
      const FieldDecl *fld_decl = info.Field();
      if (fld_decl->isBitField()) {
        Set_FLD_is_bit_field(fld);
        Set_FLD_bofst(fld, ofst%8);
        UINT bsize = fld_decl->getBitWidthValue(*ast_c);
        if (bsize > MTYPE_bit_size(TY_mtype(ty)))
          bsize = MTYPE_bit_size(TY_mtype(ty));
        Set_FLD_bsize(fld, bsize);
      }
      fld_type = fld_decl->getType().getCanonicalType().getTypePtr();
    }
    else {
      fld_type = info.Base()->getType().getCanonicalType().getTypePtr();
    }
    Set_FLD_is_anonymous(fld);
    Set_FLD_is_base_class(fld);
    if (info.IsVBase())
      Set_FLD_is_virtual(fld);
    Is_True(_fld_map[info.Field()] == 0 ||
            _fld_map[info.Field()] == next_field_id,
            ("field ID already set"));
    _fld_map[info.Field()] = next_field_id;
    Is_True(TY_kind(ty) != KIND_STRUCT || _fld_used[fld_type] + 1 == FLD_get_count(ty),
            ("field count mismatch"));
    next_field_id += TY_kind(ty) == KIND_STRUCT
                       ? _fld_used[fld_type] + 1 : 1;
  }
  _fld_used[type] = next_field_id - 1;

  // finalize ty
  Set_TY_size(ty_idx, ty_size);
  Set_TY_align(ty_idx, ty_align);
  if (next_field_id > 1) {
    FLD_IDX last_field_idx = Fld_Table.Size() - 1;
    if (last_field_idx >= first_field_idx) {
      Set_TY_fld(ty_idx, FLD_HANDLE (first_field_idx));
      Set_FLD_last_field(FLD_HANDLE(last_field_idx));
    }
  }

  if (TY_is_incomplete(ty_idx))
    Clear_TY_is_incomplete(ty_idx);

  return ty_idx;
#if 0
  // for virtual ptr
  if (cxx_decl && cxx_decl->hasDefinition()) {
    ASTContext *ast_c = _builder->Context();
    const ASTRecordLayout &layout = ast_c->getASTRecordLayout(decl);
    if (layout.hasOwnVFPtr()) {
      ItaniumVTableContext *vtable_c = cast<ItaniumVTableContext>(ast_c->getVTableContext());
      const VTableLayout &vt_layout = vtable_c->getVTableLayout(cxx_decl);
      TY_IDX vty = Make_Pointer_Type(ty_idx);
      if (TY_align(vty) > max_align)
        max_align = TY_align(vty);
      fld_align = ty_size % TY_align(vty);
      if (fld_align != 0)
        ty_size += TY_align(vty) - fld_align;
      FLD_HANDLE fld = New_FLD();
      STR_IDX fld_name = Save_Str2("_vptr.", Index_To_Str(name_idx));
      FLD_Init(fld, fld_name, vty, ty_size);
      ty_size += TY_size(vty);
      if (TY_size(vty) > max_size)
        max_size = TY_size(vty);
      fld_count++;
      next_field_id++;
      load_vptr = true;
    }
    // for base
    for (const CXXBaseSpecifier &base : cxx_decl->bases()) {
      const RecordType *base_type = base.getType()->getAs<RecordType>();
      TY_IDX base_ty = ConvertType(base_type);
      Is_True(isa<CXXRecordDecl>(base_type->getDecl()),
              ("base decl shoule be CXXRecordDecl"));
      base_decl = base_type->getAsCXXRecordDecl();
      //if (base.isVirtual() ||
      //    (base_decl->field_empty() && TY_vtable(base_ty) == (TY_IDX)0))
      if (base.isVirtual() || TY_size(base_ty) == 0)
        continue;
      if (!load_vptr && TY_vtable(base_ty)) {
        FLD_HANDLE fld = New_FLD();
        FLD_HANDLE base_fld = TY_fld(base_ty);
        FLD_Init(fld, FLD_name_idx(base_fld), FLD_type(base_fld), 0);
        load_vptr = true;
        fld_count++;
      }
      Is_True(_fld_map[(const Decl*)&base] == 0 ||
              _fld_map[(const Decl*)&base] == next_field_id,
              ("field ID already set"));
      _fld_map[(const Decl*)&base] = next_field_id;
      next_field_id += _fld_used[base_type] + 1;
      if (TY_align(base_ty) > max_align)
        max_align = TY_align(base_ty);
      fld_align = ty_size % TY_align(base_ty);
      if (fld_align != 0)
        ty_size += TY_align(base_ty) - fld_align;
      FLD_HANDLE fld = New_FLD();
      STR_IDX fld_name = _builder->EnterString("anonymous_field");
      FLD_Init(fld, fld_name, base_ty, ty_size);
      Set_FLD_is_anonymous(fld);
      Set_FLD_is_base_class(fld);
      ty_size += TY_size(base_ty);
      // resize ty size for vptr
      if (base_decl->getNumBases() && load_vptr) {
        for (const CXXBaseSpecifier &base_spec : base_decl->bases()) {
          TY_IDX base_ty_idx = ConvertType(base_spec.getType());
          ty_size -= TY_size(base_ty_idx);
        }
      }
      if (TY_size(base_ty) > max_size)
        max_size = TY_size(base_ty);
      fld_count++;
      last_decl = base_decl;
    }
  }

  for (RecordDecl::field_iterator iter = decl->field_begin();
       iter != decl->field_end(); iter++) {
    FieldDecl *fld_decl = *iter;
  //  const Type *fld_type = _builder->Context()->getCanonicalType(fld_decl->getType()).getTypePtr();
    const Type *fld_type = fld_decl->getType().getCanonicalType().getTypePtr();
    Is_True(_fld_map[fld_decl] == 0 ||
            _fld_map[fld_decl] == next_field_id,
            ("field ID already set"));
    _fld_map[fld_decl] = next_field_id;
    next_field_id += _fld_used[fld_type] + 1;
    TY_IDX fld_ty;
    FLD_HANDLE fld;
    // Don't expand the field's type if it's a pointer
    // type, in order to avoid circular dependences
    // involving member object types and base types.
    if (fld_type->isPointerType()) {
      // Defer expanding the field's type.  Put in a
      // generic pointer type for now.
      fld = New_FLD();
      fld_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8), FALSE);
      defer_field(fld_decl, fld);
    } else {
      if (fld_decl->isBitField())
        fld_ty = MTYPE_To_TY(MTYPE_I1);
      else
        fld_ty = ConvertType(fld_decl->getType());
      fld = New_FLD();
    }
    STR_IDX fld_name = 0;
    // some field have no name, like lambda
    if(fld_decl->getIdentifier())
      fld_name = _builder->DeclBuilder().ConvertName(decl, fld_decl->getIdentifier()->getName());
    else
      fld_name = _builder->EnterString("anonymous_field");
    int field_align = TY_align(fld_ty);
    if (field_align > max_align)
      max_align = field_align;
    fld_align = ty_size % field_align;
    if (fld_align != 0)
      ty_size += field_align - fld_align;
    FLD_Init(fld, fld_name, fld_ty, ty_size);

    ty_size += TY_size(fld_ty);
    if (TY_size(fld_ty) > max_size)
      max_size = TY_size(fld_ty);
    fld_count++;
    if (type->isUnionType())
      Set_FLD_ofst(fld, 0);
    if (fld_decl->isBitField()) {
      Set_FLD_is_bit_field(fld);
    }
  }

// for vbase
  if (cxx_decl && cxx_decl->hasDefinition()) {
    // for base
    for (const CXXBaseSpecifier &base : cxx_decl->vbases()) {
      const RecordType *base_type = base.getType()->getAs<RecordType>();
      TY_IDX base_ty = ConvertType(base_type);
      const RecordDecl *base_decl = base_type->getDecl();
      //if (base_decl->field_empty() && TY_vtable(base_ty) == (TY_IDX)0)
      if (TY_size(base_ty) == 0)
        continue;
      if (!load_vptr &&
          isa<CXXRecordDecl>(base_decl) &&
          _builder->DeclBuilder().HasVTable(cast<CXXRecordDecl>(base_decl))) {
        FLD_HANDLE fld = New_FLD();
        FLD_HANDLE base_fld = TY_fld(base_ty);
        FLD_Init(fld, FLD_name_idx(base_fld), FLD_type(base_fld), 0);
        load_vptr = true;
        fld_count++;
      }
      Is_True(_fld_map[(const Decl*)&base] == 0 ||
              _fld_map[(const Decl*)&base] == next_field_id,
              ("field ID already set"));
      _fld_map[(const Decl*)&base] = next_field_id;
      next_field_id += _fld_used[base_type] + 1;
      if (TY_align(base_ty) > max_align)
        max_align = TY_align(base_ty);
      fld_align = ty_size % TY_align(base_ty);
      if (fld_align != 0)
        ty_size += TY_align(base_ty) - fld_align;
      FLD_HANDLE fld = New_FLD();
      STR_IDX fld_name = _builder->EnterString("anonymous_field");
      FLD_Init(fld, fld_name, base_ty, ty_size);
      Set_FLD_is_anonymous(fld);
      Set_FLD_is_base_class(fld);
      ty_size += TY_size(base_ty);
      if (TY_size(base_ty) > max_size)
        max_size = TY_size(base_ty);
      fld_count++;
    }
  }

  _fld_used[type] = next_field_id - 1;
  if (max_align != 0)
    fld_align = ty_size % max_align;
  if (fld_align != 0)
    ty_size += max_align - fld_align;
  if (type->isUnionType())
    Set_TY_size(pty, max_size);
  else
    Set_TY_size(pty, ty_size);
  if (fld_count != 0) {
    Set_TY_align(ty_idx, max_align);
    FLD_IDX last_field_idx = Fld_Table.Size() - 1;
    if (last_field_idx >= first_field_idx) {
      Set_TY_fld(pty, FLD_HANDLE (first_field_idx));
      Set_FLD_last_field(FLD_HANDLE(last_field_idx));
    }
  }
  
  if (TY_is_incomplete(ty_idx))
    Clear_TY_is_incomplete(ty_idx);
  
  return ty_idx;
#endif
}

// RTTI
// Gen mangled RTTI name: return STR_IDX from given type
STR_IDX
WhirlTypeBuilder::GetMangledRTTIName(const QualType type) {
  SmallString<64> mangled_name;
  llvm::raw_svector_ostream out(mangled_name);
  _builder->MangleContext()->mangleCXXRTTI(type.getUnqualifiedType(), out);
  return _builder->EnterString(out.str());
}

// get rtti st from _rtti_st_map by given type
ST_IDX
WhirlTypeBuilder::GetRTTIForType(const Type * type) const {
  RTTI_ST_MAP::const_iterator it = _rtti_st_map.find(type);
  if (it != _rtti_st_map.end())
    return it->second;
  return (ST_IDX)0;
}

/// Type_info_is_in_standard_library - Given a builtin type, returns whether the type
/// info for that type is defined in the standard library.
static bool
Type_info_is_in_standard_library(const BuiltinType *type) {
  // Itanium C++ ABI 2.9.2:
  //   Basic type information (e.g. for "int", "bool", etc.) will be kept in
  //   the run-time support library. Specifically, the run-time support
  //   library should contain type_info objects for the types X, X* and
  //   X const*, for every X in: void, std::nullptr_t, bool, wchar_t, char,
  //   unsigned char, signed char, short, unsigned short, int, unsigned int,
  //   long, unsigned long, long long, unsigned long long, float, double,
  //   long double, char16_t, char32_t, and the IEEE 754r decimal and
  //   half-precision floating point types.
  //
  // GCC also emits RTTI for __int128.
  switch (type->getKind()) {
    case BuiltinType::Void:
    case BuiltinType::NullPtr:
    case BuiltinType::Bool:
    case BuiltinType::WChar_S:
    case BuiltinType::WChar_U:
    case BuiltinType::Char_U:
    case BuiltinType::Char_S:
    case BuiltinType::UChar:
    case BuiltinType::SChar:
    case BuiltinType::Short:
    case BuiltinType::UShort:
    case BuiltinType::Int:
    case BuiltinType::UInt:
    case BuiltinType::Long:
    case BuiltinType::ULong:
    case BuiltinType::LongLong:
    case BuiltinType::ULongLong:
    case BuiltinType::Half:
    case BuiltinType::Float:
    case BuiltinType::Double:
    case BuiltinType::LongDouble:
    case BuiltinType::Float16:
    case BuiltinType::Float128:
    case BuiltinType::Char8:
    case BuiltinType::Char16:
    case BuiltinType::Char32:
    case BuiltinType::Int128:
    case BuiltinType::UInt128:
      return true;

#define IMAGE_TYPE(ImgType, Id, SingletonId, Access, Suffix) \
    case BuiltinType::Id:
#include "clang/Basic/OpenCLImageTypes.def"
    case BuiltinType::OCLSampler:
    case BuiltinType::OCLEvent:
    case BuiltinType::OCLClkEvent:
    case BuiltinType::OCLQueue:
    case BuiltinType::OCLReserveID:
    case BuiltinType::ShortAccum:
    case BuiltinType::Accum:
    case BuiltinType::LongAccum:
    case BuiltinType::UShortAccum:
    case BuiltinType::UAccum:
    case BuiltinType::ULongAccum:
    case BuiltinType::ShortFract:
    case BuiltinType::Fract:
    case BuiltinType::LongFract:
    case BuiltinType::UShortFract:
    case BuiltinType::UFract:
    case BuiltinType::ULongFract:
    case BuiltinType::SatShortAccum:
    case BuiltinType::SatAccum:
    case BuiltinType::SatLongAccum:
    case BuiltinType::SatUShortAccum:
    case BuiltinType::SatUAccum:
    case BuiltinType::SatULongAccum:
    case BuiltinType::SatShortFract:
    case BuiltinType::SatFract:
    case BuiltinType::SatLongFract:
    case BuiltinType::SatUShortFract:
    case BuiltinType::SatUFract:
    case BuiltinType::SatULongFract:
      return false;

    case BuiltinType::Dependent:
#define BUILTIN_TYPE(Id, SingletonId)
#define PLACEHOLDER_TYPE(Id, SingletonId) \
    case BuiltinType::Id:
#include "clang/AST/BuiltinTypes.def"
      Is_True(false, ("asking for RRTI for a placeholder type!"));

    case BuiltinType::ObjCId:
    case BuiltinType::ObjCClass:
    case BuiltinType::ObjCSel:
      Is_True(false, ("Objective-C types are unsupported!"));

    default:
      Is_True(false, ("Unsupported builtin type: %d", type->getKind()));
  }
}

static bool
Type_info_is_in_standard_library(const PointerType *pointer_ty) {
  QualType pointee_ty = pointer_ty->getPointeeType();
  const BuiltinType *builtin_ty = dyn_cast<BuiltinType>(pointee_ty);
  if (!builtin_ty)
    return false;

  // Check the qualifiers.
  Qualifiers quals = pointee_ty.getQualifiers();
  quals.removeConst();

  if (!quals.empty())
    return false;

  return Type_info_is_in_standard_library(builtin_ty);
}

/// Is_standard_library_rtti_descriptor - Returns whether the type
/// information for the given type exists in the standard library.
static bool
Is_standard_library_rtti_descriptor(QualType type) {
  // Type info for builtin types is defined in the standard library.
  if (const BuiltinType *builtin_ty = dyn_cast<BuiltinType>(type))
    return Type_info_is_in_standard_library(builtin_ty);

  // Type info for some pointer types to builtin types is defined in the
  // standard library.
  if (const PointerType *pointer_ty = dyn_cast<PointerType>(type))
    return Type_info_is_in_standard_library(pointer_ty);

  return false;
}

/// Should_use_external_rtti_descriptor - Returns whether the type information for
/// the given type exists somewhere else, and that we should not emit the type
/// information in this translation unit.  Assumes that it is not a
/// standard-library type.
static bool
Should_use_external_rtti_descriptor(ASTContext &context, QualType type) {

  // If RTTI is disabled, assume it might be disabled in the
  // translation unit that defines any potential key function, too.
  if (!context.getLangOpts().RTTI) return false;

  if (const RecordType *record_ty = dyn_cast<RecordType>(type)) {
    const CXXRecordDecl *rd = cast<CXXRecordDecl>(record_ty->getDecl());
    if (!rd->hasDefinition())
      return false;

    if (!rd->isDynamicClass())
      return false;

    // FIXME: this may need to be reconsidered if the key function
    // changes.
    // N.B. We must always emit the RTTI data ourselves if there exists a key
    // function.
    bool is_dll_import = rd->hasAttr<DLLImportAttr>();

    if (is_dll_import)
      return true;
  }

  return false;
}

// TY_IDXs for type info pseudo type
static TY_IDX type_info_pseudo = TY_IDX_ZERO;
static TY_IDX std_type_info = TY_IDX_ZERO;
static TY_IDX base_class_type_info_pseudo = TY_IDX_ZERO;
#define VMI_TI_PSEUDO_PREFIX   "__vmi_class_type_info_pseudo"

// generate struct ty: __type_info_pseudo
TY_IDX
GetTypeInfoPseudoTy() {
  if (type_info_pseudo)
    return type_info_pseudo;

  TY_IDX ty_idx;
  int first_field_idx = Fld_Table.Size();
  TY &pty = New_TY(ty_idx);
  TY_Init(pty, 0, KIND_STRUCT, MTYPE_M,
          Save_Str("__type_info_pseudo"));
  // first field
  FLD_HANDLE fld = New_FLD();
  TY_IDX ptr1 = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
  FLD_Init(fld, Save_Str(".anonymous"), ptr1, 0);
  Set_FLD_is_anonymous(fld);
  // second field
  fld = New_FLD();
  TY_IDX ptr2 = Make_Pointer_Type(MTYPE_To_TY(MTYPE_I1));
  FLD_Init(fld, Save_Str(".anonymous"), ptr2, TY_size(ptr1));
  Set_FLD_is_anonymous(fld);
  Set_TY_size(pty, TY_size(ptr1) + TY_size(ptr2));
  Set_TY_align(ty_idx, TY_align(ptr1));
  Set_TY_fld(pty, FLD_HANDLE(first_field_idx));
  Set_FLD_last_field(FLD_HANDLE(Fld_Table.Size() - 1));

  type_info_pseudo = ty_idx;
  return ty_idx;
}

// gererate struct ty: std::type_info
TY_IDX
GetTypeInfoTy() {
  if (std_type_info)
    return std_type_info;

  TY_IDX ty_idx;
  TY &ptr_ty = New_TY(ty_idx);
  TY_Init(ptr_ty, 0, KIND_STRUCT, MTYPE_M,
          Save_Str("std::type_info"));
  std_type_info = ty_idx;
  return std_type_info;
}

// generate struct ty: __base_class_type_info_pseudo
TY_IDX
GetBaseClassTypeInfoPseudoTy() {
  if (base_class_type_info_pseudo)
    return base_class_type_info_pseudo;

  TY_IDX pseudo_idx;
  int first_field_idx = Fld_Table.Size();
  TY &pty = New_TY(pseudo_idx);
  TY_Init(pty, 0, KIND_STRUCT, MTYPE_M,
          Save_Str("__base_class_type_info_pseudo"));
  // first field
  FLD_HANDLE fld = New_FLD();
  TY_IDX ptr1 = Make_Pointer_Type(GetTypeInfoTy());
  FLD_Init(fld, Save_Str(".anonymous"), ptr1, 0);
  Set_FLD_is_anonymous(fld);
  // second field
  fld = New_FLD();
  TY_IDX ptr2 = Make_Pointer_Type(MTYPE_To_TY(MTYPE_I8));
  FLD_Init(fld, Save_Str(".anonymous"), ptr2, TY_size(ptr1));
  Set_FLD_is_anonymous(fld);
  Set_TY_size(pty, TY_size(ptr1) + TY_size(ptr2));
  Set_TY_align(pseudo_idx, TY_align(ptr1));
  Set_TY_fld(pty, FLD_HANDLE(first_field_idx));
  Set_FLD_last_field(FLD_HANDLE(Fld_Table.Size() - 1));

  base_class_type_info_pseudo = pseudo_idx;
  return pseudo_idx;
}

// generate array ty whose size is number of base
// get elem type from GetBaseClassTypeInfoPseudoTy()
TY_IDX
GetBaseArrayTy(int num_base) {
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  TY_IDX elem_ty = GetBaseClassTypeInfoPseudoTy();
  TY_Init(ty, num_base * TY_size(elem_ty),
          KIND_ARRAY, MTYPE_M, Save_Str(".anonymous"));
  Set_TY_etype(ty, elem_ty);
  Set_TY_align(ty_idx, TY_align(elem_ty));
  Set_TY_anonymous(ty);
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, 0, 0, 0);
  Set_TY_arb(ty, arb);
  Set_ARB_first_dimen(arb);
  Set_ARB_last_dimen(arb);
  Set_ARB_dimension(arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_lbnd_val(arb, 0);
  Set_ARB_ubnd_val(arb, num_base - 1);
  return ty_idx;
}

// Generate __type_info_pseudo ty
TY_IDX
WhirlTypeBuilder::EmitTypeInfoPseudoType(STR_IDX name) {
  // generate base type info ty
  BOOL is_vmi = FALSE;
  BOOL is_si = FALSE;
  TY_IDX base_ti_ty;
  if (!strncmp(Index_To_Str(name), VMI_TI_PSEUDO_PREFIX, strlen(VMI_TI_PSEUDO_PREFIX))) {
    int num_base = atoi(Index_To_Str(name) + strlen(VMI_TI_PSEUDO_PREFIX));
    base_ti_ty = GetBaseArrayTy(num_base);
    is_vmi = TRUE;
  } else if (!strcmp(Index_To_Str(name), "__si_class_type_info_pseudo")) {
    base_ti_ty = Make_Pointer_Type(GetTypeInfoTy());
    is_si = TRUE;
  }

  int ty_size = 0;
  TY_IDX ty_idx;
  TY &ptr_ty = New_TY(ty_idx);
  TY_Init(ptr_ty, 0, KIND_STRUCT, MTYPE_M, name);
  // first field (__type_info_pseudo)
  TY_IDX pseudo_idx = GetTypeInfoPseudoTy();
  int first_field_idx = Fld_Table.Size();
  FLD_HANDLE fld = New_FLD();
  FLD_Init(fld, Save_Str(".anonymous"), pseudo_idx, 0);
  Set_FLD_is_anonymous(fld);
  ty_size += TY_size(pseudo_idx);

  if (is_si) {
    fld = New_FLD();
    FLD_Init(fld, Save_Str(".anonymous"),
             Make_Pointer_Type(GetTypeInfoTy()), ty_size);
    Set_FLD_is_anonymous(fld);
    ty_size += MTYPE_byte_size(Pointer_Mtype);
  } else if (is_vmi) {
    fld = New_FLD();
    FLD_Init(fld, Save_Str(".anonymous"), MTYPE_To_TY(MTYPE_I4), ty_size);
    Set_FLD_is_anonymous(fld);
    ty_size += MTYPE_byte_size(MTYPE_I4);

    fld = New_FLD();
    FLD_Init(fld, Save_Str(".anonymous"), MTYPE_To_TY(MTYPE_I4), ty_size);
    Set_FLD_is_anonymous(fld);
    ty_size += MTYPE_byte_size(MTYPE_I4);

    fld = New_FLD();
    FLD_Init(fld, Save_Str(".anonymous"), base_ti_ty, ty_size);
    Set_FLD_is_anonymous(fld);
    ty_size += TY_size(base_ti_ty);
  }

  Set_TY_size(ptr_ty, ty_size);
  Set_TY_align(ty_idx, TY_align(pseudo_idx));
  Set_TY_fld(ptr_ty, FLD_HANDLE(first_field_idx));
  Set_FLD_last_field(FLD_HANDLE(Fld_Table.Size() - 1));
  return ty_idx;
}

// Generate class type info pseudo ty from given type name
TY_IDX
WhirlTypeBuilder::GetClassTypeInfoPseudo(STR_IDX type_name) {
  RTTI_TY_MAP::const_iterator it = _rtti_ty_map.find(type_name);
  if (it != _rtti_ty_map.end())
    return it->second;

  TY_IDX ty_idx = EmitTypeInfoPseudoType(type_name);
  _rtti_ty_map[type_name] = ty_idx;
  return ty_idx;
}

// returns the constant for the RTTI descriptor of the given type.
ST_IDX
WhirlTypeBuilder::GetExternalRTTIForType(QualType qtype, const Type *type) {
  // Mangle the RTTI name.
  STR_IDX type_name = GetMangledRTTIName(qtype);

  TY_IDX _class_type_info_pseudo =
    GetClassTypeInfoPseudo(Save_Str("__fundamental_type_info_pseudo"));

  ST *st = New_ST(GLOBAL_SYMTAB);
  ST_Init(st, type_name, CLASS_VAR, SCLASS_EXTERN,
          EXPORT_PREEMPTIBLE, _class_type_info_pseudo);
  ST_IDX st_idx = ST_st_idx(st);

  // add to _rtti_st_map
  _rtti_st_map[type] = st_idx;
  return st_idx;
}

// Can_use_single_inheritance - Return whether the given record decl has a "single,
// public, non-virtual base at offset zero (i.e. the derived class is dynamic
// iff the base is)", according to Itanium C++ ABI, 2.95p6b.
static bool
Can_use_single_inheritance(const CXXRecordDecl *rd) {
  // Check the number of bases.
  if (rd->getNumBases() != 1)
    return false;

  // Get the base.
  CXXRecordDecl::base_class_const_iterator base = rd->bases_begin();

  // Check that the base is not virtual.
  if (base->isVirtual())
    return false;

  // Check that the base is public.
  if (base->getAccessSpecifier() != AS_public)
    return false;

  // Check that the class is dynamic iff the base is.
  const CXXRecordDecl *base_decl =
    cast<CXXRecordDecl>(base->getType()->getAs<RecordType>()->getDecl());
  if (!base_decl->isEmpty() &&
      base_decl->isDynamicClass() != rd->isDynamicClass())
    return false;

  return true;
}

// TODO: refine the code below into a RTTIBuilder
// Build the vtable pointer for the given type
// abi::__class_type_info.
static const char * const class_type_info =
    "_ZTVN10__cxxabiv117__class_type_infoE";
// abi::__si_class_type_info.
static const char * const si_class_type_info =
    "_ZTVN10__cxxabiv120__si_class_type_infoE";
// abi::__vmi_class_type_info.
static const char * const vmi_class_type_info =
    "_ZTVN10__cxxabiv121__vmi_class_type_infoE";
static const char * const si_class_type_info_ty =
    "__si_class_type_info";
static const char * const vmi_class_type_info_ty =
    "__vmi_class_type_info";

ST_IDX
Build_vtable_pointer(const Type *type) {
  const char *vtable_name = nullptr;
  const char *type_info_name = "__class_type_info";

  switch (type->getTypeClass()) {
#define TYPE(Class, Base)
#define ABSTRACT_TYPE(Class, Base)
#define NON_CANONICAL_UNLESS_DEPENDENT_TYPE(Class, Base) case Type::Class:
#define NON_CANONICAL_TYPE(Class, Base) case Type::Class:
#define DEPENDENT_TYPE(Class, Base) case Type::Class:
#if LLVM_VERSION_MAJOR == 11
#include "clang/AST/TypeNodes.inc"
#else
#include "clang/AST/TypeNodes.def"
#endif
    Is_True(false, ("Non-canonical and dependent types shouldn't get here"));

  case Type::LValueReference:
  case Type::RValueReference:
    Is_True(false, ("References shouldn't get here"));

  case Type::Auto:
  case Type::DeducedTemplateSpecialization:
    Is_True(false, ("Undeduced type shouldn't get here"));

  case Type::Pipe:
    Is_True(false, ("Pipe types shouldn't get here"));

  case Type::Builtin:
  // GCC treats vector and complex types as fundamental types.
  case Type::Vector:
  case Type::ExtVector:
  case Type::Complex:
  case Type::Atomic:
  // FIXME: GCC treats block pointers as fundamental types?!
  case Type::BlockPointer:
    // abi::__fundamental_type_info.
    vtable_name = "_ZTVN10__cxxabiv123__fundamental_type_infoE";
    break;

  case Type::ConstantArray:
  case Type::IncompleteArray:
  case Type::VariableArray:
    // abi::__array_type_info.
    vtable_name = "_ZTVN10__cxxabiv117__array_type_infoE";
    break;

  case Type::FunctionNoProto:
  case Type::FunctionProto:
    // abi::__function_type_info.
    vtable_name = "_ZTVN10__cxxabiv120__function_type_infoE";
    break;

  case Type::Enum:
    // abi::__enum_type_info.
    vtable_name = "_ZTVN10__cxxabiv116__enum_type_infoE";
    break;

  case Type::Record: {
    const CXXRecordDecl *rd =
      cast<CXXRecordDecl>(cast<RecordType>(type)->getDecl());

    if (!rd->hasDefinition() || !rd->getNumBases()) {
      vtable_name = class_type_info;
    } else if (Can_use_single_inheritance(rd)) {
      vtable_name = si_class_type_info;
      type_info_name = si_class_type_info_ty;
    } else {
      vtable_name = vmi_class_type_info;
      type_info_name = vmi_class_type_info_ty;
    }

    break;
  }

  case Type::ObjCObject:
    // Ignore protocol qualifiers.
    type = cast<ObjCObjectType>(type)->getBaseType().getTypePtr();

    // Handle id and Class.
    if (isa<BuiltinType>(type)) {
      vtable_name = class_type_info;
      break;
    }

    assert(isa<ObjCInterfaceType>(type));
    // Fall through.

  case Type::ObjCInterface:
    if (cast<ObjCInterfaceType>(type)->getDecl()->getSuperClass()) {
      vtable_name = si_class_type_info;
      type_info_name = si_class_type_info_ty;
    } else {
      vtable_name = class_type_info;
    }
    break;

  case Type::ObjCObjectPointer:
  case Type::Pointer:
    // abi::__pointer_type_info.
    vtable_name = "_ZTVN10__cxxabiv119__pointer_type_infoE";
    break;

  case Type::MemberPointer:
    // abi::__pointer_to_member_type_info.
    vtable_name = "_ZTVN10__cxxabiv129__pointer_to_member_type_infoE";
    break;
  default:
    Is_True(false,
            ("Unsupported type class: %s", type->getTypeClassName()));
  }

  // TY for __class_type_info
  TY_IDX class_ty_info;
  TY &class_ty = New_TY(class_ty_info);
  TY_Init(class_ty, 0, KIND_STRUCT, MTYPE_M, Save_Str(type_info_name));

  // TY for VTable (an array type whose elem type is ptr of pu type (return I4))
  // pu type return MTYPE_I4
  TY_IDX pu_idx;
  TY &func = New_TY(pu_idx);
  TY_Init(func, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align(pu_idx, 1);
  TYLIST tylist_idx;
  TY_IDX rty = MTYPE_To_TY(MTYPE_I4);
  Set_TYLIST_type(New_TYLIST(tylist_idx), rty);
  Set_TY_tylist(pu_idx, tylist_idx);
  Set_TYLIST_type(New_TYLIST(tylist_idx), 0);

  // array type
  TY_IDX ptr_ty = Make_Pointer_Type(pu_idx);
  TY_IDX arr_ty_idx;
  TY &arr_ty = New_TY(arr_ty_idx);
  TY_Init(arr_ty, TY_size(ptr_ty), KIND_ARRAY, MTYPE_M, (STR_IDX) 0);
  Set_TY_etype(arr_ty, ptr_ty);
  Set_TY_align(arr_ty_idx, TY_align(ptr_ty));
  Set_TY_anonymous(arr_ty);
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, 0, 0, 0);
  Set_TY_arb(arr_ty, arb);
  Set_ARB_first_dimen(arb);
  Set_ARB_last_dimen(arb);
  Set_ARB_dimension(arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_lbnd_val(arb, 0);
  Set_ARB_ubnd_val(arb, TY_size(ptr_ty));

  // vtable
  ST *rtti_vt = New_ST(GLOBAL_SYMTAB);
  ST_Init(rtti_vt, Save_Str(vtable_name), CLASS_VAR, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, pu_idx);
  Set_ST_is_vtable(rtti_vt);
  Set_ST_vtable_ty_idx(rtti_vt, class_ty_info);
  return ST_st_idx(rtti_vt);
}

ST_IDX
WhirlTypeBuilder::GetSTForRTTIName(QualType qtype) {
  SmallString<256> mangled_name;
  llvm::raw_svector_ostream Out(mangled_name);
  _builder->MangleContext()->mangleCXXRTTIName(qtype, Out);
  STR_IDX type_name = _builder->EnterString(Out.str());

  int index = 4;
  int len = Out.str().size() - index  + 1;
  TCON tcon = Host_To_Targ_String(MTYPE_STRING, mangled_name.substr(index).str().c_str(), len);
  TCON_IDX tcon_idx = Enter_tcon(tcon);
  INITV_IDX cur = New_INITV();
  INITV_Set_VAL(Initv_Table[cur], tcon_idx, 1);

  // create const array ty whose size is len
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  TY_IDX elem_ty = MTYPE_To_TY(MTYPE_I1);
  TY_Init(ty, len, KIND_ARRAY, MTYPE_M, (STR_IDX) 0);
  Set_TY_etype(ty, elem_ty);
  Set_TY_align(ty_idx, TY_align(elem_ty));
  Set_TY_anonymous(ty);
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, 0, 0, 0);
  Set_TY_arb(ty, arb);
  Set_ARB_first_dimen(arb);
  Set_ARB_last_dimen(arb);
  Set_ARB_dimension(arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_lbnd_val(arb, 0);
  Set_ARB_ubnd_val(arb, len - 1);

  ST *type_name_st = New_ST(GLOBAL_SYMTAB);
  ST_Init (type_name_st, type_name, CLASS_VAR, SCLASS_DGLOBAL, EXPORT_PREEMPTIBLE, ty_idx);
  Set_ST_is_initialized(type_name_st);
  New_INITO(type_name_st, cur);
  Set_ST_is_weak_symbol(type_name_st);

  return ST_st_idx(type_name_st);
}

struct BaseInfo {
  typedef hash_set<const CXXRecordDecl *, wgen_ptr_hash> DECL_SET;
  DECL_SET _virt_bases;
  DECL_SET _non_virt_bases;
  template<BOOL virt>
  BOOL Has(const CXXRecordDecl *decl) {
    DECL_SET &set = virt ? _virt_bases : _non_virt_bases;
    return set.find(decl) != set.end();
  }
  template<BOOL virt>
  BOOL Add(const CXXRecordDecl *decl) {
    DECL_SET &set = virt ? _virt_bases : _non_virt_bases;
    return set.insert(decl).second == true;
  }
};

enum {
  VMI_DiamondShaped    = 0x1,
  VMI_NonDiamondRepeat = 0x2,
};

enum {
  BCTI_Virtual         = 0x1,
  BCTI_Public          = 0x2,
};

static UINT
ComputeVMIRIITFlags(const CXXBaseSpecifier *base, BaseInfo& info) {
  UINT flag = 0;

  // check the decl
  const CXXRecordDecl *decl = base->getType()->getAsCXXRecordDecl();
  Is_True(decl != NULL, ("null base decl"));
  if (base->isVirtual()) {
    if (!info.Add<TRUE>(decl))
      // virtual base seen in virtual base
      flag |= VMI_DiamondShaped;
    else if (info.Has<FALSE>(decl))
      // virtual base seen in non-virtual base
      flag |= VMI_NonDiamondRepeat;
  }
  else {
    if (!info.Add<FALSE>(decl))
      // non-virtual base seen in non-virtual base
      flag |= VMI_NonDiamondRepeat;
    else if (info.Has<TRUE>(decl))
      // non-virtual base seen in non-virtual base
      flag |= VMI_NonDiamondRepeat;
  }

  // check all bases
  for (const CXXBaseSpecifier &spec : decl->bases())
    flag |= ComputeVMIRIITFlags(&spec, info);

  return flag;
}

static UINT
ComputeVMIRIITFlags(const CXXRecordDecl *decl) {
  UINT flag = 0;
  BaseInfo baseinfo;
  for (const CXXBaseSpecifier &base : decl->bases())
    flag |= ComputeVMIRIITFlags(&base, baseinfo);

  return flag;
}

// EmitRTTIForVMIClassType
// Build an abi::__vmi_class_type_info, used for
// classes with bases that do not satisfy the abi::__si_class_type_info
// constraints, according ti the Itanium C++ ABI, 2.9.5p5c.
void
WhirlTypeBuilder::EmitRTTIForVMIClassType(const CXXRecordDecl *decl, INITV_IDX ti_blk) {
  Is_True(decl != NULL, ("CXXRecordDecl is NULL"));
  Is_True(ti_blk != INITV_IDX_ZERO, ("null initv block"));

  UINT num_base = decl->getNumBases();
  if (num_base) {
    UINT flag = ComputeVMIRIITFlags(decl);
    INITV_IDX flag_inv = New_INITV();
    INITV_Init_Integer(flag_inv, MTYPE_U4, flag);
    Set_INITV_next(ti_blk, flag_inv);
    INITV_IDX nbase_inv = New_INITV();
    INITV_Init_Integer(nbase_inv, MTYPE_U4, num_base);
    Set_INITV_next(flag_inv, nbase_inv);
    INITV_IDX bases_blk = New_INITV();
    INITV_Init_Block(bases_blk, INITV_Next_Idx());
    Set_INITV_next(nbase_inv, bases_blk);

    // get record layout
    const ASTRecordLayout &layout = _builder->Context()->getASTRecordLayout(decl);
    INITV_IDX last_inv = 0;
    for (const CXXBaseSpecifier &spec : decl->bases()) {
      INITV_IDX base_blk = New_INITV();
      if (last_inv != 0)
        Set_INITV_next(last_inv, base_blk);
      last_inv = base_blk;
      ST_IDX base_rtti_st = ConvertRTTIForType(spec.getType());
      INITV_Init_Block(base_blk, INITV_Next_Idx());
      INITV_IDX rtti_inv = New_INITV();
      INITV_Init_Symoff(rtti_inv, ST_ptr(base_rtti_st), 0);
      const CXXRecordDecl* base_decl = spec.getType()->getAsCXXRecordDecl();
      Is_True(base_decl != NULL, ("base decl is NULL"));
      UINT64 ofst;
      if (spec.isVirtual()) {
        ItaniumVTableContext *ctx =
          cast<ItaniumVTableContext>(_builder->Context()->getVTableContext());
        ofst = ctx->getVirtualBaseOffsetOffset(decl, base_decl).getQuantity();
      }
      else {
        ofst = layout.getBaseClassOffset(base_decl).getQuantity();
      }
      UINT64 base_flag = ofst << 8;
      if (spec.isVirtual())
        base_flag |= BCTI_Virtual;
      if (spec.getAccessSpecifier() == AS_public)
        base_flag |= BCTI_Public;
      INITV_IDX bflag_inv = New_INITV();
      INITV_Init_Integer(bflag_inv, MTYPE_U8, base_flag);
      Set_INITV_next(rtti_inv, bflag_inv);
    }
  }
}

// Pointer type info flags.
enum {
  PTI_Const                     = 0x01, // const qualifier
  PTI_Volatile                  = 0x02, // volatile qualifier
  PTI_Restrict                  = 0x04, // restrict qualifier
  PTI_Incomplete                = 0x08, // incomplete
  PTI_ContainingClassIncomplete = 0x10, // containing class is incomplete
  PTI_TransactionSafe           = 0x20, // Pointee is transaction
                                        // safe function (C++ TM TS)
  PTI_Noexcept                  = 0x40, // Pointee is noexcept function (C++1z)
};

// Contains_incomplete_class_type
// Returns whether the given type contains an
// incomplete class type. This is true if
//
//   * The given type is an incomplete class type.
//   * The given type is a pointer type whose pointee type contains an
//     incomplete class type.
//   * The given type is a member pointer type whose class is an incomplete
//     class type.
//   * The given type is a member pointer type whoise pointee type contains an
//     incomplete class type.
// is an indirect or direct pointer to an incomplete class type.
static bool Contains_incomplete_class_type(QualType qtype) {
  if (const RecordType *record_ty = dyn_cast<RecordType>(qtype)) {
    if (!record_ty->getDecl()->isCompleteDefinition())
      return true;
  }

  if (const PointerType *pointer_ty = dyn_cast<PointerType>(qtype))
    return Contains_incomplete_class_type(pointer_ty->getPointeeType());

  if (const MemberPointerType *member_ptr_ty=
      dyn_cast<MemberPointerType>(qtype)) {
    // Check if the class type is incomplete.
    const RecordType *class_type= cast<RecordType>(member_ptr_ty->getClass());
    if (!class_type->getDecl()->isCompleteDefinition())
      return true;

    return Contains_incomplete_class_type(member_ptr_ty->getPointeeType());
  }

  return false;
}

// Compute the flags for a __pbase_type_info, and remove the corresponding
// pieces from \p Type.
static unsigned
Extract_pbase_flags(ASTContext *ctx, QualType &type) {
  unsigned Flags = 0;

  if (type.isConstQualified())
    Flags |= PTI_Const;
  if (type.isVolatileQualified())
    Flags |= PTI_Volatile;
  if (type.isRestrictQualified())
    Flags |= PTI_Restrict;
  type = type.getUnqualifiedType();

  // Itanium C++ ABI 2.9.5p7:
  //   When the abi::__pbase_type_info is for a direct or indirect pointer to an
  //   incomplete class type, the incomplete target type flag is set.
  if (Contains_incomplete_class_type(type))
    Flags |= PTI_Incomplete;

  if (auto *Proto = type->getAs<FunctionProtoType>()) {
    if (Proto->isNothrow()) {
      Flags |= PTI_Noexcept;
      type = ctx->getFunctionTypeWithExceptionSpec(type, EST_None);
    }
  }

  return Flags;
}

// EmitRTTIForMemberPointerType
// Build an abi::__pointer_to_member_type_info
// struct, used for member pointer types.
void
WhirlTypeBuilder::EmitRTTIForMemberPointerType(const MemberPointerType *ty, INITV_IDX ti_blk) {
  Is_True(ti_blk != INITV_IDX_ZERO, ("null initv block"));

  QualType pointee_ty = ty->getPointeeType();

  // Itanium C++ ABI 2.9.5p7:
  // __flags is a flag word describing the cv-qualification and other
  // attributes of the type pointed to.
  unsigned flags = Extract_pbase_flags(_builder->Context(), pointee_ty);
  const RecordType *ClassType = cast<RecordType>(ty->getClass());
  if (!ClassType->getDecl()->isCompleteDefinition())
    flags |= PTI_ContainingClassIncomplete;

  INITV_IDX flag_inv = New_INITV();
  INITV_Init_Integer(flag_inv, MTYPE_U4, flags);
  Set_INITV_next(ti_blk, flag_inv);

  INITV_IDX inv = New_INITV();
  INITV_Init_Pad(inv, MTYPE_byte_size(MTYPE_U4));
  Set_INITV_next(flag_inv, inv);

  // Itanium C++ ABI 2.9.5p7:
  // __pointee is a pointer to the std::type_info derivation for the
  // unqualified type being pointed to.
  ST_IDX pointee_ty_st = ConvertRTTIForType(pointee_ty);
  INITV_IDX pointee_inv = New_INITV();
  INITV_Init_Symoff(pointee_inv, ST_ptr(pointee_ty_st), 0);
  Set_INITV_next(inv, pointee_inv);

  // Itanium C++ ABI 2.9.5p9:
  // __context is a pointer to an abi::__class_type_info corresponding to the
  // class type containing the member pointed to
  // (e.g., the "A" in "int A::*").
  ST_IDX record_st = ConvertRTTIForType(QualType(ClassType, 0));
  INITV_IDX record_inv = New_INITV();
  INITV_Init_Symoff(record_inv, ST_ptr(record_st), 0);
  Set_INITV_next(pointee_inv, record_inv);
}

ST_IDX
WhirlTypeBuilder::ConvertRTTIForType(QualType qtype) {

  // check if we've already emitted an RTTI descriptor for this type
  if (qtype->getTypeClass() == Type::Elaborated)
    qtype = cast<ElaboratedType>(qtype)->getNamedType();

  if (qtype->isLValueReferenceType())
    qtype = qtype.getNonReferenceType();

  const Type * type = _builder->Context()->getCanonicalType(qtype).getTypePtr();
  ST_IDX ty = _builder->TB().ConvertType(type);

  ST_IDX st_idx = GetRTTIForType(type);
  if (st_idx)
    return st_idx;

  // check if there is already an external RTTI descriptor for this type.
  if (Is_standard_library_rtti_descriptor(qtype) ||
      Should_use_external_rtti_descriptor((*(_builder->Context())), qtype))
    return GetExternalRTTIForType(qtype, type);

  // operate on the canonical type
  qtype = qtype.getCanonicalType();

  // Add the vtable pointer
  ST_IDX rtti_vt = Build_vtable_pointer(cast<Type>(qtype));

  // get addr of type name
  ST_IDX type_name = GetSTForRTTIName(qtype);

  // type info name
  STR_IDX type_info_name = Save_Str("__class_type_info_pseudo");

  // Generate INITO
  INITV_IDX inv_blk = New_INITV();
  INITV_Init_Block(inv_blk, INITV_Next_Idx());
  // block for type_info
  INITV_IDX ti_blk = New_INITV();
  INITV_Init_Block(ti_blk, INITV_Next_Idx());
  INITV_IDX vtable_inv = New_INITV();
  INITV_Init_Symoff(vtable_inv, ST_ptr(rtti_vt), 16);
  INITV_IDX type_name_initv = New_INITV();
  INITV_Init_Symoff(type_name_initv, ST_ptr(type_name), 0);
  Set_INITV_next(vtable_inv, type_name_initv);

  switch (qtype->getTypeClass()) {
#define TYPE(Class, Base)
#define ABSTRACT_TYPE(Class, Base)
#define NON_CANONICAL_UNLESS_DEPENDENT_TYPE(Class, Base) case Type::Class:
#define NON_CANONICAL_TYPE(Class, Base) case Type::Class:
#define DEPENDENT_TYPE(Class, Base) case Type::Class:
#if LLVM_VERSION_MAJOR == 11
#include "clang/AST/TypeNodes.inc"
#else
#include "clang/AST/TypeNodes.def"
#endif
    Is_True(false, ("Non-canonical and dependent types shouldn't get here"));

  case Type::Builtin:
  case Type::Vector:
  case Type::ExtVector:
  case Type::Complex:
  case Type::BlockPointer:
    // Itanium C++ ABI 2.9.5p4:
    // abi::__fundamental_type_info adds no data members to std::type_info.
    break;

  case Type::LValueReference:
  case Type::RValueReference:
    llvm_unreachable("References shouldn't get here");

  case Type::Auto:
  case Type::DeducedTemplateSpecialization:
    llvm_unreachable("Undeduced type shouldn't get here");

  case Type::Pipe:
    llvm_unreachable("Pipe type shouldn't get here");

  case Type::ConstantArray:
  case Type::IncompleteArray:
  case Type::VariableArray:
    // Itanium C++ ABI 2.9.5p5:
    // abi::__array_type_info adds no data members to std::type_info.
    break;

  case Type::FunctionNoProto:
  case Type::FunctionProto:
    // Itanium C++ ABI 2.9.5p5:
    // abi::__function_type_info adds no data members to std::type_info.
    type_info_name = Save_Str("__function_type_info_pseudo");
    break;

  case Type::Enum:
    // Itanium C++ ABI 2.9.5p5:
    // abi::__enum_type_info adds no data members to std::type_info.
    break;

  case Type::Record: {
    const CXXRecordDecl *record =
      cast<CXXRecordDecl>(cast<RecordType>(qtype)->getDecl());
    if (!record->hasDefinition() || !record->getNumBases()) {
      // We don't need to emit any fields.
      break;
    }

    if (Can_use_single_inheritance(record)) {
      Is_True(!strcmp(ST_name(rtti_vt), si_class_type_info),
              ("ST_name should be si_class_type_info"));
      ST_IDX base_rtti_st =
        ConvertRTTIForType(record->bases_begin()->getType());
      INITV_IDX rtti_inv = New_INITV();
      INITV_Init_Symoff(rtti_inv, ST_ptr(base_rtti_st), 0);
      Set_INITV_next(ti_blk, rtti_inv);
      type_info_name = Save_Str("__si_class_type_info_pseudo");
    } else {
      // block for vmi class
      Is_True(!strcmp(ST_name(rtti_vt), vmi_class_type_info),
              ("ST_name should be vmi_class_type_info"));
      EmitRTTIForVMIClassType(record, ti_blk);
      type_info_name = Save_Str2i(VMI_TI_PSEUDO_PREFIX, "", record->getNumBases());
    }
    break;
  }

  case Type::ObjCObject:
  case Type::ObjCInterface:
    Is_True(false, ("Objective-C types are unsupported!"));
    break;

  case Type::ObjCObjectPointer:
    ConvertRTTIForType(cast<ObjCObjectPointerType>(qtype)->getPointeeType());
    break;

  case Type::Pointer: {
    QualType pointee_ty = cast<PointerType>(qtype)->getPointeeType();

    unsigned flags = Extract_pbase_flags(_builder->Context(), pointee_ty);
    INITV_IDX flag_inv = New_INITV();
    INITV_Init_Integer(flag_inv, MTYPE_U4, flags);
    Set_INITV_next(ti_blk, flag_inv);

    INITV_IDX inv = New_INITV();
    INITV_Init_Pad(inv, MTYPE_byte_size(MTYPE_U4));
    Set_INITV_next(flag_inv, inv);

    ST_IDX pointee_ty_st = ConvertRTTIForType(pointee_ty);
    INITV_IDX pointee_inv = New_INITV();
    INITV_Init_Symoff(pointee_inv, ST_ptr(pointee_ty_st), 0);
    Set_INITV_next(inv, pointee_inv);
    type_info_name = Save_Str("__pointer_type_info_pseudo");
    break;
  }

  case Type::MemberPointer:
    EmitRTTIForMemberPointerType(cast<MemberPointerType>(qtype), ti_blk);
    type_info_name = Save_Str("__pointer_to_member_type_info_pseudo");
    break;

  case Type::Atomic:
    // No fields, at least for the moment.
    break;
  default:
    Is_True(false,
            ("Unsupported type class: %s", type->getTypeClassName()));
}
  // Generate Global RTTI st
  STR_IDX str = GetMangledRTTIName(qtype);
  ST *st = New_ST(GLOBAL_SYMTAB);
  ST_Init(st, str, CLASS_VAR, SCLASS_DGLOBAL, EXPORT_PREEMPTIBLE,
          GetClassTypeInfoPseudo(type_info_name));
  Set_ST_is_initialized(st);
  // set record ty for rtti st
  Set_ST_is_rtti(st);
  Set_ST_vtable_ty_idx(st, ty);
  Set_ST_is_weak_symbol(st);


  New_INITO(st, inv_blk);
  st_idx = ST_st_idx(st);

  // add to _rtti_st_map
  _rtti_st_map[type] = st_idx;
  return st_idx;
}


STR_IDX WhirlTypeBuilder::getTempName() {
  static char buf[64];
  sprintf(buf, ".anonymous.%d", nameIdx++);
  return Save_Str(buf);
}

TY_IDX WhirlTypeBuilder::ConvertVTableType(const CXXRecordDecl *decl) {
  return _vtc.Get(decl);
}

TY_IDX WhirlTypeBuilder::ConvertVTTType(const CXXRecordDecl *decl) {
  return _vtt_tc.Get(decl);
}

TY_IDX
WhirlTypeBuilder::ConvertType(const Type *type, BOOL incomplete, const Type *record_type) {
  TY_IDX_MAP::iterator it = _type_map.find(type);
  if (it != _type_map.end()) {
    TY_IDX ty = it->second;
    // update incomplete record type if incomplete type isn't allowed
    if (!incomplete &&
        type->isRecordType() &&
        !type->isIncompleteType() &&
        TY_is_incomplete(ty))
      ConvertRecordType(cast<RecordType>(type), FALSE, ty);
    return ty;
  }
  TY_IDX ty_idx = (TY_IDX) 0;
  switch (type->getTypeClass()) {
    case Type::Atomic:
      ty_idx = ConvertAtomicType(cast<AtomicType>(type));
      break;
    case Type::BlockPointer:
      ty_idx = ConvertBlockPointerType(cast<BlockPointerType>(type));
      break;
    case Type::Builtin:
      ty_idx = ConvertBuiltinType(cast<BuiltinType>(type));
      break;
    case Type::Complex:
      ty_idx = ConvertComplexType(cast<ComplexType>(type));
      break;
    case Type::ConstantArray:
      ty_idx = ConvertConstantArrayType(cast<ConstantArrayType>(type));
      break;
    case Type::Elaborated:
      ty_idx = ConvertElaboratedType(cast<ElaboratedType>(type));
      break;
    case Type::Enum:
      ty_idx = ConvertEnumType(cast<EnumType>(type));
      break;
    case Type::ExtVector:
    case Type::Vector:
      ty_idx = ConvertVectorType(cast<VectorType>(type));
      break;
    case Type::FunctionNoProto:
    case Type::FunctionProto:
      ty_idx = ConvertFunctionType(cast<FunctionType>(type), record_type);
      break;
    case Type::IncompleteArray:
      ty_idx = ConvertIncompleteArrayType(cast<IncompleteArrayType>(type));
      break;
    case Type::LValueReference:
    case Type::RValueReference:
      ty_idx = ConvertReferenceType(cast<ReferenceType>(type));
      break;
    case Type::MemberPointer:
      ty_idx = ConvertMemberPointerType(cast<MemberPointerType>(type));
      break;
#ifdef C2W_ENABLE_OBJC
    case Type::ObjCObject:
    case Type::ObjCInterface:
    case Type::ObjCObjectPointer:
        break;
#endif
    case Type::Pointer:
      ty_idx = ConvertPointerType(cast<PointerType>(type));
      break;
    case Type::Record:
      ty_idx = ConvertRecordType(cast<RecordType>(type), incomplete, TY_IDX_ZERO);
      break;
    case Type::VariableArray:
      ty_idx = ConvertVariableArrayType(cast<VariableArrayType>(type));
      break;
    case Type::Paren:
      ty_idx = ConvertParenType(cast<ParenType>(type));
      break;
    default:
      Is_True(false,
              ("Unsupported type class: %s", type->getTypeClassName()));
  }
#if 0
  if (T.withConst())
      Set_TY_is_const(ty_idx);
  if (T.withRestrict())
      Set_TY_is_restrict(ty_idx);
  if (T.withVolatile())
      Set_TY_is_volatile(ty_idx);
#endif
  _type_map[type] = ty_idx;

  return ty_idx;
}

TY_IDX WhirlTypeBuilder::ConvertType(QualType qtype, BOOL incomplete) {
  const Type *type = _builder->Context()->getCanonicalType(qtype).getTypePtr();
  TY_IDX ty_idx = ConvertType(type, incomplete);
  DST_INFO_IDX dst = _builder->DstBuilder().CreateDstForType(qtype, ty_idx);
  return ty_idx;
}

FLD_IDX
WhirlTypeBuilder::GetFieldIDFromDecl(const Decl *decl)
{
  FLD_IDX_MAP::iterator it = _fld_map.find(decl);
  if (it != _fld_map.end())
    return it->second;
  return (FLD_IDX)0;
}

} // namespace wgen
