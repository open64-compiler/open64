/*
  Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

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

#ifndef CLANG2WHIRL_TYPE_H
#define CLANG2WHIRL_TYPE_H

// clang forward declarations
#include "clangdecl.h"
// open64 forward declarations
#include "open64decl.h"

#include "c2w_map.h"
#include "c2w_converter.h"

namespace wgen {

class WhirlBuilder;

class VTableTypeConverter : public Converter<VTableTypeConverter, const clang::CXXRecordDecl *, TY_IDX> {
public:
  TY_IDX Convert(const clang::CXXRecordDecl *decl);
  explicit VTableTypeConverter(WhirlBuilder *builder): Converter(builder) {}
};

class VTTTypeConverter : public Converter<VTTTypeConverter, const clang::CXXRecordDecl *, TY_IDX> {
public:
  TY_IDX Convert(const clang::CXXRecordDecl *decl);
  explicit VTTTypeConverter(WhirlBuilder *builder): Converter(builder) {}
};

class WhirlTypeBuilder {
private:
  static UINT32 nameIdx;

private:
  WhirlBuilder *_builder;
  TY_IDX_MAP    _type_map;
  RTTI_ST_MAP   _rtti_st_map;
  VTableTypeConverter _vtc;
  VTTTypeConverter _vtt_tc;
  FLD_IDX_MAP   _fld_map;
  TYPE_FLD_MAP  _fld_used;
  TYPE_ID       _m_uintptr_ty;
  RTTI_TY_MAP   _rtti_ty_map;

public:
  WhirlTypeBuilder(WhirlBuilder *builder);
  
  ~WhirlTypeBuilder();
  
  void Initialize();
  
  void Finalize();
  
  void PU_Init();
  
  void PU_Fini();

private:
  TY_IDX ConvertAtomicType(const clang::AtomicType *type);
  
  TY_IDX ConvertBlockPointerType(const clang::BlockPointerType *type);
  
  TY_IDX ConvertBuiltinType(const clang::BuiltinType *type);
  
  TY_IDX ConvertComplexType(const clang::ComplexType *type);
  
  TY_IDX ConvertConstantArrayType(const clang::ConstantArrayType *type);

  TY_IDX ConvertElaboratedType(const clang::ElaboratedType *type);
  
  TY_IDX ConvertEnumType(const clang::EnumType *type);
  
  TY_IDX ConvertFunctionType(const clang::FunctionType *type, const clang::Type *record_type = NULL);
  
  TY_IDX ConvertIncompleteArrayType(const clang::IncompleteArrayType *type);
  
  TY_IDX ConvertMemberPointerType(const clang::MemberPointerType *type);
  
  TY_IDX ConvertPointerType(const clang::PointerType *type);
  
  TY_IDX ConvertReferenceType(const clang::ReferenceType *type);
  
  TY_IDX ConvertVariableArrayType(const clang::VariableArrayType *type);

  TY_IDX ConvertParenType(const clang::ParenType *type);
  
  TY_IDX ConvertVectorType(const clang::VectorType *type);
  
  TY_IDX ConvertRecordType(const clang::RecordType *type, BOOL imcomplete, TY_IDX ty_idx);
  
  void addTypeToMap(const clang::Type *type, TY_IDX ty_idx) {
    _type_map[type] = ty_idx;
  }

private:
  // RTTI related
  STR_IDX GetMangledRTTIName(const clang::QualType type);

  TY_IDX  EmitTypeInfoPseudoType(STR_IDX name);

  ST_IDX  GetRTTIForType(const clang::Type * type) const;

  ST_IDX  GetExternalRTTIForType(clang::QualType Ty, const clang::Type *type);

  ST_IDX  GetSTForRTTIName(const clang::QualType type);

  void EmitRTTIForMemberPointerType(const clang::MemberPointerType *Ty, INITV_IDX ti_blk);

  void EmitRTTIForVMIClassType(const clang::CXXRecordDecl *decl, INITV_IDX ti_blk);

  TY_IDX GetClassTypeInfoPseudo(STR_IDX type_name);

public:
  ST_IDX  ConvertRTTIForType(const clang::QualType type);

public:
  STR_IDX getTempName();

  TY_IDX ConvertVTableType(const clang::CXXRecordDecl *decl);

  TY_IDX ConvertVTTType(const clang::CXXRecordDecl *decl);

  TY_IDX ConvertType(const clang::Type *type, BOOL incomplete = FALSE, const clang::Type *record_type = NULL);

  TY_IDX ConvertType(clang::QualType qtype, BOOL incomplete = FALSE);

  FLD_IDX GetFieldIDFromDecl(const clang::Decl *decl);

  TYPE_ID GetUIntPtrMType() { return _m_uintptr_ty; }

  TY_IDX EmitGlobalInitializerType();

  TY_IDX EmitCXXPureVirtualType();

  bool NeedFakeParm(clang::QualType type);
};

} // namespace wgen

#endif /* CLANG2WHIRL_TYPE_H */
