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

#ifndef CLANG2WHIRL_DECL_H
#define CLANG2WHIRL_DECL_H

// forward clang declarations
#include "clangdecl.h"

// forward open64 declarations
#include "open64decl.h"
#include "clanginc.h"
#include <string>

// clang2whirl maps
#include "c2w_map.h"

namespace wgen {

class WhirlBuilder;

class WhirlDeclBuilder {
private:
  WhirlBuilder *_builder;

  ST_IDX_MAP _dtor_tcf_map;

  typedef std::list< std::pair<const clang::NamedDecl*, const clang::AliasAttr*> > AliasList;
  AliasList _alias_list;

public:
  WhirlDeclBuilder(WhirlBuilder *builder);

  ~WhirlDeclBuilder();

  void Initialize();

  void Finalize();

private:
  BOOL IsStaticInitialized(const clang::Expr *init_expr, bool &no_need_initialize);

  INITV_IDX ConvertAPValue(clang::QualType type, const clang::APValue *value, INT repeat = 1);

private:
  void ConvertAccessSpec(const clang::AccessSpecDecl *decl);

  void ConvertCaptured(const clang::CapturedDecl *decl);

  void ConvertClassScopeFunctionSpecialization(const clang::ClassScopeFunctionSpecializationDecl *decl);

  void ConvertClassTemplatePartialSpecialization(const clang::ClassTemplatePartialSpecializationDecl *decl);

  void ConvertClassTemplateSpecialization(const clang::ClassTemplateSpecializationDecl *decl);

  void ConvertCXXConversion(const clang::CXXConversionDecl *decl);

  void ConvertCXXMethod(const clang::CXXMethodDecl *decl);

  void ConvertCXXRecord(const clang::CXXRecordDecl *decl);

  void ConvertEnum(const clang::EnumDecl *decl);

  void ConvertEnumConstant(const clang::EnumConstantDecl *decl);

  void ConvertField(const clang::FieldDecl *decl);

  void ConvertFileScopeAsm(const clang::FileScopeAsmDecl *decl);

  void ConvertFriend(const clang::FriendDecl *decl);

  void ConvertFriendTemplate(const clang::FriendTemplateDecl *decl);

  void ConvertImplicitParam(const clang::ImplicitParamDecl *decl);

  void ConvertImport(const clang::ImportDecl *decl);

  void ConvertIndirectField(const clang::IndirectFieldDecl *decl);

  void ConvertLabel(const clang::LabelDecl *decl);

  void ConvertLinkageSpec(const clang::LinkageSpecDecl *decl);

  void ConvertMSProperty(const clang::MSPropertyDecl *decl);

  void ConvertNamespace(const clang::NamespaceDecl *decl);

  void ConvertNamespaceAlias(const clang::NamespaceAliasDecl *decl);

  void ConvertNonTypeTemplateParm(const clang::NonTypeTemplateParmDecl *decl);

#ifdef C2W_ENABLE_OBJC
  void ConvertObjCAtDefsField(const clang::ObjCAtDefsFieldDecl* decl);
  void ConvertObjCCategory(const clang::ObjCCategoryDecl* decl);
  void ConvertObjCCategoryImpl(const clang::ObjCCategoryImplDecl* decl);
  void ConvertObjCCompatibleAlias(const clang::ObjCCompatibleAliasDecl* decl);
  void ConvertObjCImplementation(const clang::ObjCImplementationDecl* decl);
  void ConvertObjCInterface(const clang::ObjCInterfaceDecl* decl);
  void ConvertObjCIvar(const clang::ObjCIvarDecl* decl);
  void ConvertObjCMethod(const clang::ObjCMethodDecl* decl);
  void ConvertObjCProperty(const clang::ObjCPropertyDecl* decl);
  void ConvertObjCPropertyImpl(const clang::ObjCPropertyImplDecl* decl);
  void ConvertObjCProtocol(const clang::ObjCProtocolDecl* decl);
#endif
#ifdef C2W_ENABLE_OPENMP
  void ConvertOMPThreadPrivate(const clang::OMPThreadPrivateDecl* decl);
#endif

  void ConvertParmVar(const clang::ParmVarDecl *decl);

  void ConvertRecord(const clang::RecordDecl *decl);

  void ConvertTemplateTemplateParm(const clang::TemplateTemplateParmDecl *decl);

  void ConvertTemplateTypeParm(const clang::TemplateTypeParmDecl *decl);

  void ConvertTranslationUnit(const clang::TranslationUnitDecl *decl);

  void ConvertTypeAlias(const clang::TypeAliasDecl *decl);

  void ConvertTypedef(const clang::TypedefDecl *decl);

  void ConvertUnresolvedUsingTypename(const clang::UnresolvedUsingTypenameDecl *decl);

  void ConvertUnresolvedUsingValue(const clang::UnresolvedUsingValueDecl *decl);

  void ConvertUsing(const clang::UsingDecl *decl);

  void ConvertUsingDirective(const clang::UsingDirectiveDecl *decl);

  void CreateInitoInitvEntry(const clang::VarDecl *decl, ST_IDX st_idx);

  std::string GetMangledName(const clang::Decl *decl, INT variant = 0);

  std::string GetMangledName(const clang::GlobalDecl gd, const clang::ThunkInfo &thunk_info);

  INITV_IDX ConvertVTableComponent(const clang::VTableLayout &layout, unsigned idx, unsigned &thunk_idx);

  void HandleAttrs(const clang::NamedDecl *decl, ST_IDX st_idx);

public:
  STR_IDX ConvertName(const clang::Decl *decl, const llvm::StringRef &str, INT variant = 0);

  STR_IDX ConvertName(const clang::CXXConstructorDecl *decl, clang::CXXCtorType ctor);

  STR_IDX ConvertName(const clang::CXXDestructorDecl *decl, clang::CXXDtorType dtor);

  STR_IDX ConvertName(const clang::GlobalDecl gd, const clang::ThunkInfo &ti);

  void ConvertCXXConstructor(const clang::CXXConstructorDecl *decl, clang::CXXCtorType ctor);

  void ConvertCXXDestructor(const clang::CXXDestructorDecl *decl, clang::CXXDtorType dtor);

  BOOL ConvertFunction(clang::GlobalDecl gd);

  void ConvertVar(const clang::VarDecl *decl);

  void ConvertDecl(const clang::Decl *decl);

  BOOL HasVTable(const clang::CXXRecordDecl *decl);

  BOOL ShouldEmitVTable(const clang::CXXRecordDecl *decl);

  ST_IDX GetVTableST(const clang::CXXRecordDecl *decl);

  void ConvertVTable(const clang::CXXRecordDecl *decl);

  const ST_IDX GetTcfST(const clang::Decl *decl);

  const clang::CXXRecordDecl *ConvertConstantArrayType(const clang::QualType type, int &num_element);

  void CollectInitListItems(const clang::InitListExpr *init, std::vector<const clang::Expr*>& elems);

  void EmitGlobalInitializer();

  // get non-trivial init expression, return NULL if not exist
  const clang::Expr *GetNonTrivialInitializer(const clang::Expr *init);

  BOOL Call_nothrow(const clang::Decl *decl);
};

} // namespace wgen

#endif /* CLANG2WHIRL_DECL_H */
