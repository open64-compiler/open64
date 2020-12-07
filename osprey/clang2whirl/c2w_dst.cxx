/*
  Copyright (C) 2019-2020 Xcalibyte Limited, Inc.  All Rights Reserved.

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

#include "c2w_dst.h"
#include "c2w_builder.h"
#include "c2w_tracer.h"

// clang header files
#include "clanginc.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"

using namespace clang;

// open64 header files
#include "open64inc.h"
#include "dwarf.h"
#include "dwarf_DST.h"
#include "dwarf_DST_producer.h"
#include "c2w_version.h"

// stdc++
#include <string>

namespace wgen {

WhirlDstBuilder::WhirlDstBuilder(WhirlBuilder *builder)
  : _builder(builder) {
}

WhirlDstBuilder::~WhirlDstBuilder() {
}

void
WhirlDstBuilder::Initialize() {
  TRACE_FUNC();
  DST_Init(NULL, 0);
}

void
WhirlDstBuilder::Finalize() {
  TRACE_FUNC();
}


DST_language
WhirlDstBuilder::GetLanguage() const {
  switch (_builder->Lang()) {
    case IL_C:
      return DW_LANG_C89;
    case IL_CPP:
      return DW_LANG_C_plus_plus;
    default:
      return DW_LANG_C89;
  }
}

DST_identifier_case
WhirlDstBuilder::GetIdentifierCase() const {
  return DW_ID_case_sensitive;
}

const char *
WhirlDstBuilder::GetProducer() const {
  return COMPILER_FULL_VERSION;
}

static const char *
DropDirectory(const char *file) {
  const char *ptr = file;
  char ch;
  while ((ch = *ptr) != '\0') {
    ++ ptr;
    if (ch == '\\' || ch == '/')
      file = ptr;
  }
  return file;
}

static void
SplitFile(const char *fname, std::string &file, std::string &dir) {
  const char *sav = fname;
  const char *ptr = fname;
  char ch;
  while ((ch = *ptr) != '\0') {
    ++ ptr;
    if (ch == '\\' || ch == '/')
      fname = ptr;
  }
  file.assign(fname);
  if (fname != sav)
    dir.assign(sav, fname - sav - 1);
}

bool
WhirlDstBuilder::GetFileInfo(const FileEntry *fe, std::string &file, std::string &dir,
                             off_t *psize, time_t *ptime) {
  Is_True(fe, ("FileEntry is NULL"));
  file = fe->getName().str();
  if (file.empty())
    file = "<stdin>";
  else
    file = DropDirectory(file.c_str());
  SourceManager &SM = _builder->Context()->getSourceManager();
  dir = SM.getFileManager().getCanonicalName(fe->getDir()).str();
  if (psize)
    *psize = fe->getSize();
  if (ptime)
    *ptime = fe->getModificationTime();
  return true;
}

UINT32
WhirlDstBuilder::WhirlDirID(const char *dir) {
  STR_IDX_MAP::iterator it = _dir_map.find(dir);
  if (it != _dir_map.end())
    return it->second;
  DST_mk_include_dir(const_cast<char *>(dir));
  UINT32 dir_num = _dir_map.size() + 1;
  _dir_map[dir] = dir_num;
  return dir_num;
}

UINT32
WhirlDstBuilder::WhirlFileID(const char* fname) {
  SourceManager &SM = _builder->Context()->getSourceManager();
  llvm::ErrorOr<const FileEntry *> ret = SM.getFileManager().getFile(fname);
  if (ret && ret.get()) {
    return WhirlFileID(ret.get());
  }
  STR_IDX_MAP::iterator it = _file_map.find(fname);
  if (it != _file_map.end())
    return it->second;
  std::string file;
  std::string dir;
  SplitFile(fname, file, dir);
  DST_mk_file_name(const_cast<char *>(file.c_str()),
                   WhirlDirID(dir.c_str()), 0, 0);
  UINT32 file_num = _file_map.size() + 1;
  _file_map[fname] = file_num;
  return file_num;
}

UINT32
WhirlDstBuilder::WhirlFileID(const FileID id) {
  SourceManager &SM = _builder->Context()->getSourceManager();
  const FileEntry *fe = SM.getFileEntryForID(id);
  Is_True(fe, ("bad FileEntry"));
  return WhirlFileID(fe);
}

UINT32
WhirlDstBuilder::WhirlFileID(const FileEntry *fe) {
  Is_True(fe, ("bad FileEntry"));
  const char* fname = fe->getName().data();
  STR_IDX_MAP::iterator it = _file_map.find(fname);
  if (it != _file_map.end())
    return it->second;
  std::string file;
  std::string dir;
  off_t file_size = 0;
  time_t mod_time = 0;
  GetFileInfo(fe, file, dir, &file_size, &mod_time);
  DST_mk_file_name(const_cast<char *>(file.c_str()),
                   WhirlDirID(dir.c_str()), file_size, mod_time);
  UINT32 file_num = _file_map.size() + 1;
  _file_map[fname] = file_num;
  return file_num;
}

USRCPOS
WhirlDstBuilder::GetSrcPos(SourceLocation sl) {
  if (sl.isInvalid())
    return _cur_loc;
  USRCPOS_clear(_cur_loc);
  PresumedLoc ploc = _builder->Context()->getSourceManager().getPresumedLoc(sl);
  // Newer CLANG has PresumedLoc::getFileID(), use code below:
  // USRCPOS_filenum(_cur_loc) = WhirlFileID(ploc.getFileID());
  USRCPOS_filenum(_cur_loc) = WhirlFileID(ploc.getFilename());
  USRCPOS_linenum(_cur_loc) = ploc.getLine();
  USRCPOS_column(_cur_loc) = ploc.getColumn();
  return _cur_loc;
}

SRCPOS
WhirlDstBuilder::SetSrcPos(SourceLocation sl) {
  return USRCPOS_srcpos(GetSrcPos(sl));
}

SRCPOS
WhirlDstBuilder::GetSrcPos(void) {
  return USRCPOS_srcpos(_cur_loc);
}

  
DST_INFO_IDX
WhirlDstBuilder::CreateDstForCompileUnit() {
  TRACE_FUNC();
  SourceManager &SM = _builder->Context()->getSourceManager();
  std::string dir;
  std::string file;
  const FileEntry *fe = SM.getFileEntryForID(SM.getMainFileID());
  GetFileInfo(fe, file, dir, NULL, NULL);
  return DST_mk_compile_unit(const_cast<char *>(file.c_str()),
                             const_cast<char *>(dir.c_str()),
                             const_cast<char *>(GetProducer()),
                             GetLanguage(),
                             GetIdentifierCase());
}

DST_INFO_IDX
WhirlDstBuilder::CreateDstForStmt(const Stmt *stmt) {
  TRACE_FUNC();
  DST_INFO_IDX dst_idx = DST_mk_lexical_block(NULL, (ST_IDX) 0, (ST_IDX) 0, DST_INVALID_IDX);
  return dst_idx;
}

DST_INFO_IDX
WhirlDstBuilder::CreateDstForFunc(ST_IDX st, TY_IDX rtype) {
  USRCPOS spos;
  USRCPOS_clear(spos);
  DST_INFO_IDX type_dst = DST_INVALID_IDX;
  DST_INFO_IDX func_dst = DST_mk_subprogram(spos,
                                            const_cast<char *>(ST_name(st)),
                                            type_dst,
                                            DST_INVALID_IDX,
                                            st,
                                            DW_INL_not_inlined,
                                            DW_VIRTUALITY_none,
                                            0,                    // vtbl index
                                            false,                // is decl
                                            false,                // prototyped
                                            true,                 // artificial
                                            true);                // internal
  return func_dst;
}

DST_INFO_IDX
WhirlDstBuilder::CreateDstForFunctionDecl(GlobalDecl gd) {
  const FunctionDecl* decl = cast<FunctionDecl>(gd.getDecl());
  USRCPOS usrcpos = GetSrcPos(decl->getLocation());
  IdentifierInfo *info = decl->getIdentifier();
  // TODO: figure out how to get CXX function name
  const char *name_str = NULL;
  if (info) {
    name_str = info->getName().data();
  }
  DST_INFO_IDX type_dst = CreateDstForType(decl->getReturnType());
  ST_IDX st_idx = _builder->SB().ConvertSymbol(gd);
  
  return DST_mk_subprogram(usrcpos,
                           const_cast<char *>(name_str),
                           type_dst,
                           DST_INVALID_IDX,
                           st_idx,
                           DW_INL_not_inlined, // TODO
                           DW_VIRTUALITY_none, // TODO
                           0,                  // vtbl index
                           false,              // is decl
                           true,               // prototyped
                           false,              // artificial
                           false);             // internal
}

DST_INFO_IDX
WhirlDstBuilder::CreateDstForVarDecl(const VarDecl *decl) {
  TRACE_FUNC();
  return DST_INVALID_IDX;
}

DST_INFO_IDX
WhirlDstBuilder::CreateDstForDecl(const Decl *decl) {
  TRACE_FUNC();
  DST_INFO_IDX ret_dst;
  switch (decl->getKind()) {
    case Decl::Function:
    case Decl::CXXMethod:
    case Decl::CXXConversion:
      ret_dst = CreateDstForFunctionDecl(GlobalDecl(cast<FunctionDecl>(decl)));
      break;
    case Decl::CXXConstructor:
    case Decl::CXXDestructor:
      Is_True(FALSE, ("FIXME: ctor/dtor"));
      break;
    case Decl::Var:
      ret_dst = CreateDstForVarDecl(cast<VarDecl>(decl));
    default:
      ret_dst = DST_INVALID_IDX;
  }
  if (!DST_IS_NULL(ret_dst)) {
    Is_True(!DST_IS_NULL(_builder->Scope().CurrentDst()), ("cur dst is NULL"));
    DST_append_child(_builder->Scope().CurrentDst(), ret_dst);
  }
  return ret_dst;
}


DST_INFO_IDX
WhirlDstBuilder::CreateDstForAtomicType(const AtomicType *type) {
  TRACE_FUNC();
  return DST_INVALID_IDX;
}

DST_INFO_IDX
WhirlDstBuilder::CreateDstForBuiltinType(const BuiltinType *type) {
  TRACE_FUNC();
  DST_INFO_IDX dst_idx = DST_INVALID_IDX;
  switch (type->getKind()) {
    case BuiltinType::Void:
      break;
    case BuiltinType::Bool:
      dst_idx = DST_mk_basetype("bool", DW_ATE_signed_char, 1);
      break;
    case BuiltinType::Char_S:
    case BuiltinType::SChar:
      dst_idx = DST_mk_basetype("char", DW_ATE_signed_char, 1);
      break;
    case BuiltinType::Char_U:
    case BuiltinType::UChar:
      dst_idx = DST_mk_basetype("unsigned char", DW_ATE_unsigned_char, 1);
      break;
    case BuiltinType::Short:
      dst_idx = DST_mk_basetype("short", DW_ATE_signed, 2);
      break;
    case BuiltinType::UShort:
      dst_idx = DST_mk_basetype("unsigned short", DW_ATE_unsigned, 2);
      break;
    case BuiltinType::Int:
      dst_idx = DST_mk_basetype("int", DW_ATE_signed, 4);
      break;
    case BuiltinType::UInt:
      dst_idx = DST_mk_basetype("unsigned int", DW_ATE_unsigned, 4);
      break;
    case BuiltinType::Long:
      dst_idx = DST_mk_basetype("long", DW_ATE_signed, 8);
      break;
    case BuiltinType::ULong:
      dst_idx = DST_mk_basetype("unsigned long", DW_ATE_unsigned, 8);
      break;
    case BuiltinType::LongLong:
      dst_idx = DST_mk_basetype("long long", DW_ATE_signed, 8);
      break;
    case BuiltinType::ULongLong:
      dst_idx = DST_mk_basetype("unsigned long long", DW_ATE_unsigned, 8);
      break;
    case BuiltinType::WChar_S:
      dst_idx = DST_mk_basetype("wchar_t", DW_ATE_signed, 2);
      break;
    case BuiltinType::WChar_U:
      dst_idx = DST_mk_basetype("wchar_t", DW_ATE_unsigned, 2);
      break;
    case BuiltinType::Char16:
      dst_idx = DST_mk_basetype("char16_t", DW_ATE_signed, 2);
      break;
    case BuiltinType::Char32:
      dst_idx = DST_mk_basetype("char32_t", DW_ATE_signed, 4);
      break;
    case BuiltinType::Half:
      dst_idx = DST_mk_basetype("half float", DW_ATE_float, 4);
      break;
    case BuiltinType::Float:
      dst_idx = DST_mk_basetype("float", DW_ATE_float, 4);
      break;
    case BuiltinType::Double:
      dst_idx = DST_mk_basetype("double", DW_ATE_float, 8);
      break;
    case BuiltinType::LongDouble:
    case BuiltinType::Float128:
      dst_idx = DST_mk_basetype("long double", DW_ATE_float, 16);
      break;
    case BuiltinType::NullPtr:
      dst_idx = DST_mk_pointer_type(DST_INVALID_INIT,
                                    DW_ADDR_none,
                                    MTYPE_size_min(Pointer_Mtype));
      break;
    case BuiltinType::UInt128:
      dst_idx = DST_mk_basetype("int128_t", DW_ATE_signed, 8);
      break;
    case BuiltinType::Int128:
      dst_idx = DST_mk_basetype("uint128_t", DW_ATE_unsigned, 8);
      break;
#ifdef C2W_ENABLE_OBJC
    case BuiltinType::ObjCId:
    case BuiltinType::ObjCClass:
    case BuiltinType::ObjCSel:
      break;
#endif
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
      //TRACE_WARN(("Unsupported builtin type: %d", type->getKind()));
      break;
  }
  // always append builtin type to global scope
  if (!DST_IS_NULL(dst_idx)) {
    Is_True(!DST_IS_NULL(_builder->Scope().GlobalDst()), ("global dst is NULL"));
    DST_append_child(_builder->Scope().GlobalDst(), dst_idx);
  }
  return dst_idx;
}

DST_INFO_IDX
WhirlDstBuilder::CreateDstForFunctionType(const FunctionType *type) {
  TRACE_FUNC();
#if 0
  USRCPOS usrcpos = GetSrcPos(decl->getLocation());
  const char* name_str = decl->getIdentifier()->getName().data();
  DST_INFO_IDX type_dst = CreateDstForType(decl->getReturnType());
  return DST_mk_subroutine_type(usrcpos,
                                const_cast<char*>(name_str),
                                type_dst, DST_INVALID_IDX, FALSE);
#endif
  return DST_INVALID_IDX;
}

DST_INFO_IDX
WhirlDstBuilder::CreateDstForArrayType(const ConstantArrayType *type, TY_IDX ty_idx) {
  TRACE_FUNC();
  //USRCPOS src = GetSrcPos(_builder->Scope().CurrentDecl()->getLocation());
  USRCPOS src;
  DST_INFO_IDX dst_idx;
  DST_INFO_IDX inner_dst = CreateDstForType(type->getElementType());
  dst_idx = DST_mk_array_type(src, 0, inner_dst, 0, DST_INVALID_IDX, TRUE);
#if 0
  TY& ty = Ty_Table[ty_idx];
  ARB_HANDLE arb = TY_arb(ty_idx);
  DST_INFO_IDX d;
  if ( TY_kind (ty) != KIND_INVALID ) {
      for (INT index = TY_AR_ndims(ty_idx) - 1; index >= 0; index--) {
          if (!ARB_ubnd_var(arb[index]))
              break;
          if (!ARB_const_ubnd(arb[index])) {
              ST* var_st = &St_Table[ARB_ubnd_var(arb[index])];
              if (ST_sclass(var_st) == SCLASS_AUTO)
                  break;
          }
          d = DST_enter_subrange_type(arb[index]);
          DST_append_child(dst_idx,d);
      }
  }
#endif
  if (!DST_IS_NULL(dst_idx)) {
    Is_True(!DST_IS_NULL(_builder->Scope().CurrentDst()), ("cur dst is NULL"));
    DST_append_child(_builder->Scope().CurrentDst(), dst_idx);
  }
  return dst_idx;
}

DST_INFO_IDX
WhirlDstBuilder::CreateDstForRecordType(const RecordType *type) {
  TRACE_FUNC();
  return DST_INVALID_IDX;
}

DST_INFO_IDX
WhirlDstBuilder::CreateDstForType(const QualType T, TY_IDX ty_idx) {
  TRACE_FUNC();
  const Type *type = T.getTypePtr();
  TYPE_DST_MAP::iterator it = _type_map.find(type);
  if (it != _type_map.end())
    return it->second;
  DST_INFO_IDX dst_idx = DST_INVALID_IDX;
  switch (type->getTypeClass()) {
    case Type::Atomic:
      dst_idx = CreateDstForAtomicType(cast<AtomicType>(type));
      break;
    case Type::Builtin:
      dst_idx = CreateDstForBuiltinType(cast<BuiltinType>(type));
      break;
    case Type::FunctionNoProto:
    case Type::FunctionProto:
      dst_idx = CreateDstForFunctionType(cast<FunctionType>(type));
      break;
    case Type::ConstantArray:
      dst_idx = CreateDstForArrayType(cast<ConstantArrayType>(type), ty_idx);
      break;
    case Type::Elaborated:
      dst_idx = CreateDstForType(cast<ElaboratedType>(type)->getNamedType(), ty_idx);
      break;
    case Type::Record:
      dst_idx = CreateDstForRecordType(cast<RecordType>(type));
      break;
    case Type::Pointer:
    case Type::Paren:
    case Type::Enum:
    case Type::Vector:
    case Type::Complex:
    case Type::Decayed:
    case Type::Typedef:
    case Type::TypeOfExpr:
    case Type::LValueReference:
    case Type::RValueReference:
    case Type::SubstTemplateTypeParm:
    case Type::TemplateSpecialization:
      break;
    default:
      //TRACE_WARN(("Unsupported type class: %s", type->getTypeClassName()));
      break;
  }
  _type_map[type] = dst_idx;
  return dst_idx;
}

} // namespace wgen
