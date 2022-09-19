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

#ifndef CLANG2WHIRL_DST_H
#define CLANG2WHIRL_DST_H

// clang forward declaration
#include "clangdecl.h"

// open64 forward declaration
#include "dwarf_DST.h"
#include "dwarf_DST_mem.h"
// workaround for define inline/WEAK in defs.h
#undef inline
#undef WEAK

#include "open64decl.h"

// stdc++
#include "c2w_map.h"
#include <string>

namespace wgen {

class WhirlBuilder;

class WhirlDstBuilder {
private:
  WhirlBuilder *_builder;
  USRCPOS _cur_loc;
  
  STR_IDX_MAP  _dir_map;
  STR_IDX_MAP  _file_map;
  TYPE_DST_MAP _type_map;
  ST_DST_MAP  _sym_map;

public:
  WhirlDstBuilder(WhirlBuilder *builder);
  
  ~WhirlDstBuilder();
  
  void Initialize();
  
  void Finalize();

private:
  
  DST_language GetLanguage() const;
  
  DST_identifier_case GetIdentifierCase() const;
  
  const char *GetProducer() const;
  
  bool GetFileInfo(const clang::FileEntry *fe, std::string &file, std::string &dir, off_t *psize, time_t *ptime);
  
  UINT32 WhirlDirID(const char *dir);
  
  UINT32 WhirlFileID(const char *file);

  UINT32 WhirlFileID(const clang::FileID id);

  UINT32 WhirlFileID(const clang::FileEntry *fe);

public:
  USRCPOS GetSrcPos(clang::SourceLocation sl);
  
  SRCPOS  SetSrcPos(clang::SourceLocation sl);
  SRCPOS  GetSrcPos(void);

  DST_INFO_IDX CreateDstForCompileUnit();
  
  DST_INFO_IDX CreateDstForStmt(const clang::Stmt *stmt);

  DST_INFO_IDX CreateDstForFunc(ST_IDX st, TY_IDX rtype);

private:
  DST_INFO_IDX CreateDstForVarDecl(const clang::VarDecl *decl);

public:
  DST_INFO_IDX CreateDstForDecl(const clang::Decl *decl);

  DST_INFO_IDX CreateDstForFunctionDecl(clang::GlobalDecl decl);

private:
  DST_INFO_IDX CreateDstForAtomicType(const clang::AtomicType *type);
  
  DST_INFO_IDX CreateDstForBuiltinType(const clang::BuiltinType *type);
  
  DST_INFO_IDX CreateDstForFunctionType(const clang::FunctionType *type);
  
  DST_INFO_IDX CreateDstForArrayType(const clang::ConstantArrayType *type, TY_IDX ty_idx);
  
  DST_INFO_IDX CreateDstForRecordType(const clang::RecordType *type);

public:
  DST_INFO_IDX CreateDstForType(const clang::QualType T, TY_IDX ty_idx = 0);
  
};

} // namespace wgen

#endif /* CLANG2WHIRL_DST_H */
