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

#ifndef CLANG2WHIRL_SYM_H
#define CLANG2WHIRL_SYM_H

// clang forward declarations
#include "clangdecl.h"
#include "clang/AST/GlobalDecl.h"
// open64 forward declarations
#include "open64decl.h"
// clang2whirl maps
#include "c2w_map.h"
#include "symtab.h"
#include "c2w_converter.h"

#undef inline
#undef WEAK

namespace wgen {

class WhirlBuilder;

class VTableSymConverter: public Converter<VTableSymConverter, const clang::CXXRecordDecl *, ST_IDX> {
public:
  ST_IDX Convert(const clang::CXXRecordDecl *decl);
  explicit VTableSymConverter(WhirlBuilder *builder): Converter(builder) {}
};

class VTTSymConverter: public Converter<VTTSymConverter, const clang::CXXRecordDecl *, ST_IDX> {
public:
  ST_IDX Convert(const clang::CXXRecordDecl *decl);
  explicit VTTSymConverter(WhirlBuilder *builder): Converter(builder) {}
};


class WhirlSymBuilder {
private:
  enum FUNC_KIND {
    FUNC_NORMAL,    // normal function
    FUNC_CTOR,      // constructor
    FUNC_THUNK      // thunk function
  };

  WhirlBuilder *_builder;
  ST_IDX_MAP _st_map;
  VTableSymConverter _vsc;
  VTTSymConverter _vttsc;
  THUNK_STR_ST_MAP _thunk_str_st_map;

public:
  WhirlSymBuilder(WhirlBuilder *builder);
  
  ~WhirlSymBuilder();
  
  void Initialize();
  
  void Finalize();

private:
  ST_EXPORT ExportClass(const clang::FunctionDecl *decl);
  
  ST_IDX ConvertFunction(const clang::FunctionDecl *decl,
                         STR_IDX mangle_name = STR_IDX_ZERO,
                         FUNC_KIND kind = FUNC_NORMAL);
  
  ST_IDX ConvertParmVar(const clang::ParmVarDecl *decl);
  
  ST_IDX ConvertVar(const clang::VarDecl *decl);
  
  ST_IDX ConvertCXXConstructor(const clang::CXXConstructorDecl *decl, clang::CXXCtorType ctor);
  
  ST_IDX ConvertCXXDestructor(const clang::CXXDestructorDecl *decl, clang::CXXDtorType dtor);
  
  ST_IDX ConvertCXXRecord(const clang::CXXRecordDecl *decl);
  
  ST_IDX ConvertCXXMethod(const clang::CXXMethodDecl *decl);

  ST_IDX ConvertCXXConversion(const clang::CXXConversionDecl *decl);

public:
  ST_IDX ConvertSymbol(const clang::NamedDecl *decl);

  ST_IDX ConvertSymbol(const clang::GlobalDecl gd);

  VTableSymConverter &VSC() {
    return _vsc;
  }

  VTTSymConverter &VTTSC() {
    return _vttsc;
  }

  ST_IDX ConvertThunkFunction(const clang::NamedDecl *decl, STR_IDX mangled_name);

  const ST_IDX GetThunkST(STR_IDX mangled_name);

  const ST_IDX GetST(const clang::Decl *decl);

  const ST_IDX GetST(const clang::GlobalDecl gd);

  ST_IDX EmitCXXPureVirtualST();

  void  RemoveLocalSymbol(SYMTAB_IDX level);
};

} // namespace wgen

#endif /* WHIRL_GENERATOR_SYMTAB_BUILDER_H */
