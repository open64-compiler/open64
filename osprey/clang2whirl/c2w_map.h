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

#ifndef CLANG2WHIRL_MAP_H
#define CLANG2WHIRL_MAP_H

// forward clang declarations
#include "clangdecl.h"
// forward open64 declarations
#include "open64decl.h"
#include "c2w_result.h"

#define _GLIBCXX_PERMIT_BACKWARD_HASH

#include <ext/hash_map>
#include <ext/hash_set>

#undef _GLIBCXX_PERMIT_BACKWARD_HASH

namespace wgen {

using __gnu_cxx::hash_map;
using __gnu_cxx::hash_set;

struct wgen_ptr_hash {
  size_t operator()(const void *p) const {
    return reinterpret_cast<size_t>(p);
  }
};

struct wgen_tstr {
  const char *_ptr;
  size_t _length;
  
  wgen_tstr(const char *ptr, size_t len) : _ptr(ptr), _length(len) {}
};

struct wgen_tstr_hash {
  size_t operator()(const wgen_tstr &s) const {
    size_t h = 0;
    for (size_t i = 0; i < s._length; ++i) {
      h = h * 5 + s._ptr[i];
    }
    return h;
  }
};

struct wgen_tstr_equal {
  bool operator()(const wgen_tstr &s1, const wgen_tstr &s2) const {
    if (s1._length != s2._length)
      return false;
    for (size_t i = 0; i < s1._length; ++i) {
      if (s1._ptr[i] != s2._ptr[i])
        return false;
    }
    return true;
  }
};

struct wgen_cstr_hash {
  size_t operator()(const char *s) const {
    size_t h = 0;
    char c;
    do {
      c = *s++;
      h = h * 5 + c;
    } while (c != '\0');
    return h;
  }
};

struct wgen_cstr_equal {
  bool operator()(const char *s1, const char *s2) const {
    char c1, c2;
    do {
      c1 = *s1++;
      c2 = *s2++;
      if (c1 != c2)
        return false;
    } while (c1 != '\0');
    return true;
  }
};

// clang FunctionDecl conanical decl to deferred decl map
typedef hash_map<const clang::FunctionDecl *, const clang::FunctionDecl *, wgen_ptr_hash> FUNCTION_DECL_MAP;
// clang VarDecl conanical decl to init decl map
typedef hash_map<const clang::VarDecl *, const clang::VarDecl *, wgen_ptr_hash> VAR_DECL_MAP;

// clang GlobalDecl set, use GlobalDecl::getAsOpaquePtr() as key
typedef hash_set<void *, wgen_ptr_hash> GLOBALDECL_SET;

// clang to WHIRL symtab
typedef hash_map<const clang::Type *, TY_IDX, wgen_ptr_hash> TY_IDX_MAP;
typedef hash_map<const clang::Decl *, ST_IDX, wgen_ptr_hash> ST_IDX_MAP;
typedef hash_map<const clang::Decl *, ST_IDX, wgen_ptr_hash> THIS_ST_MAP;
typedef hash_map<const clang::LabelDecl *, LABEL_IDX, wgen_ptr_hash> LABEL_IDX_MAP;
typedef hash_map<const clang::Stmt*, LABEL_IDX, wgen_ptr_hash> STMT_LABEL_IDX_MAP;
typedef hash_map<const clang::Decl *, FLD_IDX, wgen_ptr_hash> FLD_IDX_MAP;
typedef hash_map<const clang::Type *, FLD_IDX, wgen_ptr_hash> TYPE_FLD_MAP;
typedef hash_map<const clang::Decl *, DST_INFO_IDX, wgen_ptr_hash> DECL_DST_MAP;
typedef hash_map<const clang::Stmt *, DST_INFO_IDX, wgen_ptr_hash> STMT_DST_MAP;
typedef hash_map<wgen_tstr, TCON_IDX, wgen_tstr_hash, wgen_tstr_equal> STRING_TCON_MAP;
typedef hash_map<wgen_tstr, ST_IDX, wgen_tstr_hash, wgen_tstr_equal> STRCST_ST_MAP;
typedef hash_map<TCON_IDX, long double> TCON_VALUE_MAP;
typedef hash_map<const clang::Type *, ST_IDX, wgen_ptr_hash> RTTI_ST_MAP;
typedef hash_map<ST_IDX, clang::ThunkInfo> ST_THUNK_MAP;
typedef hash_map<uint64_t, ST_IDX> THUNK_STR_ST_MAP;
typedef hash_map<uint64_t, TY_IDX> RTTI_TY_MAP;
typedef hash_map<const clang::Expr *, ST_IDX, wgen_ptr_hash> VLA_SIZE_MAP;

typedef hash_map<const clang::CXXCatchStmt *, LABEL_IDX, wgen_ptr_hash> HANDLER_LABEL_MAP;
typedef hash_map<const clang::OpaqueValueExpr *, ST_IDX, wgen_ptr_hash> OPAQUE_VALUE_MAP;
typedef hash_map<ST_IDX, ST_IDX> REAL_PARM_MAP;

// clang to R for const initialization
typedef hash_map<const clang::VarDecl *, Result, wgen_ptr_hash> VAR_INITR_MAP;

// clang to WHIRL dst
typedef hash_map<UINT32, UINT32> FILE_IDX_MAP;
typedef hash_map<const char *, UINT32, wgen_cstr_hash, wgen_cstr_equal> STR_IDX_MAP;
typedef hash_map<const clang::Type *, DST_INFO_IDX, wgen_ptr_hash> TYPE_DST_MAP;
typedef hash_map<const clang::Decl *, DST_INFO_IDX, wgen_ptr_hash> ST_DST_MAP;

// clang lambda
typedef hash_map<const clang::LambdaExpr *, const clang::VarDecl *, wgen_ptr_hash> LAMBDA_VAR_MAP;
typedef hash_map<const clang::Decl *, const clang::FieldDecl *, wgen_ptr_hash>  LAMBDA_CAP_FLD;
} // namespace wgen


#endif /* CLANG2WHIRL_MAP_H */
