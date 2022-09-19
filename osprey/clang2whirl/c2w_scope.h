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

#ifndef CLANG2WHIRL_SCOPE_H
#define CLANG2WHIRL_SCOPE_H

#include "c2w_tracer.h"

// forward clang declarations
#include "clangdecl.h"

// forward open64 declarations
#include "dwarf_DST_mem.h"
// workaround for define inline/WEAK in defs.h
#undef inline
#undef WEAK

#include "open64decl.h"
#include "open64inc.h"

namespace wgen {

class WhirlBuilder;

class WhirlScopeManager {
private:
  WhirlBuilder *_builder;
  
  class LexicalBlockScope {
    friend class WhirlScopeManager;
  
  private:
    DST_INFO_IDX _dst;                         // dst for the lexical scope, DW_TAG_lexical_block
    const clang::Stmt *_stmt;                  // the stmt starts the scope
    std::vector<const clang::VarDecl *> _vars;  // vars in the scope for cleanup
  public:
    LexicalBlockScope(const clang::Stmt *stmt)
      : _dst(DST_INVALID_IDX), _stmt(stmt) {
    }
  };
  
  class Scope {
    friend class WhirlScopeManager;
  
  private:
    UINT32 _level;                 // level of the scope, starts from GLOBAL_SYMTAB
    DST_INFO_IDX _dst;                   // dst for the scope
    const clang::Decl *_decl;                  // the decl starts the scope
    std::vector<LexicalBlockScope> _blocks;    // lexicalscopes in the function
    ST_IDX _this;                  // symbol of 'this' in current scope
    //ST_IDX_MAP         _st_map;                // symbols in the scope (include lexical blocks)   // 2018
  public:
    Scope(UINT32 level, const clang::Decl *decl)
      : _level(level), _dst(DST_INVALID_IDX), _decl(decl), _this(0) {
    }
  };
  
  std::vector<Scope> _scopes;
  bool _switch_to_global;

private:
  DST_INFO_IDX CreateDst(const Scope &scope) const;
  
  DST_INFO_IDX CreateDst(const LexicalBlockScope &scope) const;

public:
  WhirlScopeManager(WhirlBuilder *builder);
  
  ~WhirlScopeManager();
  
  void Initialize();
  
  void Finalize();
  
  // since the consumer doesn't start from TranslationUnitDecl
  // use this as a mark for top scope.
  static const clang::Decl *TopScopeDecl;
  
  // The following structures start new symtab in open64:
  // FunctionDecl, CXXMethodDecl (PARENT_SYMTAB + 1)
  //
  // The following structures start new lexical scope in C
  // TranslationUnitDecl, FunctionDecl, CompoundStmt, ForStmt, ...
  // In addition, the following structures start new scope in C++
  // NamespaceDecl, CXXRecordDecl, CXXMethodDecl, CXXCatchStmt
  //void EnterScope(const clang::FunctionDecl *decl);
  //void EnterScope(const clang::CXXMethodDecl *decl);
  void EnterScope(clang::GlobalDecl gd);

  void ExitScope(clang::GlobalDecl gd);
  
  void EnterScope(const clang::Decl *decl);
  
  void EnterScope(const clang::CXXRecordDecl *decl);
  
  void EnterScope(const clang::NamespaceDecl *decl);
  
  void ExitScope(const clang::Decl *decl);
  
  void EnterScope(const clang::Stmt *stmt);
  
  void ExitScope(const clang::Stmt *stmt);
  
  void SwitchToGlobalScope() { _switch_to_global = true; }
  
  void RestoreScope() { _switch_to_global = false; }

public:
  // access the current scope
  SYMTAB_IDX GlobalSymtab() const {
    Is_True(_scopes.size() > 0,
            ("empty scopes"));
    return _scopes.front()._level;
  }
  
  SYMTAB_IDX ParentSymtab() const {
    Is_True(_scopes.size() > 1,
            ("empty scopes"));
    return _scopes[_scopes.size() - 2]._level;
  }
  
  SYMTAB_IDX CurrentSymtab() const {
    Is_True(_scopes.size() > 0,
            ("empty scopes"));
    return _switch_to_global ?
           _scopes.front()._level : _scopes.back()._level;
  }
  
  DST_INFO_IDX GlobalDst() const {
    Is_True(_scopes.size() > 0,
            ("empty scopes"));
    return _scopes.front()._dst;
  }
  
  DST_INFO_IDX ParentDst() const {
    Is_True(_scopes.size() > 1,
            ("empty scopes"));
    const Scope &scope = _scopes[_scopes.size() - 2];
    if (DST_IS_NULL(scope._dst))
      return CreateDst(scope);
    else
      return scope._dst;
  }
  
  DST_INFO_IDX CurrentDst() const {
    Is_True(_scopes.size() > 0,
            ("empty scopes"));
    if (_switch_to_global)
      return _scopes.front()._dst;
    const Scope &scope = _scopes.back();
    if (DST_IS_NULL(scope._dst))
      CreateDst(scope);
    else
      return scope._dst;
  }
 
  void SetCurrentDst(DST_INFO_IDX dst) {
    Is_True(_scopes.size() > 0,
            ("empty scopes"));
    Is_True(!DST_IS_NULL(dst),
            ("dst is null"));
    if (_switch_to_global)
      _scopes.front()._dst = dst;
    else
      _scopes.back()._dst = dst;
  }
 
  const clang::Decl *CurrentDecl() const {
    Is_True(_scopes.size() > 0,
            ("empty scopes"));
    if (_switch_to_global)
      return _scopes.front()._decl;
    const Scope &scope = _scopes.back();
    return scope._decl;
  }
  
  const clang::Stmt *CurrentStmt() const {
    Is_True(_scopes.size() > 0,
            ("empty scopes"));
    Is_True(_switch_to_global == false,
            ("no stmt in global scope"));
    const Scope &scope = _scopes.back();
    return scope._blocks.back()._stmt; //scope._is_decl ? NULL : scope._stmt;  // 2018
  }
  
  void AppendVar(const clang::VarDecl *decl) {
    Is_True(_scopes.size() > 0,
            ("empty scopes"));
    if (_switch_to_global)
      _scopes.front()._blocks.back()._vars.push_back(decl); // 2018
    else
      _scopes.back()._blocks.back()._vars.push_back(decl);  // 2018
  }

  void Set_this(ST_IDX);
  const ST_IDX Get_this();
}; // WhirlScopeManager


template<typename _T>
class ScopeHelper {
private:
  WhirlScopeManager &_mgr;
  _T _t;

public:
  ScopeHelper(WhirlScopeManager &mgr, _T t)
    : _mgr(mgr), _t(t) {
    _mgr.EnterScope(_t);
  }
  
  ~ScopeHelper() {
    _mgr.ExitScope(_t);
  }
}; // ScopeHelper

class GlobalScope {
private:
  WhirlScopeManager &_mgr;
public:
  GlobalScope(WhirlScopeManager &mgr)
    : _mgr(mgr) {
    _mgr.SwitchToGlobalScope();
  }
  
  ~GlobalScope() {
    _mgr.RestoreScope();
  }
};

} // namespace wgen


#endif /* CLANG2WHIRL_SCOPE_H */

