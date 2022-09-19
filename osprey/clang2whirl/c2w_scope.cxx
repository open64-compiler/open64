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

#include "c2w_scope.h"
#include "c2w_builder.h"
#include "c2w_tracer.h"

// clang includes
#include "clanginc.h"

using namespace clang;

// open64 includes
#include "open64inc.h"

namespace wgen {

const Decl *WhirlScopeManager::TopScopeDecl = reinterpret_cast<const Decl *>(-1);

WhirlScopeManager::WhirlScopeManager(WhirlBuilder *builder)
  : _builder(builder), _switch_to_global(false) {
}

WhirlScopeManager::~WhirlScopeManager() {
}

void
WhirlScopeManager::Initialize() {
  TRACE_FUNC();
  Is_True(_scopes.size() == 0,
          ("_scopes mismatch"));
  _scopes.push_back(Scope(GLOBAL_SYMTAB, TopScopeDecl));
  DST_INFO_IDX dst_idx = _builder->DstBuilder().CreateDstForCompileUnit();
  _scopes.back()._dst = dst_idx;
}


void
WhirlScopeManager::Finalize() {
  TRACE_FUNC();
  Is_True(_scopes.size() == 1,
          ("_scopes mismatch"));
  Is_True(_scopes.back()._decl == TopScopeDecl,
          ("Invalid top scope"));
  _scopes.pop_back();
}

DST_INFO_IDX
WhirlScopeManager::CreateDst(const Scope &scope) const {
  Scope &scp = const_cast<Scope &>(scope);
  DST_INFO_IDX dst;
  Is_True(scp._decl != TopScopeDecl,
          ("top decl dst already created"));
  dst = _builder->DstBuilder().CreateDstForDecl(scp._decl);
  scp._dst = dst;
  return dst;
}

DST_INFO_IDX
WhirlScopeManager::CreateDst(const LexicalBlockScope &scope) const {
  LexicalBlockScope &scp = const_cast<LexicalBlockScope &>(scope);
  DST_INFO_IDX dst;
  dst = _builder->DstBuilder().CreateDstForStmt(scp._stmt);
  scp._dst = dst;
  return dst;
}

void
WhirlScopeManager::EnterScope(GlobalDecl gd) {
  TRACE_FUNC();
  Is_True(_scopes.size() > 0,
          ("Function can not be top scope"));
  UINT32 level = CurrentSymtab();
  DST_INFO_IDX dst_idx;
  dst_idx = _builder->DstBuilder().CreateDstForFunctionDecl(gd);
  _scopes.push_back(Scope(level + 1, gd.getDecl()));
  _scopes.back()._dst = dst_idx;
}

void
WhirlScopeManager::ExitScope(GlobalDecl gd) {
  ExitScope(gd.getDecl());
}

void
WhirlScopeManager::EnterScope(const Decl *decl) {
  TRACE_FUNC();
  Is_True(_scopes.size() > 0,
          ("Function can not be top scope"));
  UINT32 level = CurrentSymtab();
  DST_INFO_IDX dst_idx = _builder->DstBuilder().CreateDstForDecl(decl);
  _scopes.push_back(Scope(level + 1, decl));
  _scopes.back()._dst = dst_idx;
}

#if 0
void
WhirlScopeManager::EnterScope(const CXXMethodDecl *decl) {
  TRACE_FUNC();
  Is_True(_scopes.size() > 1,
          ("CXXMethodDecl can not be top scope"));
  UINT32 level = CurrentSymtab();
  DST_INFO_IDX dst_idx = _builder->DstBuilder().CreateDstForDecl(decl);
  _scopes.push_back(Scope(level + 1, decl));
  _scopes.back()._dst = dst_idx;
}
#endif

void
WhirlScopeManager::EnterScope(const CXXRecordDecl *decl) {
  TRACE_FUNC();
  Is_True(_scopes.size() > 0,
          ("CXXRecordDecl can not be top scope"));
  UINT32 level = CurrentSymtab(); // should be in GLOBAL_SYMTAB?
  DST_INFO_IDX dst_idx = _builder->DstBuilder().CreateDstForDecl(decl);
  _scopes.push_back(Scope(level, decl));
  _scopes.back()._dst = dst_idx;
}

void
WhirlScopeManager::EnterScope(const NamespaceDecl *decl) {
  TRACE_FUNC();
  Is_True(_scopes.size() > 0,
          ("NamespaceDecl can not be top scope"));
  UINT32 level = CurrentSymtab(); // should be in GLOBAL_SYMTAB?
  _scopes.push_back(Scope(level, decl));
}

void
WhirlScopeManager::ExitScope(const clang::Decl *decl) {
  TRACE_FUNC();
  Is_True(_scopes.size() > 0,
          ("Exit empty scope"));
  Is_True(_scopes.back()._decl == decl,
          ("Decl mismatch"));
  _scopes.pop_back();
}

void
WhirlScopeManager::EnterScope(const clang::Stmt *stmt) {
  TRACE_FUNC();
  Is_True(stmt->getStmtClass() == Stmt::CompoundStmtClass ||
          stmt->getStmtClass() == Stmt::CXXCatchStmtClass,
          ("invalid stmt class"));
  Is_True(_scopes.size() > 1,
          ("Stmt must be in function scope"));
  SYMTAB_IDX level = CurrentSymtab();
  Is_True(level != GLOBAL_SYMTAB,
          ("Stmt must be in function scope"));
  Scope &scope = _scopes.back();
  scope._blocks.push_back(LexicalBlockScope(/*Scope(level,*/ stmt)); // 2018
  if (scope._blocks.size() == 1)
    scope._blocks.back()._dst = scope._dst;
}

void
WhirlScopeManager::ExitScope(const clang::Stmt *stmt) {
  TRACE_FUNC();
  // TODO: check _scope.size() should be > 1?
  Is_True(_scopes.size() > 1,
          ("Stmt must be in function scope"));
  Is_True(_scopes.back()._blocks.back()._stmt == stmt,
          ("Stmt mismatch"));
  Scope &scope = _scopes.back();
  scope._blocks.pop_back();
}

void
WhirlScopeManager::Set_this(ST_IDX this_st) {
  const clang::Decl *decl = CurrentDecl();
  Is_True(decl && _builder->Lang_CPP() &&
          (isa<CXXConstructorDecl>(decl) ||
           isa<CXXDestructorDecl>(decl) ||
           isa<CXXMethodDecl>(decl)), ("illegal request to set this"));
  _scopes.back()._this = this_st;
}

const ST_IDX
WhirlScopeManager::Get_this() {
  const clang::Decl *decl = CurrentDecl();
  Is_True(decl && _builder->Lang_CPP() &&
          (isa<CXXConstructorDecl>(decl) ||
           isa<CXXDestructorDecl>(decl) ||
           isa<CXXMethodDecl>(decl)), ("illegal request to get this"));
  return _scopes.back()._this;
}

} // namespace wgen


