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

#ifndef CLANG_COMPAT_H
#define CLANG_COMPAT_H

//
// API to get SourceLocation from Decl/Stmt/Expr
//
static inline clang::SourceLocation
getLocation(const clang::Decl *decl) {
  return decl->getLocation();
}

static inline clang::SourceLocation
getLocation(const clang::Stmt *stmt) {
#if LLVM_VERSION_MAJOR == 11
  return stmt->getBeginLoc();
#else
  return stmt->getLocStart();
#endif
}

static inline clang::SourceLocation
getLocation(const clang::Expr *expr) {
#if LLVM_VERSION_MAJOR == 11
  return expr->getExprLoc();
#else
  return expr->getLocStart();
#endif
}

static inline clang::SourceLocation
getEndLocation(const clang::Decl *decl) {
#if LLVM_VERSION_MAJOR == 11
  return decl->getEndLoc();
#else
  return decl->getLocEnd();
#endif
}

static inline clang::SourceLocation
getEndLocation(const clang::Stmt *stmt) {
#if LLVM_VERSION_MAJOR == 11
  return stmt->getEndLoc();
#else
  return stmt->getLocEnd();
#endif
}

//
// API to get temporary expr from MaterializeTemporaryExpr
//
static inline clang::Expr *
getSubExpr(const clang::MaterializeTemporaryExpr *expr) {
#if LLVM_VERSION_MAJOR == 11
  return expr->getSubExpr();
#else
  return expr->GetTemporaryExpr();
#endif
}

//
// API to create AST dumper
//
static inline clang::ASTConsumer *
createASTDumper() {
#if LLVM_VERSION_MAJOR == 11
  return clang::CreateASTDumper(nullptr, "all", true, false, false, false, clang::ADOF_Default).release();
#else
  return clang::CreateASTDumper(nullptr, "all", true, false, false).release();
#endif
}

//
// API to evaluate expr into integer
//
static inline bool
evaluateAsInt(const clang::Expr *expr, llvm::APSInt &val, const clang::ASTContext *ctx) {
#if LLVM_VERSION_MAJOR == 11
  clang::Expr::EvalResult res;
  if (expr->EvaluateAsInt(res, *ctx)) {
    val = res.Val.getInt();
    return true;
  }
  return false;
#else
  return expr->EvaluateAsInt(val, *ctx);
#endif
}

//
// API to mangle CXXName
//
static inline void
mangleCXXName(clang::MangleContext *mc, const clang::CXXConstructorDecl *ctor,
              clang::CXXCtorType variant, llvm::raw_svector_ostream& out) {
#if LLVM_VERSION_MAJOR == 11
  mc->mangleCXXName(clang::GlobalDecl(ctor, variant), out);
#else
  mc->mangleCXXCtor(ctor, variant, out);
#endif
}

static inline void
mangleCXXName(clang::MangleContext *mc, const clang::CXXDestructorDecl *dtor,
              clang::CXXDtorType variant, llvm::raw_svector_ostream& out) {
#if LLVM_VERSION_MAJOR == 11
  mc->mangleCXXName(clang::GlobalDecl(dtor, variant), out);
#else
  mc->mangleCXXDtor(dtor, variant, out);
#endif
}

//
// API to get Array Size
//
static inline const clang::Expr *
getArraySize(const clang::CXXNewExpr *expr) {
#if LLVM_VERSION_MAJOR == 11
  return expr->getArraySize().getValue();
#else
  return expr->getArraySize();
#endif
}

#endif /* CLANGINC_H */
