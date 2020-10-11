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

#ifndef CLANGINC_H
#define CLANGINC_H

#ifdef KEY
#define MASTIFF_CP_KEY KEY
#undef KEY
#endif

#ifdef WEAK
#define MASTIFF_CP_WEAK WEAK
#undef WEAK
#endif

// clang header files
#include "clang/AST/ASTConsumer.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Frontend/ASTConsumers.h"
#if LLVM_VERSION_MAJOR == 11
#include "clang/Basic/CodeGenOptions.h"
#else
#include "clang/Frontend/CodeGenOptions.h"
#endif
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/LangOptions.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Mangle.h"
#include "clang/Basic/ABI.h"
// clang Decl
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclFriend.h"
#include "clang/AST/GlobalDecl.h"
// clang Expr
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
// clanf Stmt
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtCXX.h"
// clang Type
#include "clang/AST/Type.h"

#ifdef MASTIFF_CP_KEY
#define KEY MASTIFF_CP_KEY
#undef MASTIFF_CP_KEY
#endif

#ifdef MASTIFF_CP_WEAK
#define WEAK MASTIFF_CP_WEAK
#undef MASTIFF_CP_WEAK
#endif

#include "clang_compat.h"

#endif /* CLANGINC_H */
