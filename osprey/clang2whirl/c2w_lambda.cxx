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

#include "c2w_lambda.h"
#include "clanginc.h"

using namespace clang;
namespace wgen {

void
WhirlLambdaHelper::AddLambdaVar(const LambdaExpr * expr, const VarDecl * var_decl) {
  _lambda_var_map[expr] = var_decl;
}

void
WhirlLambdaHelper::AddCapFld(const Decl * decl, const FieldDecl *fld) {
  Is_True(isa<VarDecl>(decl) || isa<CXXRecordDecl>(decl),
          ("not VarDecl or CXXRecordDecl"));
  _lambda_cap_fld_map[decl] = fld;
}

const LambdaExpr *
WhirlLambdaHelper::GetLambdaExpr(const VarDecl *var_decl) {
  if (!var_decl || !var_decl->hasInit())
    return NULL;
  // skip init for empty cpature list
  if(ExprWithCleanups::classof(var_decl->getInit())) {
    auto init_expr = ((ExprWithCleanups *)(var_decl->getInit()))->getSubExpr();
    if(CXXConstructExpr::classof(init_expr)) {
      init_expr = ((CXXConstructExpr*)init_expr)->getArg(0);
      auto mte = dyn_cast_or_null<MaterializeTemporaryExpr>(init_expr);
      if(mte && getSubExpr(mte)) {
        auto lambda = dyn_cast_or_null<LambdaExpr>(getSubExpr(mte));
        if(lambda) {
          return lambda;
        }
      }
    }
  }
  return NULL;
}

const FieldDecl *
WhirlLambdaHelper::GetCapFld(const Decl *decl) {
  Is_True(isa<VarDecl>(decl) || isa<CXXRecordDecl>(decl),
          ("not VarDecl or CXXRecordDecl"));
  if(_lambda_cap_fld_map.find(decl) != _lambda_cap_fld_map.end()) {
    return _lambda_cap_fld_map[decl];
  } else {
    return NULL;
  }
}

const VarDecl *
WhirlLambdaHelper::GetLambdaVar(const LambdaExpr *expr) {
  if(_lambda_var_map.find(expr) != _lambda_var_map.end()) {
    return _lambda_var_map[expr];
  } else {
    return NULL;
  }
}

} // namespace wgen
