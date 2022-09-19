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

#ifndef CLANG2WHIRL_LAMBDA_H
#define CLANG2WHIRL_LAMBDA_H
// clang headers
#include "clangdecl.h"

#include "c2w_map.h"

namespace wgen {

enum LambdaPhase {
  NOT_IN_LAMBDA,
  IN_LAMBDA_CONSTRUCTOR,
  IN_LAMBDA_BODY,
};
class WhirlLambdaHelper {
  friend class WhirlLambdaContext;
private:
  LambdaPhase     _lambda_phase;
  LAMBDA_VAR_MAP  _lambda_var_map;
  LAMBDA_CAP_FLD  _lambda_cap_fld_map;

public:
  WhirlLambdaHelper() : _lambda_phase(NOT_IN_LAMBDA) {}

  static const clang::LambdaExpr *GetLambdaExpr(const clang::VarDecl *var);

  void  AddLambdaVar(const clang::LambdaExpr * expr, const clang::VarDecl *var_decl);
  void  AddCapFld(const clang::Decl *decl, const clang::FieldDecl *fld);
  const clang::FieldDecl *GetCapFld(const clang::Decl *decl);
  const clang::VarDecl   *GetLambdaVar(const clang::LambdaExpr *expr);
  void  CleanCapFld()                { _lambda_cap_fld_map.clear(); }
  bool  IsInLambda()                 { return _lambda_phase == IN_LAMBDA_CONSTRUCTOR ||
                                              _lambda_phase == IN_LAMBDA_BODY; }
  bool  IsInLambdaConstruct()        { return _lambda_phase == IN_LAMBDA_CONSTRUCTOR; }
  bool  IsInLambdaBody()             { return _lambda_phase == IN_LAMBDA_BODY; }

  LambdaPhase SetLambdaPhase(LambdaPhase p) {
    LambdaPhase prev = _lambda_phase;
    _lambda_phase = p;
    return prev;
  }
};

class WhirlLambdaContext {
private:
  WhirlLambdaHelper &_lambda;
  LAMBDA_CAP_FLD     _lambda_cap_fld_map;
  LambdaPhase        _lambda_phase;
  BOOL               _is_lambda;

public:
  WhirlLambdaContext(WhirlLambdaHelper& lambda, BOOL is_lambda)
    : _lambda(lambda), _is_lambda(is_lambda) {
    if (_is_lambda) {
      _lambda_phase = _lambda._lambda_phase;
      _lambda_cap_fld_map.swap(_lambda._lambda_cap_fld_map);
    }
  }

  ~WhirlLambdaContext() {
    if (_is_lambda) {
      _lambda._lambda_phase = _lambda_phase;
      _lambda._lambda_cap_fld_map.swap(_lambda_cap_fld_map);
    }
  }
};

} // namespace wgen
#endif /* CLANG2WHIRL_LAMBDA_H */
