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

#ifndef CLANG2WHIRL_BUILDER_H
#define CLANG2WHIRL_BUILDER_H

#include "open64decl.h"
#include "clangdecl.h"
#include "llvm/Support/CommandLine.h"
#if LLVM_VERSION_MAJOR >= 14
#include "clang/Basic/Thunk.h"
#endif

#include "c2w_enum.h"
#include "c2w_const.h"
#include "c2w_dst.h"
#include "c2w_decl.h"
#include "c2w_sym.h"
#include "c2w_type.h"
#include "c2w_scope.h"
#include "c2w_lambda.h"

extern BOOL emit_exceptions;  // from driver.cxx
extern BOOL TARGET_64BIT;

namespace wgen {

extern const clang::CXXBaseSpecifier*
Find_base(const clang::CXXRecordDecl *base, const clang::CXXRecordDecl *derived);
extern const clang::CXXBaseSpecifier*
Find_vbase(const clang::CXXRecordDecl *base, const clang::CXXRecordDecl *derived);
extern INT64
ComputeOffsetHind(clang::ASTContext *ctx, const clang::CXXRecordDecl *src,
                  const clang::CXXRecordDecl *dst, bool dyncast);
extern clang::GlobalDecl
GetGlobalDecl(const clang::FunctionDecl *decl);
extern clang::GlobalDecl
GetGlobalDecl(const clang::FunctionDecl *decl, clang::GlobalDecl gd);

enum DECL_TYPE {
  TOP_DECL,      // processing a top level decl
  DEFERRED_DECL, // processing a deferred decl
  GLOBAL_DECL,   // processing global decls
};

class WhirlBuilder {
private:
  // individual builders
  WhirlDeclBuilder  _decl_builder;
  WhirlDstBuilder   _dst_builder;
  WhirlSymBuilder   _sym_builder;
  WhirlTypeBuilder  _type_builder;
  WhirlScopeManager _scope;
  PU_Info *_pu_root;
  PU_Info *_pu_last;
  const char *_saved_error_phase;
  
  const INPUT_LANG _lang;
  DECL_TYPE        _decl_type;

  clang::DiagnosticsEngine &_Diags;
  clang::ASTContext *_Context;
  clang::MangleContext *_MC;

  // label idx map
  LABEL_IDX_MAP _label_map;
  VLA_SIZE_MAP _vla_size_map;

private:
  static llvm::cl::opt<bool>        _verbose;         // verbose
  static llvm::cl::opt<bool>        _use_cpp_intrn;   // convert c++ known function call to intrinsic
  static llvm::cl::opt<bool>        _gen_cpp_intrn;   // generate files to describe c++ function with intrinsic
  static llvm::cl::opt<std::string> _cpp_intrn_prefix;// prefix for c++ library been analyzed to generate code
  static llvm::cl::opt<std::string> _cpp_intrn_filter;// filter for c++ library been analyzed to generate code

  FILE *_com_intrn_entry_file;  // common/com/cxx_<prefix>_intrn_entry.def
  FILE *_c2w_intrn_use_file;    // clang2whirl/c2w_cxx_<prefix>_intrn.cxx
  FILE *_rbc_intrn_model_file;  // be/rnc/certc/cxx_<prefix>.cxx
  FILE *_verbose_list_file;     // verbose list file for debug purpose

  METHOD_SET       _cpp_func_filter_set;   // set to filter function to generate code
  CLASS_METHOD_MAP _cpp_class_filter_map;  // map to filter class/method to generate code

  void InitGenCppIntrnFilter(const char* filter);
  void InitGenCppIntrn();
  void FiniGenCppIntrn();

  void GenCppIntrnForClassTemplate(const clang::ClassTemplateDecl *decl);
  void GenCppIntrnForClassMethod(const char *cls_pfx, const char *mtd_pfx, const char *mangle_name,
                                 const clang::FunctionDecl *method, const clang::FunctionTemplateDecl *tmpl);
  void GenCppIntrnForFunctionTemplate(const clang::FunctionTemplateDecl *decl);
  void GenCppIntrnForDecl(const clang::Decl *decl);

private:
  // handled function decl map
  // void * is the OpaquePtr of GlobalDecl
  GLOBALDECL_SET _handled_decls;

  // function map from canonical decl to definition
  FUNCTION_DECL_MAP _func_decl_map;

  // vardecl map from canonical decl to definition
  VAR_DECL_MAP _var_decl_map;

  // deferred VarDecl which should be emitted
  GLOBALDECL_SET _deferred_vars;

  // deferred CXXRecordType which needs a vtable
  GLOBALDECL_SET _deferred_vtables;

  // deferred Thunk functions
  ST_THUNK_MAP _deferred_thunks;

  // deferred CLANG functions
  // use list and make sure new deferred funcs are always appended at end
  // so that they can be processed during handling the list
  // void * is the OpaquePtr of GlobalDecl
  typedef std::list<void *> DeferredFuncList;
  // deferred decls during handle top level decls
  DeferredFuncList _deferred_funcs;
  // deferred decls globally
  DeferredFuncList _global_deferred_funcs;

  BOOL HasDeferredVars() const {
    return !_deferred_vars.empty();
  }

  BOOL HasDeferredVTables() const {
    return !_deferred_vtables.empty();
  }

  BOOL HasDeferredThunks() const {
    return !_deferred_thunks.empty();
  }

  BOOL HasDeferredFuncs(BOOL global) const {
    return !(global ? _global_deferred_funcs.empty()
                    : _deferred_funcs.empty());
  }

  BOOL HasDeferredDecls(BOOL global) const {
    return HasDeferredVars() || HasDeferredVTables() ||
           HasDeferredThunks() || HasDeferredFuncs(global);
  }

  // handle deferred vars
  void HandleDeferredVars();

  // handle deferred functions
  void HandleDeferredFuncs(BOOL global);

  // handle deferred vtables
  void HandleDeferredVTables();

  // handle deferred thunk functions
  void HandleDeferredThunks();

  // handle deferred vars, vtables and functions
  void HandleDeferredDecls(BOOL global);

  // handle function decl and all subclass types
  void HandleFunctionDecl(const clang::FunctionDecl *decl);

private:
  // lambda builder helper
  WhirlLambdaHelper _lambda_helper;

public:
  WhirlBuilder(const INPUT_LANG lang, clang::DiagnosticsEngine &diag);
  
  ~WhirlBuilder();
  
  void Initialize(const char *infile, const char *outfile);
  
  void Finalize();

public:
  bool Lang_CPP() const { return _lang == IL_CPP; }
  
  bool Lang_C() const { return _lang == IL_C; }
  
  INPUT_LANG Lang() const { return _lang; }

  DECL_TYPE DeclType() const { return _decl_type; }

public:
  static bool Verbose()     { return _verbose.getValue();       }
  static bool UseCppIntrn() { return _use_cpp_intrn.getValue(); }
  static bool GenCppIntrn() { return _gen_cpp_intrn.getValue(); }
  static std::string CppIntrnPrefix() { return _cpp_intrn_prefix.getValue(); }

public:
  STR_IDX EnterString(const llvm::StringRef &str);
  
  STR_IDX EnterString(const char *str);

public:
  WhirlDeclBuilder &DeclBuilder() { return _decl_builder; }
  
  WhirlDstBuilder &DstBuilder() { return _dst_builder; }
  
  WhirlSymBuilder &SB() { return _sym_builder; }
  
  WhirlTypeBuilder &TB() { return _type_builder; }
  
  WhirlScopeManager &Scope() { return _scope; }

  WhirlLambdaHelper &LambdaHelper() { return _lambda_helper; }

public:
  clang::DiagnosticsEngine &DiagEngine() { return _Diags; }
  
  clang::ASTContext *Context() { return _Context; }

  clang::MangleContext *MangleContext() { return _MC; }

  void SetContext(clang::ASTContext *ctx)
  {
     _Context = ctx;
     _MC = ctx->createMangleContext();
  }
  
  void AddPUInfo(PU_Info *pu_info);
  
  void AddFunctionDecl(const clang::FunctionDecl* decl);

  void AddDeferredFunc(const clang::GlobalDecl gd, BOOL force_global = FALSE);

  void AddDeferredVar(const clang::VarDecl *vd);

  void AddDeferredVTable(const clang::CXXRecordDecl *rd);

  void AddDeferredThunk(ST_IDX st, const clang::ThunkInfo &ti);

  BOOL MustBeEmitted(const clang::Decl *decl);

  BOOL IsDeclHandled(const clang::GlobalDecl gd);

  // get LABEL_IDX from LabelDecl
  LABEL_IDX Get_label_idx(const clang::LabelDecl *decl);

  void Clear_label_map();

  ST_IDX Get_thunk_st(const clang::GlobalDecl gd, clang::ThunkInfo ti);

  ST_IDX Get_func_st(const clang::GlobalDecl gd);

  ST_IDX Get_var_st(const clang::VarDecl *decl);

  ST_IDX Get_decl_st(const clang::Decl *decl);

  void EmitVariablyModifiedType(clang::QualType type);

  ST_IDX Get_vla_bound_st(const clang::Expr *expr);

  void EmitTopLevelDecl(const clang::Decl *D);

public:
  bool HandleTopLevelDecl(clang::DeclGroupRef D);
  
  void HandleTranslationUnit(clang::ASTContext &Ctx);

  SRCPOS  SetSrcPos(clang::SourceLocation sl) { return _dst_builder.SetSrcPos(sl); }
  SRCPOS  GetSrcPos(void)                     { return _dst_builder.GetSrcPos(); }

}; // WhirlBuilder


} // namespace wgen

#endif /* CLANG2WHIRL_BUILDER_H */

