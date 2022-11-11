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

#include "c2w_builder.h"
#include "c2w_sym.h"
#include "c2w_decl.h"
#include "c2w_dst.h"
#include "c2w_func.h"
#include "c2w_expr.h"
#include "c2w_misc.h"
#include "c2w_tracer.h"

// clang header files
#include "clanginc.h"
#include "clang/AST/StmtVisitor.h"

using namespace clang;

// Open64 header files
#include "open64inc.h"
#include "erglob.h"
#include <unistd.h>

// Open64 globals
INT Debug_Level = 0;
INT trace_verbose = FALSE;
BOOL TARGET_64BIT;
static Output_File *Irb_Output_File = NULL;

static llvm::cl::opt<bool>
    ExternInlineEnabled(
      "emit-extern-inline", 
      llvm::cl::Hidden,
      llvm::cl::desc("Emit the extern inline function"),
      llvm::cl::init(false)
    );

void
Cleanup_Files(BOOL report, BOOL delete_dotofile) {
  Set_Error_Line(ERROR_LINE_UNKNOWN);

  /* Close and delete Irb file: */
  if (Irb_Output_File != NULL) {
    WN_close_file(Irb_Output_File);
    Irb_Output_File = NULL;
  }
  if (Irb_File_Name && unlink(Irb_File_Name)) {
    if (report) ErrMsg(EC_IR_Delete, Irb_File_Name, errno);
    Irb_File_Name = NULL;
  }
  Set_Error_File(NULL);
  Set_Error_Source(NULL);
}

namespace wgen {

// InitExprChecker
// walk VarDecl's init expr and check vars and functions used in init expr
//
class InitExprChecker : public ConstStmtVisitor<InitExprChecker> {
  WhirlBuilder *_builder;
  GLOBALDECL_SET _visited;

public:
  InitExprChecker(WhirlBuilder *bldr) : _builder(bldr) { }

  void VisitStmt(const Stmt *expr) {
    // visit all chiildren by default
    for (const Stmt *child : expr->children()) {
      if (child) Visit(child);
    }
  }

  void VisitCallExpr(const CallExpr *expr) {
    const FunctionDecl *callee = expr->getDirectCallee();
    // check direct callee
    if (callee != NULL) {
      if (!callee->isTrivial() &&
          callee->getBuiltinID() != 0)
        _builder->Get_func_st(GetGlobalDecl(callee));
      if (isa<CXXMethodDecl>(callee))
        if (!(cast<CXXMethodDecl>(callee)->isTrivial()))
          _builder->Get_func_st(GetGlobalDecl(callee));
    }
    // check indirect callee
    if (const Expr *ce = expr->getCallee())
      Visit(ce);
    // visit arguments
    for (const Stmt *arg : expr->arguments()) {
      if (arg) Visit(arg);
    }
  }

  void VisitCXXConstructExpr(const CXXConstructExpr *expr) {
    const CXXConstructorDecl *ctor = expr->getConstructor();
    // check constructor
    if (!ctor->isTrivial()) {
      _builder->Get_func_st(GlobalDecl(ctor, CXXCtorType::Ctor_Complete));
    }
    const CXXRecordDecl *record = ctor->getParent();
    // check destructor
    if (record && record->hasNonTrivialDestructor() &&
        record->getDestructor())
      _builder->Get_func_st(GlobalDecl(record->getDestructor(),
                                       CXXDtorType::Dtor_Complete));
    // visit argument
    for (const Stmt *arg : expr->arguments()) {
      if (arg) Visit(arg);
    }
  }

  void VisitCXXMemberCallExpr(const CXXMemberCallExpr *expr) {

    if (_visited.find((void *)expr) != _visited.end())
      return;
    _visited.insert((void *)expr);

    // check this
    if (const Expr *e = expr->getImplicitObjectArgument())
      Visit(e);
    // check method
    if (const CXXMethodDecl *m = expr->getMethodDecl())
      _builder->Get_func_st(GetGlobalDecl(m));
    // check as CallExpr
    VisitCallExpr(expr);
  }

  void VisitDeclRefExpr(const DeclRefExpr *expr) {
    const ValueDecl *value = expr->getDecl();
    value = cast<ValueDecl>(value->getCanonicalDecl());
    // check decl
    if (isa<FunctionDecl>(value) || isa<VarDecl>(value))
      _builder->Get_decl_st(value);
  }

  void VisitCXXDefaultArgExpr(const CXXDefaultArgExpr *expr) {
    // check expr
    Visit(expr->getExpr());
  }

  void VisitInitListExpr(const InitListExpr *expr) {
    // check filler
    if (const Expr *filler = expr->getArrayFiller())
      Visit(filler);

    const CXXRecordDecl *record = expr->getType()->getAsCXXRecordDecl();
    // check destructor
    if (record && record->hasNonTrivialDestructor() &&
        record->getDestructor())
      _builder->Get_func_st(GlobalDecl(record->getDestructor(),
                                       CXXDtorType::Dtor_Complete));

    // check as Stmt
    VisitStmt(expr);
  }

  void VisitMemberExpr(const MemberExpr *expr) {
    const ValueDecl *decl = expr->getMemberDecl();
    if (isa<VarDecl>(decl)) {
      // check static member
      _builder->Get_var_st(cast<VarDecl>(decl));
    }
    else {
      // visit base
      Visit(expr->getBase());
    }
  }

  void VisitCXXDeleteExpr(const CXXDeleteExpr *expr) {
    // check operator delete
    const FunctionDecl *func = expr->getOperatorDelete();
    _builder->Get_func_st(func);
    // check destructor
    const CXXRecordDecl *decl = expr->getDestroyedType()->getAsCXXRecordDecl();
    if (decl && decl->hasNonTrivialDestructor() &&
        decl->getDestructor())
      _builder->Get_func_st(GlobalDecl(decl->getDestructor(),
                                       CXXDtorType::Dtor_Complete));
    // visit argument
    Visit(expr->getArgument());
  }

  void VisitCXXNewExpr(const CXXNewExpr *expr) {
    // check operator new
    const FunctionDecl *func = expr->getOperatorNew();
    _builder->Get_func_st(func);
    // check placement args
    for (const Expr *parg : expr->placement_arguments()) {
      if (parg) Visit(parg);
    }
    // check constructor
    const CXXConstructExpr *ctor = expr->getConstructExpr();
    if (ctor)
      VisitCXXConstructExpr(ctor);
  }

  void VisitLambdaExpr(const LambdaExpr *expr) {
    // ignore lambda
  }
};

// Extra options to control WhirlBuilder
llvm::cl::opt<bool> WhirlBuilder::_verbose(
  "verbose", llvm::cl::init(false), llvm::cl::Hidden,
  llvm::cl::desc("Display verbose message"));
llvm::cl::opt<bool> WhirlBuilder::_use_cpp_intrn(
  "use-cpp-intrn", llvm::cl::init(false), llvm::cl::Hidden,
  llvm::cl::desc("Turn functins in C++ standard library (std) "
    "and well-defined 3rd party libraries (like boost) "
    "into intrinsic."));
llvm::cl::opt<bool> WhirlBuilder::_gen_cpp_intrn(
  "gen-cpp-intrn", llvm::cl::init(false), llvm::cl::Hidden,
  llvm::cl::desc("Generate files to describe functions in C++ "
    "standard library (std) and well-defined 3rd party libraries "
    "(like boost) with intrinsic."));
llvm::cl::opt<std::string> WhirlBuilder::_cpp_intrn_prefix(
  "gen-cpp-intrn-prefix", llvm::cl::init(""), llvm::cl::Hidden,
  llvm::cl::desc("Prefix used in file names generated from "
    "`-gen-cpp-intrn'. For example, `std' can be used for standard "
    "template library (STL) and `boost' can be for BOOST library."));
llvm::cl::opt<std::string> WhirlBuilder::_cpp_intrn_filter(
  "gen-cpp-intrn-filter", llvm::cl::init(""), llvm::cl::Hidden,
  llvm::cl::desc("Filter used to identiify template class should be "
    "processed to generate intrinsic descriptions. For example, "
    "`vector.push_back,map.*' to generate intrinsic for vector and map. @filename "
    "can be used to specify a file contains these filters."));

WhirlBuilder::WhirlBuilder(const INPUT_LANG lang, DiagnosticsEngine &diag)
  : _decl_builder(this),
    _sym_builder(this),
    _type_builder(this),
    _dst_builder(this),
    _scope(this),
    _pu_root(NULL),
    _pu_last(NULL),
    _saved_error_phase(NULL),
    _lang(lang),
    _decl_type(TOP_DECL),
    _Diags(diag),
    _Context(NULL),
    _MC(NULL),
    _com_intrn_entry_file(NULL),
    _c2w_intrn_use_file(NULL),
    _rbc_intrn_model_file(NULL),
    _verbose_list_file(NULL),
    _lambda_helper() {
}

WhirlBuilder::~WhirlBuilder() {
}

void
WhirlBuilder::Initialize(const char *infile, const char *outfile) {
  TRACE_FUNC();
  if (Context()->getTargetInfo().getPointerWidth(0) == 32) {
    TARGET_64BIT = FALSE;
    ABI_Name = "n32";
  }
  else {
    TARGET_64BIT = TRUE;
    ABI_Name = "n64";
  }

#ifdef BUILD_MASTIFF
  // initialize use or gen c++ intrinsic
  if (_use_cpp_intrn.getValue()) {
    if (_gen_cpp_intrn.getValue()) {
      printf("Warning: both -use-cpp-intrn and -gen-cpp-intrn are on, turn -gen-cpp-intrn off.\n");
      _gen_cpp_intrn.setValue(false);
    }
  }
  if (_gen_cpp_intrn.getValue()) {
    if (_cpp_intrn_prefix.getValue().empty()) {
      printf("Warning: -gen-cpp-intrn is on but -cpp-intrn-prefix is not set, set it to `temp'.\n");
      _cpp_intrn_prefix.setValue("temp");
    }
    InitGenCppIntrn();
  }
#endif

  MEM_Initialize();
//    MEM_POOL_Initialize(&_phase_pool, "Phase pool", FALSE);
//    MEM_POOL_Push(&_phase_pool);
  const char *const phase_name = "Whirl Builder";
  //Set_Error_Tables(Phases, host_errlist);
  Set_Error_Phase(phase_name);
  Set_Error_File(NULL);
  Set_Error_Line(ERROR_LINE_UNKNOWN);
  
  Is_True(!_pu_root && !_pu_last,
          ("_pu_root/_pu_last already assigned?"));
  Src_File_Name = (char *) infile;
  Irb_File_Name = (char *) outfile;
  
  Preconfigure();
  Init_Controls_Tbl();
  Configure();
  Initialize_Symbol_Tables(TRUE); // reserve index zero
  IR_reader_init();
  Irb_Output_File = Open_Output_Info((char *) outfile);
  _saved_error_phase = Get_Error_Phase();
  Set_Error_Phase(phase_name);
  
  // initialize DST
  _dst_builder.Initialize();
  // initialize Scope
  _scope.Initialize();
  // initialize TypeBuilder
  _type_builder.Initialize();
  // initialize builtin table
  WhirlExprBuilder::InitializeBuiltinTable();
}

void
WhirlBuilder::Finalize() {
  TRACE_FUNC();
  // TODO: finalize PU tree

  // finalize Decl
  _decl_builder.Finalize();
  // finalize DST
  _dst_builder.Finalize();
  // finalize Scope
  _scope.Finalize();
  // TODO: verify Whirl IR & Symtab
  Verify_SYMTAB(GLOBAL_SYMTAB);
  
  Restore_Cmd_Line_Ctrls();
  Set_Error_Phase(_saved_error_phase);
  Write_Global_Info(_pu_root);
  Close_Output_Info();
  IR_reader_finish();
//    MEM_POOL_Pop(&_phase_pool);

#ifdef BUILD_MASTIFF
  if (_gen_cpp_intrn.getValue()) {
    FiniGenCppIntrn();
  }
#endif
}

STR_IDX
WhirlBuilder::EnterString(const llvm::StringRef &str) {
  return Save_Str(str.str().c_str());
}

STR_IDX
WhirlBuilder::EnterString(const char *str) {
  return Save_Str(str);
}

void
WhirlBuilder::AddPUInfo(PU_Info *pu_info) {
  TRACE_FUNC();
  Is_True(pu_info != NULL,
          ("try to add NULL pu_info"));
  if (_pu_root == NULL)
    _pu_root = pu_info;
  else
    PU_Info_next(_pu_last) = pu_info;
  _pu_last = pu_info;
}

void
WhirlBuilder::EmitTopLevelDecl(const Decl *D) {
  TRACE_FUNC();

  // ignore dependent declarations.
  if (D->isTemplated()) {
#ifdef BUILD_MASTIFF
    if (GenCppIntrn()) {
      GenCppIntrnForDecl(D);
    }
#endif
    return;
  }

  // refer CodeGenModule::EmitTopLevelDecl CodeGenModule.cpp:3048
  // ignore dependent declarations.
  if (D->getDeclContext() && D->getDeclContext()->isDependentContext())
    return;

  // handle CXXRecordDecl:
  // emit any static data members, they may be definitions
  if (isa<CXXRecordDecl>(D)) {
    const CXXRecordDecl *record = cast<CXXRecordDecl>(D);
    for (auto *I : record->decls()) {
      if (isa<VarDecl>(I) || isa<CXXRecordDecl>(I)) {
        EmitTopLevelDecl(I);
      }
      else if (isa<FunctionDecl>(I)) {
        HandleFunctionDecl(cast<FunctionDecl>(I));
      }
    }
    return;
  }

  // handle NamespaceDecl
  if (isa<NamespaceDecl>(D)) {
    for (auto *I : cast<NamespaceDecl>(D)->decls())
      EmitTopLevelDecl(I);
    return;
  }

  // handle FunctionDecl
  if (isa<FunctionDecl>(D)) {
    HandleFunctionDecl(cast<FunctionDecl>(D));
    return;
  }

  if (isa<VarDecl>(D)) {
    if (MustBeEmitted(D) ||
        isa<VarTemplateSpecializationDecl>(D) && cast<VarDecl>(D)->hasInit())
      AddDeferredVar(cast<VarDecl>(D));
    return;
  }

  // verify scope
  //Is_True(_scope.CurrentDecl() == reinterpret_cast<const Decl*>(WhirlScopeManager::TOP_SCOPE),
  //           ("invalid top scope"));
  //D->dump(llvm::outs());
  _decl_builder.ConvertDecl(D);
  //Is_True(_scope.CurrentDecl() == reinterpret_cast<const Decl*>(WhirlScopeManager::TOP_SCOPE),
  //           ("invalid top scope"));
}

bool
WhirlBuilder::HandleTopLevelDecl(DeclGroupRef D) {
  TRACE_FUNC();
  if (_Diags.hasErrorOccurred())
    return true;
  // set decl_type to TOP_DECL
  _decl_type = TOP_DECL;

  for (DeclGroupRef::iterator I = D.begin(), E = D.end(); I != E; ++I) {
    EmitTopLevelDecl(*I);
  }

  // set decl_type to DEFERRED_DECL
  _decl_type = DEFERRED_DECL;

  // handle local deferred decls
  HandleDeferredDecls(FALSE);

  return true;
}

void
WhirlBuilder::HandleTranslationUnit(ASTContext &Ctx) {
  TRACE_FUNC();
  if (_Diags.hasErrorOccurred()) {
    // TODO: do cleanup
    _deferred_funcs.clear();
    _global_deferred_funcs.clear();
    return;
  }

  // in case any functions in local list
  if (HasDeferredFuncs(FALSE))
    HandleDeferredFuncs(FALSE);

  // set decl_type to GLOBAL_DECL
  _decl_type = GLOBAL_DECL;

  // do an iteration to make sure no deferred vars, vtables and functions
  do {
    HandleDeferredDecls(TRUE);
  } while (HasDeferredDecls(TRUE));

  // generate initializer for global vars
  _decl_builder.EmitGlobalInitializer();

  Is_True(_deferred_vars.empty(), ("deferred vars is not empty"));
  Is_True(_deferred_vtables.empty(), ("deferred funcs is not empty"));
  Is_True(_deferred_funcs.empty(), ("local decls is not empty"));
  Is_True(_global_deferred_funcs.empty(), ("global decls is not empty"));
}

void
WhirlBuilder::HandleDeferredFuncs(BOOL global) {
  DeferredFuncList &list = global ? _global_deferred_funcs
                                  : _deferred_funcs;

  while (!list.empty()) {
    GlobalDecl gd = GlobalDecl::getFromOpaquePtr(list.front());
    list.pop_front();
    const FunctionDecl *decl = dyn_cast<FunctionDecl>(gd.getDecl());
    Is_True(decl != NULL, ("not a cxx method decl"));
    if (decl->getDefinition())
      decl = decl->getDefinition();
    if (!decl->doesThisDeclarationHaveABody()) {
      // find def decl if decl doesn't have body
      FUNCTION_DECL_MAP::iterator def = _func_decl_map.find(decl->getCanonicalDecl());
      if (def != _func_decl_map.end())
        decl = def->second;
    }

    if (!decl->doesThisDeclarationHaveABody()) {
      // if handling local list, add to global list
      if (!global)
        AddDeferredFunc(gd, TRUE);
      else if (decl->hasAttrs())
        _decl_builder.ConvertFunction(gd);
      continue;
    }

    Is_True(decl->doesThisDeclarationHaveABody(), ("decl does not have a body"));
    if (const CXXConstructorDecl *ctor = dyn_cast<CXXConstructorDecl>(decl)) {
      // convert ctor
      _decl_builder.ConvertCXXConstructor(ctor, gd.getCtorType());
    }
    else if (const CXXDestructorDecl *dtor = dyn_cast<CXXDestructorDecl>(decl)) {
      // convert dtor
      _decl_builder.ConvertCXXDestructor(dtor, gd.getDtorType());
    }
    else if (const FunctionDecl *func = dyn_cast<FunctionDecl>(decl)) {
      // convert function
      _decl_builder.ConvertFunction(func);
    }
    else {
      Is_True(FALSE, ("unknown decl"));
    }
  }
  // remove all items
  list.clear();
}

void
WhirlBuilder::HandleDeferredVars() {
  size_t sz = _deferred_vars.size();
  GLOBALDECL_SET::iterator end = _deferred_vars.end();
  for (GLOBALDECL_SET::iterator it = _deferred_vars.begin();
       it != end; ++it) {
    const VarDecl *vd = (const VarDecl *)*it;
    DeclBuilder().ConvertVar(vd);
  }
  Is_True(sz == _deferred_vars.size(), ("size mismatch"));
  _deferred_vars.clear();
}

void
WhirlBuilder::HandleDeferredVTables() {
  size_t sz = _deferred_vtables.size();
  GLOBALDECL_SET::iterator end = _deferred_vtables.end();
  for (GLOBALDECL_SET::iterator it = _deferred_vtables.begin();
       it != end; ++it) {
    const CXXRecordDecl *rd = (const CXXRecordDecl *)*it;
    if (DeclBuilder().ShouldEmitVTable(rd))
      DeclBuilder().ConvertVTable(rd);
  }
  Is_True(sz == _deferred_vtables.size(), ("size mismatch"));
  _deferred_vtables.clear();
}

void
WhirlBuilder::HandleDeferredThunks() {
  size_t sz = _deferred_thunks.size();
  ST_THUNK_MAP::iterator end = _deferred_thunks.end();
  for (ST_THUNK_MAP::iterator it = _deferred_thunks.begin();
       it != end; ++it) {
    Is_True(it->first != ST_IDX_ZERO && it->second.Method,
            ("bad thunk info"));
    const ThunkInfo &ti = it->second;
    const GlobalDecl gd = GlobalDecl::getFromOpaquePtr((void*)ti.Method);
    const FunctionDecl *func = cast<FunctionDecl>(gd.getDecl());
    if (!func->doesThisDeclarationHaveABody())
      continue;
    ScopeHelper<GlobalDecl> shlp(Scope(), gd);
    WhirlFuncBuilder func_bldr(this);
    func_bldr.EmitThunkFunction(ti, it->first);
  }
  Is_True(sz == _deferred_thunks.size(), ("size mismatch"));
  _deferred_thunks.clear();
}

void
WhirlBuilder::HandleDeferredDecls(BOOL global) {
  // handle deferred vars
  if (HasDeferredVars())
    HandleDeferredVars();

  // handle vtables
  if (HasDeferredVTables())
    HandleDeferredVTables();

  // handle global deferred decls for any new decls created above
  if (HasDeferredFuncs(TRUE))
    HandleDeferredFuncs(TRUE);

  // handle deferred thunk functions
  if (HasDeferredThunks())
    HandleDeferredThunks();
}

void
WhirlBuilder::HandleFunctionDecl(const clang::FunctionDecl *decl) {
  // ignore decl which will be available externally
  //if (Context()->GetGVALinkageForFunction(decl) == GVA_AvailableExternally)
  //  return;
  // emit if it's must be emitted
  if (MustBeEmitted(decl)) {
    _decl_builder.ConvertDecl(decl);
    // continue add ctor and dtor for ctor_base/ctor_base
    if (!isa<CXXConstructorDecl>(decl) && !isa<CXXDestructorDecl>(decl))
      return;
  }
  AddFunctionDecl(decl);
}

void
WhirlBuilder::AddFunctionDecl(const clang::FunctionDecl* decl) {
  const clang::FunctionDecl* canon_decl = decl->getCanonicalDecl();
  if (_func_decl_map.find(canon_decl) == _func_decl_map.end() ||
      decl->doesThisDeclarationHaveABody())
    _func_decl_map[canon_decl] = decl;
}

void
WhirlBuilder::AddDeferredFunc(const GlobalDecl gd, BOOL force_global) {
  void *ptr = gd.getAsOpaquePtr();
  const FunctionDecl *decl = dyn_cast<FunctionDecl>(gd.getDecl());
  Is_True (!decl->isTrivial(), ("trivial decl"));
  if (const CXXDestructorDecl *dtor = dyn_cast<CXXDestructorDecl>(decl)) {
    Is_True(!dtor->getParent()->hasTrivialDestructor(), ("trivial dtor"));
  }
  if (!ExternInlineEnabled) {
    if (Context()->GetGVALinkageForFunction(decl) == GVA_AvailableExternally)
      return;
  }
  DeferredFuncList &list = (_decl_type == GLOBAL_DECL || force_global)
                              ? _global_deferred_funcs
                              : _deferred_funcs;
  DeferredFuncList::iterator end = list.end();
  if (std::find(list.begin(), end, ptr) == end)
    list.push_back(ptr);
}

void
WhirlBuilder::AddDeferredVar(const VarDecl *vd) {
  Is_True(!vd->isLocalVarDecl(), ("var must not be local"));
  if (Context()->GetGVALinkageForVariable(vd) == GVA_AvailableExternally)
    return;
  if (vd->hasInit()) {
    InitExprChecker ec(this);
    ec.Visit(vd->getInit());
  }
  _deferred_vars.insert((void*)vd);
}

void
WhirlBuilder::AddDeferredVTable(const CXXRecordDecl *rd) {
  Is_True(DeclBuilder().HasVTable(rd), ("no vtable for record"));
  _deferred_vtables.insert((void*)rd);
}

void
WhirlBuilder::AddDeferredThunk(ST_IDX st, const ThunkInfo &ti) {
  Is_True(ti.Method, ("no method in thunk"));
  _deferred_thunks[st] = ti;
}

BOOL
WhirlBuilder::MustBeEmitted(const Decl *decl) {
  return Context()->DeclMustBeEmitted(decl);
}

BOOL
WhirlBuilder::IsDeclHandled(const GlobalDecl gd) {
  void *ptr = gd.getCanonicalDecl().getAsOpaquePtr();
  if (_handled_decls.find(ptr) == _handled_decls.end()) {
    _handled_decls.insert(ptr);
    return FALSE;
  }
  return TRUE;
}

LABEL_IDX
WhirlBuilder::Get_label_idx(const LabelDecl *decl) {
  LABEL_IDX label_idx;
  LABEL_IDX_MAP::iterator it = _label_map.find(decl);
  if (it != _label_map.end())
    label_idx =  it->second;
  else {
    New_LABEL(CURRENT_SYMTAB, label_idx);
    _label_map[decl] = label_idx;
  }
  Is_True(label_idx != 0, ("label idx is NULL"));
  return label_idx;
}

void
WhirlBuilder::Clear_label_map() {
  _label_map.clear();
}

ST_IDX
WhirlBuilder::Get_vla_bound_st(const Expr *expr) {
  VLA_SIZE_MAP::iterator it = _vla_size_map.find(expr);
  if (it != _vla_size_map.end())
   return it->second;

  // gen temp sym for vla bound
  TY_IDX ty_idx = TB().ConvertType(expr->getType());
  ST *tmp_st = Gen_Temp_Symbol(ty_idx, "___vla_bound");
  Set_ST_Srcpos(*tmp_st, SetSrcPos(getLocation(expr)));
  ST_IDX tmp_st_idx = ST_st_idx(tmp_st);

  _vla_size_map[expr] = tmp_st_idx;
  return tmp_st_idx;
}

ST_IDX
WhirlBuilder::Get_thunk_st(const clang::GlobalDecl gd, clang::ThunkInfo ti) {
  STR_IDX name = DeclBuilder().ConvertName(gd, ti);
  ST_IDX st = SB().GetThunkST(name);
  if (st == ST_IDX_ZERO) {
    st = SB().ConvertThunkFunction(cast<NamedDecl>(gd.getDecl()), name);
    // set gd to ti's Method field
    const FunctionDecl *func = cast<FunctionDecl>(gd.getDecl());
    if (func->isDefined()) {
      ti.Method = (CXXMethodDecl *)GetGlobalDecl(func->getDefinition(), gd).getAsOpaquePtr();
      AddDeferredThunk(st, ti);
    }
  }
  return st;
}

ST_IDX
WhirlBuilder::Get_func_st(const clang::GlobalDecl gd) {
  ST_IDX st = SB().GetST(gd);
  if (st == ST_IDX_ZERO) {
    st = SB().ConvertSymbol(gd);
    AddDeferredFunc(gd);
  }
  return st;
}

ST_IDX
WhirlBuilder::Get_var_st(const clang::VarDecl *decl) {
  ST_IDX st = SB().GetST(decl);
  if (st == ST_IDX_ZERO) {
    st = SB().ConvertSymbol(decl);
    // get the real definition if exist
    if (!decl->isLocalVarDecl() && decl->getAnyInitializer(decl))
      AddDeferredVar(decl);
  }
  return st;
}

ST_IDX
WhirlBuilder::Get_decl_st(const clang::Decl *decl) {
  Is_True(isa<FunctionDecl>(decl) ||
          isa<VarDecl>(decl) ||
          isa<BindingDecl>(decl), ("not var or func"));
  if (isa<FunctionDecl>(decl))
    return Get_func_st(GetGlobalDecl(cast<FunctionDecl>(decl)));
  else if (isa<VarDecl>(decl))
    return Get_var_st(cast<VarDecl>(decl));
  else if (isa<BindingDecl>(decl)) {
    const Expr *expr = cast<BindingDecl>(decl)->getBinding();
    if (isa<DeclRefExpr>(expr)) {
      decl = cast<DeclRefExpr>(expr)->getDecl();
      if (isa<VarDecl>(decl))
        return Get_var_st(cast<VarDecl>(decl));
    } else if (isa<MemberExpr>(expr)) {
      const Expr *base= cast<MemberExpr>(expr)->getBase();
      if (base && isa<DeclRefExpr>(base)) {
        decl = cast<DeclRefExpr>(base)->getDecl();
        if (isa<VarDecl>(decl))
          return Get_var_st(cast<VarDecl>(decl));
      }
    }
    Is_True(FALSE, ("unsupported BindingDecl in Get_decl_st"));
  } else
    FmtAssert(FALSE, ("wrong decl"));
  return ST_IDX_ZERO;
}

// Capture all the sizes for the VLA expressions in
// the given variably-modified type and store them in _vla_size_map.
void
WhirlBuilder::EmitVariablyModifiedType(QualType type) {
  Is_True(type->isVariablyModifiedType(),
          ("Must pass variably modified type to EmitVariablyModifiedType"));

  // We're going to walk down into the type and look for VLA
  // expressions.
  do {
    Is_True(type->isVariablyModifiedType(),
            ("should be variably modified type"));

    const Type *ty = type.getTypePtr();
    switch (ty->getTypeClass()) {
    // These types are never variably-modified.
    case Type::Builtin:
    case Type::Complex:
    case Type::Vector:
    case Type::ExtVector:
    case Type::Record:
    case Type::Enum:
    case Type::Elaborated:
    case Type::TemplateSpecialization:
    case Type::ObjCTypeParam:
    case Type::ObjCObject:
    case Type::ObjCInterface:
    case Type::ObjCObjectPointer:
      Is_True(false, ("type class is never variably-modified!"));

    case Type::Adjusted:
      type = cast<AdjustedType>(ty)->getAdjustedType();
      break;

    case Type::Decayed:
      type = cast<DecayedType>(ty)->getPointeeType();
      break;

    case Type::Pointer:
      type = cast<PointerType>(ty)->getPointeeType();
      break;

    case Type::BlockPointer:
      type = cast<BlockPointerType>(ty)->getPointeeType();
      break;

    case Type::LValueReference:
    case Type::RValueReference:
      type = cast<ReferenceType>(ty)->getPointeeType();
      break;

    case Type::MemberPointer:
      type = cast<MemberPointerType>(ty)->getPointeeType();
      break;

    case Type::ConstantArray:
    case Type::IncompleteArray:
      // Losing element qualification here is fine.
      type = cast<ArrayType>(ty)->getElementType();
      break;

    case Type::VariableArray: {
      const VariableArrayType *vat = cast<VariableArrayType>(ty);

      if (const Expr *size = vat->getSizeExpr()) {
        WhirlExprBuilder bldr(this);
        WN *wn = bldr.ConvertToNode(size);
        if (WN_operator(wn) != OPR_INTCONST) {
          TYPE_ID mtyp = WN_rtype(wn);
          // get vla bound st for size expr
          ST_IDX tmp_st = Get_vla_bound_st(size);
          WN *st_wn = WN_Stid(mtyp, 0, ST_ptr(tmp_st), MTYPE_To_TY(mtyp),
                              WN_Sub(mtyp, wn, WN_Intconst(mtyp, 1)));
          WN_Set_Linenum(st_wn, ST_Srcpos(tmp_st));
          WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), st_wn);
        }
      }
      type = vat->getElementType();
      break;
    }

    case Type::FunctionProto:
    case Type::FunctionNoProto:
      type = cast<FunctionType>(ty)->getReturnType();
      break;

    case Type::Paren:
    case Type::TypeOf:
    case Type::UnaryTransform:
    case Type::Attributed:
    case Type::SubstTemplateTypeParm:
    case Type::PackExpansion:
      // Keep walking after single level desugaring.
      type = type.getSingleStepDesugaredType(*(Context()));
      break;

    case Type::Typedef:
    case Type::Decltype:
    case Type::Auto:
    case Type::DeducedTemplateSpecialization:
      // Stop walking: nothing to do.
      return;

    case Type::TypeOfExpr:
    {
      // Stop walking: emit typeof expression.
      WhirlExprBuilder bldr(this);
      bldr.ConvertToNode(cast<TypeOfExprType>(ty)->getUnderlyingExpr(),
                         Result::nwNone(), FALSE);
      return;
    }

    case Type::Atomic:
      type = cast<AtomicType>(ty)->getValueType();
      break;

    case Type::Pipe:
      type = cast<PipeType>(ty)->getElementType();
      break;

    default:
      Is_True(false, ("unexpected dependent type"));
    }
  } while (type->isVariablyModifiedType());
}

} // namespace wgen

