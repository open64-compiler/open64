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

#include "c2w_sym.h"
#include "c2w_builder.h"
#include "c2w_tracer.h"
#include "c2w_target.h"
#include <stdio.h>

// clang header files
#include "clanginc.h"
#include <clang/AST/VTableBuilder.h>

using namespace clang;

// open64 header files
#include "open64inc.h"

namespace wgen {

// specific functions which should not be inlined
#define NOINLINE_FUNC_MAX 20
const char *_noinline_func[NOINLINE_FUNC_MAX] = {
  "_Znwm", // operator new
  "_ZNSt3__123mersenne_twister_engineImLm32ELm624ELm397ELm31ELm2567483615ELm11ELm4294967295ELm7ELm2636928640ELm15ELm4022730752ELm18ELm1812433253EEC1Em", // mersenne_twister_engine
  "_ZNSt3__1lsINS_11char_traitsIcEEEERNS_13basic_ostreamIcT_EES6_PKc" // basic_ostream
};

static inline StringRef getDeclName(const clang::NamedDecl *decl) {
  if (decl->getIdentifier()) {
    return decl->getIdentifier()->getName();
  }
  return StringRef(".anon.");
}

// Gen mangled VTT name: return STR_IDX from given decl
ST_IDX
VTTSymConverter::Convert(const clang::CXXRecordDecl *decl) {
  ASTContext *ast_c = B()->Context();
  ItaniumMangleContext *mangle_c = cast<ItaniumMangleContext>(B()->MangleContext());
  Is_True(mangle_c != NULL, ("just support Itanium abi for creating vtable."));
  SmallString<256> name;
  llvm::raw_svector_ostream out(name);
  mangle_c->mangleCXXVTT(decl, out);

  ST *st = New_ST(GLOBAL_SYMTAB);
  TY_IDX ty_idx = B()->TB().ConvertVTTType(decl);
  ST_Init (st, Save_Str(out.str().str().c_str()),
           CLASS_VAR, SCLASS_DGLOBAL,
           EXPORT_PREEMPTIBLE, ty_idx);
  Set_ST_is_const_var(st);
  Set_ST_is_weak_symbol(st);

  SRCPOS spos = B()->SetSrcPos(decl->getLocation());
  Set_ST_Srcpos(*st, spos);

  return ST_st_idx(st);
}

ST_IDX VTableSymConverter::Convert(const clang::CXXRecordDecl *decl) {
  ASTContext *ast_c = B()->Context();
  ItaniumMangleContext *mangle_c = cast<ItaniumMangleContext>(B()->MangleContext());
  Is_True(mangle_c != NULL, ("just support Itanium abi for creating vtable."));
  SmallString<256> name;
  llvm::raw_svector_ostream o(name);
  mangle_c->mangleCXXVTable(decl, o);
#if 0
  ItaniumVTableContext *vtable_c = cast<ItaniumVTableContext>(ast_c->getVTableContext());
  const VTableLayout &layout = vtable_c->getVTableLayout(decl);
#endif
  ST *st = New_ST(GLOBAL_SYMTAB);
  TY_IDX ty_idx = B()->TB().ConvertVTableType(decl);
  ST_Init (st, Save_Str(o.str().str().c_str()), CLASS_VAR, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, ty_idx);
  Set_ST_is_vtable(st);
  Set_ST_is_const_var(st);
  Set_ST_is_weak_symbol(st);

  SRCPOS spos = B()->SetSrcPos(decl->getLocation());
  Set_ST_Srcpos(*st, spos);

  return ST_st_idx(st);
}

WhirlSymBuilder::WhirlSymBuilder(WhirlBuilder *builder)
  : _vsc(builder), _vttsc(builder), _builder(builder) {
}

WhirlSymBuilder::~WhirlSymBuilder() {
}

void
WhirlSymBuilder::Initialize() {
  TRACE_FUNC();
}

void
WhirlSymBuilder::Finalize() {
  TRACE_FUNC();
}

ST_EXPORT
WhirlSymBuilder::ExportClass(const FunctionDecl *decl) {
  if (decl->isGlobal()) {
    // inline function, functions defined in class,
    // implicit instantiation functions to be INTERNAL
    if (decl->isInlineSpecified() ||
        (isa<CXXMethodDecl>(decl) && cast<CXXMethodDecl>(decl)->hasInlineBody()) ||
        decl->getTemplateSpecializationKind() == TSK_ImplicitInstantiation)
      return EXPORT_INTERNAL;
    switch (decl->getVisibility()) {
      case HiddenVisibility:
        return EXPORT_HIDDEN;
      case ProtectedVisibility:
        return EXPORT_PROTECTED;
      default:
        return EXPORT_PREEMPTIBLE;
    }
  } else {
    GVALinkage linkage = _builder->Context()->GetGVALinkageForFunction(decl);
    if (linkage == GVA_StrongExternal) {
      return EXPORT_PREEMPTIBLE;
    }
    return EXPORT_LOCAL;
  }
}

ST_IDX
WhirlSymBuilder::ConvertFunction(const FunctionDecl *decl,
                                 STR_IDX mangled_name, FUNC_KIND kind) {
  SYMTAB_IDX symtab = _builder->Scope().CurrentSymtab();

  // No need to check symtab here for nested function decl,
  // since decl may didn't be handled before, because it has no body.

  STR_IDX str_idx;
  // mangled name exist for thunk function name
  if (mangled_name) {
    str_idx = mangled_name;
  } else {
    llvm::StringRef name = getDeclName(decl);
    if (decl->getBuiltinID() && name.startswith("__builtin_"))
      name = name.slice(sizeof("__builtin_") - 1, StringRef::npos);
    str_idx = _builder->DeclBuilder().ConvertName(decl, name);
  }

  // for function, ty_idx is pu_idx, if it's 0, create a new PU entry
  TY_IDX ty_idx;

  // FIXME: confused, why create pu in here?
  PU_IDX pu_idx;
  PU &pu = New_PU(pu_idx);

  TY_IDX pu_ty_idx;
  // C++ Method decl has implicit this, which should always be args[0].
  if (isa<CXXMethodDecl>(decl) &&
      !(cast<CXXMethodDecl>(decl)->isStatic())) {
    const clang::Type *record_type = cast<CXXMethodDecl>(decl)->getParent()->getTypeForDecl();
    pu_ty_idx = _builder->TB().ConvertType(decl->getType().getTypePtr(), FALSE, record_type);
  } else {
    pu_ty_idx = _builder->TB().ConvertType(decl->getType());
  }

  PU_Init(pu, pu_ty_idx, GLOBAL_SYMTAB + 1);
  if (_builder->Lang_C())
    Set_PU_c_lang(pu);
  else if (_builder->Lang_CPP())
    Set_PU_cxx_lang(pu);
  else
    Is_True(false, ("unknown language"));

  GVALinkage linkage = _builder->Context()->GetGVALinkageForFunction(decl);
  BOOL hasAliasAttr = decl->hasAttr<AliasAttr>();
  if (decl->isInlineSpecified() || decl->isInlined() ||
      (decl->getStorageClass() == SC_Static)) {
    Set_PU_is_inline_function(pu);
    Set_PU_is_marked_inline(pu);
    if (linkage == GVA_AvailableExternally || linkage == GVA_StrongExternal)
      Set_PU_is_extern_inline(pu);
  }

  if (decl->isMain()) {
    Set_PU_is_mainpu(pu);
    Set_PU_no_inline(pu);
  }

  if (kind == FUNC_CTOR)
    Set_PU_is_constructor(pu);

  if (decl->isNoReturn())
    Set_PU_has_attr_noreturn(pu);

  ty_idx = (TY_IDX) pu_idx;
  ST *st = New_ST(GLOBAL_SYMTAB);
  ST_EXPORT st_exp = ExportClass(decl);

  BOOL isDefined = decl->isDefined();
  if (decl->getTemplateSpecializationInfo() != NULL) {
    isDefined = TRUE;
  }
  if (hasAliasAttr) {
    isDefined = FALSE;
    st_exp = EXPORT_PREEMPTIBLE;
  }
  if (st_exp != EXPORT_LOCAL &&
      (linkage == GVA_DiscardableODR || linkage == GVA_StrongODR)) {
    // set export class to LOCAL so it can be deleted in inliner after inlining
    st_exp = EXPORT_LOCAL;
    Set_ST_is_odr(st);
  }

  // set no inline flag for specific functions
  for (int j = 0; j < NOINLINE_FUNC_MAX; j++) {
    const char *func_name = _noinline_func[j];
    if (!func_name)
      break;
    if (!strcmp(func_name, Index_To_Str(str_idx))) {
      Clear_PU_must_inline(pu);
      Set_PU_no_inline(pu);
      break;
    }
  }

  ST_Init(st, str_idx, CLASS_FUNC,
          isDefined ? SCLASS_TEXT : SCLASS_EXTERN,
          st_exp, ty_idx);
  if (kind == FUNC_THUNK) {
    Set_PU_is_thunk(pu);
  }

  // set weak
  const NamedDecl *named_decl = dyn_cast<NamedDecl>(decl);
  Is_True(named_decl != NULL, ("decl is not NamedDecl"));
  if (hasAliasAttr ||
      named_decl->hasAttr<WeakAttr>() ||
      named_decl->isWeakImported()) {
    Set_ST_is_weak_symbol(st);
    if (_builder->Lang_CPP()) {
      Set_ST_export(st, EXPORT_INTERNAL);
    }
  }

  return ST_st_idx(st);
}

ST_IDX
WhirlSymBuilder::ConvertParmVar(const ParmVarDecl *decl) {
  SYMTAB_IDX symtab = _builder->Scope().CurrentSymtab();
  Is_True(symtab > GLOBAL_SYMTAB,
          ("invalid scope for function param"));
  Is_True(_builder->Scope().CurrentDecl() != NULL &&
          (isa<FunctionDecl>(_builder->Scope().CurrentDecl())),
          ("invalid scope for function param"));
  ST *st = New_ST(symtab);
  STR_IDX str_idx = _builder->DeclBuilder().ConvertName(decl, getDeclName(decl));
  TY_IDX ty_idx = _builder->TB().ConvertType(decl->getType());
  ST_Init(st, str_idx, CLASS_VAR, SCLASS_FORMAL, EXPORT_LOCAL, ty_idx);
  Set_ST_is_value_parm(st);
  if (decl->hasAttr<UnusedAttr>())
    Set_ST_is_not_used(st);
  return ST_st_idx(st);
}

ST_IDX
WhirlSymBuilder::ConvertVar(const VarDecl *decl) {
  ST_SCLASS sclass;
  ST_EXPORT eclass = EXPORT_PREEMPTIBLE;
  SYMTAB_IDX symtab = _builder->Scope().CurrentSymtab();
  STR_IDX str_idx = _builder->DeclBuilder().ConvertName(decl, getDeclName(decl));
  if (decl->hasExternalStorage()) {
    sclass = SCLASS_EXTERN;
    symtab = GLOBAL_SYMTAB;
  } else {
    if (decl->isLocalVarDecl()) {
      eclass = EXPORT_LOCAL;
      symtab = CURRENT_SYMTAB;
      if (decl->isStaticLocal()) {
        sclass = SCLASS_PSTATIC;
        eclass = EXPORT_LOCAL;
      } else
        sclass = SCLASS_AUTO;
    } else {
      symtab = GLOBAL_SYMTAB;
      if (decl->getStorageClass() == SC_Static) {
        sclass = SCLASS_FSTATIC;
        eclass = EXPORT_LOCAL;
      } else {
        //After set INITO, set sclass to DGLOBAL
        sclass = SCLASS_UGLOBAL;
      }
    }
  }
  ST *st = New_ST(symtab);
  TY_IDX ty_idx = _builder->TB().ConvertType(decl->getType());
  ST_Init(st, str_idx, CLASS_VAR, sclass, eclass, ty_idx);
  GVALinkage linkage = _builder->Context()->GetGVALinkageForVariable(decl);
  if (eclass != EXPORT_LOCAL &&
      (linkage == GVA_DiscardableODR || linkage == GVA_StrongODR)) {
    Set_ST_is_odr(st);
  }
  if (decl->hasAttr<UnusedAttr>() || decl->isConstexpr())
    Set_ST_is_not_used(st);
  BOOL hasAsmAttr = decl->hasAttr<AsmLabelAttr>();
  if (hasAsmAttr) {
    // set st volatile
    Set_TY_is_volatile(ty_idx);
    Set_ST_type(st, ty_idx);
    // set st assigned to dedicated preg flag
    Set_ST_assigned_to_dedicated_preg(st);
    // create st_attr
    const clang::AsmLabelAttr *attr = decl->getAttr<AsmLabelAttr>();
    PREG_NUM preg = AsmDecodeRegisterName(attr->getLabel().data());
    ST_ATTR_IDX st_attr_idx;
    ST_ATTR& st_attr = New_ST_ATTR(symtab, st_attr_idx);
    ST_ATTR_Init(st_attr, ST_st_idx (st), ST_ATTR_DEDICATED_REGISTER, preg);
  }
  return ST_st_idx(st);
}

ST_IDX
WhirlSymBuilder::ConvertCXXConstructor(const CXXConstructorDecl *decl, CXXCtorType ctor) {
  TRACE_FUNC();
  STR_IDX name = _builder->DeclBuilder().ConvertName(decl, ctor);
  FUNC_KIND kind = (ctor == CXXCtorType::Ctor_Complete)
                     ? FUNC_CTOR : FUNC_NORMAL;
  return ConvertFunction(cast<FunctionDecl>(decl), name, kind);
}

ST_IDX
WhirlSymBuilder::ConvertCXXDestructor(const CXXDestructorDecl *decl, CXXDtorType dtor) {
  TRACE_FUNC();
  STR_IDX name = _builder->DeclBuilder().ConvertName(decl, dtor);
  return ConvertFunction(cast<FunctionDecl>(decl), name);
}

ST_IDX
WhirlSymBuilder::ConvertCXXMethod(const CXXMethodDecl *decl) {
  TRACE_FUNC();
  return ConvertFunction(cast<FunctionDecl>(decl));
}

ST_IDX
WhirlSymBuilder::ConvertCXXConversion(const CXXConversionDecl *decl) {
  TRACE_FUNC();
  return ConvertFunction(cast<FunctionDecl>(decl));
}

ST_IDX
WhirlSymBuilder::ConvertCXXRecord(const CXXRecordDecl *decl) {
  TRACE_FUNC();
  SYMTAB_IDX symtab = _builder->Scope().CurrentSymtab();
  Is_True(symtab > GLOBAL_SYMTAB,
          ("invalid scope for function param"));
  Is_True(_builder->Scope().CurrentDecl() != NULL &&
          _builder->Scope().CurrentDecl()->getKind() == Decl::CXXRecord,
          ("invalid scope for function param"));
  ST *st = New_ST(symtab);
  STR_IDX str_idx = _builder->DeclBuilder().ConvertName(decl, getDeclName(decl));
  TY_IDX ty_idx = _builder->TB().ConvertType(decl->getTypeForDecl());
  ST_Init(st, str_idx, CLASS_VAR, SCLASS_FORMAL, EXPORT_LOCAL, ty_idx);
  Set_ST_is_value_parm(st);
  return ST_st_idx(st);
}

// emit symbol for pure virtual function
ST_IDX
WhirlSymBuilder::EmitCXXPureVirtualST() {
  TRACE_FUNC();
  TY_IDX ty_idx = _builder->TB().EmitCXXPureVirtualType();

  PU_IDX pu_idx;
  PU &pu = New_PU(pu_idx);
  TY_IDX pu_ty_idx = ty_idx;
  PU_Init(pu, pu_ty_idx, GLOBAL_SYMTAB + 1);
  Set_PU_cxx_lang(pu);
  ty_idx = (TY_IDX) pu_idx;
  ST *st = New_ST(GLOBAL_SYMTAB);
  ST_Init (st, Save_Str("__cxa_pure_virtual"), CLASS_FUNC,
           SCLASS_TEXT, EXPORT_LOCAL, ty_idx);
  return ST_st_idx(st);
}

ST_IDX
WhirlSymBuilder::ConvertSymbol(const NamedDecl *decl) {
  TRACE_FUNC();
  // find existing st_idx
  ST_IDX st_idx = GetST(decl);
  if (st_idx != ST_IDX_ZERO)
    return st_idx;

  // convert the decl
  switch (decl->getKind()) {
    case Decl::Function:
      st_idx = ConvertFunction(cast<FunctionDecl>(decl));
      break;
    case Decl::ParmVar:
      st_idx = ConvertParmVar(cast<ParmVarDecl>(decl));
      break;
    case Decl::Var:
    case Decl::VarTemplateSpecialization:
    case Decl::Decomposition:
      st_idx = ConvertVar(cast<VarDecl>(decl));
      break;
    case Decl::CXXConstructor:
      // assume complete ctor
      st_idx = ConvertCXXConstructor(cast<CXXConstructorDecl>(decl),
                                     CXXCtorType::Ctor_Complete);
      break;
    case Decl::CXXDestructor:
      // assume complete dtor
      st_idx = ConvertCXXDestructor(cast<CXXDestructorDecl>(decl),
                                    CXXDtorType::Dtor_Complete);
      break;
    case Decl::CXXMethod:
      st_idx = ConvertCXXMethod(cast<CXXMethodDecl>(decl));
      break;
    case Decl::CXXConversion:
      st_idx = ConvertCXXConversion(cast<CXXConversionDecl>(decl));
      break;
    case Decl::CXXRecord:
      st_idx = ConvertCXXRecord(cast<CXXRecordDecl>(decl));
      break;
    default:
      st_idx = ST_IDX_ZERO;
      Is_True(false, ("unsupported decl: %s", decl->getDeclKindName()));
  }

  if (st_idx) {
    SRCPOS spos = _builder->SetSrcPos(decl->getLocation());
    Set_ST_Srcpos(*ST_ptr(st_idx), spos);
  }

  // add to map
  const Decl *canon_decl = decl->getCanonicalDecl();
  _st_map[canon_decl] = st_idx;

//    DST_INFO_IDX dst = _builder->DstBuilder().CreateDstForDecl(decl);
  
  return st_idx;
}

ST_IDX
WhirlSymBuilder::ConvertSymbol(const GlobalDecl gd) {
  TRACE_FUNC();

  // find existing st_idx
  ST_IDX st_idx = GetST(gd);
  if (st_idx != ST_IDX_ZERO)
    return st_idx;

  const Decl *decl = gd.getDecl();
  switch (decl->getKind()) {
  case Decl::CXXConstructor:
    st_idx = ConvertCXXConstructor(cast<CXXConstructorDecl>(decl),
                                   gd.getCtorType());
    break;
  case Decl::CXXDestructor:
    st_idx = ConvertCXXDestructor(cast<CXXDestructorDecl>(decl),
                                  gd.getDtorType());
    break;
  default:
    return ConvertSymbol(cast<NamedDecl>(decl));
  }

  if (st_idx) {
    SRCPOS spos = _builder->SetSrcPos(decl->getLocation());
    Set_ST_Srcpos(*ST_ptr(st_idx), spos);
  }

  // add to map
  Is_True(ST_IDX_level(st_idx) == GLOBAL_SYMTAB, ("not in global symtab"));
  decl = reinterpret_cast<const Decl *>(gd.getCanonicalDecl().getAsOpaquePtr());
  _st_map[decl] = st_idx;

  return st_idx;
}

ST_IDX
WhirlSymBuilder::ConvertThunkFunction(const NamedDecl *decl, STR_IDX mangled_name) {
  Is_True(isa<CXXMethodDecl>(decl), ("Thunk function should come from CXXMethodDecl"));
  Is_True(mangled_name != (STR_IDX)0, ("Invalid mangled name for thunk function"));

  // find existing st_idx
  ST_IDX st_idx = GetThunkST(mangled_name);
  if (st_idx != ST_IDX_ZERO)
    return st_idx;

  st_idx = ConvertFunction(cast<FunctionDecl>(decl), mangled_name,
                           FUNC_THUNK);
  _thunk_str_st_map[mangled_name] = st_idx;

  SRCPOS spos = _builder->SetSrcPos(decl->getLocation());
  Set_ST_Srcpos(*ST_ptr(st_idx), spos);
  return st_idx;
}

const ST_IDX
WhirlSymBuilder::GetThunkST(STR_IDX mangled_name) {
  THUNK_STR_ST_MAP::iterator it = _thunk_str_st_map.find(mangled_name);
  if (it != _thunk_str_st_map.end())
    return it->second;
  return (ST_IDX)0;
}

const ST_IDX
WhirlSymBuilder::GetST(const Decl *decl) {
  const Decl *canon_decl = decl->getCanonicalDecl();
  ST_IDX_MAP::iterator it = _st_map.find(canon_decl);
  if (it != _st_map.end())
    return it->second;
  return (ST_IDX)0;
}

const ST_IDX
WhirlSymBuilder::GetST(const GlobalDecl gd) {
  const Decl *decl = reinterpret_cast<const Decl *>(gd.getCanonicalDecl().getAsOpaquePtr());
  ST_IDX_MAP::iterator it = _st_map.find(decl);
  if (it != _st_map.end())
    return it->second;
  return (ST_IDX)0;
}

void
WhirlSymBuilder::RemoveLocalSymbol(SYMTAB_IDX level) {
  ST_IDX_MAP::iterator it;
  for (it = _st_map.begin(); it != _st_map.end(); ) {
    Is_True(ST_IDX_level(it->second) >= GLOBAL_SYMTAB &&
            ST_IDX_level(it->second) <= level, ("wrong level"));
    ST_IDX_MAP::iterator cur = it;
    ++ it;
    if (ST_IDX_level(cur->second) == level)
      _st_map.erase(cur);
  }
}

} // namespace wgen
