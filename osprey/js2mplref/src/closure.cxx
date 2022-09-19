/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/// Copyright [year] <Copyright Owner>
#include <string>
#include "js/src/vm/ScopeObject.h"
#include "../include/closure.h"

namespace maple {

bool JSClosure::IsFuncModified(char *name) {
  std::vector<char *>::iterator i;
  for (i = funcMod.begin(); i != funcMod.end(); i++)
    if (!strcmp(name, *i)) {
      return true;
    }
  return false;
}

void JSClosure::UpdateFuncMod(char *name) {
  if (scope_->IsFunction(name))
    if (!IsFuncModified(name)) {
      funcMod.push_back(name);
    }
}

// ??? Use scope chain.
MIRSymbol *JSClosure::GetSymbolFromEnclosingScope(JSMIRFunction *func, const char *name) {
  gstridx_t idx = jsbuilder_->GetStringIndex(name);
  if (idx == 0) {
    return NULL;
  }

  MIRSymbol *st = func->symTab->GetSymbolFromStrIdx(idx);
  if (st) {
    return st;
  }
  st = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(idx);
  if (st) {
    return st;
  }
  return NULL;
}

MIRSymbol *JSClosure::GetSymbolFromEnclosingScope(JSMIRFunction *func, stidx_t stidx) {
  if (stidx.FullIdx() == 0) {
    return NULL;
  }

  MIRSymbol *st = func->symTab->GetSymbolFromStIdx(stidx.Idx());
  if (st) {
    return st;
  }
  st = GlobalTables::GetGsymTable().GetSymbolFromStIdx(stidx.Idx());
  if (st) {
    return st;
  }
  return NULL;
}

MIRType *JSClosure::GetOrCreateEnvType(JSMIRFunction *func) {
  std::stringstream ss;
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  ss << funcSt->GetName();
  std::string envName = ss.str() + "_env_type";
  DEBUGPRINT2(envName);

  if (func->envtype) {
    DEBUGPRINTsv2("env for func has been setup!", envName);
    return func->envtype;
  }

  FieldVector envFields;
  FieldVector prntFields;

  gstridx_t argnums = jsbuilder_->GetOrCreateStringIndex("argnums");
  envFields.push_back(FieldPair(argnums, TyidxFieldAttrPair(GlobalTables::GetTypeTable().GetUInt32()->tyIdx, FieldAttrs())));

  gstridx_t parentenv = jsbuilder_->GetOrCreateStringIndex("parentenv");
  if (func->scope->IsTopLevel()) {
    envFields.push_back(FieldPair(parentenv, TyidxFieldAttrPair(GlobalTables::GetTypeTable().GetVoidPtr()->tyIdx, FieldAttrs())));
  } else {
    ScopeNode *sn = func->scope;
    JSMIRFunction *parent = sn->GetParentFunc();
    MIRType *parentenvType = parent->envtype;
    DEBUGPRINT3(parentenvType);
    MIRType *envptr = GlobalTables::GetTypeTable().GetOrCreatePointerType(parentenvType);
    DEBUGPRINT3(envptr);
    func->penvtype = parentenvType;
    func->penvptr = envptr;
    envFields.push_back(FieldPair(parentenv, TyidxFieldAttrPair(envptr->tyIdx, FieldAttrs())));
  }

  MIRType *envType = GlobalTables::GetTypeTable().CreateStructType(envName.c_str(), envFields, prntFields, module_);
  DEBUGPRINT2(envName);
  DEBUGPRINT2(envType);
  MIRStructType *stf = (MIRStructType *)(envType);
  DEBUGPRINT2(stf->GetElemType(0));
  gstridx_t idxf = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx())->GetNameStridx();
  DEBUGPRINT2(idxf.GetIdx());
  DEBUGPRINT2(func->stIdx.FullIdx());

  func->envtype = envType;
  func->envptr = GlobalTables::GetTypeTable().GetOrCreatePointerType(envType);

  return envType;
}

void JSClosure::AddAliasToEnvType(MIRType *envType, char *name, MIRType *t) {
  DEBUGPRINTsv3("add env", name);
  gstridx_t stridx = jsbuilder_->GetOrCreateStringIndex(name);
  MIRStructType *envStruct = static_cast<MIRStructType *>(envType);

  envStruct->fields.push_back(FieldPair(stridx, TyidxFieldAttrPair(t->tyIdx, FieldAttrs())));
}

void JSClosure::AddFuncFormalsToEnvType(JSMIRFunction *func) {
  // add formal parameters to env, they are posible aliased vars
  typedef std::pair<JSFunction *, std::vector<JSAtom *>> funcVarVecPair;
  std::vector<funcVarVecPair> formals = jsscript_->funcFormals;

  char *name;
  JSFunction *jsfun;
  std::vector<funcVarVecPair>::iterator i;
  std::vector<char *> nameVec;
  for (i = formals.begin(); i != formals.end(); i++) {
    jsfun = (*i).first;
    DEBUGPRINT2(jsfun);
    if (func->jsfun == jsfun) {
      std::vector<JSAtom *> args = (*i).second;
      std::vector<JSAtom *>::iterator ia;
      DEBUGPRINT3((args.size()));
      for (ia = args.begin(); ia != args.end(); ia++) {
        name = Util::GetString(*ia, mp_, jscontext_);
        nameVec.push_back(name);
        AddAliasToEnvType(func->envtype, name, jsvalueType);
      }
      funcArgPair p(func, nameVec);
      funcFormals.push_back(p);
      break;
    }
  }

  return;
}

JSMIRFunction *JSClosure::ProcessFunc(JSFunction *jsfun, char *funcname) {
  MIRType *retuenType = jsvalueType;
  ArgVector arguments(module_->GetMPAllocator().Adapter());

  JSMIRFunction *func = jsbuilder_->GetOrCreateFunction(funcname, jsfun, retuenType,
                                                        arguments, false);
  module_->AddFunction(func);
  SetJSMIRFunc(funcname, func);
  DEBUGPRINT2(funcname);
  DEBUGPRINT2(func);

  ScopeNode *sn = scope_->GetOrCreateSN(funcname);
  sn->SetFunc(func);

  func->initAliasList();
  arguments.push_back(ArgPair("_this", GlobalTables::GetTypeTable().GetDynany()));

  DEBUGPRINT3((sn->IsWithEnv()));
  if (sn->IsWithEnv()) {
    GetOrCreateEnvType(func);
    AddFuncFormalsToEnvType(func);
  }

  if (!sn->IsTopLevel() && (sn->IsWithEnv() || sn->UseAliased())) {
    if (!func->with_env_arg) {
      JSMIRFunction *parent = sn->GetParentFunc();
      arguments.push_back(ArgPair("_env", GlobalTables::GetTypeTable().GetDynany()));
      DEBUGPRINTsv3("_env", funcname);
      func->with_env_arg = true;
    }
  }

  typedef std::pair<JSFunction *, std::vector<JSAtom *>> funcVarVecPair;
  std::vector<funcVarVecPair> formals = jsscript_->funcFormals;

  func->argc = 0;
  for (int j = 0; j < formals.size(); j++) {
    JSFunction *fun = formals[j].first;
    if (fun == jsfun) {
      std::vector<JSAtom *> args = formals[j].second;
      func->argc = args.size();
      for (uint32 i = 0; i < jsfun->nargs(); i++) {
        char *name = Util::GetString(args[i], mp_, jscontext_);
        DEBUGPRINT3(name);
        MapleString argname(name, module_->memPool);
        arguments.push_back(ArgPair(argname.c_str(), GlobalTables::GetTypeTable().GetDynany()));
      }
      break;
    }
  }

  jsbuilder_->UpdateFunction(func, NULL, arguments);

  std::pair<JSScript *, JSMIRFunction *> p(jsfun->nonLazyScript(), func);
  scriptstack_.push(p);

  return func;
}

// JSOP_DEFFUN 127
bool JSClosure::ProcessOpDefFun(jsbytecode *pc) {
  JSFunction *jsfun = currscr_->getFunction(GET_UINT32_INDEX(pc));
  JSScript *scr = jsfun->nonLazyScript();
  MIRType *retuenType = jsvalueType;
  ArgVector arguments(module_->GetMPAllocator().Adapter());
  JSAtom *atom = jsfun->displayAtom();
  DEBUGPRINT2(atom);
  JSMIRFunction *parent = jsbuilder_->GetCurrentFunction();
  const char *name = Util::GetString(atom, mp_, jscontext_);
  if (!name) {
    return false;
  }
  const char *par_name = parent ? parent->GetName().c_str() : NULL;
  char *funcname = Util::GetFuncName(par_name, name, 0, mp_);
  JSMIRFunction *func = ProcessFunc(jsfun, funcname);

  funcstack_.push(func);

  return true;
}

// JSOP_LAMBDA 131
void JSClosure::ProcessOpLambda(jsbytecode *pc) {
  JSFunction *jsfun = currscr_->getFunction(GET_UINT32_INDEX(pc));
  JSAtom *atom = jsfun->displayAtom();

  // isLambda() does not seem reliable
  DEBUGPRINT3((jsfun->isLambda()));
  // we already know it is a Lambda so only check other two parts in isNamedLambda()
  // isLambda() && displayAtom() && !hasGuessedAtom()
  // if((jsfun->isNamedLambda()))
  JSMIRFunction *parent = jsbuilder_->GetCurrentFunction();
  const char *par_name = parent ? parent->GetName().c_str() : NULL;
  const char *name = (atom && !jsfun->hasGuessedAtom()) ?
                       Util::GetString(atom, mp_, jscontext_) : NULL;

  char *funcname = Util::GetFuncName(par_name, name,
                                     scope_->GetAnonyidx(jsfun), mp_);
  ProcessFunc(jsfun, funcname);

  return;
}

bool JSClosure::IsLocalVar(JSMIRFunction *func, char *name) {
  typedef std::pair<JSFunction *, std::vector<JSAtom *>> funcVarVec;
  std::vector<funcVarVec> locals = jsscript_->funcLocals;

  JSFunction *jsfun;
  std::vector<funcVarVec>::iterator i;
  std::vector<char *> nameVec;
  for (i = locals.begin(); i != locals.end(); i++) {
    jsfun = (*i).first;
    if (func->jsfun == jsfun) {
      std::vector<JSAtom *> vars = (*i).second;
      std::vector<JSAtom *>::iterator ia;
      DEBUGPRINT3((vars.size()));
      char *varname;
      for (ia = vars.begin(); ia != vars.end(); ia++) {
        varname = Util::GetString(*ia, mp_, jscontext_);
        DEBUGPRINT2(varname);
        if (strcmp(name, varname) == 0) {
          return true;
        }
      }
      return false;
    }
  }
  return false;
}

char *JSClosure::GetLocalVar(JSMIRFunction *func, uint32_t localNo) {
  typedef std::pair<JSFunction *, std::vector<JSAtom *>> funcVarVecPair;
  std::vector<funcVarVecPair> locals = jsscript_->funcLocals;

  char *name = NULL;
  JSFunction *jsfun;
  int i;
  for (i = 0; i < locals.size(); i++) {
    jsfun = locals[i].first;
    if (func->jsfun == jsfun) {
      std::vector<JSAtom *> args = locals[i].second;
      if (localNo < args.size()) {
        name = Util::GetString(args[localNo], mp_, jscontext_);
      }
      break;
    }
  }
  if (!name) {
    name = Util::GetSequentialName("local_var_", localNo, mp_);
  }
  return name;
}

// JSOP_GETALIASEDVAR 136
void JSClosure::ProcessAliasedVar(jsbytecode *pc) {
  JSAtom *atom = ScopeCoordinateName(jscontext_->runtime()->scopeCoordinateNameCache, currscr_, pc);
  JSMIRFunction *func = funcstack_.top();
  DEBUGPRINT2((func->scope->GetName()));
  char *name = Util::GetString(atom, mp_, jscontext_);
  if (!name) {
    return;
  }
  DEBUGPRINT3(name);
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  const char *funcname = funcSt->GetName().c_str();
  ScopeNode *sn = scope_->GetOrCreateSN((char *)funcname);
  ScopeNode *psn = sn->GetParent();

  if (!psn) {
    DEBUGPRINT3("alias var not found, could be from block");
    return;
  }

  JSMIRFunction *parent = psn->GetFunc();

  int idx = 0;
  MIRType *envType = NULL;

  // check if this alias is in current func's env
  if (sn->IsWithEnv()) {
    envType = func->envtype;
    idx = jsbuilder_->GetStructFieldIdFromFieldName(envType, name);

    if (idx) {
      return;
    }
  }

  // check if this alias is a local var need to be added into env
  if (sn->IsWithEnv() && IsLocalVar(func, name)) {
    envType = func->envtype;
    AddAliasToEnvType(envType, name, jsvalueType);
    idx = jsbuilder_->GetStructFieldIdFromFieldName(envType, name);
    DEBUGPRINT3(idx);
    return;
  }

  // recursively search in parent's alias list
  JSMIRFunction *p = parent;
  while (p != jsmain_ && !idx && psn->IsWithEnv()) {
    envType = p->envtype;
    idx = jsbuilder_->GetStructFieldIdFromFieldName(envType, name);
    DEBUGPRINT3(idx);
    if (idx) {
      return;
    }
    psn = psn->GetParent();

    if (psn) {
      p = psn->GetFunc();
    }
  }

  if (!idx) {
    DEBUGPRINT3("alias var not found, could be from block");
  }

  return;
}

void JSClosure::CloseFuncBookKeeping() {
  funcstack_.pop();

  // process inner functions
  DEBUGPRINT3((scriptstack_.size()));
  while (scriptstack_.size()) {
    JSScript *scr = scriptstack_.top().first;
    JSMIRFunction *lambda = scriptstack_.top().second;
    jsbuilder_->SetCurrentFunction(lambda);
    funcstack_.push(lambda);
    scriptstack_.pop();

    Build(scr);
  }

  if (funcstack_.size()) {
    JSMIRFunction *currFunc = funcstack_.top();
    jsbuilder_->SetCurrentFunction(currFunc);
  }
}

void JSClosure::Init() {
  jsmain_ = jsbuilder_->jsmain_;
  funcstack_.push(jsmain_);
  char *name = "main";
  if (jsbuilder_->IsPlugin()) {
    name = jsbuilder_->GetWrapperName();
  }
  // TODO: since GetOrCreateSN() doesn't change name, so we should declare the parameter as const name
  scope_->GetOrCreateSN(name)->SetFunc(jsmain_);

  jsvalueType = jsbuilder_->jsvalueType;
  jsvalue_ptr_ = jsbuilder_->jsvalue_ptr_;
}

bool JSClosure::Build(JSScript *script) {
  jsbytecode *start = script->code();
  jsbytecode *end = script->codeEnd();
  currscr_ = script;

  bool ret = BuildSection(script, start, end);

  return ret;
}

bool JSClosure::BuildSection(JSScript *script, jsbytecode *pcstart, jsbytecode *pcend) {
  jsbytecode *pc = pcstart;
  JSFunction *jsfun;

  char *name;
  char *parent;
  JSOp lastOp;
  while (pc < pcend) {
    JSOp op = JSOp(*pc);
    JSScript *scr;
    DEBUGPRINT3((Util::getOpcodeName[op]));

    Util::SetIndent(4);

    switch (op) {
      case JSOP_DEFFUN: { /*127, 5, 0, 0*/
        ProcessOpDefFun(pc);
        break;
      }
      case JSOP_LAMBDA: { /*130, 5, 0, 1*/
        ProcessOpLambda(pc);
        break;
      }
      case JSOP_GETALIASEDVAR:   /*136, 5, 0, 1*/
      case JSOP_SETALIASEDVAR: { /*137, 5, 1, 1*/
        ProcessAliasedVar(pc);
        break;
      }
    }

    // check if func is reassigned
    switch (op) {
      case JSOP_SETLOCAL: {
        uint32_t i = GET_LOCALNO(pc);
        JSMIRFunction *func = jsbuilder_->GetCurrentFunction();
        char *name = GetLocalVar(func, i);
        // not count the init
        if (lastOp != JSOP_LAMBDA) {
          UpdateFuncMod(name);
        }
        break;
      }
      case JSOP_SETGNAME:
      case JSOP_SETNAME: {
        JSAtom *atom = script->getName(pc);
        char *name = Util::GetString(atom, mp_, jscontext_);
        UpdateFuncMod(name);
        break;
      }
    }

    lastOp = op;
    pc = js::GetNextPc(pc);
  }

  if (lastOp == JSOP_RETRVAL) {
    CloseFuncBookKeeping();
  }

  return true;
}

bool JSClosure::FuncUseEnv(char *name) {
  ScopeNode *sn = scope_->GetOrCreateSN(name);
  return sn->IsWithEnv() || sn->UseAliased();
}

JSMIRFunction *JSClosure::GetJSMIRFunc(char *name) {
  std::vector<std::pair<char *, JSMIRFunction *>>::iterator i;
  for (i = nameJSMIRfunc_.begin(); i != nameJSMIRfunc_.end(); i++) {
    if (strcmp(name, (*i).first) == 0) {
      return (*i).second;
    }
  }
  return NULL;
}

void JSClosure::SetJSMIRFunc(char *name, JSMIRFunction *func) {
  if (GetJSMIRFunc(name)) {
    return;
  }
  std::pair<char *, JSMIRFunction *> p(name, func);
  nameJSMIRfunc_.push_back(p);
}

}  // namespace maple
