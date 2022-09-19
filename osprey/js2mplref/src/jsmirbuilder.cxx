/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/// Copyright [year] <Copyright Owner>
#include <string>
#include "../include/jsmirbuilder.h"

namespace maple {
// Create jsvalue_type as the JS::Value from mozjs-31.2.0/js/public/Value.h.
// We only consider littel_endian and 32-bit architectures here.
MIRType *JSMIRBuilder::CreateJSValueType() {
  return GlobalTables::GetTypeTable().GetDynany();
}

JSMIRFunction *JSMIRBuilder::CreateJSMain() {
  ArgVector arguments(mirModule->memPoolAllocator.Adapter());
  JSMIRFunction *jsmain = NULL;
  if (IsPlugin()) {
    jsmain = GetOrCreateFunction(GetWrapperName(), NULL, GlobalTables::GetTypeTable().GetDynany(), arguments, false);
    SetCurrentFunction(jsmain);
  } else {
    jsmain = GetOrCreateFunction("main", NULL, GlobalTables::GetTypeTable().GetInt32(), arguments, false);
    SetCurrentFunction(jsmain);
    MapleVector<BaseNode*> arguments(mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
    IntrinsiccallNode *stmt = CreateStmtIntrinsicCallAssigned((MIRIntrinsicId)INTRN_JS_INIT_CONTEXT, arguments, (MIRSymbol*)NULL);
    stmt->srcPosition.SetLinenum(1);
    AddStmtInCurrentFunctionBody(stmt);
  }
  jsmain->srcPosition.SetLinenum(1);
  return jsmain;
}

void JSMIRBuilder::InitBuiltinMethod() {
#define DEFBUILTINMETHOD(name, intrn_code, need_this) #name,
  const char *name[18] = {
#include "../include/builtinmethod.def"
  };
#undef DEFBUILTINMETHOD

  for (uint32_t i = 0; i < sizeof(name) / sizeof(const char *); i++) {
    if (!name[i]) {
      return;
    }
    MIRSymbol *st = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
    st->SetNameStridx(GetOrCreateStringIndex(name[i]));
    if (!GlobalTables::GetGsymTable().AddToStringSymbolMap(st)) {
      return;
    }
    st->storageClass = kScText;
    st->sKind = kStFunc;
    InsertGlobalName((char *)name[i]);
  }
}

void JSMIRBuilder::Init() {
  jsvalueType = CreateJSValueType();
  jsvalue_ptr_ = GlobalTables::GetTypeTable().GetOrCreatePointerType(jsvalueType);

  InitGlobalName();
  // InitBuiltinMethod();
  // Now we create the main function as jsmain.
  // If the script is called by another program, the name should be jsmain.
  jsmain_ = CreateJSMain();
  char *name = "main";
  if (IsPlugin()) {
    name = GetWrapperName();
  }
  InsertGlobalName(name);
}

JSMIRFunction *JSMIRBuilder::GetFunction(const char *name) {
  JSMIRFunction *fn = GetFunc(name);
  if (!fn) {
    assert(false && "function is not created");
  }
  return fn;
}

JSMIRFunction *JSMIRBuilder::GetOrCreateFunction(const char *name, JSFunction *jsfunc, MIRType *returnType, 
                                                 const ArgVector &arguments, bool isvarg) {
  //DEBUGPRINTsv2("GetOrCreateFunction", name);
  JSMIRFunction *fn = GetFunc(name);
  if (fn) {
    DEBUGPRINTsv2("function is alread created: ", name);
    return fn;
  }

  MapleString fname(name, mirModule->memPool);
  MIRSymbol *funcst = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  gstridx_t stridx = GetOrCreateStringIndex(fname);
  stidx_t stidx = GlobalTables::GetGsymTable().GetStIdxFromStrIdx(stridx);
  //DEBUGPRINT3(fname);
  //DEBUGPRINT3(stridx.GetIdx());
  //DEBUGPRINT3(stidx.Idx());

  funcst->SetNameStridx(stridx);
  if (!GlobalTables::GetGsymTable().AddToStringSymbolMap(funcst)) {
    return NULL;
  }
  funcst->storageClass = kScText;
  funcst->sKind = kStFunc;

  fn = mirModule->memPool->New<JSMIRFunction>(mirModule, jsfunc, funcst->GetStIdx());
  fn->Init();
  fn->puIdx = GlobalTables::GetFunctionTable().funcTable.size();
  GlobalTables::GetFunctionTable().funcTable.push_back(fn);

  std::vector<TyIdx> funcvectype;
  std::vector<TypeAttrs> funcvecattr;
  for (uint32 i = 0; i < arguments.size(); i++) {
    MapleString var(arguments[i].first, mirModule->memPool);
    MIRSymbol *argst = fn->symTab->CreateSymbol(kScopeLocal);
    argst->SetNameStridx(GetOrCreateStringIndex(var));
    MIRType *ty = arguments[i].second;
    argst->SetTyIdx(ty->tyIdx);
    argst->storageClass = kScFormal;
    argst->sKind = kStVar;
    fn->symTab->AddToStringSymbolMap(argst);
    fn->AddArgument(argst);
    funcvectype.push_back(ty->tyIdx);
    funcvecattr.push_back(TypeAttrs());
  }
  MIRFuncType *funcType = static_cast<MIRFuncType *>(GlobalTables::GetTypeTable().GetOrCreateFunctionType(mirModule, returnType->tyIdx, funcvectype, funcvecattr, isvarg));
  funcst->SetTyIdx(funcType->tyIdx);
  funcst->SetFunction(fn);
  fn->funcType = funcType;

  if (strcmp(name, "main") == 0 || jsfunc != NULL) {
    //fn->body = fn->codeMemPool->New<BlockNode>();
    fn->NewBody();
  }
  AddNameFunc(name, fn);

  //DEBUGPRINTsv2("function created: ", name);
  //DEBUGPRINT2(fn);
  return fn;
}

void JSMIRBuilder::AddStmtInCurrentFunctionBody(StmtNode *n) {
  MIRBuilder::AddStmtInCurrentFunctionBody(n);
  n->srcPosition.SetFilenum(1); // assume the first file
  DEBUGPRINTnode(n);
}

NaryStmtNode *JSMIRBuilder::CreateStmtReturn(BaseNode *rval, bool adjType, unsigned linenum) {
  NaryStmtNode *stmt = MIRBuilder::CreateStmtReturn(rval);
  stmt->srcPosition.SetLinenum(linenum);
  AddStmtInCurrentFunctionBody(stmt);

  JSMIRFunction *func = GetCurrentFunction();
  if (adjType && !IsMain(func) && rval->op == OP_dread) {
    DEBUGPRINTsv2("modify _return_type", (rval->op));
    AddrofNode *dn = (AddrofNode *)rval;
    stidx_t stidx = dn->stIdx;
    MIRSymbol *var = mirModule->CurFunction()->GetLocalOrGlobalSymbol(stidx);
    MIRType *type = var->GetType();
    DEBUGPRINT3(type);
    int fid = dn->fieldID;
    if (fid) {
      MIRStructType *stype = static_cast<MIRStructType *>(type);
      // fieldid in a structure starts from 1 while type vector starts from 0
      type = stype->GetElemType(fid - 1);
    }
  }
  return stmt;
}

void JSMIRBuilder::UpdateFunction(JSMIRFunction *func, MIRType *returnType, const ArgVector &arguments) {
  TyIdx retTyIdx;
  if (returnType) {
    retTyIdx = returnType->tyIdx;
    func->SetReturnTyIdx(retTyIdx);
  }
  else {
    retTyIdx = func->GetReturnTyIdx();
  }

  std::vector<TyIdx> paramTypeList;
  std::vector<TypeAttrs> paramAttrsList;
  for (uint32 i = 0; i < arguments.size(); i++) {
    MapleString var(arguments[i].first, mirModule->memPool);
    MIRSymbol *st = func->symTab->CreateSymbol(kScopeLocal);
    st->SetNameStridx(GetOrCreateStringIndex(var));
    MIRType *ty = arguments[i].second;
    st->SetTyIdx(ty->tyIdx);
    st->storageClass = kScFormal;
    st->sKind = kStVar;
    func->symTab->AddToStringSymbolMap(st);
    func->AddArgument(st);
    paramTypeList.push_back(ty->tyIdx);
    paramAttrsList.push_back(TypeAttrs());
  }
  assert(func->formalDefVec.size() == paramTypeList.size());

  // update func st type
  MIRFuncType funcType(retTyIdx, paramTypeList, paramAttrsList);
  func->GetFuncSymbol()->SetTyIdx(GlobalTables::GetTypeTable().CreateMIRTypeNode(&funcType)->tyIdx);
}

#if 0
void JSMIRBuilder::SaveReturnValue(MIRSymbol *var)
{
    DEBUGPRINT4("in SaveReturnValue")

    BaseNode *bn = CreateExprRegread(GlobalTables::GetTypeTable().type_table_[var->GetTyIdx().idx]->GetPrimType(), -kSregRetval0);
    StmtNode *stmt = CreateStmtDassign(var, 0, bn);
    stmt->srcPosition.SetLinenum(lineNo);
    MIRBuilder::AddStmtInCurrentFunctionBody(stmt);
}

#endif

StmtNode *JSMIRBuilder::CreateStmtDassign(MIRSymbol *symbol, fldid_t fieldId, BaseNode *src, unsigned linenum) {
  //DEBUGPRINT4("in CreateStmtDassign")

  StmtNode *stmt = MIRBuilder::CreateStmtDassign(symbol, fieldId, src);
  stmt->srcPosition.SetLinenum(linenum);
  AddStmtInCurrentFunctionBody(stmt);
  return stmt;
}

IntrinsiccallNode *JSMIRBuilder::CreateStmtIntrinsicCall1N(MIRIntrinsicId idx, BaseNode *arg0,
                                                           MapleVector<BaseNode *> &args) {
  IntrinsiccallNode *stmt = mirModule->CurFuncCodeMemPool()->New<IntrinsiccallNode>(mirModule, OP_intrinsiccall);
  MapleVector<BaseNode *> arguments(mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(arg0);
  for (int i = 0; i < args.size(); i++) {
    arguments.push_back(args[i]);
  }
  stmt->intrinsic = idx;
  stmt->nOpnd = arguments;
  return stmt;
}

}  // namespace maple
