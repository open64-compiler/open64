/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/// Copyright [year] <Copyright Owner>
#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <assert.h>
#include "js/src/jsscript.h"
#include "js/src/jsopcode.h"
#include "js/src/jsfun.h"
#include "js/src/jsatom.h"
#include "js/src/jscntxt.h"
#include "js/src/jsatominlines.h"
#include "js/src/vm/ScopeObject.h"
#include "../include/compiler.h"
#include "maple_ir/include/bin_mplt.h"

//const maple::OpcodeTable maple::kOpcodeInfo;

namespace maple {

enum js_builtin_string_id {
#define JSBUILTIN_STRING_DEF(id, length, str) id,
#include "../include/jsbuiltinstrings.inc.h"
#undef JSBUILTIN_STRING_DEF
  JSBUILTIN_STRING_ID_COUNT
};

void JSCompiler::Init() {
  jsmain_ = jsbuilder_->jsmain_;
  jsvalueType = jsbuilder_->jsvalueType;
  jsvalue_ptr_ = jsbuilder_->jsvalue_ptr_;

  // push main() on funcstack_
  char *name = "main";
  if (jsbuilder_->IsPlugin()) {
    name = jsbuilder_->GetWrapperName();
  }
  scope_->GetOrCreateSN(name)->SetFunc(jsmain_);
  funcstack_.push(jsmain_);
  jsbuilder_->SetCurrentFunction(jsmain_);
  DEBUGPRINT2(jsmain_);

  funcFormals = closure_->funcFormals;

  dummyNode = CompileOpConstValue(JSTYPE_UNDEFINED, 0);
}

void JSCompiler::Finish(const std::string& outFileName) {
  if (jsbuilder_->WithMain()) {
    module_->AddFunction(jsmain_);  // add jsmain_ in the end
  }

  // write text format .mpl file with Emit()
  // module_->Emit(outFileName);
  // write binary format .bpl file with BinaryMplt::Export()
  module_->flavor = kFeProduced;
  module_->numFuncs = module_->functionList.size();
  BinaryMplt binMplt(*module_);
  binMplt.GetBinExport().not2mplt = true;
  binMplt.Export(outFileName);


  // more forgiving about stack integrety
  int expected = scope_->GetDepth();
  DEBUGPRINT2(opstack_->GetDepth());
  DEBUGPRINT2(expected);
  assert(opstack_->CheckDepth(0) || opstack_->CheckDepth(expected));
}

void JSCompiler::SetupMainFuncRet(BaseNode *rval) {
  jsbuilder_->CreateStmtReturn(rval, false, linenum_);
}

static bool IsCCall(const char *name) {
  return (strcmp(name, "ccall") == 0);
}

static bool IsXcCall(const char *name) {
  return (strcmp(name, "xccall") == 0);
}

MIRSymbol *JSCompiler::CreateTempVar(MIRType *type) {
  const char *name = Util::GetSequentialName("temp_var_", temp_var_no_, mp_);
  MIRSymbol *var = jsbuilder_->GetOrCreateLocalDecl(name, type);
  return var;
}

MIRSymbol *JSCompiler::CreateTempJSValueTypeVar() {
  return CreateTempVar(jsvalueType);
}

void JSCompiler::InitWithUndefined(bool doit, MIRSymbol *var) {
#if 0
    if (doit) {
        BaseNode *undefined = CompileOpConstValue(JSTYPE_UNDEFINED, 0);
        BaseNode *stmt = jsbuilder_->CreateStmtDassign(var, 0, undefined);
    }
#endif
}

uint32_t JSCompiler::GetFieldidFromTag(uint32_t tag) {
  char *tagname;
  switch (tag) {
    case JSTYPE_BOOLEAN:
      tagname = "boo";
      break;
    case JSTYPE_STRING:
      tagname = "str";
      break;
    case JSTYPE_OBJECT:
      tagname = "prop_list";
      break;
    default:
      tagname = "i32";
      break;
  }
  return jsbuilder_->GetStructFieldIdFromFieldName(jsvalueType, tagname);
}

MIRType *JSCompiler::DetermineTypeFromNode(BaseNode *node) {
  TyIdx tyidx;
  if (kOpcodeInfo.IsCompare(node->op)) {
    return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_u1);
  }
  if (node->op == OP_intrinsicop) {
    // TODO: look up intrinsic table
    MIRIntrinsicId intrnid = static_cast<IntrinsicopNode *>(node)->intrinsic;
    IntrinDesc *intrndesc = &IntrinDesc::intrintable[intrnid];
    return intrndesc->GetTypeFromArgTy(intrndesc->argtypes_[0]);
  }
  if (node->op == OP_dread) {
    AddrofNode *dread = static_cast<AddrofNode *>(node);
    MIRSymbol *st = module_->CurFunction()->GetLocalOrGlobalSymbol(dread->stIdx);
    tyidx = st->GetTyIdx();
  } else if (node->op == OP_iread) {
    IreadNode *iread = static_cast<IreadNode *>(node);
    tyidx = iread->tyIdx;
  } else {
    tyidx = TyIdx(node->primType);
  }
  if (tyidx == jsvalueType->tyIdx) {
    return jsvalueType;
  }

  return GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyidx);
}

// create a new temporary, store expr to the temporary and return the temporary
MIRSymbol *JSCompiler::SymbolFromSavingInATemp(BaseNode *expr, bool jsvalueP) {
  MIRType *exprty;
  if (jsvalueP) {
    exprty = jsvalueType;
    expr = CheckConvertToJSValueType(expr);
  } else {
    exprty = GlobalTables::GetTypeTable().GetPrimType(expr->primType);
  }
  MIRSymbol *tempVar = CreateTempVar(exprty);
  jsbuilder_->CreateStmtDassign(tempVar, 0, expr, linenum_);
  return tempVar;
}

// create a new temporary, store expr to the temporary and return a dread node
// of the new temporary
AddrofNode *JSCompiler::NodeFromSavingInATemp(BaseNode *expr) {
  MIRSymbol *tempVar = SymbolFromSavingInATemp(expr, false);
  return jsbuilder_->CreateExprDread(tempVar->GetType(), tempVar);
}

// JSOP_UNDEFINED 1
// JSOP_ZERO 62
// JSOP_ONE 63
// JSOP_UINT16 88
// JSOP_UINT24 188
// JSOP_INT8 215
// JSOP_INT32 216
BaseNode *JSCompiler::CompileOpConstValue(uint32_t jsvalueTag, int32_t payload) {
  PrimType pty;
  switch (jsvalueTag) {
    case JSTYPE_NUMBER:
      return jsbuilder_->CreateIntConst((uint64_t)(uint32_t)payload, PTY_i32);
    case JSTYPE_UNDEFINED:
      pty = PTY_dynundef;
      break;
    case JSTYPE_NULL:
      pty = PTY_dynnull;
      break;
    case JSTYPE_BOOLEAN:
      pty = PTY_dynbool;
      break;
    case JSTYPE_NONE:
      pty = PTY_dynnone;
      break;
    default:
      assert(false && "NIY");
      break;
  }
  //int64_t val = (int64_t)((uint64_t)(uint32_t)jsvalueTag << 32 | (uint64_t)(uint32_t)payload);
  int64_t val = (uint64_t)(uint32_t)payload;
  return jsbuilder_->CreateIntConst(val, pty);
}

// JSOP_DOUBLE
BaseNode *JSCompiler::CompileDoubleConst(double value) {
  return jsbuilder_->CreateDoubleConst(value);
}

// Return the corresponding intrinsic code for JSop binary or unary opcode.
uint32_t JSCompiler::FindIntrinsicForOp(JSOp opcode) {
  static const uint32_t opToIntrinsic[][2] = { { JSOP_STRICTEQ, INTRN_JSOP_STRICTEQ },
                                               { JSOP_STRICTNE, INTRN_JSOP_STRICTNE },
                                               { JSOP_INSTANCEOF, INTRN_JSOP_INSTANCEOF },
                                               { JSOP_IN, INTRN_JSOP_IN },
                                               { JSOP_POS, INTRN_JS_NUMBER },
                                               { JSOP_OR, INTRN_JSOP_OR },
                                               { JSOP_AND, INTRN_JSOP_AND },
                                               { JSOP_TYPEOF, INTRN_JSOP_TYPEOF } };
  int32_t intrinsicCode = -1;
  for (uint32_t i = 0; i < sizeof(opToIntrinsic) / 8; i++) {
    if (opToIntrinsic[i][0] == opcode) {
      intrinsicCode = opToIntrinsic[i][1];
      break;
    }
  }
  assert(intrinsicCode >= 0);
  return (uint32_t)intrinsicCode;
}

// JSOP_BITOR JSOP_BITXOR JSOP_BITAND JSOP_EQ JSOP_NE JSOP_LT JSOP_GT
// JSOP_GE JSOP_LSH JSOP_RSH JSOP_URSH JSOP_ADD JSOP_SUB JSOP_MUL JSOP_DIV
// JSOP_MOD 15 ~ 31
// JSOP_STRICTEQ 72
// JSOP_STRICTNE 73
BaseNode *JSCompiler::CompileOpBinary(JSOp opcode, BaseNode *op0, BaseNode *op1) {
  Opcode mop = (Opcode)0;
  MIRType *restype = GlobalTables::GetTypeTable().GetDynany();
  switch (opcode) {
    case JSOP_BITOR:
      mop = OP_bior;
      restype = GlobalTables::GetTypeTable().GetInt32();
      break;
    case JSOP_BITXOR:
      mop = OP_bxor;
      restype = GlobalTables::GetTypeTable().GetInt32();
      break;
    case JSOP_BITAND:
      mop = OP_band;
      restype = GlobalTables::GetTypeTable().GetInt32();
      break;
    case JSOP_EQ:
      mop = OP_eq;
      restype = GlobalTables::GetTypeTable().GetUInt1();
      break;
    case JSOP_NE:
      mop = OP_ne;
      restype = GlobalTables::GetTypeTable().GetUInt1();
      break;
    case JSOP_LT:
      mop = OP_lt;
      restype = GlobalTables::GetTypeTable().GetUInt1();
      break;
    case JSOP_LE:
      mop = OP_le;
      restype = GlobalTables::GetTypeTable().GetUInt1();
      break;
    case JSOP_GT:
      mop = OP_gt;
      restype = GlobalTables::GetTypeTable().GetUInt1();
      break;
    case JSOP_GE:
      mop = OP_ge;
      restype = GlobalTables::GetTypeTable().GetUInt1();
      break;
    case JSOP_LSH:
      mop = OP_shl;
      restype = GlobalTables::GetTypeTable().GetInt32();
      break;
    case JSOP_RSH:
      mop = OP_ashr;
      restype = GlobalTables::GetTypeTable().GetInt32();
      break;
    case JSOP_URSH:
      mop = OP_lshr;
      restype = GlobalTables::GetTypeTable().GetUInt32();
      break;
    case JSOP_SUB:
      mop = OP_sub;
      restype = GlobalTables::GetTypeTable().GetInt32();
      break;
    case JSOP_MUL:
      mop = OP_mul;
      restype = GlobalTables::GetTypeTable().GetInt32();
      break;
    case JSOP_DIV:
      mop = OP_div;
      restype = GlobalTables::GetTypeTable().GetInt32();
      break;
    case JSOP_MOD:
      mop = OP_rem;
      restype = GlobalTables::GetTypeTable().GetInt32();
      break;
    case JSOP_ADD:
      return CompileGeneric2(INTRN_JSOP_ADD, CheckConvertToJSValueType(op0), CheckConvertToJSValueType(op1), true);
    default:
      break;
  }
  if (mop != 0) {
    if (op0->primType == PTY_i32 && op1->primType == PTY_i32) {
      if (kOpcodeInfo.IsCompare(mop)) {
        return jsbuilder_->CreateExprCompare(mop, restype, GlobalTables::GetTypeTable().GetInt32(), op0, op1);
      }
      return jsbuilder_->CreateExprBinary(mop, GlobalTables::GetTypeTable().GetInt32(), op0, op1);
    }

    if (kOpcodeInfo.IsCompare(mop)) {
      return jsbuilder_->CreateExprCompare(mop, restype, GlobalTables::GetTypeTable().GetDynany(), CheckConvertToJSValueType(op0),
                                           CheckConvertToJSValueType(op1));
    } else if (restype->GetPrimType() == PTY_u32) {
      return jsbuilder_->CreateExprBinary(mop, restype, CheckConvertToUInt32(op0), CheckConvertToUInt32(op1));
    } else if (restype->GetPrimType() == PTY_dynany) {
      return jsbuilder_->CreateExprBinary(mop, restype, CheckConvertToJSValueType(op0), CheckConvertToJSValueType(op1));
    } else {
      assert(restype->GetPrimType() == PTY_i32);
      return jsbuilder_->CreateExprBinary(mop, restype, CheckConvertToInt32(op0), CheckConvertToInt32(op1));
    }
  }

  MIRIntrinsicId idx = (MIRIntrinsicId)FindIntrinsicForOp(opcode);
  IntrinDesc *intrindesc = &IntrinDesc::intrintable[idx];
  MIRType *retty = intrindesc->GetReturnType();
  MapleVector<BaseNode*> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(CheckConvertToJSValueType(op0));
  arguments.push_back(CheckConvertToJSValueType(op1));
  return jsbuilder_->CreateExprIntrinsicop(idx, retty, arguments);
}

// JSOP_NOT JSOP_BITNOT JSOP_NEG JSOP_POS 32~35
BaseNode *JSCompiler::CompileOpUnary(JSOp opcode, BaseNode *val) {
  Opcode mop = (Opcode)0;
  MIRType *restype = GlobalTables::GetTypeTable().GetDynany();
  switch (opcode) {
    case JSOP_NOT:
      mop = OP_lnot;
      restype = GlobalTables::GetTypeTable().GetUInt1();
      val = CheckConvertToBoolean(val);
      break;
    case JSOP_BITNOT:
      mop = OP_bnot;
      restype = GlobalTables::GetTypeTable().GetInt32();
      val = CheckConvertToInt32(val);
      break;
    case JSOP_NEG:
      mop = OP_neg;
      restype = GlobalTables::GetTypeTable().GetInt32();
      val = CheckConvertToInt32(val);
      break;
    default:
      break;
  }
  if (mop != 0) {
    return jsbuilder_->CreateExprUnary(mop, restype, val);
  }

  if (opcode == JSOP_POS) {
    PrimType pty;
    if (val->op == OP_constval) {
      pty = val->primType;
    }
    if (val->op == OP_dread) {
      AddrofNode *node = static_cast<AddrofNode *>(val);
      MIRSymbol *st = module_->CurFunction()->GetLocalOrGlobalSymbol(node->stIdx);
      MIRType *type = st->GetType();
      pty = type->GetPrimType();
    }

    if (pty == PTY_dyni32 || IsPrimitiveInteger(pty)) {
      return val;
    }
  }

  MIRIntrinsicId idx = (MIRIntrinsicId)FindIntrinsicForOp(opcode);
  IntrinDesc *intrindesc = &IntrinDesc::intrintable[idx];
  MIRType *retty = intrindesc->GetReturnType();
  MapleVector<BaseNode*> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(CheckConvertToJSValueType(val));
  return jsbuilder_->CreateExprIntrinsicop(idx, retty, arguments);
}

int32_t JSCompiler::GetBuiltinMethod(uint32_t argc, bool *needThis) {
  BaseNode *bn = GetOpAt(argc + 1);
  if (bn->op == OP_intrinsicop) {
    IntrinsicopNode *ion = static_cast<IntrinsicopNode *>(bn);
    if (ion->intrinsic == INTRN_JS_GET_BIOBJECT) {
      ConstvalNode *cval = static_cast<ConstvalNode *>(ion->Opnd(0));
      MIRIntConst *intconst = static_cast<MIRIntConst *>(cval->constVal);
      switch (intconst->value) {
        case JS_BUILTIN_OBJECT:
          if (argc == 0) {
            return INTRN_JS_NEW_OBJECT_0;
          } else {
            return INTRN_JS_NEW_OBJECT_1;
          }
        case JS_BUILTIN_STRING:
          return INTRN_JS_STRING;
        case JS_BUILTIN_BOOLEAN:
          return INTRN_JS_BOOLEAN;
          break;
        case JS_BUILTIN_NUMBER:
          return INTRN_JS_NUMBER;
          break;
        case JS_BUILTIN_ARRAY:
          if (argc == 1) {
            return INTRN_JS_NEW_ARR_LENGTH;
          } else {
            return INTRN_JS_NEW_ARR_ELEMS;
          }
          break;
        default:
          break;
      }
    }
  }

  if (bn->op != OP_dread) {
    return -1;
  }
  AddrofNode *drn = static_cast<AddrofNode *>(bn);
  assert(drn);
  // MIRSymbol *var = module_->symtab->GetSymbolFromStIdx(drn->stIdx.Idx());
  MIRSymbol *var = module_->CurFunction()->GetLocalOrGlobalSymbol(drn->stIdx);
  std::string name = var->GetName();
  //DEBUGPRINT3(name);

#define DEFBUILTINMETHOD(name, intrn_code, need_this) { #name, intrn_code, need_this },
  struct builtin_method_map {
    const char *name;
    int32_t intrn_code;
    bool need_this;
  };
  builtin_method_map map[18] = {
#include "../include/builtinmethod.def"
  };
#undef DEFBUILTINMETHOD
  for (uint32_t i = 0; i < sizeof(map) / sizeof(builtin_method_map); i++) {
    if (!map[i].name) {
      break;
    }
    if (!strcmp(name.c_str(), map[i].name)) {
      *needThis = map[i].need_this;
      return (int32_t)map[i].intrn_code;
    }
  }
  return -1;
}

BaseNode *JSCompiler::CompileBuiltinMethod(int32_t idx, int argNum, bool needThis) {
  // Reverse the order of args.
  std::stack<BaseNode *> tmpStack;
  if (argNum == 0 && (MIRIntrinsicId)idx == INTRN_JS_BOOLEAN) {
    Pop();
    Pop();
    return CompileOpConstValue(JSTYPE_BOOLEAN, 0);
  }

  for (uint32_t i = 0; i < argNum; i++) {
    tmpStack.push(Pop());
  }
  if (needThis) {
    tmpStack.push(Pop());
    argNum += 1;
  } else {
    Pop();
  }
  Pop();

  if ((MIRIntrinsicId)idx == INTRN_JS_NEW_ARR_ELEMS) {
    MapleVector<BaseNode *> args(module_->CurFuncCodeMemPoolAllocator()->Adapter());
    MIRSymbol *arguments = NULL;
    arguments = jsbuilder_->GetCurrentFunction()->symTab->CreateSymbol(kScopeLocal);
    const char *tempName = Util::GetSequentialName("js_arguments_", temp_var_no_, mp_);
    std::string argname(tempName);
    arguments->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(argname));
    jsbuilder_->GetCurrentFunction()->symTab->AddToStringSymbolMap(arguments);
    arguments->storageClass = kScAuto;
    arguments->sKind = kStVar;

    uint32_t sizeArray[1];
    sizeArray[0] = argNum;
    MIRType *arrayType = GlobalTables::GetTypeTable().GetOrCreateArrayType(jsvalueType, 1, sizeArray);
    MIRType *arrayPtrType = GlobalTables::GetTypeTable().GetOrCreatePointerType(arrayType);
    TyIdx tyidx = arrayType->tyIdx;
    arguments->SetTyIdx(tyidx);
    BaseNode *bn;
    MIRType *pargtype = GlobalTables::GetTypeTable().GetOrCreatePointerType(arguments->GetType());
    BaseNode *addrBase = jsbuilder_->CreateExprAddrof(0, arguments);

    for (uint32_t i = 0; i < argNum; i++) {
      bn = CheckConvertToJSValueType(tmpStack.top());
      DEBUGPRINT3(bn->op);
      tmpStack.pop();
      MapleVector<BaseNode *> opnds(module_->CurFuncCodeMemPoolAllocator()->Adapter());
      opnds.push_back(static_cast<BaseNode *>(addrBase));
      BaseNode *addrOffset = jsbuilder_->GetConstInt(i);
      opnds.push_back(static_cast<BaseNode *>(addrOffset));
      BaseNode *arrayExpr = jsbuilder_->CreateExprArray(arrayType, opnds);
      StmtNode *stmt = jsbuilder_->CreateStmtIassign(arrayPtrType, 0, arrayExpr, bn);
      stmt->srcPosition.SetLinenum(linenum_);
      jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
    }

    BaseNode *addrofArg;
    if (arguments) {
      addrofArg = jsbuilder_->CreateExprAddrof(0, arguments);
    } else {
      addrofArg = jsbuilder_->GetConstUInt32(0);
    }
    args.push_back(addrofArg);
    BaseNode *argLength = jsbuilder_->GetConstUInt32(argNum);
    args.push_back(argLength);

    IntrinDesc *intrindesc = &IntrinDesc::intrintable[idx];
    MIRType *retty = intrindesc->GetReturnType();
    MIRSymbol *temp = CreateTempVar(retty);
    StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned((MIRIntrinsicId)idx, args, temp);
    stmt->srcPosition.SetLinenum(linenum_);
    jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
    return jsbuilder_->CreateExprDread(retty, temp);
  }

  if ((MIRIntrinsicId)idx == INTRN_JS_NUMBER || (MIRIntrinsicId)idx == INTRN_JS_STRING) {
    BaseNode *argument;
    if (argNum == 0) {
      argument = CompileOpConstValue(JSTYPE_NONE, 0);
    } else {
      BaseNode *bn = tmpStack.top();
      argument = CheckConvertToJSValueType(bn);
    }
    IntrinDesc *intrindesc = &IntrinDesc::intrintable[idx];
    MIRType *retty = intrindesc->GetReturnType();
    MIRSymbol *temp = CreateTempVar(retty);
    MapleVector<BaseNode*> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
    arguments.push_back(argument);
    StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned((MIRIntrinsicId)idx, arguments, temp);
    stmt->srcPosition.SetLinenum(linenum_);
    jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
    return jsbuilder_->CreateExprDread(retty, temp);
  }

  IntrinDesc *intrindesc = &IntrinDesc::intrintable[idx];
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  // Push args into arguments.
  for (uint32_t i = 0; i < argNum; i++) {
    BaseNode *bn = tmpStack.top();
    tmpStack.pop();
    arguments.push_back(CheckConvertToRespectiveType(bn, intrindesc->GetArgType(i)));
  }

  MIRType *retty = intrindesc->GetReturnType();
  MIRSymbol *temp = CreateTempVar(retty);
  StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned((MIRIntrinsicId)idx, arguments, temp);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);

  return jsbuilder_->CreateExprDread(retty, temp);
}

BaseNode *JSCompiler::CompileOpCall(uint32_t argc) {
  //DEBUGPRINT2(argc);
  // May be call the method of builtin-object, just emit the corresponding intrinsic call.
  bool needThis = false;
  int32_t idx = GetBuiltinMethod(argc, &needThis);
  if (idx != -1) {
    return CompileBuiltinMethod(idx, argc, needThis);
  }

  // to reverse the order of args
  std::vector<BaseNode *> argsvec;
  for (uint32_t i = 0; i < argc; i++) {
    // argsvec.insert(argsvec.begin(), Pop());
    argsvec.push_back(CheckConvertToJSValueType(Pop()));
  }

  MIRSymbol *var;

  // impnode: implicitethis - first arg for closure node if needed
  // funcnode: it is intervened with arg setup
  BaseNode *impnode = CheckConvertToJSValueType(Pop());
  BaseNode *funcnode = CheckConvertToJSValueType(Pop());
  StmtNode *stmt = NULL;
  MIRSymbol *symbol;
  char *name;
  if (js2mplDebug > 2) {
    static_cast<BaseNode *>(funcnode)->Dump(module_);
  }

  MapleVector<BaseNode *> args(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  bool useSimpleCall = false;
  char *funcname = NULL;
  PUIdx puidx;

  if (funcnode->op == OP_dread) {
    AddrofNode *dread = static_cast<AddrofNode *>(funcnode);
    MIRSymbol *funcobj = module_->CurFunction()->GetLocalOrGlobalSymbol(dread->stIdx, true);
    // the function might not be a global one, embeded in obj
    if (funcobj) {
      std::string funcobjname = funcobj->GetName();
      //DEBUGPRINT3(funcobjname);

      funcname = GetFuncName(funcobjname.c_str());
      if (funcname) {
        DEBUGPRINT3(funcname);
        JSMIRFunction *func = closure_->GetJSMIRFunc(funcname);
        if (func) {
          puidx = func->puIdx;
          useSimpleCall = UseSimpleCall(funcname);
        }
      } else if (IsCCall(funcobjname.c_str()) || IsXcCall(funcobjname.c_str())) {
        funcname = (char *)funcobjname.c_str();
      }
    }
  }

  MIRSymbol *returnVar = CreateTempVar(jsvalueType);

  if (useSimpleCall) {
    JSMIRFunction *func = closure_->GetJSMIRFunc(funcname);
    BaseNode *undefined = CompileOpConstValue(JSTYPE_UNDEFINED, 0);
    // This value.
    args.push_back(undefined);
    // Discard redundant arguments.
    int32_t last = argc > func->argc ? argc - func->argc : 0;
    for (int32_t i = argc - 1; i >= last; i--) {
      args.push_back(argsvec[i]);
    }

    if (argc < func->argc) {
      for (int32_t i = argc; i < func->argc; i++) {
        args.push_back(undefined);
      }
    }

    stmt = jsbuilder_->CreateStmtCallAssigned(puidx, args, returnVar, OP_callassigned);
  } else if (funcname && IsCCall(funcname)) {
    args.push_back(argsvec[argc - 1]);
    BaseNode *argcNode = jsbuilder_->GetConstUInt32((uint32_t)argc - 1);
    args.push_back(argcNode);
    for (int32_t i = argc - 2; i >= 0; i--) {
      args.push_back(argsvec[i]);
    }

    stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JSOP_CCALL, args, returnVar);
  } else if (funcname && IsXcCall(funcname)) {
    for (int32_t i = argc - 1; i >= 0; i--) {
      args.push_back(argsvec[i]);
    }
    stmt = jsbuilder_->CreateStmtXintrinsicCallAssigned((MIRIntrinsicId)0, args, returnVar);
  } else {
    args.push_back(funcnode);
    args.push_back(impnode);
    for (int32_t i = argc - 1; i >= 0; i--) {
      args.push_back(argsvec[i]);
    }

    stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JSOP_CALL, args, returnVar);
  }

  if (stmt) {
    stmt->srcPosition.SetLinenum(linenum_);
  }
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);

  return jsbuilder_->CreateExprDread(jsvalueType, 0, returnVar);
}

BaseNode *JSCompiler::CompileOpNew(uint32_t argc) {
  // Reverse the order of args.
  std::vector<BaseNode *> argsvec;
  for (uint32_t i = 0; i < argc; i++) {
    argsvec.push_back(CheckConvertToJSValueType(Pop()));
  }

  // impnode: implicitethis - first arg for closure node if needed
  // funcnode: it is intervened with arg setup
  BaseNode *impnode = Pop();
  BaseNode *funcnode = CheckConvertToJSValueType(Pop());

  MapleVector<BaseNode *> args(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  args.push_back(funcnode);
  args.push_back(impnode);
  for (int32_t i = argc - 1; i >= 0; i--) {
    args.push_back(argsvec[i]);
  }
  MIRSymbol *returnVar = CreateTempVar(jsvalueType);
  StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JSOP_NEW, args, returnVar);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  return jsbuilder_->CreateExprDread(jsvalueType, 0, returnVar);
}

js_builtin_id JSCompiler::EcmaNameToId(char *name) {
  if (!strcmp(name, "Object")) {
    return JS_BUILTIN_OBJECT;
  } else if (!strcmp(name, "Array")) {
    return JS_BUILTIN_ARRAY;
  } else if (!strcmp(name, "String")) {
    return JS_BUILTIN_STRING;
  } else if (!strcmp(name, "Boolean")) {
    return JS_BUILTIN_BOOLEAN;
  } else if (!strcmp(name, "Number")) {
    return JS_BUILTIN_NUMBER;
  } else if (!strcmp(name, "Function")) {
    return JS_BUILTIN_FUNCTION;
  } else if (!strcmp(name, "exports")) {
    return JS_BUILTIN_EXPORTS;  // for plugin
  } else if (!strcmp(name, "module")) {
    return JS_BUILTIN_MODULE;
  } else if (!strcmp(name, "Math")) {
    return JS_BUILTIN_MATH;
  } else if (!strcmp(name, "JSON")) {
    return JS_BUILTIN_JSON;
  } else {
    return JS_BUILTIN_COUNT;
  }
}

BaseNode *JSCompiler::CompileBuiltinObject(char *name) {
  js_builtin_id id = EcmaNameToId(name);
  if (id == JS_BUILTIN_COUNT) {
    return NULL;
  }
  BaseNode *idNode = jsbuilder_->GetConstUInt32((uint32_t)id);
  return CompileGeneric1(INTRN_JS_GET_BIOBJECT, idNode, false);
}

// JSOP_NAME 59
BaseNode *JSCompiler::CompileOpName(JSAtom *atom, jsbytecode *pc) {
  char *name = Util::GetString(atom, mp_, jscontext_);
  JS_ASSERT(name && "empty name");

#if 0
  // Report error when use unspported name.
  if (!strcmp(name, "NaN")) {
    assert(false && "Can not support NaN.");
  }
  if (!strcmp(name, "Infinity")) {
    assert(false && "Can not support Infinity.");
  }
  if (!strcmp(name, "parseInt")) {
    assert(false && "Can not support parseInt.");
  }
  if (!strcmp(name, "parseFloat")) {
    assert(false && "Can not support parseFloat.");
  }
  if (!strcmp(name, "isNaN")) {
    assert(false && "Can not support isNaN.");
  }
  if (!strcmp(name, "isFinite")) {
    assert(false && "Can not support isFinite.");
  }
  if (!strcmp(name, "decodeURI")) {
    assert(false && "Can not support decodeURI.");
  }
  if (!strcmp(name, "decodeURIComponent")) {
    assert(false && "Can not support decodeURIComponent.");
  }
  if (!strcmp(name, "encodeURI")) {
    assert(false && "Can not support encodeURI.");
  }
  if (!strcmp(name, "encodeURIComponent")) {
    assert(false && "Can not support encodeURIComponent.");
  }
#endif

  // Null or undefined.
  if (!strcmp(name, "null")) {
    return CompileOpConstValue(JSTYPE_NULL, 0);
  }
  if (!strcmp(name, "undefined")) {
    return CompileOpConstValue(JSTYPE_UNDEFINED, 0);
  }

  BaseNode *builtinObject = CompileBuiltinObject(name);
  if (builtinObject) {
    return builtinObject;
  }

  BaseNode *bn = NULL;
  if (scope_->IsFunction(name)) {
    DEBUGPRINT2(name);
    char *objname = Util::GetNameWithSuffix(name, "_obj_", mp_);
    if (!GetFuncName(objname)) {
      std::pair<char *, char *> p(objname, name);
      objFuncMap.push_back(p);
    }
    name = objname;
  }

  // ??? Generate a dread node to pass the name.
  MIRSymbol *var;
  bool created;
  if (jsbuilder_->IsGlobalName(name) || IsCCall(name) || IsXcCall(name)) {
    var = jsbuilder_->GetOrCreateGlobalDecl(name, jsvalueType);
  } else {
    var = jsbuilder_->GetOrCreateLocalDecl(name, jsvalueType);
  }

  // print is a builtin function.
  if (!strcmp(name, "print") || !strcmp(name, "$ERROR") || !strcmp(name, "SetCycleHeader") || IsCCall(name) ||
      IsXcCall(name)) {
    created = false;
  }

  InitWithUndefined(created, var);

  StIdx stidx = var->GetStIdx();
  //DEBUGPRINT3(stidx.Idx());

  bn = jsbuilder_->CreateExprDread(jsvalueType, var);

  if (created && eh_->IsInEHrange(pc)) {
    StmtNode *throwstmt = jsbuilder_->CreateStmtThrow(bn);
    throwstmt->srcPosition.SetLinenum(linenum_);
    jsbuilder_->AddStmtInCurrentFunctionBody(throwstmt);
  }

  return bn;
}

BaseNode *JSCompiler::CompileLibraryCall(BaseNode *obj, JSString *str) {
  if (obj->op != OP_intrinsicop ||
      static_cast<IntrinsicopNode *>(obj)->intrinsic != INTRN_JS_GET_BIOBJECT)
    return NULL;
  IntrinsicopNode *iop = static_cast<IntrinsicopNode *>(obj);
  assert(iop->numOpnds == 1 && iop->Opnd(0)->op == OP_constval);
  size_t length = 0;
  const jschar *chars = JS_GetStringCharsZAndLength(jscontext_, str, &length);
  assert(chars != NULL && length > 0);
  std::string libcall;
  libcall.reserve(16 + length);
  ConstvalNode *opnd0 = static_cast<ConstvalNode *>(iop->Opnd(0));
  MIRIntConst *cv = static_cast<MIRIntConst *>(opnd0->constVal);
  switch (cv->value) {
  case JS_BUILTIN_MATH:
    libcall.append(".Math.");
    libcall.append(chars, chars + length);
    break;
  default:
    break;
  }
  if (libcall.empty())
    return NULL;
  ArgVector arguments(module_->memPoolAllocator.Adapter());
  MapleString fname(libcall, mp_);
  JSMIRFunction *func = jsbuilder_->GetOrCreateFunction(fname.c_str(), NULL, GlobalTables::GetTypeTable().GetDynany(), arguments, false);
  // pre-create a ptr-to-func type for later use in addroffunc
  GlobalTables::GetTypeTable().GetOrCreatePointerType(func->funcType->tyIdx, PTY_ptr);
  BaseNode *ptr = jsbuilder_->CreateExprAddroffunc(func->puIdx);
  return ptr;
}

int32_t JSCompiler::GetBuiltinStringId(const jschar *chars, uint32_t length) {
  static const struct {
    const char *chars;
    uint32_t length;
  } builtinStrings[JSBUILTIN_STRING_ID_COUNT] = {
#define JSBUILTIN_STRING_DEF(id, length, str) { (const char *)str, length },
#include "../include/jsbuiltinstrings.inc.h"
#undef JSBUILTIN_STRING_DEF
  };
  uint32_t i;
  for (i = 0; i < JSBUILTIN_STRING_ID_COUNT; i++) {
    if (builtinStrings[i].length != length) {
      continue;
    }
    uint32_t j;
    for (j = 0; j < length; j++) {
      if (builtinStrings[i].chars[j + 4] != chars[j]) {
        break;
      }
    }
    if (j == length) {
      break;
    }
  }
  if (i == JSBUILTIN_STRING_ID_COUNT) {
    return -1;
  }
  return (int32_t)i;
}

bool IsAsciiChars(const jschar *chars, uint32_t length) {
  for (uint32_t i = 0; i < length; i++) {
    if (chars[i] >= 256) {
      return false;
    }
  }
  return true;
}

// JSOP_STRING 61
BaseNode *JSCompiler::CompileOpString(JSString *str) {
  size_t length = 0;
  //const jschar *chars = JS_GetInternedStringCharsAndLength(str, &length);
  const jschar *chars = JS_GetStringCharsZAndLength(jscontext_, str, &length);
  int32_t id = GetBuiltinStringId(chars, length);
  if (jsstring_map_[chars]) {
    return jsstring_map_[chars];
  }
  if (id != -1) {
    BaseNode *expr = CompileGeneric1((MIRIntrinsicId)INTRN_JS_GET_BISTRING, jsbuilder_->GetConstUInt32(id), false);
    jsstring_map_[chars] = expr;
    return expr;
  }

  if (length >= pow(2, 16)) {
    assert(false && "Not Support too long string now");
  }
  MIRType *unitType = IsAsciiChars(chars, length) ? GlobalTables::GetTypeTable().GetUInt8() : GlobalTables::GetTypeTable().GetUInt16();
  uint32_t pad = IsAsciiChars(chars, length) ? 4 : 2;
  uint32_t stringClass = IsAsciiChars(chars, length) ? 0 : JSSTRING_UNICODE;

  uint32_t paddingLength = length + pad;
  MIRType *type = GlobalTables::GetTypeTable().GetOrCreateArrayType(unitType, 1, &(paddingLength));
  const char *tempName = Util::GetSequentialName("const_chars_", temp_var_no_, mp_);
  MIRSymbol *var = jsbuilder_->GetOrCreateGlobalDecl(tempName, type);
  MIRAggConst *init = module_->memPool->New<MIRAggConst>(module_, type);

  uint8_t cl[4];
  cl[0] = stringClass;
  cl[1] = 0;
  cl[2] = length & 0xff;
  cl[3] = (length & 0xff00) >> 8;

  if ((stringClass & JSSTRING_UNICODE) == 0) {
    uint64_t val = (uint64_t)(cl[0]);
    MIRIntConst *intConst = mp_->New<MIRIntConst>(val, unitType);
    init->constVec.push_back(intConst);
    val = (uint64_t)(cl[1]);
    intConst = mp_->New<MIRIntConst>(val, unitType);
    init->constVec.push_back(intConst);

    val = (uint64_t)(cl[2]);
    intConst = mp_->New<MIRIntConst>(val, unitType);
    init->constVec.push_back(intConst);
    val = (uint64_t)(cl[3]);
    intConst = mp_->New<MIRIntConst>(val, unitType);
    init->constVec.push_back(intConst);
  } else {
    uint16_t *tmp = (uint16_t *)cl;
    uint64_t val = (uint64_t)(cl[0]);
    MIRIntConst *intConst = mp_->New<MIRIntConst>(val, unitType);
    init->constVec.push_back(intConst);
    val = (uint64_t)(tmp[1]);
    intConst = mp_->New<MIRIntConst>(val, unitType);
    init->constVec.push_back(intConst);
  }

  for (uint32_t i = 0; i < length; i++) {
    uint64_t val = chars[i];
    MIRIntConst *intConst = mp_->New<MIRIntConst>(val, unitType);
    init->constVec.push_back(intConst);
  }
  var->value.konst = init;
  BaseNode *expr = jsbuilder_->CreateExprAddrof(0, var);
  expr->primType = PTY_simplestr;
  jsstring_map_[chars] = expr;
  return expr;
}

// JSOP_ITER 75
BaseNode *JSCompiler::CompileOpNewIterator(BaseNode *bn, uint8_t flags) {
  MIRType *retty = GlobalTables::GetTypeTable().GetOrCreatePointerType(GlobalTables::GetTypeTable().GetVoid());
  MIRSymbol *retsy = CreateTempVar(retty);

  MapleVector<BaseNode*> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(bn);
  arguments.push_back(jsbuilder_->GetConstUInt32(flags));
  StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned((MIRIntrinsicId)INTRN_JSOP_NEW_ITERATOR,
                                                                arguments, retsy);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  return jsbuilder_->CreateExprDread(retty, 0, retsy);
}

BaseNode *JSCompiler::CompileOpMoreIterator(BaseNode *iterator) {
  MIRSymbol *retsy = CreateTempVar(GlobalTables::GetTypeTable().GetUInt32());
  MapleVector<BaseNode*> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(iterator);
  StmtNode *stmt =
    jsbuilder_->CreateStmtIntrinsicCallAssigned((MIRIntrinsicId)INTRN_JSOP_MORE_ITERATOR, arguments, retsy);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);

  return jsbuilder_->CreateExprDread(GlobalTables::GetTypeTable().GetUInt32(), retsy);
}

// JSOP_ITERNEXT 77
BaseNode *JSCompiler::CompileOpIterNext(BaseNode *iterator) {
  MIRSymbol *var = CreateTempJSValueTypeVar();
  MapleVector<BaseNode*> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(iterator);
  StmtNode *stmt =
    jsbuilder_->CreateStmtIntrinsicCallAssigned((MIRIntrinsicId)INTRN_JSOP_NEXT_ITERATOR, arguments, var);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);

  return jsbuilder_->CreateExprDread(jsvalueType, var);
}

// JSOP_GETARG 84
BaseNode *JSCompiler::CompileOpGetArg(uint32_t i) {
  DEBUGPRINT2(i);
  JSMIRFunction *fun = jsbuilder_->GetCurrentFunction();
  int start = (fun->with_env_arg) ? 2 : 1;
  MIRSymbol *arg = jsbuilder_->GetFunctionArgument(fun, i + start);
  BaseNode *irn = jsbuilder_->CreateExprDread(GlobalTables::GetTypeTable().GetDynany(), arg);
  return irn;
}

// JSOP_SETARG 85
void JSCompiler::CompileOpSetArg(uint32_t i, BaseNode *val) {
  DEBUGPRINT2(i);
  JSMIRFunction *fun = jsbuilder_->GetCurrentFunction();
  int start = (fun->with_env_arg) ? 2 : 1;
  MIRSymbol *arg = jsbuilder_->GetFunctionArgument(fun, i + start);  // skip this and env parameters
  opstack_->ReplaceStackItemsWithTemps(this, arg);
  jsbuilder_->CreateStmtDassign(arg, 0, val, linenum_);
  BaseNode *bn = jsbuilder_->CreateExprDread(jsvalueType, arg);
  Push(bn);
  return;
}

// JSOP_GETLOCAL 86
BaseNode *JSCompiler::CompileOpGetLocal(uint32_t localNo) {
  JSMIRFunction *func = jsbuilder_->GetCurrentFunction();
  char *name = closure_->GetLocalVar(func, localNo);
  bool created;
  MIRSymbol *var;

  // for function name, use suffix _obj_
  if (scope_->IsFunction(name)) {
    char *objname = Util::GetNameWithSuffix(name, "_obj_", mp_);
    if (!GetFuncName(objname)) {
      std::pair<char *, char *> p(objname, name);
      objFuncMap.push_back(p);
    }
    name = objname;
  }
  var = jsbuilder_->GetOrCreateLocalDecl(name, jsvalueType);
  InitWithUndefined(created, var);
  if (!created) {
    MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(var->GetTyIdx());
    return jsbuilder_->CreateExprDread(type, var);
  }

  return jsbuilder_->CreateExprDread(jsvalueType, var);
}

// JSOP_SETLOCAL 87
StmtNode *JSCompiler::CompileOpSetLocal(uint32_t localNo, BaseNode *src) {
  JSMIRFunction *func = jsbuilder_->GetCurrentFunction();
  char *name = closure_->GetLocalVar(func, localNo);
  MIRSymbol *var;

  // for function name, use suffix _obj_
  if (scope_->IsFunction(name)) {
    char *objname = Util::GetNameWithSuffix(name, "_obj_", mp_);
    if (!GetFuncName(objname)) {
      std::pair<char *, char *> p(objname, name);
      objFuncMap.push_back(p);
    }
    name = objname;
  }
  var = jsbuilder_->GetOrCreateLocalDecl(name, jsvalueType);
  // if the stack is not empty, for each stack item that contains the
  // variable being set, evaluate and store the result in a new temp and replace
  // the stack items by the temp
  opstack_->ReplaceStackItemsWithTemps(this, var);

  BaseNode *bn = CheckConvertToJSValueType(src);
  return jsbuilder_->CreateStmtDassign(var, 0, bn, linenum_);
}

// JSOP_NEWINIT 89
BaseNode *JSCompiler::CompileOpNewInit(uint32_t kind) {
  if (kind == JSProto_Array) {
    assert(false && "NIY");
  } else {
    assert(kind == JSProto_Object);
    return CompileGeneric0(INTRN_JS_NEW_OBJECT_0, true);
  }
  return NULL;
}

BaseNode *JSCompiler::CompileGenericN(int32_t intrinId, MapleVector<BaseNode *> &arguments, bool isCall) {
  IntrinDesc *intrindesc = &IntrinDesc::intrintable[intrinId];
  MIRType *retty = intrindesc->GetReturnType();
  if (isCall) {
    MIRSymbol *var = CreateTempVar(retty);
    StmtNode *call = jsbuilder_->CreateStmtIntrinsicCallAssigned((MIRIntrinsicId)intrinId, arguments, var);
    call->srcPosition.SetLinenum(linenum_);
    jsbuilder_->AddStmtInCurrentFunctionBody(call);
    //  TODO: if retty is void, return NULL
    return jsbuilder_->CreateExprDread(retty, var);
  } else {
    return jsbuilder_->CreateExprIntrinsicop((MIRIntrinsicId)intrinId, retty, arguments);
  }
}

BaseNode *JSCompiler::CompileGeneric0(int32_t intrinId, bool isCall) {
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  return CompileGenericN(intrinId, arguments, isCall);
}

BaseNode *JSCompiler::CompileGeneric1(int32_t intrinId, BaseNode *arg, bool isCall) {
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(arg);
  return CompileGenericN(intrinId, arguments, isCall);
}

BaseNode *JSCompiler::CompileGeneric2(int32_t intrinId, BaseNode *arg1, BaseNode *arg2, bool isCall) {
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(arg1);
  arguments.push_back(arg2);
  return CompileGenericN(intrinId, arguments, isCall);
}

BaseNode *JSCompiler::CompileGeneric3(int32_t intrinId, BaseNode *arg1, BaseNode *arg2, BaseNode *arg3, bool isCall) {
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(arg1);
  arguments.push_back(arg2);
  arguments.push_back(arg3);
  return CompileGenericN(intrinId, arguments, isCall);
}

BaseNode *JSCompiler::CompileGeneric4(int32_t intrinId, BaseNode *arg1, BaseNode *arg2, BaseNode *arg3, BaseNode *arg4,
                                      bool isCall) {
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(arg1);
  arguments.push_back(arg2);
  arguments.push_back(arg3);
  arguments.push_back(arg4);
  return CompileGenericN(intrinId, arguments, isCall);
}

bool JSCompiler::CompileOpSetElem(BaseNode *obj, BaseNode *index, BaseNode *val) {
  index = CheckConvertToJSValueType(index);
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(obj);
  arguments.push_back(index);
  arguments.push_back(CheckConvertToJSValueType(val));
  StmtNode *stmt =
    jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JSOP_SETPROP, arguments, (MIRSymbol*)NULL);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  return true;
}

bool JSCompiler::CompileOpInitPropGetter(BaseNode *obj, JSString *str, BaseNode *val) {
  BaseNode *name = CheckConvertToJSValueType(CompileOpString(str));
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(obj);
  arguments.push_back(name);
  arguments.push_back(val);
  StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JSOP_INITPROP_GETTER, arguments, (MIRSymbol*)NULL);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  return true;
}

bool JSCompiler::CompileOpInitPropSetter(BaseNode *obj, JSString *str, BaseNode *val) {
  BaseNode *name = CheckConvertToJSValueType(CompileOpString(str));
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(obj);
  arguments.push_back(name);
  arguments.push_back(val);
  StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JSOP_INITPROP_SETTER, arguments, (MIRSymbol*)NULL);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  return true;
}

bool JSCompiler::CompileOpInitElemGetter(BaseNode *obj, BaseNode *index, BaseNode *val) {
  index = CheckConvertToJSValueType(index);
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(obj);
  arguments.push_back(index);
  arguments.push_back(val);
  StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JSOP_INITPROP_GETTER, arguments, (MIRSymbol*)NULL);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  return true;
}

bool JSCompiler::CompileOpInitElemSetter(BaseNode *obj, BaseNode *index, BaseNode *val) {
  index = CheckConvertToJSValueType(index);
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(obj);
  arguments.push_back(index);
  arguments.push_back(val);
  StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JSOP_INITPROP_SETTER, arguments, (MIRSymbol*)NULL);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  return true;
}

// JSOP_SETPROP 54
bool JSCompiler::CompileOpSetProp(BaseNode *obj, JSString *str, BaseNode *val) {
  BaseNode *name = CompileOpString(str);
  MapleVector<BaseNode *> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(obj);
  arguments.push_back(name);
  arguments.push_back(CheckConvertToJSValueType(val));
  StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JSOP_SETPROP_BY_NAME, arguments, (MIRSymbol*)NULL);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  return true;
}

// JSOP_BINDNAME 110
BaseNode *JSCompiler::CompileOpBindName(JSAtom *atom) {
  char *name = Util::GetString(atom, mp_, jscontext_);
  JS_ASSERT(name && "empty name");
  BaseNode *builtinObject = CompileBuiltinObject(name);
  if (builtinObject) {
    return builtinObject;
  }

  MIRSymbol *var;
  JSMIRFunction *func = funcstack_.top();
  bool created;
  // search the scope chain
  while (func) {
    if (closure_->IsLocalVar(func, name)) {
      //var = jsbuilder_->GetOrCreateDeclInFunc(name, jsvalueType, func, created);
      var = jsbuilder_->GetSymbol(TyIdx(0),
		                  jsbuilder_->GetStringIndex(name), kStVar, kScAuto, kScopeLocal, func);
      if (!var)
        var = MIRSymbolBuilder::Instance().CreateLocalDecl(*func->symTab,
			                                   jsbuilder_->GetOrCreateStringIndex(name), *jsvalueType);
      InitWithUndefined(created, var);
      break;
    }

    // function introduced a global var
    if (func == jsmain_) {
      var = jsbuilder_->GetOrCreateGlobalDecl(name, jsvalueType);
      InitWithUndefined(created, var);
      jsbuilder_->InsertGlobalName(name);
      break;
    }

    func = func->scope->GetParentFunc();
  }

  // ??? Generate a dread node to pass the name.
  BaseNode *bn = jsbuilder_->CreateExprDread(jsvalueType, var);

  return bn;
}

// JSOP_SETNAME 110
bool JSCompiler::CompileOpSetName(JSAtom *atom, BaseNode *val) {
  char *name = Util::GetString(atom, mp_, jscontext_);
  JS_ASSERT(name && "empty name");
  JSMIRFunction *func = funcstack_.top();
  MIRSymbol *var = closure_->GetSymbolFromEnclosingScope(func, name);
  if (!var) {
    var = jsbuilder_->GetOrCreateLocalDecl(name, jsvalueType);
  }

  // if the stack is not empty, for each stack item that contains the
  // variable being set, evaluate and store the result in a new temp and replace
  // the stack items by the temp
  opstack_->ReplaceStackItemsWithTemps(this, var);
  jsbuilder_->CreateStmtDassign(var, 0, CheckConvertToJSValueType(val), linenum_);
  BaseNode *bn = jsbuilder_->CreateExprDread(jsvalueType, var);
  Push(bn);
  return true;
}

// JSOP_DEFFUN 127
bool JSCompiler::CompileOpDefFun(JSFunction *jsfun) {
  JSScript *scr = jsfun->getOrCreateScript(jscontext_);
  JSAtom *atom = jsfun->displayAtom();
  //DEBUGPRINT2(atom);
  const char *fname = Util::GetString(atom, mp_, jscontext_);
  if (!fname) {
    return false;
  }
  JSMIRFunction *parent = jsbuilder_->GetCurrentFunction();
  const char *par_name = parent ? parent->GetName().c_str() : NULL;
  char *funcname = Util::GetFuncName(par_name, fname, 0, mp_);
  JSMIRFunction *mfun = jsbuilder_->GetFunction(funcname);
  mfun->srcPosition.SetLinenum(linenum_);
  mfun->SetUserFunc();

  MIRSymbol *funcSt = jsbuilder_->GetOrCreateGlobalDecl(funcname, jsvalueType);
  BaseNode *ptr = jsbuilder_->CreateExprAddroffunc(funcSt->value.mirFunc->puIdx);
  assert(jsfun && "not a jsfunction");

  char *name = Util::GetNameWithSuffix(funcname, "_obj_", mp_);
  if (jsbuilder_->GetStringIndex(name).GetIdx() == 0) {
    uint32_t vargP = 0;
    uint32_t nargs = (uint32_t)(uint8_t)jsfun->nargs();
    uint32_t length = nargs;
    uint32_t flag = jsfun->strict() ? JSFUNCPROP_STRICT | JSFUNCPROP_USERFUNC : JSFUNCPROP_USERFUNC;
    uint32_t attrs = vargP << 24 | nargs << 16 | length << 8 | flag;
    BaseNode *funcNode =
      CompileGeneric3(INTRN_JS_NEW_FUNCTION, ptr, jsbuilder_->GetConstInt(0), jsbuilder_->GetConstUInt32(attrs), true);

    MIRSymbol *funcObj;
    if (mfun->scope->IsWithEnv()) {
      funcObj = jsbuilder_->GetOrCreateLocalDecl(name, jsvalueType);
    } else {
      funcObj = jsbuilder_->GetOrCreateGlobalDecl(name, jsvalueType);
      jsbuilder_->InsertGlobalName(name);
    }

    StmtNode *stmt = jsbuilder_->CreateStmtDassign(funcObj, 0, funcNode, linenum_);
    stmt->srcPosition.SetLinenum(linenum_);
    jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  }

  funcstack_.push(mfun);
  jsbuilder_->SetCurrentFunction(mfun);
  DEBUGPRINTfunc(funcname);

  CompileScript(scr);
  return true;
}

// JSOP_DEFVAR 129
bool JSCompiler::CompileOpDefVar(JSAtom *atom) {
  char *name = Util::GetString(atom, mp_, jscontext_);
  JS_ASSERT(name && "empty name");
  JSMIRFunction *fun = jsbuilder_->GetCurrentFunction();
  //DEBUGPRINT2((fun == funcstack_.top()));
  //DEBUGPRINT2(fun);
  bool created;
  MIRSymbol *var = jsbuilder_->GetOrCreateGlobalDecl(name, jsvalueType);
  InitWithUndefined(created, var);
  if (fun == jsmain_) {
    jsbuilder_->InsertGlobalName(name);
  }
  return true;
}

// JSOP_LAMBDA 131
BaseNode *JSCompiler::CompileOpLambda(jsbytecode *pc, JSFunction *jsfun) {
  JSAtom *atom = jsfun->displayAtom();

  // isLambda() does not seem reliable
  //DEBUGPRINT3((jsfun->isLambda()));
  // we already know it is a Lambda so only check other two parts in isNamedLambda()
  // isLambda() && displayAtom() && !hasGuessedAtom()
  bool hasName = false;
  const char *name = (atom && !jsfun->hasGuessedAtom()) ?
                       Util::GetString(atom, mp_, jscontext_) : NULL;
  JSMIRFunction *parent = jsbuilder_->GetCurrentFunction();
  const char *par_name = parent ? parent->GetName().c_str() : NULL;
  char *funcname = Util::GetFuncName(par_name, name,
                                     scope_->GetAnonyidx(jsfun), mp_);
  JSMIRFunction *lambda = jsbuilder_->GetFunction(funcname);
  lambda->srcPosition.SetLinenum(linenum_);
  lambda->SetUserFunc();
  //DEBUGPRINT2(lambda);

  JSMIRFunction *parentFunc = funcstack_.top();

  MIRSymbol *funcsymbol = jsbuilder_->GetOrCreateGlobalDecl(funcname, jsvalueType);
  BaseNode *ptr = jsbuilder_->CreateExprAddroffunc(funcsymbol->value.mirFunc->puIdx);
  MIRSymbol *envVar = NULL;
  BaseNode *node;
  //DEBUGPRINT2((lambda->scope->GetName()));
  //DEBUGPRINT2((lambda->scope->IsTopLevel()));
  if (parentFunc->scope->IsWithEnv()) {
    envVar = jsbuilder_->GetOrCreateLocalDecl("environment", GlobalTables::GetTypeTable().GetDynany());
    node = jsbuilder_->CreateExprDread(GlobalTables::GetTypeTable().GetDynany(), envVar);
    lambda->penvtype = parentFunc->envptr;
  } else {
    node = jsbuilder_->GetConstInt(0);
    DEBUGPRINTs3("lambda->scope->IsTopLevel()");
  }

  uint32_t vargP = 0;
  uint32_t nargs = (uint32_t)(uint8_t)jsfun->nargs();
  uint32_t length = nargs;
  uint32_t flag = jsfun->strict() ? JSFUNCPROP_STRICT | JSFUNCPROP_USERFUNC : JSFUNCPROP_USERFUNC;
  uint32_t attrs = vargP << 24 | nargs << 16 | length << 8 | flag;
  BaseNode *bn = CompileGeneric3(INTRN_JS_NEW_FUNCTION, ptr, node, jsbuilder_->GetConstUInt32(attrs), true);

  if (hasName) {
    char *name = Util::GetNameWithSuffix(funcname, "_obj_", mp_);
    MIRSymbol *funcObj = jsbuilder_->GetOrCreateGlobalDecl(name, jsvalueType);
    jsbuilder_->InsertGlobalName(name);
    jsbuilder_->CreateStmtDassign(funcObj, 0, bn, linenum_);
    bn = jsbuilder_->CreateExprDread(funcObj);
  }
  std::pair<JSScript *, JSMIRFunction *> p(jsfun->nonLazyScript(), lambda);
  scriptstack_.push(p);
  return bn;
}

// JSOP_GETALIASEDVAR 136
int JSCompiler::ProcessAliasedVar(JSAtom *atom, MIRType *&envPtr, BaseNode *&envNode, int &depth) {
  JSMIRFunction *func = funcstack_.top();
  //DEBUGPRINT3(func);
  char *name = Util::GetString(atom, mp_, jscontext_);
  JS_ASSERT(name && "empty name");
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  const char *funcname = funcSt->GetName().c_str();
  ScopeNode *sn = scope_->GetOrCreateSN((char *)funcname);
  ScopeNode *psn = sn->GetParent();

  int idx = 0;
  if (!psn) {
    DEBUGPRINT3("alias var from catch block");
    return idx;
  }

  JSMIRFunction *pfunc = psn->GetFunc();

  depth = 0;
  MIRSymbol *envVar = NULL;
  const char *envName;
  BaseNode *bn = NULL;
  StmtNode *stmt = NULL;

  // search in current func's alias list
  // depth = 0
  if (!idx && sn->IsWithEnv()) {
    envVar = jsbuilder_->GetOrCreateLocalDecl("environment", func->envptr);
    //DEBUGPRINTsv3("environment", func->envptr);
    idx = jsbuilder_->GetStructFieldIdFromFieldName(func->envtype, name);
    DEBUGPRINT3(idx);
    depth = 0;

    //DEBUGPRINTsv3("func to get env_ptr", func);

    envPtr = func->envptr;
    envNode = jsbuilder_->CreateExprDread(func->envptr, envVar);
  }

  // recursively search in parent's alias list
  // depth >= 1
  if (pfunc != jsmain_ && !idx) {
    depth = 1;
    DEBUGPRINTs3("recursively search in parent's alias lists");
    envVar = func->formalDefVec[ENV_POSITION_IN_ARGS].formalSym;

    envPtr = pfunc->envptr;
    envNode = jsbuilder_->CreateExprDread(pfunc->envptr, envVar);
    idx = jsbuilder_->GetStructFieldIdFromFieldName(pfunc->envtype, name);
    DEBUGPRINT3(idx);

    sn = psn;
    psn = psn->GetParent();

    if (psn) {
      func = pfunc;
      pfunc = psn->GetFunc();

      while (pfunc != jsmain_ && !idx && psn && psn->IsWithEnv()) {
        depth++;
        DEBUGPRINTsv3("func to get env_ptr", pfunc);

        envPtr = pfunc->envptr;
        envNode = jsbuilder_->CreateExprDread(pfunc->envptr, envVar);
        idx = jsbuilder_->GetStructFieldIdFromFieldName(pfunc->envtype, "parentenv");
        envNode = jsbuilder_->CreateExprIread(pfunc->envptr, pfunc->envptr, idx, envNode);

        envName = Util::GetSequentialName("env_", temp_var_no_, mp_);
        envVar = jsbuilder_->GetOrCreateLocalDecl(envName, pfunc->envptr);
        stmt = jsbuilder_->CreateStmtDassign(envVar, 0, envNode, linenum_);
        envNode = jsbuilder_->CreateExprDread(pfunc->envptr, envVar);
        idx = jsbuilder_->GetStructFieldIdFromFieldName(pfunc->envtype, name);
        DEBUGPRINT3(idx);

        sn = psn;
        psn = psn->GetParent();
        func = pfunc;

        if (psn) {
          pfunc = psn->GetFunc();
        }
      }
    }
  }

  //DEBUGPRINT2(depth);

  return idx;
}

BaseNode *JSCompiler::CompileOpGetAliasedVar(JSAtom *atom) {
  MIRType *envPtr;
  BaseNode *envNode;
  int depth = 0;

  int idx = ProcessAliasedVar(atom, envPtr, envNode, depth);

  BaseNode *bn = NULL;
  if (idx) {
    bn = jsbuilder_->CreateExprIread(jsvalueType, envPtr, idx, envNode);
  } else {
    // add to local
    DEBUGPRINT3("alias var not found, could be from block");
    char *name = Util::GetString(atom, mp_, jscontext_);
    MIRSymbol *var = jsbuilder_->GetOrCreateLocalDecl(name, jsvalueType);
    bn = jsbuilder_->CreateExprDread(jsvalueType, var);
  }

  return bn;
}

// JSOP_SETALIASEDVAR 137
BaseNode *JSCompiler::CompileOpSetAliasedVar(JSAtom *atom, BaseNode *val) {
  MIRType *envPtr;
  BaseNode *envNode;
  int depth = 0;

  int idx = ProcessAliasedVar(atom, envPtr, envNode, depth);

  StmtNode *bn = NULL;
  if (idx) {
    bn = jsbuilder_->CreateStmtIassign(envPtr, idx, envNode, val);
  } else {
    // add to local
    DEBUGPRINT3("alias var not found, could be from block");
    char *name = Util::GetString(atom, mp_, jscontext_);
    MIRSymbol *var = jsbuilder_->GetOrCreateLocalDecl(name, jsvalueType);
    bn = jsbuilder_->CreateStmtDassign(var, 0, val, linenum_);
  }
  bn->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(bn);

  return bn;
}

void JSCompiler::CloseFuncBookKeeping() {
  DEBUGPRINTfuncEnd();

  funcstack_.pop();

  // process inner functions
  DEBUGPRINT3((scriptstack_.size()));
  while (scriptstack_.size()) {
    JSScript *scr = scriptstack_.top().first;
    JSMIRFunction *lambda = scriptstack_.top().second;
    jsbuilder_->SetCurrentFunction(lambda);
    DEBUGPRINT0;
    MIRSymbol *lambdaSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(lambda->stIdx.Idx());
    DEBUGPRINTfunc((lambdaSt->GetName().c_str()));
    funcstack_.push(lambda);
    scriptstack_.pop();

    CompileScript(scr);
  }

  if (funcstack_.size()) {
    JSMIRFunction *currFunc = funcstack_.top();
    jsbuilder_->SetCurrentFunction(currFunc);

    if (funcstack_.size() == 1) {
      DEBUGPRINT0;
      DEBUGPRINTfunc("main");
    }
  }
}

StmtNode *JSCompiler::CompileOpIfJump(JSOp op, BaseNode *cond, jsbytecode *pcend) {
  LabelIdx labidx = GetorCreateLabelofPc(pcend);
  StmtNode *gotonode =
    jsbuilder_->CreateStmtCondGoto(cond, (op == JSOP_IFEQ || op == JSOP_AND) ? OP_brfalse : OP_brtrue, labidx);
  gotonode->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(gotonode);
  return gotonode;
}

int64_t JSCompiler::GetIntValue(jsbytecode *pc) {
  JSOp op = JSOp(*pc);
  switch (op) {
    case JSOP_ZERO: {
      return (int64_t)0;
    }
    case JSOP_ONE: {
      return (int64_t)1;
    }
    case JSOP_INT8: {
      int ival = GET_INT8(pc);
      return (int64_t)ival;
    }
    case JSOP_INT32: {
      int ival = GET_INT32(pc);
      return (int64_t)ival;
    }
    case JSOP_UINT16: {
      unsigned uval = GET_UINT16(pc);
      return (int64_t)uval;
    }
    case JSOP_UINT24: {
      unsigned uval = GET_UINT24(pc);
      return (int64_t)uval;
    }
    default: {
      assert(0);
    }
  }
}

SwitchNode *JSCompiler::CompileOpCondSwitch(BaseNode *opnd, JSScript *script, jsbytecode *pcstart, jsbytecode *pcend) {
  int64_t constVal;
  int offset;
  jsbytecode *pcjump;
  jsbytecode *pctemp1 = pcstart;
  jsbytecode *pctemp2 = js::GetNextPc(pcend);
  LabelIdx mirlabel;
  CaseVector switchtable(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  LabelIdx defaultlabel;

  while (pctemp1 < pctemp2) {
    JSOp op = JSOp(*pctemp1);
    unsigned lineNo = js::PCToLineNumber(script, pctemp1);
    linenum_ = lineNo;
    Util::SetIndent(4);
    DEBUGPRINTnn(lineNo, Util::getOpcodeName[op]);
    DEBUGPRINT0;
    pctemp1 = js::GetNextPc(pctemp1);
  }

  while (pcstart < pcend) {
    constVal = GetIntValue(pcstart);
    pcstart = js::GetNextPc(pcstart);
    offset = GET_JUMP_OFFSET(pcstart);
    pcjump = pcstart + offset;
    mirlabel = GetorCreateLabelofPc(pcjump);
    switchtable.push_back(CasePair(constVal, mirlabel));
    pcstart = js::GetNextPc(pcstart);
  }

  offset = GET_JUMP_OFFSET(pcstart);
  pcjump = pcstart + offset;
  mirlabel = GetorCreateLabelofPc(pcjump);
  defaultlabel = mirlabel;
  SwitchNode *switchnode = jsbuilder_->CreateStmtSwitch(opnd, defaultlabel, switchtable);
  switchnode->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(switchnode);
  return switchnode;
}

SwitchNode *JSCompiler::CompileOpTableSwitch(BaseNode *opnd, int32_t len, JSScript *script, jsbytecode *pc) {
  int offset, i;
  jsbytecode *pcjump, *pctemp1, *pctemp2;
  LabelIdx mirlabel;
  CaseVector switchtable(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  LabelIdx defaultlabel;

  pctemp1 = pc;
  pctemp1 += JUMP_OFFSET_LEN;
  int32_t low = GET_JUMP_OFFSET(pctemp1);
  pctemp1 += JUMP_OFFSET_LEN;
  int32_t high = GET_JUMP_OFFSET(pctemp1);
  pctemp1 += JUMP_OFFSET_LEN;

  for (i = 0; i < high - low + 1; i++) {
    pctemp2 = pctemp1 + JUMP_OFFSET_LEN * i;
    offset = GET_JUMP_OFFSET(pctemp2);
    if (!offset) {
      continue;
    }
    pcjump = pc + offset;
    mirlabel = GetorCreateLabelofPc(pcjump);
    switchtable.push_back(CasePair((int64_t)(low + i), mirlabel));
  }

  pcjump = pc + len;
  mirlabel = GetorCreateLabelofPc(pcjump);
  defaultlabel = mirlabel;
  BaseNode *cond = CheckConvertToInt32(opnd);

  SwitchNode *stmt = jsbuilder_->CreateStmtSwitch(cond, defaultlabel, switchtable);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  return stmt;
}

LabelIdx JSCompiler::CreateLabel(char *pref) {
  const char *tempName = Util::GetSequentialName(pref ? pref : "label", temp_var_no_, mp_);
  LabelIdx labidx = jsbuilder_->GetorCreateMIRLabel(tempName);
  return labidx;
}

LabelIdx JSCompiler::GetorCreateLabelofPc(jsbytecode *pc, char *pref) {
  LabelIdx labidx;
  if (label_map_[pc] != 0) {
    labidx = label_map_[pc];
  } else {
    labidx = CreateLabel(pref);
    label_map_[pc] = labidx;
  }
  return labidx;
}

GotoNode *JSCompiler::CompileOpGoto(jsbytecode *pc, jsbytecode *jumptopc, BaseNode *expr) {
  // use special endtry label for goto within the try range
  // to simplify interpreting endtry
  LabelIdx labidx;
  EHstruct *eh = eh_->GetEHstruct(pc);
  if (eh && jumptopc == eh->endtrypc) {
    labidx = eh->label;
  } else {
    labidx = GetorCreateLabelofPc(jumptopc);
  }

  // save expr to tempvar if it's not null
  if (expr) {
    MIRSymbol *tempvar = label_tempvar_map_[labidx];
    if (tempvar == NULL) {
      tempvar = SymbolFromSavingInATemp(expr, true);
      label_tempvar_map_[labidx] = tempvar;
    }
    else {
      jsbuilder_->CreateStmtDassign(tempvar, 0, expr, linenum_);
    }
  }

  // check if it iss in try range and will jump out of it, add cleanuptry stmt
  EHstruct *ehjump = eh_->GetEHstruct(jumptopc);
  if (eh && eh != ehjump) {
    DEBUGPRINTs("creating cleanuptry");
    StmtNode *cleanuptrynode = module_->CurFuncCodeMemPool()->New<StmtNode>(OP_cleanuptry);
    cleanuptrynode->srcPosition.SetLinenum(linenum_);
    jsbuilder_->AddStmtInCurrentFunctionBody(cleanuptrynode);
  }

  GotoNode *gotonode = jsbuilder_->CreateStmtGoto(OP_goto, labidx);
  gotonode->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(gotonode);
  return gotonode;
}

GotoNode *JSCompiler::CompileOpGosub(jsbytecode *pc) {
  LabelIdx mirlabel = GetorCreateLabelofPc(pc, "f@");
  GotoNode *gosubnode = jsbuilder_->CreateStmtGoto(OP_gosub, mirlabel);
  gosubnode->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(gosubnode);
  return gosubnode;
}

JsTryNode *JSCompiler::CompileOpTry(jsbytecode *catchPc) {
  JsTryNode *trynode = jsbuilder_->CreateStmtJsTry(OP_jstry, 0, 0);
  trynode->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(trynode);

  // set up label for endtry
  LabelIdx mirlabel = CreateLabel();
  DEBUGPRINT3(mirlabel);
  // passed in is the catchpc which could also be finallypc in case there is no catch
  EHstruct *eh = eh_->GetEHstruct(0, catchPc, catchPc, 0);
  MIR_ASSERT(eh);
  eh_->SetEHLabel(eh, mirlabel);

  // set catch label in the try statement
  if (eh->catchpc) {
    mirlabel = GetorCreateLabelofPc(eh->catchpc, "h@");
    trynode->catchOffset = (uint16)mirlabel;
  }

  // set finally label in the try statement
  if (eh->finallypc) {
    mirlabel = GetorCreateLabelofPc(eh->finallypc, "f@");
    trynode->finallyOffset = (uint16)mirlabel;
  }

  return trynode;
}

BaseNode *JSCompiler::CompileOpLoopHead(jsbytecode *pc) {
  LabelIdx mirlabel = NULL;
  if (label_map_[pc] != 0) {
    mirlabel = label_map_[pc];
  } else {
    const char *tempName = Util::GetSequentialName("label", temp_var_no_, mp_);
    mirlabel = jsbuilder_->GetorCreateMIRLabel(tempName);
    label_map_[pc] = mirlabel;
  }
  StmtNode *stmt = jsbuilder_->CreateStmtLabel(label_map_[pc]);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  return stmt;
}

BaseNode *JSCompiler::CheckConvertToJSValueType(BaseNode *data) {
  MIRType *toType = NULL;
  if (IsPrimitiveDynType(data->primType)) {
    return data;
  }
  switch (data->primType) {
    case PTY_ptr:
      return data;
    case PTY_u1:
      toType = GlobalTables::GetTypeTable().GetDynbool();
      break;
    case PTY_i32:
    case PTY_u32:
      return data;
#if 0
      toType = GlobalTables::GetTypeTable().GetDyni32();
      if (data->op == OP_constval) {
        ConstvalNode *cv = static_cast<ConstvalNode *>(data);
        if (cv) {
          cv->primType = PTY_dyni32;
          MIRIntConst *ic = static_cast<MIRIntConst *>(cv->constVal);
          ic->value = (int64_t)((uint64_t)JSTYPE_NUMBER << 32 | (ic->value & 0xffffffff));
          cv->constVal = ic;
          return cv;
        }
      }
#endif
      break;
    case PTY_f32:
      toType = GlobalTables::GetTypeTable().GetDynf32();
      break;
    case PTY_f64:
      toType = GlobalTables::GetTypeTable().GetDynf64();
      break;
    case PTY_simplestr:
      toType = GlobalTables::GetTypeTable().GetDynstr();
      break;
    case PTY_simpleobj:
      toType = GlobalTables::GetTypeTable().GetDynobj();
      break;
    default:
      assert("NIY");
      break;
  }
  return jsbuilder_->CreateExprTypeCvt(OP_cvt, toType, GlobalTables::GetTypeTable().GetPrimType(data->primType), data);
}

// Pops the top two values on the stack as rval and lval, compare them with ===,
// if the result is true, jumps to a 32-bit offset from the current bytecode,
// re-pushes lval onto the stack if false.
void JSCompiler::CompileOpCase(jsbytecode *pc, int offset, BaseNode *rval, BaseNode *lval) {
  MIRIntrinsicId idx = (MIRIntrinsicId)FindIntrinsicForOp(JSOP_STRICTEQ);
  IntrinDesc *intrindesc = &IntrinDesc::intrintable[idx];
  MIRType *retty = intrindesc->GetReturnType();
  MapleVector<BaseNode*> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(CheckConvertToJSValueType(rval));
  arguments.push_back(CheckConvertToJSValueType(lval));
  BaseNode *expr =
    jsbuilder_->CreateExprIntrinsicop(idx, retty, arguments);
  BaseNode *cond = CheckConvertToBoolean(expr);

  LabelIdx mirlabel = GetorCreateLabelofPc(pc + offset);
  CondGotoNode *gotonode = jsbuilder_->CreateStmtCondGoto(cond, OP_brtrue, mirlabel);
  gotonode->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(gotonode);
  Push(lval);
  return;
}

BaseNode *JSCompiler::CheckConvertToBoolean(BaseNode *node) {
  if (IsPrimitiveInteger(node->primType)) {
    if (node->primType == PTY_u1) {
      return node;
    }
    return jsbuilder_->CreateExprTypeCvt(OP_cvt, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetPrimType(node->primType), node);
  }
  MapleVector<BaseNode*> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(node);
  return jsbuilder_->CreateExprIntrinsicop(INTRN_JS_BOOLEAN, GlobalTables::GetTypeTable().GetUInt1(), arguments);
}

BaseNode *JSCompiler::CheckConvertToInt32(BaseNode *node) {
  if (IsPrimitiveInteger(node->primType)) {
    return node;
  }
#ifdef DYNAMICLANG
  return jsbuilder_->CreateExprTypeCvt(OP_cvt, GlobalTables::GetTypeTable().GetInt32(), GlobalTables::GetTypeTable().GetPrimType(node->primType), node);
#else
  BaseNode *expr = jsbuilder_->CreateExprIntrinsicop(INTRN_JS_INT32, GlobalTables::GetTypeTable().GetInt32(), node);
  MIRSymbol *var = CreateTempVar(GlobalTables::GetTypeTable().GetInt32());
  return jsbuilder_->CreateExprDread(GlobalTables::GetTypeTable().GetInt32(), var);
#endif
}

BaseNode *JSCompiler::CheckConvertToUInt32(BaseNode *node) {
  if (IsPrimitiveInteger(node->primType)) {
    return node;
  }
#ifdef DYNAMICLANG
  return jsbuilder_->CreateExprTypeCvt(OP_cvt, GlobalTables::GetTypeTable().GetUInt32(), GlobalTables::GetTypeTable().GetPrimType(node->primType), node);
#else
  BaseNode *expr = jsbuilder_->CreateExprIntrinsicop(INTRN_JS_INT32, GlobalTables::GetTypeTable().GetInt32(), node);
  MIRSymbol *var = CreateTempVar(GlobalTables::GetTypeTable().GetInt32());
  return jsbuilder_->CreateExprDread(GlobalTables::GetTypeTable().GetInt32(), var);
#endif
}

BaseNode *JSCompiler::CheckConvertToRespectiveType(BaseNode *node, MIRType *ty) {
  if (ty == NULL || ty == jsvalueType) {
    return CheckConvertToJSValueType(node);
  }
  if (ty == GlobalTables::GetTypeTable().GetPrimType(PTY_u1)) {
    return CheckConvertToBoolean(node);
  }
  return CheckConvertToInt32(node);
}

// This variable holds a special opcode value which is greater than all normal
// opcodes, and is chosen such that the bitwise or of this value with any
// opcode is this value.
static const jsbytecode EnableInterruptsPseudoOpcode = -1;

#define NOTHANDLED DEBUGPRINTs("--- not yet handled <<<<<<<<<")
#define SIMULATESTACK(nuses, ndefs)                                      \
  do {                                                                   \
    DEBUGPRINTs("--- not yet handled but stack is simulated <<<<<<<<<"); \
    for (uint32_t i = 0; i < nuses; i++)                                 \
      Pop();                                                             \
    for (uint32_t i = 0; i < ndefs; i++)                                 \
      Push(dummyNode);                                                   \
  } while (0);

void JSCompiler::EnvInit(JSMIRFunction *func) {
  if (!func) {
    return;
  }

  if (!func->scope->IsWithEnv()) {
    return;
  }

  DEBUGPRINT3((func->env_setup));
  if (func->env_setup) {
    return;
  }

  BaseNode *bn;
  StmtNode *stmt;

  MIRType *envType = func->envtype;
  MIRType *envPtr = func->envptr;
  MIRSymbol *envVar = jsbuilder_->GetOrCreateLocalDecl("environment", GlobalTables::GetTypeTable().GetDynany());
  DEBUGPRINTsv3("environment", envPtr);

  BaseNode *size = jsbuilder_->CreateExprSizeoftype(envType);
  MapleVector<BaseNode*> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
  arguments.push_back(size);
  stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JS_NEW, arguments, envVar);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  func->env_setup = true;

  BaseNode *env = jsbuilder_->CreateExprDread(envPtr, 0, envVar);
  MIRStructType *envStruct = static_cast<MIRStructType *>(envType);
  bn = jsbuilder_->GetConstInt(envStruct->fields.size() - 2);
  int idx = jsbuilder_->GetStructFieldIdFromFieldName(envType, "argnums");
  stmt = jsbuilder_->CreateStmtIassign(envPtr, idx, env, bn);
  stmt->srcPosition.SetLinenum(linenum_);
  jsbuilder_->AddStmtInCurrentFunctionBody(stmt);

  // set up parentenv in env
  if (func->with_env_arg) {
    MIRSymbol *envArg = jsbuilder_->GetFunctionArgument(func, ENV_POSITION_IN_ARGS);
    bn = jsbuilder_->CreateExprDread(envPtr, envArg);
    idx = jsbuilder_->GetStructFieldIdFromFieldName(envType, "parentenv");
    StmtNode *stmt = jsbuilder_->CreateStmtIassign(envPtr, idx, env, bn);
    stmt->srcPosition.SetLinenum(linenum_);
    jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
  }

#ifdef DYNAMICLANG
  // set up arguments in env

  std::vector<funcArgPair>::iterator it;
  for (it = funcFormals.begin(); it != funcFormals.end(); it++) {
    if (func == (*it).first) {
      std::vector<char *>::iterator in;
      int i = (func->with_env_arg) ? 2 : 1;
      for (in = (*it).second.begin(); in != (*it).second.end(); in++, i++) {
        MIRSymbol *arg = jsbuilder_->GetFunctionArgument(func, i);
        bn = jsbuilder_->CreateExprDread(arg->GetType(), 0, arg);
        uint32_t id = jsbuilder_->GetStructFieldIdFromFieldName(envType, *in);
        stmt = jsbuilder_->CreateStmtIassign(envPtr, id, env, bn);
        stmt->srcPosition.SetLinenum(linenum_);
        jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
      }
      break;
    }
  }
#else
  // set up arguments in env
  MIRSymbol *formal = jsbuilder_->GetFunctionArgument(func, FORMAL_POSITION_IN_ARGS);
  BaseNode *base = jsbuilder_->CreateExprDread(jsvalue_ptr_, 0, formal);
  BaseNode *env = jsbuilder_->CreateExprDread(env_ptr, 0, env_var);
  BaseNode *offset;
  BaseNode *addr;
  size = jsbuilder_->CreateExprSizeoftype(jsvalueType);

  std::vector<funcArgPair>::iterator I;
  for (I = funcFormals.begin(); I != funcFormals.end(); I++) {
    if (func == (*I).first) {
      int i = 0;
      addr = base;
      std::vector<char *>::iterator IN;
      for (IN = (*I).second.begin(); IN != (*I).second.end(); IN++, i++) {
        if (i) {
          offset = jsbuilder_->CreateExprBinary(OP_mul, GlobalTables::GetTypeTable().GetVoidPtr(), jsbuilder_->GetConstInt(i), size);
          addr = jsbuilder_->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetVoidPtr(), base, offset);
        }
        bn = jsbuilder_->CreateExprIread(jsvalueType, jsvalue_ptr_, 0, addr);
        uint32_t id = jsbuilder_->GetStructFieldIdFromFieldName(env_type, *IN);
        stmt = jsbuilder_->CreateStmtIassign(env_ptr, id, env, bn);
        stmt->srcPosition.SetLinenum(linenum_);
        jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
      }
      break;
    }
  }
#endif

  return;
}

// created labels beforehand to avoid issue of missing labeled target for back edges
bool JSCompiler::MarkLabels(JSScript *script, jsbytecode *pcstart, jsbytecode *pcend) {
  jsbytecode *pc = pcstart;
  while (pc < pcend) {
    JSOp op = JSOp(*pc);
    jsbytecode *target = NULL;
    switch (op) {
      case JSOP_IFEQ:
      case JSOP_IFNE:
      case JSOP_GOTO:
      case JSOP_GOSUB:
        target = pc + GET_JUMP_OFFSET(pc);
        break;
    }

    if (target && label_map_[target] == 0) {
      if (js2mplDebug > 0) {
        printf("      0x%x jump target: 0x%x\n", pc, target);
      }
      LabelIdx mirlabel;
      if (op == JSOP_GOSUB || op == JSOP_FINALLY) {
        mirlabel = GetorCreateLabelofPc(target, "f@");
      } else {
        mirlabel = GetorCreateLabelofPc(target);
      }
      label_map_[target] = mirlabel;
    }

    pc = js::GetNextPc(pc);
  }
}

// The main entry to convert a js script to mapleir.
bool JSCompiler::CompileScript(JSScript *script) {
  jsbytecode *start = script->code();
  jsbytecode *end = script->codeEnd();

  JSMIRFunction *func = funcstack_.top();

  // here opstack_ none-empty is an error
  assert(opstack_->Empty() && "opstack is not empty");
  // clear label_map_ because nested functions in different closure
  // will share butecode
  label_map_.clear();

  EnvInit(func);

  // mark labels to avoid issue of missing labeled target for back edges
  MarkLabels(script, start, end);

  bool ret = CompileScriptBytecodes(script, start, end, NULL);

  return ret;
}

bool JSCompiler::CompileScriptBytecodes(JSScript *script, jsbytecode *pcstart, jsbytecode *pcend, jsbytecode **newpc) {
  jsbytecode *pc = pcstart;
  unsigned lastLineNo = 0;
  unsigned lastLinePrinted = 0;
  FILE *srcfileptr = script->sourceObject() ? fopen(script->filename(), "r") : NULL;  // for src line printing
  char linenoText[64];  // for printing current src line number
  char srcText[40];     // for content of current src line to be printed

  BaseNode *iter_node = NULL;
  JSOp lastOp = JSOP_NOP;
  while (pc < pcend) {
    JSOp op = JSOp(*pc);  // Convert *pc to JSOP
    unsigned lineNo = js::PCToLineNumber(script, pc);
    linenum_ = lineNo;
    Util::SetIndent(2);
    // DEBUGPRINTnn(lineNo, Util::getOpcodeName[op]);
    if (js2mplDebug > 0) {
      printf("  %4d %-25s pc = 0x%x\n", lineNo, Util::getOpcodeName[op], pc);
    }
    if (lastLineNo != lineNo && module_->CurFunction() != NULL) {
      sprintf(linenoText, "LINE %d: ", lineNo);

      srcText[0] = 0;
      if (lineNo > lastLinePrinted && srcfileptr != NULL) {
        // read and skip source file to start of current line
        for (unsigned i = lastLinePrinted + 1; i < lineNo; i++) {
          fgets(srcText, sizeof(srcText), srcfileptr);
        }
        fgets(srcText, sizeof(srcText), srcfileptr);  // read current line
        srcText[strlen(srcText) - 1] = 0;             // trim away the last \n character
        lastLinePrinted = lineNo;
      }
      // Create Comments node, line no text and src text
      StmtNode *cmntstmt = jsbuilder_->CreateStmtComment(strcat(linenoText, srcText));
      cmntstmt->srcPosition.SetLinenum(lineNo);
      jsbuilder_->AddStmtInCurrentFunctionBody(cmntstmt);
      lastLineNo = lineNo;
    }
    Util::SetIndent(4);

    // add endtry node
    EHstruct *eh = eh_->GetEHstruct(0, 0, 0, pc);
    if (eh) {
      LabelIdx labidx;
      labidx = eh->label;
      StmtNode *stmt = jsbuilder_->CreateStmtLabel(labidx);
      stmt->srcPosition.SetLinenum(lineNo);
      jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
      StmtNode *endtrynode = module_->CurFuncCodeMemPool()->New<StmtNode>(OP_endtry);
      endtrynode->srcPosition.SetLinenum(lineNo);
      jsbuilder_->AddStmtInCurrentFunctionBody(endtrynode);
    }

    if (label_map_[pc] != 0) {
      LabelIdx labidx = label_map_[pc];
      if (label_tempvar_map_[labidx] != 0) {
        // handle the else part of the conditional expression by storing to
        // the same temp
        assert(! opstack_->Empty());
        MIRSymbol *tempvar = label_tempvar_map_[labidx];
        BaseNode *expr = CheckConvertToJSValueType(Pop());
        jsbuilder_->CreateStmtDassign(tempvar, 0, expr, linenum_);
        Push(jsbuilder_->CreateExprDread(tempvar->GetType(), tempvar));
#if 0
        // assert(! opstack_->Empty());
        if (!opstack_->Empty()) {
          MIRSymbol *tempvar = label_tempvar_map_[labidx];
          BaseNode *expr = CheckConvertToJSValueType(Pop());
          jsbuilder_->CreateStmtDassign(tempvar, 0, expr, linenum_);
          if (!opstack_->Empty()) {
            BaseNode *topValue = Top();
            if (!(topValue->op == OP_dread && static_cast<AddrofNode *>(topValue)->stIdx.Islocal() &&
                  ((AddrofNode *)topValue)->stIdx == tempvar->GetStIdx())) {
              Push(jsbuilder_->CreateExprDread(tempvar->GetType(), tempvar));
            } else {
              scope_->DecDepth();
            }
          } else {
            Push(jsbuilder_->CreateExprDread(tempvar->GetType(), tempvar));
          }
        }
#endif
        label_tempvar_map_[labidx] = 0;  // re-initialize to 0
      }
      StmtNode *stmt = jsbuilder_->CreateStmtLabel(labidx);
      stmt->srcPosition.SetLinenum(lineNo);
      jsbuilder_->AddStmtInCurrentFunctionBody(stmt);

      // jump to finally for catch = pc
      eh = eh_->GetEHstruct(0, pc, 0, 0);
      if (eh) {
        StmtNode *catchnode = module_->CurFuncCodeMemPool()->New<StmtNode>(OP_jscatch);
        catchnode->srcPosition.SetLinenum(lineNo);
        jsbuilder_->AddStmtInCurrentFunctionBody(catchnode);
      }
    }

    switch (op) {
      case EnableInterruptsPseudoOpcode: {
        NOTHANDLED;
        break;
      }
      // case JSOP_NOP: /*OPC, length, stackUse, stackDef*/
      case JSOP_NOP: { /*0, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED2: { /*2, 1, 1, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED45: { /*45, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED46: { /*46, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED47: { /*47, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED48: { /*48, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED49: { /*49, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED50: { /*50, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED51: { /*51, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED52: { /*52, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED57: { /*57, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED101: { /*101, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED102: { /*102, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED103: { /*103, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED104: { /*104, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED105: { /*105, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED107: { /*107, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED124: { /*124, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED125: { /*125, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED126: { /*126, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED138: { /*138, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED139: { /*139, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED140: { /*140, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED141: { /*141, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED142: { /*142, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED146: { /*146, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED147: { /*147, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED148: { /*148, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED150: { /*150, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED156: { /*156, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED157: { /*157, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED158: { /*158, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED159: { /*159, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED161: { /*161, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED162: { /*162, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED163: { /*163, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED164: { /*164, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED165: { /*165, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED166: { /*166, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED167: { /*167, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED168: { /*168, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED169: { /*169, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED170: { /*170, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED171: { /*171, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED172: { /*172, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED173: { /*173, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED174: { /*174, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED175: { /*175, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED176: { /*176, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED177: { /*177, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED178: { /*178, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED179: { /*179, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED180: { /*180, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED181: { /*181, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED182: { /*182, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED183: { /*183, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED185: { /*185, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED186: { /*186, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED187: { /*187, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED189: { /*189, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED190: { /*190, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED191: { /*191, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED192: { /*192, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED196: { /*196, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED201: { /*201, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED205: { /*205, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED206: { /*206, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED207: { /*207, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED208: { /*208, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED209: { /*209, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED210: { /*210, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED211: { /*211, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED212: { /*212, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED213: { /*213, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED219: { /*219, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED220: { /*220, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED221: { /*221, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED222: { /*222, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNUSED223: { /*223, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_BACKPATCH: { /*149, 5, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_CONDSWITCH: { /*120, 1, 0, 0*/
        pc = js::GetNextPc(pc);
        continue;
      }
      case JSOP_TRY: { /*134, 1, 0, 0*/
        JSTryNote *tn = script->trynotes()->vector;
        JSTryNote *tnlimit = tn + script->trynotes()->length;
        for (; tn < tnlimit; tn++) {
          if ((tn->start + script->mainOffset()) == (pc - script->code() + 1)) {
            jsbytecode *catchpc = pc + 1 + tn->length;
            CompileOpTry(catchpc);
            break;
          }
        }
        break;
      }
      case JSOP_LOOPHEAD: { /*109, 1, 0, 0*/
        // jsbytecode *headpc = pc;
        // CompileOpLoopHead(headpc);
        break;
      }
      case JSOP_LABEL: { /*106, 5, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_LOOPENTRY: { /*227, 2, 0, 0*/
        if (iter_node && lastOp == JSOP_RETRVAL) {
          // has return in loop using iterator. push the iter back
          Push(iter_node);
        }
        break;
      }
      case JSOP_LINENO: { /*119, 3, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_UNDEFINED: { /*1, 1, 0, 1*/
        BaseNode *undefined = CompileOpConstValue(JSTYPE_UNDEFINED, 0);
        Push(undefined);
        break;
      }
      case JSOP_POP: { /*81, 1, 1, 0*/
        Pop();
        opstack_->flag_after_throwing = false;
        break;
      }
      case JSOP_POPN: { /*11, 3, -1, 0*/
        uint32_t n = GET_UINT16(pc);
        DEBUGPRINT2(n);
        // for (uint32_t i = 0; i < n; i++) Pop();
        break;
      }
      case JSOP_DUPAT: { /*44, 4, 0, 1*/
        uint32_t n = GET_UINT8(pc);
        BaseNode *bn = GetOpAt(n);
        Push(bn);
        break;
      }
      case JSOP_SETRVAL: { /*152, 1, 1, 0*/
        opstack_->flag_has_rval = true;
        BaseNode *rval = Pop();
        opstack_->rval = rval;
        break;
      }
      case JSOP_ENTERWITH: { /*3, 5, 1, 0*/
        SIMULATESTACK(1, 0);
        break;
      }
      case JSOP_LEAVEWITH: { /*4, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_RETURN: { /*5, 1, 1, 0*/
        BaseNode *rval = static_cast<BaseNode *>(Pop());
        BaseNode *bn = CheckConvertToJSValueType(rval);
        jsbuilder_->CreateStmtReturn(bn, true, linenum_);
        break;
      }
      case JSOP_RETRVAL: { /*153, 1, 0, 0*/
        if (true == opstack_->flag_has_rval) {
          JSMIRFunction *func = jsbuilder_->GetCurrentFunction();
          if (jsbuilder_->IsPluginFunc(func)) {
            // set to return exports anyway
            // BaseNode *id_node = jsbuilder_->GetConstUInt32((uint32_t) JS_BUILTIN_MODULE);
            BaseNode *node1 = CheckConvertToJSValueType(
              CompileGeneric1(INTRN_JS_GET_BIOBJECT, jsbuilder_->GetConstUInt32((uint32_t)JS_BUILTIN_MODULE), false));
            BaseNode *node2 = CheckConvertToJSValueType(CompileGeneric1(
              INTRN_JS_GET_BISTRING, jsbuilder_->GetConstUInt32((uint32_t)JSBUILTIN_STRING_EXPORTS), false));
            BaseNode *retExpr = CompileGeneric2(INTRN_JSOP_GETPROP, node1, node2, true);
            SetupMainFuncRet(retExpr);
          } else if (func == jsmain_) {
            SetupMainFuncRet(jsbuilder_->GetConstInt(0));  // main function always returns 0
          } else {
            jsbuilder_->CreateStmtReturn(CheckConvertToJSValueType(opstack_->rval), false, linenum_);
          }
          opstack_->flag_has_rval = false;
        } else {
          JSMIRFunction *func = jsbuilder_->GetCurrentFunction();
          if (jsbuilder_->IsPluginFunc(func)) {
            // BaseNode *id_node = jsbuilder_->GetConstUInt32((uint32_t) JS_BUILTIN_MODULE);
            BaseNode *node1 = CheckConvertToJSValueType(
              CompileGeneric1(INTRN_JS_GET_BIOBJECT, jsbuilder_->GetConstUInt32((uint32_t)JS_BUILTIN_MODULE), false));
            BaseNode *node2 = CheckConvertToJSValueType(CompileGeneric1(
              INTRN_JS_GET_BISTRING, jsbuilder_->GetConstUInt32((uint32_t)JSBUILTIN_STRING_EXPORTS), false));
            BaseNode *retExpr = CompileGeneric2(INTRN_JSOP_GETPROP, node1, node2, true);
            jsbuilder_->CreateStmtReturn(retExpr, false, linenum_);
          } else if (func == jsmain_) {
            BaseNode *undefined = CompileOpConstValue(JSTYPE_UNDEFINED, 0);
            jsbuilder_->CreateStmtReturn(undefined, false, linenum_);
          } else {
            jsbuilder_->CreateStmtReturn(jsbuilder_->GetConstInt(0), false, linenum_);
          }
        }
        break;
      }
      case JSOP_DEFAULT: { /*122, 5, 1, 0*/
        Pop();
      }
      case JSOP_GOTO: { /*6, 5, 0, 0*/
        int offset = GET_JUMP_OFFSET(pc);
        jsbytecode* next = js::GetNextPc(pc);
        if (depth_map_.find(next) != depth_map_.end() &&
            GetDepth() > depth_map_[next]) {
          // in middle of conditional expression; save in a temp and associate
          // the temp with the goto label
          // TODO should not pop from opstack_
          BaseNode *expr = Pop();
          CompileOpGoto(pc, pc + offset, expr);
        } else {
          CompileOpGoto(pc, pc + offset, NULL);
        }
        break;
      }

      case JSOP_IFEQ:   /*7, 5, 1, 0*/
      case JSOP_IFNE: { /*8, 5, 1, 0*/
        int offset = GET_JUMP_OFFSET(pc);
        BaseNode *opnd = Pop();
        BaseNode *cond = CheckConvertToBoolean(opnd);
        CompileOpIfJump(op, cond, pc + offset);
        depth_map_[pc + offset] = GetDepth();
        break;
      }

      case JSOP_OR:    /*68, 5, 1, 1*/
      case JSOP_AND: { /*69, 5, 1, 1*/
        int offset = GET_JUMP_OFFSET(pc);
        BaseNode *opnd0 = CheckConvertToJSValueType(Pop());  // Pop IFEQ stmt
        BaseNode *cond0 = CheckConvertToBoolean(opnd0);
        MIRSymbol *tempVar = SymbolFromSavingInATemp(opnd0, true);
        opnd0 = jsbuilder_->CreateExprDread(GlobalTables::GetTypeTable().GetUInt1(), tempVar);
        Push(opnd0);

        LabelIdx mirlabel = GetorCreateLabelofPc(pc + offset);
        CondGotoNode *gotonode =
          jsbuilder_->CreateStmtCondGoto(cond0, (op == JSOP_AND) ? OP_brfalse : OP_brtrue, mirlabel);
        jsbuilder_->AddStmtInCurrentFunctionBody(gotonode);

        jsbytecode *start = js::GetNextPc(pc);
        pc = pc + offset;
        // Pop(); comment out because CompileScriptBytecodes() has a Pop() which is not listed on command
        // list
        CompileScriptBytecodes(script, start, pc, NULL);
        BaseNode *opnd1 = CheckConvertToJSValueType(Pop());
        jsbuilder_->CreateStmtDassign(tempVar, 0, opnd1, linenum_);
        opnd0 = jsbuilder_->CreateExprDread(tempVar->GetType(), tempVar);
        Push(opnd0);
        continue;
      }
      case JSOP_ITER: { /*75, 2, 1, 1*/
        uint8_t flags = GET_UINT8(pc);
        BaseNode *bn = CheckConvertToJSValueType(Pop());
        BaseNode *itr = CompileOpNewIterator(bn, flags);
        Push(itr);
        opstack_->flag_has_iter = true;
        break;
      }
      case JSOP_MOREITER: { /*76, 1, 1, 2*/
        BaseNode *iterator = Top();
        BaseNode *isIterator = CompileOpMoreIterator(iterator);
        Push(isIterator);
        break;
      }
      case JSOP_ITERNEXT: { /*77, 1, 0, 1*/
        BaseNode *iterator = Top();
        BaseNode *bn = CompileOpIterNext(iterator);
        Push(bn);
        break;
      }
      case JSOP_ENDITER: { /*78, 1, 1, 0*/
        // save iterator in case there is return in loop body
        iter_node = Pop();
        opstack_->flag_has_iter = false;
        break;
      }
      case JSOP_DUP: { /*12, 1, 1, 2*/
        BaseNode *bn = Pop();
        Push(bn);
        Push(static_cast<BaseNode *>(bn->CloneTree(module_)));
        break;
      }
      case JSOP_DUP2: { /*13, 1, 2, 4*/
        BaseNode *bn1 = Pop();
        BaseNode *bn2 = Pop();
        Push(bn2);
        Push(bn1);
        Push(static_cast<BaseNode *>(bn2->CloneTree(module_)));
        Push(static_cast<BaseNode *>(bn1->CloneTree(module_)));
        break;
      }
      case JSOP_SWAP: { /*10, 1, 2, 2*/
        BaseNode *bn1 = Pop();
        BaseNode *bn2 = Pop();
        Push(bn1);
        Push(bn2);
        break;
      }
      case JSOP_PICK: { /*133, 2, 0, 0*/
        uint32_t n = GET_UINT8(pc);
        std::vector<BaseNode *> tempStack;
        for (uint32_t i = 0; i < n; i++) {
          tempStack.push_back(Pop());
        }
        BaseNode *bn = Pop();
        for (uint32_t i = 0; i < n; i++) {
          Push(tempStack[tempStack.size() - 1]);
          tempStack.pop_back();
        }
        Push(bn);
        break;
      }
      case JSOP_SETCONST: { /*14, 5, 1, 1*/
        SIMULATESTACK(1, 1);
        break;
      }
      case JSOP_BINDINTRINSIC: { /*145, 5, 0, 1*/
        SIMULATESTACK(0, 1);
        break;
      }
      case JSOP_BINDGNAME: { /*214, 5, 0, 1*/
        case JSOP_BINDNAME:  /*110, 5, 0, 1*/
          JSAtom *atom = script->getName(pc);
          BaseNode *node = CompileOpBindName(atom);
          Push(node);
          break;
      }
      case JSOP_CASE: { /*121, 5, 2, 1*/
        BaseNode *rval = Pop();
        BaseNode *lval = Pop();
        int offset = GET_JUMP_OFFSET(pc);
        CompileOpCase(pc, offset, rval, lval);
        break;
      }
      // Binary operations.
      case JSOP_BITOR:      /*15, 1, 2, 1*/
      case JSOP_BITXOR:     /*16, 1, 2, 1*/
      case JSOP_BITAND:     /*17, 1, 2, 1*/
      case JSOP_EQ:         /*18, 1, 2, 1*/
      case JSOP_NE:         /*19, 1, 2, 1*/
      case JSOP_LT:         /*20, 1, 2, 1*/
      case JSOP_LE:         /*21, 1, 2, 1*/
      case JSOP_GT:         /*22, 1, 2, 1*/
      case JSOP_GE:         /*23, 1, 2, 1*/
      case JSOP_LSH:        /*24, 1, 2, 1*/
      case JSOP_RSH:        /*25, 1, 2, 1*/
      case JSOP_URSH:       /*26, 1, 2, 1*/
      case JSOP_ADD:        /*27, 1, 2, 1*/
      case JSOP_SUB:        /*28, 1, 2, 1*/
      case JSOP_MUL:        /*29, 1, 2, 1*/
      case JSOP_DIV:        /*30, 1, 2, 1*/
      case JSOP_MOD:        /*31, 1, 2, 1*/
      case JSOP_STRICTEQ:   /*72, 1, 2, 1*/
      case JSOP_STRICTNE:   /*73, 1, 2, 1*/
      case JSOP_INSTANCEOF: /*114, 1, 2, 1*/
      case JSOP_IN: {       /*113, 1, 2, 1*/
        BaseNode *opnd1 = Pop();
        BaseNode *opnd0 = Pop();
        BaseNode *result = CompileOpBinary(op, opnd0, opnd1);
        Push(result);
        break;
      }
      case JSOP_NOT:    /*32, 1, 1, 1*/
      case JSOP_BITNOT: /*33, 1, 1, 1*/
      case JSOP_NEG:    /*34, 1, 1, 1*/
      case JSOP_POS: {  /*35, 1, 1, 1*/
        BaseNode *opnd = Pop();
        BaseNode *result = CompileOpUnary(op, opnd);
        Push(result);
        break;
      }
      case JSOP_DELNAME: { /*36, 5, 0, 1*/
        SIMULATESTACK(0, 1);
        break;
      }
      case JSOP_DELPROP: { /*37, 5, 1, 1*/
        JSString *str = script->getAtom(pc);
        BaseNode *nameIndex = CheckConvertToJSValueType(CompileOpString(str));
        BaseNode *obj = CheckConvertToJSValueType(Pop());
        BaseNode *res = CompileGeneric2(INTRN_JSOP_DELPROP, obj, nameIndex, true);
        Push(res);
        break;
      }
      case JSOP_DELELEM: { /*38, 1, 2, 1*/
        BaseNode *index = Pop();
        BaseNode *obj = CheckConvertToJSValueType(Pop());
        index = CheckConvertToJSValueType(index);
        BaseNode *bn = CompileGeneric2(INTRN_JSOP_DELPROP, obj, index, true);
        Push(bn);
        break;
      }
      case JSOP_TOID: { /*225, 1, 1, 1*/
        SIMULATESTACK(1, 1);
        break;
      }
      case JSOP_TYPEOFEXPR: /*197, 1, 1, 1*/
      case JSOP_TYPEOF: {   /*39, 1, 1, 1*/
        BaseNode *opnd = Pop();
        BaseNode *result = CompileOpUnary(JSOP_TYPEOF, opnd);
        Push(result);
        break;
      }
      case JSOP_VOID: { /*40, 1, 1, 1*/
        (void)Pop();
        Push(CompileOpConstValue(JSTYPE_UNDEFINED, 0));
        break;
      }
      case JSOP_THIS: { /*65, 1, 0, 1*/
        BaseNode *bn = CompileGeneric0(INTRN_JSOP_THIS, false);
        Push(bn);
        break;
      }
      case JSOP_GETPROP:    /*53, 5, 1, 1*/
      case JSOP_CALLPROP: { /*184, 5, 1, 1*/
        JSString *str = script->getAtom(pc);
        BaseNode *obj = Pop();
        BaseNode *val = CompileLibraryCall(obj, str);
        if (!val) {
          obj = CheckConvertToJSValueType(obj);
          BaseNode *name = CompileOpString(str);
          val = CompileGeneric2(INTRN_JSOP_GETPROP_BY_NAME, obj, name, true);
        }
        Push(val);
        break;
      }
      case JSOP_GETXPROP: { /*195, 5, 1, 1*/
        // See BytecodeEmitter.cpp:3678, JSOP_GETXPROP seems like JSOP_NAME.
        // Actually I think it is a bug in SpiderMonkey.
        Pop();
        JSAtom *atom = script->getAtom(GET_UINT32_INDEX(pc));
        BaseNode *bn = CompileOpName(atom, pc);
        Push(bn);
        break;
      }
      case JSOP_LENGTH: { /*217, 5, 1, 1*/
        BaseNode *array = CheckConvertToJSValueType(Pop());
        BaseNode *length = CompileGeneric1(INTRN_JSOP_LENGTH, array, false);
        Push(length);
        break;
      }
      case JSOP_SETINTRINSIC: { /*144, 5, 2, 1*/
        SIMULATESTACK(2, 1);
        break;
      }
      case JSOP_SETGNAME:  /*155, 5, 2, 1*/
      case JSOP_SETNAME: { /*111, 5, 2, 1*/
          JSAtom *atom = script->getName(pc);
          BaseNode *val = Pop();
          Pop();  // pop the scope
          if (!CompileOpSetName(atom, val)) {
            return false;
          }
          break;
      }
      case JSOP_CALLELEM:  /*193, 1, 2, 1*/
      case JSOP_GETELEM: { /*55, 1, 2, 1*/
        BaseNode *index = Pop();
        BaseNode *obj = CheckConvertToJSValueType(Pop());
        index = CheckConvertToJSValueType(index);
        BaseNode *elem = CompileGeneric2(op == JSOP_GETELEM ? INTRN_JSOP_GETPROP : INTRN_JSOP_CALLPROP,
                                         obj, index, true);
        Push(elem);
        break;
      }
      case JSOP_INITELEM:  /*94, 1, 3, 1*/
      case JSOP_SETELEM: { /*56, 1, 3, 1*/
        BaseNode *val = Pop();
        BaseNode *index = Pop();
        BaseNode *obj = CheckConvertToJSValueType(Pop());
        index = CheckConvertToJSValueType(index);
	MapleVector<BaseNode*> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
	arguments.push_back(obj);
	arguments.push_back(index);
	arguments.push_back(CheckConvertToJSValueType(val));
        StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JSOP_SETPROP, arguments, (MIRSymbol*)NULL);
        stmt->srcPosition.SetLinenum(lineNo);
        jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
        Push(obj);
        break;
      }
      case JSOP_EVAL: { /*123, 3, -1, 1*/
        assert("false && Can not support eval!");
        SIMULATESTACK(2 + GET_ARGC(pc), 1);
        break;
      }
      case JSOP_SPREADNEW: { /*42, 1, 3, 1*/
        SIMULATESTACK(3, 1);
        break;
      }
      case JSOP_SPREADCALL: { /*41, 1, 3, 1*/
        SIMULATESTACK(3, 1);
        break;
      }
      case JSOP_SPREADEVAL: { /*43, 1, 3, 1*/
        SIMULATESTACK(3, 1);
        break;
      }
      case JSOP_NEW: { /*82, 3, -1, 1*/
        uint32_t argc = GET_ARGC(pc);
        BaseNode *bn = CompileOpNew(argc);
        Push(bn);
        break;
      }
      case JSOP_FUNAPPLY: /*79, 3, -1, 1*/
      case JSOP_FUNCALL:  /*108, 3, -1, 1*/
      case JSOP_CALL: {   /*58, 3, -1, 1*/
        uint32_t argc = GET_ARGC(pc);
        BaseNode *bn = CompileOpCall(argc);
        Push(bn);
        break;
      }
      case JSOP_SETCALL: { /*74, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_IMPLICITTHIS: { /*226, 5, 0, 1*/
        BaseNode *bn = CompileOpConstValue(JSTYPE_UNDEFINED, 0);
        Push(bn);
        break;
      }
      case JSOP_GETGNAME: /*154, 5, 0, 1*/
      case JSOP_NAME: {   /*59, 5, 0, 1*/
        JSAtom *atom = script->getAtom(GET_UINT32_INDEX(pc));
        BaseNode *bn = CompileOpName(atom, pc);
        Push(bn);
        break;
      }
      case JSOP_GETINTRINSIC: { /*143, 5, 0, 1*/
        SIMULATESTACK(0, 1);
        break;
      }
      case JSOP_UINT16: { /*88, 3, 0, 1*/
        unsigned uval = GET_UINT16(pc);
        BaseNode *bn = CompileOpConstValue(JSTYPE_NUMBER, uval);
        DEBUGPRINT2(uval);
        Push(bn);
        break;
      }
      case JSOP_UINT24: { /*188, 4, 0, 1*/
        unsigned uval = GET_UINT24(pc);
        BaseNode *bn = CompileOpConstValue(JSTYPE_NUMBER, uval);
        DEBUGPRINT2(uval);
        Push(bn);
        break;
      }
      case JSOP_INT8: { /*215, 2, 0, 1*/
        int ival = GET_INT8(pc);
        BaseNode *bn = CompileOpConstValue(JSTYPE_NUMBER, ival);
        DEBUGPRINT2(ival);
        Push(bn);
        break;
      }
      case JSOP_INT32: { /*216, 5, 0, 1*/
        int ival = GET_INT32(pc);
        BaseNode *bn = CompileOpConstValue(JSTYPE_NUMBER, ival);
        DEBUGPRINT2(ival);
        Push(bn);
        break;
      }
      case JSOP_DOUBLE: { /*60, 5, 0, 1*/
        double dval = script->getConst(GET_UINT32_INDEX(pc)).toDouble();
        BaseNode *bn = CompileDoubleConst(dval);
        DEBUGPRINT2(dval);
        Push(bn);
        break;
      }
      case JSOP_STRING: { /*61, 5, 0, 1*/
        JSString *str = script->getAtom(pc);
        BaseNode *bn = CompileOpString(str);
        Push(bn);
        break;
      }
      case JSOP_OBJECT: { /*80, 5, 0, 1*/
        SIMULATESTACK(0, 1);
        break;
      }
      case JSOP_REGEXP: { /*160, 5, 0, 1*/
        js::RegExpObject *robj = script->getRegExp(GET_UINT32_INDEX(pc));
        assert(robj);
        BaseNode *bn = CompileOpString(robj->toString(jscontext_));
        BaseNode *res = CompileGeneric1(INTRN_JS_REGEXP, bn, true);
        Push(res);
        break;
      }
      case JSOP_ZERO: { /*62, 1, 0, 1*/
        BaseNode *bn = CompileOpConstValue(JSTYPE_NUMBER, 0);
        Push(bn);
        break;
      }
      case JSOP_ONE: { /*63, 1, 0, 1*/
        BaseNode *bn = CompileOpConstValue(JSTYPE_NUMBER, 1);
        Push(bn);
        break;
      }
      case JSOP_NULL: { /*64, 1, 0, 1*/
        BaseNode *bn = CompileOpConstValue(JSTYPE_NULL, 0);
        Push(bn);
        break;
      }
      case JSOP_FALSE: { /*66, 1, 0, 1*/
        BaseNode *bn = CompileOpConstValue(JSTYPE_BOOLEAN, 0);
        Push(bn);
        break;
      }
      case JSOP_TRUE: { /*67, 1, 0, 1*/
        BaseNode *bn = CompileOpConstValue(JSTYPE_BOOLEAN, 1);
        Push(bn);
        break;
      }
      case JSOP_TABLESWITCH: { /*70, -1, 1, 0*/
        int32_t len = GET_JUMP_OFFSET(pc);
        BaseNode *opnd = Pop();
        CompileOpTableSwitch(opnd, len, script, pc);
        break;
      }
      case JSOP_ARGUMENTS: { /*9, 1, 0, 1*/
        SIMULATESTACK(0, 1);
        break;
      }
      case JSOP_RUNONCE: { /*71, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_REST: { /*224, 1, 0, 1*/
        SIMULATESTACK(0, 1);
        break;
      }
      case JSOP_GETALIASEDVAR: { /*136, 5, 0, 1*/
        JSAtom *atom = ScopeCoordinateName(jscontext_->runtime()->scopeCoordinateNameCache, script, pc);
        //DEBUGPRINT2(atom);
        BaseNode *bn = CompileOpGetAliasedVar(atom);
        Push(bn);
        break;
      }
      case JSOP_SETALIASEDVAR: { /*137, 5, 1, 1*/
        BaseNode *val = CheckConvertToJSValueType(Pop());
        JSAtom *atom = ScopeCoordinateName(jscontext_->runtime()->scopeCoordinateNameCache, script, pc);
        BaseNode *bn = CompileOpSetAliasedVar(atom, val);
        Push(val);
        break;
      }
      case JSOP_GETARG: { /*84, 3, 0, 1*/
        uint32_t i = GET_ARGNO(pc);
        BaseNode *bn = CompileOpGetArg(i);
        Push(bn);
        break;
      }
      case JSOP_SETARG: { /*85, 3, 1, 1*/
        uint32_t i = GET_ARGNO(pc);
        BaseNode *bn = CheckConvertToJSValueType(Pop());
        CompileOpSetArg(i, bn);
        break;
      }
      case JSOP_GETLOCAL: { /*86, 4, 0, 1*/
        uint32_t i = GET_LOCALNO(pc);
        BaseNode *bn = CompileOpGetLocal(i);
        Push(bn);
        break;
      }
      case JSOP_SETLOCAL: { /*87, 4, 1, 1*/
        uint32_t i = GET_LOCALNO(pc);
        BaseNode *src = CheckConvertToJSValueType(Pop());
        BaseNode *bn = CompileOpSetLocal(i, src);
        Push(src);
        break;
      }
      case JSOP_NEWINIT: { /*89, 5, 0, 1*/
        // Array or Object.
        uint8_t i = GET_UINT8(pc);
        BaseNode *obj = CompileOpNewInit(i);
        Push(obj);
        break;
      }
      case JSOP_NEWARRAY: { /*90, 4, 0, 1*/
        uint32_t length = GET_UINT24(pc);
        BaseNode *len = jsbuilder_->GetConstUInt32(length);
#if 1
        BaseNode *arr = CompileGeneric1(INTRN_JS_NEW_ARR_LENGTH, CheckConvertToJSValueType(len), true);
        Push(arr);
        break;
#else
        std::stack<BaseNode *> tmp_stack;
        pc = js::GetNextPc(pc);
        op = JSOp(*pc);
        jsbytecode *new_pc;
        while (op != JSOP_ENDINIT) {
          if (op != JSOP_INITELEM_ARRAY) {
            if (op == JSOP_NEWARRAY) {
              CompileScriptBytecodes(script, pc, js::GetNextPc(pc), &new_pc);
              pc = new_pc;
            } else {
              CompileScriptBytecodes(script, pc, js::GetNextPc(pc), NULL);
            }
          } else {
            BaseNode *init = Pop();
            tmp_stack.push(init);
          }
          pc = js::GetNextPc(pc);
          op = JSOp(*pc);
        }
        if (newpc) {
          *newpc = pc;
        }

        if (length == 0) {
          BaseNode *arr = CompileGeneric1(INTRN_JS_NEW_ARR_LENGTH,
                                          CheckConvertToJSValueType(CompileOpConstValue(JSTYPE_NUMBER, 0)), true);
          Push(arr);
          break;
        }
        MIRSymbol *arguments = NULL;
        arguments = jsbuilder_->GetCurrentFunction()->symtab->CreateSymbol();
        const char *temp_name = Util::GetSequentialName("js_arguments_", temp_var_no_, mp_);
        std::string argname(temp_name, mp_);
        arguments->SetNameStridx(module_->stringtable.GetOrCreateStridxFromName(argname));
        jsbuilder_->GetCurrentFunction()->symtab->AddToStringSymbolMap(arguments);
        arguments->sclass = kScAuto;
        arguments->skind = kStVar;

        uint32_t size_array[1];
        size_array[0] = length;
        MIRType *array_type = GlobalTables::GetTypeTable().GetOrCreateArrayType(jsvalueType, 1, size_array);
        MIRType *array_ptr_type = GlobalTables::GetTypeTable().GetOrCreatePointerType(array_type);
        TyIdx tyidx = array_type->tyIdx;
        arguments->SetTyIdx(tyidx);
        BaseNode *bn;
        MIRType *pargtype = GlobalTables::GetTypeTable().GetOrCreatePointerType(arguments->GetType());
        BaseNode *addr_base = jsbuilder_->CreateExprAddrof(0, arguments);

        for (uint32_t i = 0; i < length; i++) {
          bn = CheckConvertToJSValueType(tmp_stack.top());
          DEBUGPRINT3(bn->op);
          tmp_stack.pop();
          MapleVector<BaseNode *> opnds(module_.CurFuncCodeMemPoolAllocator()->Adapter());
          opnds.push_back(addr_base);
          BaseNode *addr_offset = jsbuilder_->GetConstInt(length - i - 1);
          opnds.push_back(addr_offset);
          BaseNode *array_expr = jsbuilder_->CreateExprArray(array_type, opnds);
          BaseNode *stmt = jsbuilder_->CreateStmtIassign(array_ptr_type, 0, array_expr, bn);
          jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
        }

        BaseNode *arr = CompileGeneric2(INTRN_JS_NEW_ARR_ELEMS, addr_base, len, true);
        Push(arr);
        break;
#endif
      }
      case JSOP_NEWOBJECT: { /*91, 5, 0, 1*/
        SIMULATESTACK(0, 1);
        break;
      }
      case JSOP_ENDINIT: { /*92, 1, 0, 0*/
        // Do nothing.
        // JS_ASSERT(REGS.stackDepth() >= 1);
        // JS_ASSERT(REGS.sp[-1].isObject() || REGS.sp[-1].isUndefined());
        break;
      }
      case JSOP_DEFCONST: { /*128, 5, 0, 0*/
        DEBUGPRINT2(JSOP_DEFCONST);
        break;
      }
      case JSOP_DEFVAR: { /*129, 5, 0, 0*/
        //DEBUGPRINT2(JSOP_DEFVAR);
        JSAtom *atom = script->getAtom(GET_UINT32_INDEX(pc));
        CompileOpDefVar(atom);
        break;
      }
      case JSOP_DEFFUN: { /*127, 5, 0, 0*/
        DEBUGPRINT2(JSOP_DEFFUN);
        JSFunction *jsfun = script->getFunction(GET_UINT32_INDEX(pc));
        CompileOpDefFun(jsfun);
        break;
      }
      case JSOP_LAMBDA: { /*130, 5, 0, 1*/
        JSFunction *jsfun = script->getFunction(GET_UINT32_INDEX(pc));
        BaseNode *bn = CompileOpLambda(pc, jsfun);
        Push(bn);
        break;
      }
      case JSOP_LAMBDA_ARROW: { /*131, 5, 1, 1*/
        SIMULATESTACK(1, 1);
        break;
      }
      case JSOP_CALLEE: { /*132, 1, 0, 1*/
        char *name = funcstack_.top()->scope->GetName();
        char *objname = Util::GetNameWithSuffix(name, "_obj_", mp_);
        MIRSymbol *var = jsbuilder_->GetOrCreateGlobalDecl(objname, jsvalueType);
        BaseNode *bn = jsbuilder_->CreateExprDread(var);
        Push(bn);
        break;
      }
      case JSOP_INITPROP_GETTER: { /*97, 5, 2, 1*/
        JSString *str = script->getAtom(pc);
        DEBUGPRINT2(str);
        BaseNode *val = CheckConvertToJSValueType(Pop());
        BaseNode *obj = CheckConvertToJSValueType(Pop());
        if (!CompileOpInitPropGetter(obj, str, val)) {
          return false;
        }
        Push(obj);
        break;
      }
      case JSOP_INITPROP_SETTER: { /*98, 5, 2, 1*/
        JSString *str = script->getAtom(pc);
        DEBUGPRINT2(str);
        BaseNode *val = CheckConvertToJSValueType(Pop());
        BaseNode *obj = CheckConvertToJSValueType(Pop());
        if (!CompileOpInitPropSetter(obj, str, val)) {
          return false;
        }
        Push(obj);
        break;
      }
      case JSOP_INITELEM_GETTER: { /*99, 1, 3, 1*/
        BaseNode *val = CheckConvertToJSValueType(Pop());
        BaseNode *id = Pop();
        BaseNode *obj = CheckConvertToJSValueType(Pop());
        if (!CompileOpInitElemGetter(obj, id, val)) {
          return false;
        }
        Push(obj);
        break;
      }
      case JSOP_INITELEM_SETTER: { /*100, 1, 3, 1*/
        BaseNode *val = CheckConvertToJSValueType(Pop());
        BaseNode *id = Pop();
        BaseNode *obj = CheckConvertToJSValueType(Pop());
        if (!CompileOpInitElemSetter(obj, id, val)) {
          return false;
        }
        Push(obj);
        break;
      }
      case JSOP_HOLE: { /*218, 1, 0, 1*/
        BaseNode *holeElem = CompileOpConstValue(JSTYPE_NONE, 0);
        Push(holeElem);
        break;
      }
      case JSOP_MUTATEPROTO: { /*194, 1, 2, 1*/
        SIMULATESTACK(2, 1);
        break;
      }
      case JSOP_SETPROP: { /*54, 5, 2, 1*/
        JSString *str = script->getAtom(pc);
        DEBUGPRINT2(str);
        BaseNode *val = Pop();
        BaseNode *obj = CheckConvertToJSValueType(Pop());
        if (!CompileOpSetProp(obj, str, val)) {
          return false;
        }
        Push(val);
        break;
      }
      case JSOP_INITPROP: { /*93, 5, 2, 1*/
        JSString *str = script->getAtom(pc);
        DEBUGPRINT2(str);
        BaseNode *val = Pop();
        BaseNode *obj = CheckConvertToJSValueType(Pop());
        BaseNode *name = CompileOpString(str);
	MapleVector<BaseNode*> arguments(module_->CurFuncCodeMemPoolAllocator()->Adapter());
	arguments.push_back(obj);
	arguments.push_back(name);
	arguments.push_back(CheckConvertToJSValueType(val));
        StmtNode *stmt = jsbuilder_->CreateStmtIntrinsicCallAssigned(INTRN_JSOP_INITPROP_BY_NAME, arguments, (MIRSymbol*)NULL);
        stmt->srcPosition.SetLinenum(lineNo);
        jsbuilder_->AddStmtInCurrentFunctionBody(stmt);
        Push(obj);
        break;
      }
      case JSOP_INITELEM_ARRAY: { /*96, 4, 2, 1*/
        BaseNode *init = Pop();
        BaseNode *arr = CheckConvertToJSValueType(Pop());
        BaseNode *index = CompileOpConstValue(JSTYPE_NUMBER, (int32_t)GET_UINT24(pc));
        if (!CompileOpSetElem(arr, index, init)) {
          return false;
        }
        Push(arr);
        break;
      }
      case JSOP_INITELEM_INC: { /*95, 1, 3, 2*/
        SIMULATESTACK(3, 2);
        break;
      }
      case JSOP_SPREAD: { /*83, 1, 3, 2*/
        SIMULATESTACK(3, 2);
        break;
      }
      case JSOP_GOSUB: { /*116, 5, 0, 0*/
        int offset = GET_JUMP_OFFSET(pc);
        CompileOpGosub(pc + offset);
        break;
      }
      case JSOP_RETSUB: { /*117, 1, 2, 0*/
        BaseNode *lval = Pop();
        BaseNode *rval = Pop();
        StmtNode *retsub = module_->CurFuncCodeMemPool()->New<StmtNode>(OP_retsub);
        retsub->srcPosition.SetLinenum(lineNo);
        jsbuilder_->AddStmtInCurrentFunctionBody(retsub);
        break;
      }
      case JSOP_EXCEPTION: { /*118, 1, 0, 1*/
        BaseNode *rr = jsbuilder_->CreateExprRegread(PTY_dynany, -kSregThrownval);
        Push(rr);
        break;
      }
      case JSOP_FINALLY: { /*135, 1, 0, 2*/
        StmtNode *finally = module_->CurFuncCodeMemPool()->New<StmtNode>(OP_finally);
        finally->srcPosition.SetLinenum(lineNo);
        jsbuilder_->AddStmtInCurrentFunctionBody(finally);
        // TODO: need to Push two entries onto stack.  false, (next bytecode's PC)
        BaseNode *bval = CompileOpConstValue(JSTYPE_BOOLEAN, 0);
        BaseNode *tval = CompileOpConstValue(JSTYPE_NUMBER, GET_UINT32_INDEX(js::GetNextPc(pc)));
        Push(tval);
        Push(bval);
        break;
      }
      case JSOP_THROWING: /*151, 1, 1, 0*/
        opstack_->flag_after_throwing = true;
        SIMULATESTACK(1, 0);
        break;
      case JSOP_THROW: { /*112, 1, 1, 0*/
        BaseNode *rval = Pop();
        StmtNode *throwstmt = jsbuilder_->CreateStmtThrow(CheckConvertToJSValueType(rval));
        throwstmt->srcPosition.SetLinenum(lineNo);
        jsbuilder_->AddStmtInCurrentFunctionBody(throwstmt);
        break;
      }
      case JSOP_DEBUGGER: { /*115, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_PUSHBLOCKSCOPE: { /*198, 5, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_POPBLOCKSCOPE: { /*199, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_DEBUGLEAVEBLOCK: { /*200, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_GENERATOR: { /*202, 1, 0, 0*/
        NOTHANDLED;
        break;
      }
      case JSOP_YIELD: { /*203, 1, 1, 1*/
        SIMULATESTACK(1, 1);
        break;
      }
      case JSOP_ARRAYPUSH: { /*204, 1, 2, 0*/
        SIMULATESTACK(2, 0);
        break;
      }
      default: {
        return false;
      }
    }  // End switch (op)

    if (iter_node) {
      // no return in loop body, clear saved iter_node
      if ((op != JSOP_ENDITER) &&
          (lastOp != JSOP_ENDITER || op != JSOP_RETRVAL)) {
        iter_node = NULL;
      }
    }

    lastOp = op;
    pc = js::GetNextPc(pc);
  }  // End while (pc < script->codeEnd())

  if (srcfileptr)
    fclose(srcfileptr);

  if (lastOp == JSOP_RETRVAL) {
    CloseFuncBookKeeping();
  }

  return true;
}  // End JSCompiler::CompileScript(JSScript *script)

}  // namespace maple
