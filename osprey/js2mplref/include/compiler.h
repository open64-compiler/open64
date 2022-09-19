/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/// Copyright [year] <Copyright Owner>
#ifndef JS2MPL_INCLUDE_COMPILER_H_
#define JS2MPL_INCLUDE_COMPILER_H_
#include <list>

#include "maple_ir/include/mir_builder.h"
#include "maple_ir/include/mir_symbol_builder.h"
#include "../include/jsvalue.h"
#include "../include/jsmirbuilder.h"
#include "../include/closure.h"
#include "../include/operandstack.h"
#include "../include/scope.h"
#include "../include/eh.h"
#include "../include/util.h"

namespace maple {
class JSCompiler {
 private:
  const char *filename_;
  unsigned linenum_;
  JSContext *jscontext_;
  JSScript *jsscript_;
  JSMIRBuilder *jsbuilder_;

  MIRType *jsvalueType;
  MIRType *jsvalue_ptr_;

  JSMIRFunction *jsmain_;
  OperandStack *opstack_;
  std::stack<JSMIRFunction *> funcstack_;
  std::stack<BlockNode *> bodystack_;
  std::map<jsbytecode *, LabelIdx> label_map_;
  std::map<jsbytecode *, uint32> depth_map_;
  std::map<LabelIdx, MIRSymbol *> label_tempvar_map_;

  std::map<const jschar *, BaseNode *> jsstring_map_;

  std::stack<std::pair<JSScript *, JSMIRFunction *>> scriptstack_;

  MemPool *mp_;

 public:
  maple::MIRModule *module_;

 private:
  BaseNode *dummyNode;

  uint32_t anon_func_no_;
  uint32_t temp_var_no_;

  Scope *scope_;
  EH *eh_;
  JSClosure *closure_;

  typedef std::pair<JSMIRFunction *, std::vector<char *>> funcArgPair;
  std::vector<funcArgPair> funcFormals;
  std::vector<std::pair<char *, char *>> objFuncMap;

 public:
  explicit JSCompiler(const char *filename, JSContext *context, JSScript *script, maple::MIRModule *module,
                      JSMIRBuilder *jsbuilder, Scope *scope, EH *eh, JSClosure *closure, OperandStack *opstack)
    : filename_(filename),
      linenum_(0),
      jscontext_(context),
      jsscript_(script),
      module_(module),
      mp_(module->memPool),
      temp_var_no_(1),
      jsbuilder_(jsbuilder),
      scope_(scope),
      eh_(eh),
      closure_(closure),
      opstack_(opstack) {}

  void Push(BaseNode *node) {
    assert(node->op != OP_dassign);
    opstack_->Push(node);
  }

  uint32_t GetDepth() const {
    return opstack_->GetDepth();
  }

  BaseNode *Pop() {
    BaseNode *bn = static_cast<BaseNode *>(opstack_->Pop());
    assert(bn);
    return bn;
  }

  BaseNode *Top() {
    BaseNode *bn = static_cast<BaseNode *>(opstack_->Top());
    assert(bn);
    return bn;
  }

  BaseNode *GetOpAt(uint32_t n) {
    BaseNode *bn = static_cast<BaseNode *>(opstack_->GetOpAt(n));
    assert(bn);
    return bn;
  }

  bool UseSimpleCall(char *name) {
    return jsbuilder_->UseSimpCall() && name && scope_->IsFunction(name) && !closure_->FuncUseEnv(name) &&
           !closure_->IsFuncModified(name);
  }

  char *GetFuncName(const char *objname) {
    std::vector<std::pair<char *, char *>>::iterator I;
    for (I = objFuncMap.begin(); I != objFuncMap.end(); I++) {
      if (!strcmp(objname, (*I).first)) {
        return (*I).second;
      }
    }
    return NULL;
  }

  void Init();
  void EnvInit(JSMIRFunction *func);
  void SetupMainFuncRet(BaseNode *rval);
  void CloseFuncBookKeeping();
  int32_t GetBuiltinMethod(uint32_t argc, bool *need_this);
  MIRSymbol *CreateTempVar(MIRType *);
  MIRSymbol *CreateTempJSValueTypeVar();
  void InitWithUndefined(bool created, MIRSymbol *var);
  uint32_t GetFieldidFromTag(uint32_t tag);
  MIRType *DetermineTypeFromNode(BaseNode *node);
  MIRSymbol *SymbolFromSavingInATemp(BaseNode *expr, bool jsvalue_p);
  AddrofNode *NodeFromSavingInATemp(BaseNode *expr);
  // Compile Functions.
  BaseNode *CompileOpConstValue(uint32_t jsvalue_tag, int32_t payload);
  BaseNode *CompileDoubleConst(double value);
  js_builtin_id EcmaNameToId(char *name);
  int32_t GetBuiltinStringId(const jschar *chars, uint32_t length);
  BaseNode *CompileBuiltinObject(char *name);
  BaseNode *CompileBuiltinMethod(int32_t idx, int arg_num, bool need_this);
  uint32_t FindIntrinsicForOp(JSOp opcode);
  BaseNode *CompileOpBinary(JSOp op, BaseNode *opnd0, BaseNode *opnd1);
  BaseNode *CompileOpUnary(JSOp op, BaseNode *opnd);
  BaseNode *CompileGenericN(int32_t intrin_id, MapleVector<BaseNode *> &arguments, bool is_call);
  BaseNode *CompileGeneric0(int32_t intrin_id, bool is_call);
  BaseNode *CompileGeneric1(int32_t intrin_id, BaseNode *arg, bool is_call);
  BaseNode *CompileGeneric2(int32_t intrin_id, BaseNode *arg0, BaseNode *arg1, bool is_call);
  BaseNode *CompileGeneric3(int32_t intrin_id, BaseNode *arg0, BaseNode *arg1, BaseNode *arg2, bool is_call);
  BaseNode *CompileGeneric4(int32_t intrin_id, BaseNode *arg0, BaseNode *arg1, BaseNode *arg2, BaseNode *arg3,
                            bool is_call);
  BaseNode *CompileOpGetProp(BaseNode *obj, JSString *name);
  BaseNode *CompileOpCallprop(BaseNode *obj, JSAtom *atom);
  BaseNode *CompileOpString(JSString *str);
  BaseNode *CompileOpNewIterator(BaseNode *bn, uint8_t flags);
  BaseNode *CompileOpIterNext(BaseNode *iterator);
  BaseNode *CompileOpMoreIterator(BaseNode *iterator);
  BaseNode *CompileOpGetArg(uint32_t i);
  void CompileOpSetArg(uint32_t i, BaseNode *val);
  BaseNode *CompileOpGetLocal(uint32_t local_no);
  StmtNode *CompileOpSetLocal(uint32_t local_no, BaseNode *src);
  BaseNode *CompileOpNewInit(uint32_t kind);
  BaseNode *CompileOpNewArray(uint32_t length);
  BaseNode *CompileOpLength(BaseNode *array);
  BaseNode *CompileOpGetElem(BaseNode *obj, BaseNode *index);
  bool CompileOpSetElem(BaseNode *obj, BaseNode *index, BaseNode *val);
  bool CompileOpSetProp(BaseNode *obj, JSString *name, BaseNode *val);
  bool CompileOpInitPropGetter(BaseNode *obj, JSString *name, BaseNode *val);
  bool CompileOpInitPropSetter(BaseNode *obj, JSString *name, BaseNode *val);
  bool CompileOpInitElemGetter(BaseNode *obj, BaseNode *index, BaseNode *val);
  bool CompileOpInitElemSetter(BaseNode *obj, BaseNode *index, BaseNode *val);
  int ProcessAliasedVar(JSAtom *atom, MIRType *&env_ptr, BaseNode *&base, int &depth);
  BaseNode *CompileOpGetAliasedVar(JSAtom *atom);
  BaseNode *CompileOpSetAliasedVar(JSAtom *atom, BaseNode *val);
  BaseNode *CompileOpLambda(jsbytecode *pc, JSFunction *jsfun);
  BaseNode *CompileOpBindName(JSAtom *atom);
  BaseNode *CompileOpCall(uint32_t argc);
  BaseNode *CompileOpNew(uint32_t argc);
  BaseNode *CompileOpName(JSAtom *atom, jsbytecode *pc);
  BaseNode *CompileLibraryCall(BaseNode *obj, JSString *str);
  StmtNode *CompileOpIfJump(JSOp op, BaseNode *cond, jsbytecode *pcend);

  LabelIdx CreateLabel(char *pref = NULL);
  LabelIdx GetorCreateLabelofPc(jsbytecode *pc, char *pref = NULL);
  int64_t GetIntValue(jsbytecode *pc);
  SwitchNode *CompileOpCondSwitch(BaseNode *opnd, JSScript *script, jsbytecode *pcstart, jsbytecode *pcend);
  SwitchNode *CompileOpTableSwitch(BaseNode *opnd, int32_t len, JSScript *script, jsbytecode *pc);
  GotoNode *CompileOpGoto(jsbytecode *pc, jsbytecode *jumptopc, BaseNode *expr);
  GotoNode *CompileOpGosub(jsbytecode *pc);
  JsTryNode *CompileOpTry(jsbytecode *pc);
  BaseNode *CompileOpLoopHead(jsbytecode *pc);
  BaseNode *CheckConvertToJSValueType(BaseNode *node);
  BaseNode *CheckConvertToBoolean(BaseNode *node);
  BaseNode *CheckConvertToInt32(BaseNode *node);
  BaseNode *CheckConvertToUInt32(BaseNode *node);
  BaseNode *CheckConvertToRespectiveType(BaseNode *node, MIRType *ty);
  bool CompileOpSetName(JSAtom *atom, BaseNode *val);
  void CompileOpCase(jsbytecode *pc, int offset, BaseNode *rval, BaseNode *lval);
  bool CompileOpDefFun(JSFunction *jsfun);
  bool CompileOpDefVar(JSAtom *atom);
  bool CompileScript(JSScript *script);
  bool CompileScriptBytecodes(JSScript *script, jsbytecode *pcstart, jsbytecode *pcend, jsbytecode **newpc);
  bool MarkLabels(JSScript *s, jsbytecode *pc0, jsbytecode *pc1);
  bool CollectInfo(JSScript *script, jsbytecode *pcstart, jsbytecode *pcend);
  // Finish job.
  void Finish(const std::string& outFileName);
};  // class JSCompiler
}  // namespace maple
#endif  // JS2MPL_INCLUDE_COMPILER_H_
