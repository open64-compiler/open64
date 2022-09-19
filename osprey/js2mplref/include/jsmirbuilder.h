/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/// Copyright [year] <Copyright Owner>
#ifndef JS2MPL_INCLUDE_JSMIRBUILDER_H_
#define JS2MPL_INCLUDE_JSMIRBUILDER_H_
#include "maple_ir/include/mir_builder.h"
#include "jsfunction.h"
#include "js2mpl.h"
#include "util.h"
namespace maple {

class JSMIRBuilder : public MIRBuilder {
 private:
  std::set<char *> global_vars;
  JSMIRContext &jsmir_context_;

 public:
  JSMIRFunction *jsmain_;
  MIRType *jsvalueType;
  MIRType *jsvalue_ptr_;

 public:
  explicit JSMIRBuilder(MIRModule *module, JSMIRContext &ctx) : MIRBuilder(module), jsmir_context_(ctx) {}

  JSMIRFunction *GetFunction(const char *name);
  JSMIRFunction *GetOrCreateFunction(const char *name, JSFunction *jsfunc, MIRType *return_type,
                                     const ArgVector &arguments, bool isvarg);
  NaryStmtNode *CreateStmtReturn(BaseNode *rval, bool adj_type, unsigned linenum);
  StmtNode *CreateStmtDassign(MIRSymbol *symbol, fldid_t field_id, BaseNode *src, unsigned linenum);
  IntrinsiccallNode *CreateStmtIntrinsicCall1N(MIRIntrinsicId idx, BaseNode *arg0, MapleVector<BaseNode *> &args);

  void UpdateFunction(JSMIRFunction *fn, MIRType *return_type, const ArgVector &arguments);
  // void SaveReturnValue(MIRSymbol *var);
  void AddStmtInCurrentFunctionBody(StmtNode *n);

  std::vector<std::pair<const char *, JSMIRFunction *>> name_func_vec_;

  JSMIRFunction *GetFunc(const char *name) {
    std::vector<std::pair<const char *, JSMIRFunction *>>::iterator I;
    for (I = name_func_vec_.begin(); I != name_func_vec_.end(); I++)
      if (strcmp(name, I->first) == 0) {
        return I->second;
      }
    return NULL;
  }

  char *GetName(JSMIRFunction *func) {
    std::vector<std::pair<const char *, JSMIRFunction *>>::iterator I;
    for (I = name_func_vec_.begin(); I != name_func_vec_.end(); I++)
      if (func == I->second) {
        return (char *)I->first;
      }
    return NULL;
  }

  bool IsPluginFunc(JSMIRFunction *func) {
    if (jsmir_context_.isplugin_) {
      char *name = GetName(func);
      if (name && strncmp(name, PLUGINPREFIX, 7) == 0) {
        return true;
      }
    }
    return false;
  }

  void AddNameFunc(const char *name, JSMIRFunction *func) {
    JSMIRFunction *f = GetFunc(name);
    if (f) {
      assert(f == func && "error: function not match!");
    } else {
      std::pair<const char *, JSMIRFunction *> P(name, func);
      name_func_vec_.push_back(P);
    }
  }

  void InitGlobalName() {
    global_vars.empty();
  }

  void InsertGlobalName(char *name) {
    if (IsGlobalName(name)) {
      return;
    }
    //DEBUGPRINTsv2("global_var", name);
    global_vars.insert(name);
  }

  bool IsGlobalName(char *name) {
    std::set<char *>::iterator BI;
    for (BI = global_vars.begin(); BI != global_vars.end(); BI++) {
      if (strcmp(name, *BI) == 0) {
        DEBUGPRINTsv2("global_var", name);
        return true;
      }
    }
    //DEBUGPRINTsv2("local_var", name);
    return false;
  }

  // Initializations.
  MIRType *CreateJSValueType();
  JSMIRFunction *CreateJSMain();
  void InitBuiltinMethod();
  void Init();

  bool IsMain(JSMIRFunction *func) {
    return func == jsmain_;
  }

  JSMIRFunction *GetCurrentFunction() {
    return (JSMIRFunction *)(MIRBuilder::GetCurrentFunction());
  }

  bool IsPlugin() {
    return jsmir_context_.isplugin_;
  }

  char *GetWrapperName() {
    return (char *)jsmir_context_.wrapper_name_.c_str();
  }

  bool WithMain() {
    return jsmir_context_.with_main_;
  }

  bool UseSimpCall() {
    return jsmir_context_.simp_call_;
  }

  bool JSOPOnly() {
    return jsmir_context_.jsop_only_;
  }
};
}  // namespace maple
#endif  // JS2MPL_INCLUDE_JSMIRBUILDER_H_
