/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/// Copyright [year] <Copyright Owner>
#include "../include/scope.h"

namespace maple {

void ScopeNode::clear() {
  node_ = NULL;
  func_ = NULL;
  parent_ = NULL;
  children_.empty();
  isLeaf_ = false;
  isTopLevel_ = false;
  useAliased_ = false;
  withEnv_ = false;
  flag_ = false;
}

ScopeNode::ScopeNode(BaseNode *node) {
  clear();
  node_ = node;
}

ScopeNode::ScopeNode(JSMIRFunction *func) : func_(func) {
  clear();
  func_ = func;
}

ScopeNode::ScopeNode(char *name) : name_(name) {
  clear();
  name_ = name;
}

void ScopeNode::AddChild(ScopeNode *node) {
  list<ScopeNode *>::iterator i;
  for (i = children_.begin(); i != children_.end(); i++) {
    if (*i == node) {
      return;
    }
  }
  children_.push_back(node);
}

void ScopeNode::SetParent(ScopeNode *node) {
  parent_ = node;
}

void ScopeNode::PropWithEnv() {
  if (parent_ && !(parent_->IsMain())) {
    parent_->SetWithEnv(true);
    parent_->PropWithEnv();
  }
}

bool ScopeNode::SetChild(ScopeNode *child) {
  if (!child) {
    return false;
  }

  AddChild(child);
  child->SetParent(this);
  return true;
}

void ScopeNode::SetFunc(JSMIRFunction *func) {
  func_ = func;
  if (parent_) {
    parent_->SetChild(this);
  }
  list<ScopeNode *>::iterator i;
  for (i = children_.begin(); i != children_.end(); i++) {
    (*i)->SetParent(this);
  }

  // set scope in function
  func->scope = this;
}

void ScopeNode::Dump() {
  DEBUGPRINT2pure(name_);
  DEBUGPRINT2pure(this);
  DEBUGPRINT2pure(func_);
  DEBUGPRINT2pure(isLeaf_);
  DEBUGPRINT2pure(isTopLevel_);
  DEBUGPRINT2pure(useAliased_);
  DEBUGPRINT2pure(withEnv_);
  DEBUGPRINT2pure(parent_);
  list<ScopeNode *>::iterator i;
  ScopeNode *child;
  for (i = children_.begin(); i != children_.end(); i++) {
    child = *i;
    DEBUGPRINT2pure(child);
  }
  DEBUGPRINT0;
}

ScopeNode *Scope::GetOrCreateSN(char *name) {
  list<pair<char *, ScopeNode *>>::iterator i;
  for (i = scopeChain.begin(); i != scopeChain.end(); i++) {
    if (strcmp(name, (*i).first) == 0) {
      return (*i).second;
    }
  }

  // create new one
  ScopeNode *sn = mp_->New<ScopeNode>(name);
  pair<char *, ScopeNode *> p(name, sn);
  scopeChain.push_back(p);

  return sn;
}

void Scope::SetSNParent(char *name, char *parent) {
  ScopeNode *sn = GetOrCreateSN(name);
  ScopeNode *snp = GetOrCreateSN(parent);
  sn->SetParent(snp);
  snp->AddChild(sn);
  if (snp->IsMain()) {
    sn->SetTopLevel();
  }
}

void Scope::SetSNLeaf(char *name) {
  ScopeNode *sn = GetOrCreateSN(name);
  sn->SetLeaf();
}

void Scope::SetSNClosure(char *name) {
  ScopeNode *sn = GetOrCreateSN(name);
  if (!(sn->IsTopLevel())) {
    sn->SetUseAliased();
    sn->SetWithEnv(true);
  }
}

void Scope::AddSNChild(char *name, char *child) {
  ScopeNode *sn = GetOrCreateSN(name);
  ScopeNode *snc = GetOrCreateSN(child);
  sn->AddChild(snc);
  snc->SetParent(sn);
}

void Scope::Init() {
#if 0
  // set up anonymous function to anon_func_no_ mapping
  typedef pair<JSFunction *, vector<JSAtom *>> funcVarVecPair;
  vector<funcVarVecPair> formals = jsscript_->funcFormals;
  char *name;
  char *funcname;
  vector<funcVarVecPair>::iterator i;
  vector<char *> nameVec;
  JSFunction *jsfun;
  for (i = formals.begin(); i != formals.end(); i++) {
    jsfun = (*i).first;

    if (!jsfun->name()) {
      DEBUGPRINT3(jsfun);
      SetAnonyidx(jsfun, anon_func_no_);
      anon_func_no_++;
    }
  }

  return;
#endif
}

bool Scope::Build(JSScript *script) {
  jsbytecode *start = script->code();
  jsbytecode *end = script->codeEnd();

  bool ret = BuildSection(script, start, end);

  if (js2mplDebug > 1) {
    cout << "GetDepth() = " << GetDepth() << endl;
  }
  return ret;
}

bool Scope::BuildSection(JSScript *script, jsbytecode *pcstart, jsbytecode *pcend) {
  jsbytecode *pc = pcstart;
  JSFunction *jsfun;

  // dump JSOP code only
  if (jsbuilder_->JSOPOnly()) {
    while (pc < pcend) {
      JSOp op = JSOp(*pc);
      JSScript *scr;

      switch (op) {
        case JSOP_DEFFUN:   /*127, 5, 0, 0*/
        case JSOP_LAMBDA: { /*130, 5, 0, 1*/
          JSFunction *jsfun = script->getFunction(GET_UINT32_INDEX(pc));
          scr = jsfun->nonLazyScript();
          Build(scr);
        }
      }
      cout << Util::getOpcodeName[op] << endl;
      pc = js::GetNextPc(pc);
    }

  } else {
    if (script == jsscript_) {
      char *name = "main";
      if (jsbuilder_->IsPlugin()) {
        name = jsbuilder_->GetWrapperName();
      }
      funcstack_.push(name);
      if (js2mplDebug > 0) {
        cout << name << " {" << endl;
      }
      ScopeNode *sn = GetOrCreateSN(name);
      sn->SetTopLevel();
    }

    char *name;
    char *parent;
    JSOp lastOp;
    JSScript *scr;

    while (pc < pcend) {
      JSOp op = JSOp(*pc);
      unsigned lineNo = js::PCToLineNumber(script, pc);

      if (js2mplDebug > 0) {
        printf("  %4d %-25s pc = 0x%x\n", lineNo, Util::getOpcodeName[op], pc);
      }
      Util::SetIndent(4);

      switch (op) {
        case JSOP_DEFFUN:   /*127, 5, 0, 0*/
        case JSOP_LAMBDA: { /*130, 5, 0, 1*/
          jsfun = script->getFunction(GET_UINT32_INDEX(pc));
          DEBUGPRINT3(jsfun);
          scr = jsfun->nonLazyScript();
          JSAtom *atom = jsfun->displayAtom();
          const char *fname = (atom && !jsfun->hasGuessedAtom()) ?
                                Util::GetString(atom, mp_, ctx_) : NULL;
          const char *par_name = funcstack_.top() ?
                                   funcstack_.top() : NULL;
          uint32_t anon_idx = jsfun->isLambda() ? GetAnonyidx(jsfun) : 0;
          char *name = Util::GetFuncName(par_name, fname, anon_idx, mp_);
          DEBUGPRINT3(name);
          funcNames_.push_back(name);
          SetJSFunc(name, jsfun);
          pair<JSScript *, char *> p(scr, name);
          scriptstack_.push(p);

          parent = funcstack_.top();
          SetSNParent(name, parent);
          break;
        }
        case JSOP_GETALIASEDVAR:   /*136, 5, 0, 1*/
        case JSOP_SETALIASEDVAR: { /*137, 5, 11*/
          name = funcstack_.top();
          SetSNClosure(name);
          break;
        }
      }

      // calculate the expected stack size at the end
      {
        // get def/use for each op from mozjs js/src/vm/Opcodes.h
        int use = 0;
        int def = 0;
        switch (op) {
#define JSOPDEPTH(op, val, name, token, length, nuses, ndefs, format) \
  case op:                                                            \
    def = ndefs;                                                      \
    use = nuses;                                                      \
    break;
          FOR_EACH_OPCODE(JSOPDEPTH)
#undef JSOPDEPTH
        }

        // handles dynamic use count (-1)
        uint32_t use0 = 0;
        switch (op) {
          case JSOP_POPN:
            use0 = GET_UINT16(pc);
            use0 = 0;  // adjustment
            break;
          case JSOP_CALL:
          case JSOP_FUNAPPLY:
          case JSOP_NEW:
          case JSOP_FUNCALL:
          case JSOP_EVAL:
            use0 = GET_ARGC(pc);
            use0 += 2;  // adjustment
            break;
        }

        use = (use < 0) ? use0 : use;
        int inc = def - use;

        if (js2mplDebug > 3)
          cout << "line : " << lineNo << "  " << Util::getOpcodeName[op] << "  stackDepth: " << stackDepth << " == (u"
               << use << ", d" << def << ")==>" << stackDepth + inc << endl;
        stackDepth += inc;
        if (stackDepth < 0) {
          //assert(false && "the spidermonkey emitted bytecode has issue in operator stack");
        }
      }

      lastOp = op;
      pc = js::GetNextPc(pc);
    }

    if (lastOp == JSOP_RETRVAL) {
      name = funcstack_.top();
      if (js2mplDebug > 0) {
        cout << "}\n" << endl;
      }
      funcstack_.pop();
      DEBUGPRINT3((scriptstack_.size()));
      while (scriptstack_.size()) {
        JSScript *scr = scriptstack_.top().first;
        name = scriptstack_.top().second;
        scriptstack_.pop();
        funcstack_.push(name);
        if (js2mplDebug > 0) {
          cout << name << " {" << endl;
        }
        Build(scr);
      }
    }

    PopulateSNInfo();
  }

  return true;
}

#if 0
char *Scope::GetAnonyFunctionName(jsbytecode *pc) {
  list<pair<jsbytecode *, char *>>::iterator i;
  jsbytecode *bytecode;
  for (i = bytecodeAnonyFunc.begin(); i != bytecodeAnonyFunc.end(); i++) {
    bytecode = (*i).first;
    if (bytecode == pc) {
      return (*i).second;
    }
  }
  return NULL;
}
#endif

bool Scope::IsFunction(char *name) {
  vector<char *>::iterator i;
  for (i = funcNames_.begin(); i != funcNames_.end(); i++) {
    if (strcmp(*i, name) == 0) {
      return true;
    }
  }
  return false;
}

void Scope::DumpScopeChain() {
  list<pair<char *, ScopeNode *>>::iterator i;
  ScopeNode *sn;
  for (i = scopeChain.begin(); i != scopeChain.end(); i++) {
    sn = (*i).second;
    sn->Dump();
  }
}

void Scope::PopulateSNInfo() {
  list<pair<char *, ScopeNode *>>::iterator i;
  ScopeNode *sn;
  for (i = scopeChain.begin(); i != scopeChain.end(); i++) {
    sn = (*i).second;
    // set leaf
    if (sn->GetChildren().size() == 0) {
      sn->SetLeaf();
      sn->SetWithEnv(false);
    }
  }

  for (i = scopeChain.begin(); i != scopeChain.end(); i++) {
    sn = (*i).second;
    // propagate closure
    if (sn->IsWithEnv() || sn->UseAliased()) {
      sn->PropWithEnv();
    }
  }
}

char *Scope::GetJSFuncName(JSFunction *func) {
  vector<pair<char *, JSFunction *>>::iterator i;
  for (i = nameJSfunc_.begin(); i != nameJSfunc_.end(); i++) {
    if (func == (*i).second) {
      return (*i).first;
    }
  }
  return NULL;
}

JSFunction *Scope::GetJSFunc(char *name) {
  vector<pair<char *, JSFunction *>>::iterator i;
  for (i = nameJSfunc_.begin(); i != nameJSfunc_.end(); i++) {
    if (strcmp(name, (*i).first) == 0) {
      return (*i).second;
    }
  }
  return NULL;
}

void Scope::SetJSFunc(char *name, JSFunction *func) {
  if (GetJSFunc(name)) {
    return;
  }
  pair<char *, JSFunction *> p(name, func);
  nameJSfunc_.push_back(p);
}

}  // namespace maple
