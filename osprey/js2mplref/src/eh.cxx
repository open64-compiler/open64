/// Copyright [year] <Copyright Owner>
#include "../include/eh.h"

namespace maple {

// return the innermost EHstruct containing the pc if any
// note: we use interval (trypc, endtrypc], that is trypc2 could be endtrypc1
// of previous trypc1, and in this case, we want to let trypc2 be part of
// (trypc1, endtrypc1]
EHstruct *EH::GetEHstruct(jsbytecode *pc) {
  vector<EHstruct *>::reverse_iterator i;
  for (i = EHstructvec_.rbegin(); i != EHstructvec_.rend(); i++) {
    if (pc > (*i)->trypc && pc <= (*i)->endtrypc) {
      return *i;
    }
  }
  return NULL;
}

EHstruct *EH::GetEHstruct(jsbytecode *trypc, jsbytecode *catchpc, jsbytecode *finallypc, jsbytecode *endtrypc) {
  vector<EHstruct *>::reverse_iterator i;
  for (i = EHstructvec_.rbegin(); i != EHstructvec_.rend(); i++) {
    if ((trypc && (*i)->trypc == trypc) || (catchpc && (*i)->catchpc == catchpc) ||
        (finallypc && (*i)->finallypc == finallypc) || (endtrypc && (*i)->endtrypc == endtrypc)) {
      return *i;
    }
  }
  return NULL;
}

void EH::DumpEHstruct(EHstruct *eh) {
  if (eh)
    printf("         EHstruct { try=0x%x, catch=0x%x, finally=0x%x, endtry=0x%x, label=%d }\n", eh->trypc, eh->catchpc,
           eh->finallypc, eh->endtrypc, eh->label);
}

void EH::DumpEHstructVec() {
  vector<EHstruct *>::iterator i;
  for (i = EHstructvec_.begin(); i != EHstructvec_.end(); i++) {
    DumpEHstruct(*i);
  }
}

bool EH::Build(JSScript *script) {
  jsbytecode *start = script->code();
  jsbytecode *end = script->codeEnd();

  bool ret = BuildSection(script, start, end);
  return ret;
}

bool EH::BuildSection(JSScript *script, jsbytecode *pcstart, jsbytecode *pcend) {
  jsbytecode *pc = pcstart;
  JSFunction *jsfun;

  if (script == jsscript_) {
    char *name = "main";
    if (jsbuilder_->IsPlugin()) {
      name = jsbuilder_->GetWrapperName();
    }
    funcstack_.push(name);
    if (js2mplDebug > 0) {
      cout << name << " {" << endl;
    }
  }

  char *name;
  JSOp lastOp;
  JSScript *scr;
  EHstruct *eh;

  while (pc < pcend) {
    JSOp op = JSOp(*pc);
    unsigned lineNo = js::PCToLineNumber(script, pc);

    Util::SetIndent(4);

    // collecting EH info
    switch (op) {
      case JSOP_DEFFUN:
      case JSOP_LAMBDA: {
        jsfun = script->getFunction(GET_UINT32_INDEX(pc));
        scr = jsfun->nonLazyScript();
        name = scope_->GetJSFuncName(jsfun);
        pair<JSScript *, char *> p(scr, name);
        scriptstack_.push(p);
        break;
      }
      case JSOP_TRY: {
        DEBUGPRINTs("try {");
        JSTryNote *tn = script->trynotes()->vector;
        JSTryNote *tnlimit = tn + script->trynotes()->length;
        for (; tn < tnlimit; tn++) {
          if ((tn->start + script->mainOffset()) == (pc - script->code() + 1)) {
            jsbytecode *trythrow = pc + 1 + tn->length;
            trystack_.push(pc);

            // use the goto before trythrow to find the end of try eh
            jsbytecode *jumppc = trythrow - js_CodeSpec[JSOP_GOTO].length;
            assert(JSOp(*jumppc) == JSOP_GOTO);
            jsbytecode *aftertrypc = jumppc + GET_JUMP_OFFSET(jumppc);

            eh = (EHstruct *)malloc(sizeof(EHstruct));
            eh->trypc = pc;
            eh->catchpc = trythrow;
            eh->finallypc = 0;
            eh->endtrypc = aftertrypc;
            eh->label = 0;
            EHstructvec_.push_back(eh);
            break;
          }
        }
        break;
      }
      case JSOP_FINALLY: {
        DEBUGPRINTs("} fianlly {");
        eh = GetEHstruct(trystack_.top(), 0, 0, 0);
        assert(eh);
        eh->finallypc = pc;
        // check if no catch (catch == finally)
        if (eh->catchpc == pc) {
          eh->catchpc = 0;
        }
        break;
      }
      case JSOP_GOTO: {
        int offset = GET_JUMP_OFFSET(pc);
        if (trystack_.size()) {
          eh = GetEHstruct(trystack_.top(), 0, 0, 0);
          if (eh && eh->endtrypc < pc + offset) {
            DEBUGPRINTs("exittry");
          }
        }
        break;
      }
    }

    if (op != JSOP_FINALLY && GetEHstruct(0, pc, 0, 0)) {
      DEBUGPRINTs("} catch {");
    }

    // end of current try eh
    if (GetEHstruct(0, 0, 0, js::GetNextPc(pc))) {
      DEBUGPRINTs("} endtry");
      if (js2mplDebug > 0) {
        DumpEHstruct(eh);
      }
      trystack_.pop();
    }

    if (js2mplDebug > 0) {
      printf("  %4d %-25s pc = 0x%x\n", lineNo, Util::getOpcodeName[op], pc);
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

  return true;
}

}  // namespace maple
