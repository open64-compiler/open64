/* Copyright (C) 2026 Open64 Project */
/* Q1 common-High PREOPT lifecycle. Analysis owns copied records; the driver or
 * LNO owns root installation, analysis managers and phase ordering.
 * Design: doc/AI-COMPILER-MLA-Q1-PRODUCTION-v1.0.md. */
#include "dsl_q1_preopt.h"
#include "../vho/tensor_q1_program.h"
#include "wn.h"
#include "pu_info.h"
#include "stab.h"
#include "config_dsl.h"
#include "tracing.h"
#include "errors.h"
#include "dsl_native_tree_key.h"
#include <sstream>
namespace {
/* Phase diagnostics honor trace-off and never become executable IR. */
FILE *trace() {return VHO_DSL_AI_Opt_Trace && Get_Trace(TP_WOPT2,1)?TFile:NULL;}
/* Checkpoint labels are unique to each actual PREOPT invocation. */
std::string label(unsigned n,const char *when) {std::ostringstream s;s<<"preopt-"<<n<<'.'<<when;return s.str();}
}
/* No context switch: a foreign REGION or absent binding stays on fallback. */
DSL_Q1_PREOPT_SESSION::DSL_Q1_PREOPT_SESSION(WN *entry) : _active(false),_stage(0),_epoch("q1-production-preopt") {
    std::string why;_active=VHO_q1_program_read(entry,_program,why);
    if(!_active && trace() && entry && WN_operator(entry)==OPR_FUNC_ENTRY && ST_tensor_metadata(WN_st_idx(entry),"tensor.q1.algorithm"))
        fprintf(trace(),"AI_Q1_PREOPT 1 SKIP reason=%s fallback=ordinary_backend\n",why.c_str());
}
/* Closing invalidates tokens without dereferencing an expired native scope. */
DSL_Q1_PREOPT_SESSION::~DSL_Q1_PREOPT_SESSION() {_epoch.Close();}
/* Native snapshots are taken outside estimation-only exploration. */
void DSL_Q1_PREOPT_SESSION::Before(WN *entry) {
    if(!_active) return;
    FmtAssert(VHO_q1_checkpoint(entry,label(_stage,"before").c_str(),true),("Q1 pre-PREOPT checkpoint failed"));
    std::string why;
    bool captured=_stage?_facts.Is_live(_epoch):LNO_DSL_capture_quantized_high(entry,_program,_epoch,_facts,why);
    if(trace()) {
        if(captured) LNO_DSL_print_quantized_high(trace(),_facts,"PREOPT_BEFORE",true);
        else fprintf(trace(),"AI_Q1_PREOPT 1 BEFORE_UNKNOWN reason=%s\n",why.c_str());
    }
    _epoch.Before_mutation();
    FmtAssert(!_facts.Is_live(_epoch),("Q1 pre-PREOPT facts did not expire"));
}
/* The owner has installed the returned whole-function root before this call.
 * Annotation is separated from read-only capture; no source snapshot is reused. */
void DSL_Q1_PREOPT_SESSION::After(WN *entry,const DSL_HIGH_Q1_RESOLVER &resolver) {
    if(!_active) return;
    VHO_TENSOR_PROGRAM current;std::string why;
    if(!VHO_q1_program_read(entry,current,why) || VHO_tensor_quantized_high_key(current)!=VHO_tensor_quantized_high_key(_program)) {
        if(trace()) fprintf(trace(),"AI_Q1_PREOPT 1 REFUSED reason=stale_source_binding fallback=ordinary_backend\n");
        _active=false;_epoch.Close();return;
    }
    unsigned annotations=VHO_q1_restore_comments(entry,_program);
    unsigned types=TY_Table_Size(),symbols=ST_Table_Size(CURRENT_SYMTAB);
    std::string native_key;bool identity_known=true;
    try {native_key=DSL_NATIVE_TREE_KEY().Build(entry);}
    catch(const std::exception &) {identity_known=false;}
    DSL_HIGH_QUANTIZED_FACTS fresh;
    bool rebound=LNO_DSL_rebind_quantized_high(entry,_program,_epoch,resolver,fresh,why);
    if(rebound) FmtAssert(LNO_DSL_revalidate_rebound_quantized_high(entry,_program,_epoch,resolver,fresh,why),("Q1 fresh canonical facts were unstable"));
    if(identity_known) FmtAssert(native_key==DSL_NATIVE_TREE_KEY().Build(entry),("Q1 rebinding mutated executable WHIRL"));
    FmtAssert(types==TY_Table_Size() && symbols==ST_Table_Size(CURRENT_SYMTAB),("Q1 rebinding mutated native tables"));
    if(trace()) {
        fprintf(trace(),"AI_Q1_PREOPT 1 REBIND %s reason=%s annotations=%u old_facts_expired=true native_tables_unchanged=true search=false\n",
            rebound?"PASS":"REFUSED",rebound?"fresh_canonical_sites":why.c_str(),annotations);
        if(rebound) LNO_DSL_print_quantized_high(trace(),fresh,"PREOPT_AFTER",true);
    }
    _facts=fresh;
    FmtAssert(VHO_q1_checkpoint(entry,label(_stage,"after").c_str(),true),("Q1 post-PREOPT checkpoint failed"));++_stage;
}
/* Every subsequent mutation invalidates the freshly rebound facts together. */
void DSL_Q1_PREOPT_SESSION::Invalidate() {
    if(!_active) return;_epoch.Before_mutation();
    FmtAssert(!_facts.Is_live(_epoch),("Q1 canonical facts survived mutation"));
    if(trace()) LNO_DSL_print_quantized_high(trace(),_facts,"BEFORE_NEXT_PHASE",false);
}
