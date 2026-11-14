/* Copyright (C) 2026 Open64 Project */
/* Active-PU Q6b selected staging construction; driver owns epochs, checkpoints
 * and publication. No search, serving observation or GPU launch. Contract:
 * doc/AI-COMPILER-MLA-Q6B-NATIVE-STAGING-v1.0.md. */
#ifndef tensor_quantized_stage_INCLUDED
#define tensor_quantized_stage_INCLUDED
#include "tensor_quantized_gpu.h"
#include "dsl_native_tree_key.h"
/* Build one detached High body using the existing Q1/Q5 builder and formals.
 * Fresh shared storage, ordered dequantization and effectful staging operations
 * remain explicit WN; caller must verify and publish only the complete result. */
extern WN *VHO_q6_build_high(const VHO_TENSOR_PROGRAM &,const VHO_Q5_PLAN &,
    const VHO_Q6_MAPPING &,WN *);
/* Bind actual native code, original selected plan, source/Q3 identity and
 * complete shared inventory. Metadata is equality evidence, never a lease. */
static inline std::string VHO_q6_stage_key(WN *root) {
    std::ostringstream key;key<<DSL_NATIVE_TREE_KEY().Build(root);
    const char *fields[]={"tensor.q6.schema","tensor.q6.mapping","tensor.q6.control","tensor.q6.explanation","tensor.q5.applied"};
    for(unsigned i=0;i<5;++i) {
        const char *value=ST_tensor_metadata(WN_st_idx(root),fields[i]);
        if(!value || !*value) throw std::runtime_error("q6_missing_native_binding");
        std::string text=value;key<<text.size()<<':'<<text<<';';
    }
    const ST *s;ST_IDX index;unsigned count=0;
    FOREACH_SYMBOL(CURRENT_SYMTAB,s,index) if(ST_device_symbol_kind(*s)==DEVICE_SYMBOL_SHARED) {
        std::string declaration=DSL_NATIVE_TREE_KEY().Declaration(s);key<<declaration.size()<<':'<<declaration<<';';++count;
    }
    key<<count<<';';count=0;
    FOREACH_SYMBOL(GLOBAL_SYMTAB,s,index) if(ST_class(s)==CLASS_FUNC && std::string(ST_name(s)).find("__open64_mla_stage_")==0) {
        const char *version=ST_tensor_metadata(ST_st_idx(s),"tensor.q6.primitive");
        if(!version || std::string(version)!="q6b-byte-staging-v1") throw std::runtime_error("q6_primitive_version");
        std::string declaration=DSL_NATIVE_TREE_KEY().Declaration(s);key<<declaration.size()<<':'<<declaration<<';'<<version<<';';++count;
    }
    key<<count<<';';
    for(INT i=0;i<WN_num_formals(root);++i) {
        const char *trait=ST_tensor_metadata(WN_st_idx(WN_formal(root,i)),"tensor.cache.trait");
        std::string text=trait?trait:"";key<<i<<';'<<text.size()<<':'<<text<<';';
    }
    return key.str();
}
#endif
