/* Copyright (C) 2026 Open64 Project */
/* Q6c active-PU device realization and read-only native identity. No device
 * lease, search, cross-PU traversal or binary format change. */
#ifndef tensor_quantized_device_INCLUDED
#define tensor_quantized_device_INCLUDED
#include "tensor_quantized_stage.h"
#include <cstring>
static inline std::string VHO_q6c_key(WN *root) {
    std::ostringstream s;s<<VHO_q6_stage_key(root);
    const char *fields[]={"tensor.q6c.schema","tensor.q6c.level","tensor.q6c.stage","tensor.q6c.source_key",
        "tensor.q6c.explanation","tensor.kernel.block_x","tensor.kernel.grid_x","tensor.kernel.shared_bytes"};
    for(unsigned i=0;i<sizeof(fields)/sizeof(fields[0]);++i) {
        const char *v=ST_tensor_metadata(WN_st_idx(root),fields[i]);
        if(!v || !*v) throw std::runtime_error("q6c_missing_binding");
        std::string t=v;s<<t.size()<<':'<<t<<';';
    }
    const ST *st;ST_IDX idx;
    FOREACH_SYMBOL(GLOBAL_SYMTAB,st,idx) {
        if(!strcmp(ST_name(st),"threadIdx") || !strcmp(ST_name(st),"blockIdx") ||
           (ST_class(st)==CLASS_FUNC && std::string(ST_name(st)).find("__open64_q6c_")==0)) {
            s<<DSL_NATIVE_TREE_KEY().Declaration(st)<<';';
            const char *v=ST_tensor_metadata(ST_st_idx(st),"tensor.q6c.primitive");s<<(v?v:"")<<';';
        }
    }
    return s.str();
}
static inline void VHO_q6c_check_key(WN *root,const char *level) {
    const char *schema=ST_tensor_metadata(WN_st_idx(root),"tensor.q6c.schema");
    const char *l=ST_tensor_metadata(WN_st_idx(root),"tensor.q6c.level");
    const char *k=ST_tensor_metadata(WN_st_idx(root),"tensor.q6c.key");
    if(!schema || strcmp(schema,"mla-gpu-staging-v1") || !l || strcmp(l,level) || !k || VHO_q6c_key(root)!=k)
        throw std::runtime_error("q6c_stale_native_level_or_binding");
}
extern void VHO_q6c_worker(const VHO_Q6_MAPPING &,const std::string &,const std::string &);
extern void VHO_q6c_memory();
extern void VHO_q6c_gpu();
extern void VHO_q6c_verify(const char *);
#endif
