/* Copyright (C) 2026 Open64 Project */
/* Exact selected-MLA checkpoint bindings shared by owning phases and read-only
 * emitters. No search or PU switching. Common/com supplies only tree identity.
 * Contract: doc/AI-COMPILER-SEARCH-M5-DEVICE-BRIDGE-v1.0.md. */
#ifndef TENSOR_SELECTED_KEY_H
#define TENSOR_SELECTED_KEY_H
#include "dsl_native_tree_key.h"
#include <cstring>
/* Bind the closed MLA producer witness and all fixed obligations to the actual
 * accepted source tree. Caller owns the active PU; absent fields fail closed. */
inline std::string VHO_tensor_selected_key(WN *root) {
    const char *keys[]={"tensor.high.schema","tensor.high.recipe_key","tensor.high.buffer_contract",
        "tensor.high.selected.schema","tensor.high.selected.source","tensor.high.selected.facts",
        "tensor.high.selected.resources","tensor.high.selected.model","tensor.high.selected.controls",
        "tensor.high.selected.numerical","tensor.high.selected.width","tensor.high.selected.policy",
        "tensor.high.selected.winner","tensor.high.selected.scratch"};
    std::ostringstream key;key<<DSL_NATIVE_TREE_KEY().Build(root);
    for(unsigned i=0;i<sizeof(keys)/sizeof(keys[0]);++i) {
        const char *v=ST_tensor_metadata(WN_st_idx(root),keys[i]);
        if(!v || !*v) throw std::runtime_error("native_selected_missing_binding");
        std::string s=v;key<<s.size()<<':'<<s<<';';
    }
    /* Optional extension preserves legacy M5 seal spelling. When present it
     * binds the original manifest after M4 replaces source/analysis identities;
     * deleting or changing it invalidates the complete accepted key. */
    const char *production=ST_tensor_metadata(WN_st_idx(root),"tensor.high.production.input");
    if(production) {
        if(!*production) throw std::runtime_error("native_selected_empty_production_binding");
        std::string text=production;key<<"production;"<<text.size()<<':'<<text<<';';
    }
    /* Optional H1c extension preserves legacy seals but cannot be removed
     * without invalidating an imported selection checkpoint. */
    const char *device=ST_tensor_metadata(WN_st_idx(root),"tensor.high.selected.device");
    if(device) {std::string text=device;key<<"device;"<<text.size()<<':'<<text<<';';}
    const char *schema=ST_tensor_metadata(WN_st_idx(root),"tensor.high.selected.schema");
    const char *movement=ST_tensor_metadata(WN_st_idx(root),"tensor.high.selected.movement");
    const char *group=ST_tensor_metadata(WN_st_idx(root),"tensor.high.selected.head_group");
    if(schema && !strcmp(schema,"lno-selected-mla-v2")) {
        if(!movement || !*movement || !group || strcmp(group,"1")) throw std::runtime_error("native_selected_movement_binding");
        std::string text=movement;key<<"movement-selected-v1;"<<text.size()<<':'<<text<<";G=1;";
    } else if(movement || group) throw std::runtime_error("native_selected_unversioned_movement");
    for(INT i=0;i<WN_num_formals(root);++i) {
        const char *trait=ST_tensor_metadata(WN_st_idx(WN_formal(root,i)),"tensor.cache.trait");
        if(trait) { std::string text=trait;key<<"cache-trait-v1;"<<i<<';'<<text.size()<<':'<<text<<';'; }
    }
    return key.str();
}

/* Bind the worker level, fixed recipe, launch/storage and protocol facts to
 * its actual native tree; this read-only key is shared with CUDA admission. */
inline std::string VHO_tensor_bridge_key(WN *root) {
    std::ostringstream s;s<<DSL_NATIVE_TREE_KEY().Build(root);
    const char *fields[]={"tensor.bridge.schema","tensor.bridge.level","tensor.bridge.source_key","tensor.bridge.fixed_recipe",
        "tensor.kernel.block_x","tensor.kernel.grid_x","tensor.kernel.shared_bytes","tensor.kernel.operation",
        "tensor.kernel.split_width","tensor.kernel.split_stage","tensor.kernel.mapping",
        "tensor.kernel.attention_layout","tensor.kernel.attention_accumulation","tensor.kernel.attention_statistics",
        "tensor.kernel.explanations_count","tensor.kernel.explanations_version","tensor.kernel.body_hash",
        "tensor.bridge.source_comment_count","tensor.kernel.source_operator","tensor.kernel.source_version",
        "tensor.kernel.source_node","tensor.kernel.source_pu","tensor.kernel.elements","tensor.kernel.fused",
        "tensor.kernel.opt_level","tensor.kernel.chunk_selection","tensor.kernel.attention_search"};
    for(unsigned i=0;i<sizeof(fields)/sizeof(fields[0]);++i) {
        const char *v=ST_tensor_metadata(WN_st_idx(root),fields[i]);std::string text=v?v:"";
        s<<(v!=NULL)<<';'<<text.size()<<':'<<text<<';';
    }
    for(INT i=0;i<WN_num_formals(root);++i) {
        const char *fields[]={"value","storage","role","ordinal","elements","bytes","dtype","name"};
        for(unsigned j=0;j<8;++j) {
            std::ostringstream name;name<<"tensor.argument."<<i<<'.'<<fields[j];
            const char *v=ST_tensor_metadata(WN_st_idx(root),name.str().c_str());std::string text=v?v:"";
            s<<(v!=NULL)<<';'<<text.size()<<':'<<text<<';';
        }
        std::ostringstream name;name<<"tensor.argument."<<i<<".cache_trait";
        const char *trait=ST_tensor_metadata(WN_st_idx(root),name.str().c_str());
        if(trait) {std::string text=trait;s<<"cache-trait-v1;"<<i<<';'<<text.size()<<':'<<text<<';';}
    }
    /* Emitters declare every shared object, including an unused object. Bind
     * that complete inventory so resource metadata cannot hide a new array. */
    const ST *shared;ST_IDX idx;unsigned count=0;
    FOREACH_SYMBOL(CURRENT_SYMTAB,shared,idx) if(ST_device_symbol_kind(*shared)==DEVICE_SYMBOL_SHARED) {
        std::string text=DSL_NATIVE_TREE_KEY().Declaration(shared);s<<text.size()<<':'<<text<<';';++count;
    }
    s<<count<<';';count=0;
    /* These declarations are dormant while ownership is abstract, but Low
     * will bind to them. Guard their complete interface before that binding. */
    FOREACH_SYMBOL(GLOBAL_SYMTAB,shared,idx)
        if(!strcmp(ST_name(shared),"blockIdx") || !strcmp(ST_name(shared),"threadIdx")) {
            std::string text=DSL_NATIVE_TREE_KEY().Declaration(shared);s<<text.size()<<':'<<text<<';';++count;
        }
    s<<count<<';';return s.str();
}
#endif
