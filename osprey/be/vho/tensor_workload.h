/* Copyright (C) 2026 Open64 Project */
/* Phase-owned MLA workload analysis over detached source/invocation records.
 * No WN/ST/TY, runtime probing, serving dispatch or candidate transformation.
 * Design: doc/AI-COMPILER-MLA-Q2-WORKLOAD-v1.0.md. */
#ifndef tensor_workload_INCLUDED
#define tensor_workload_INCLUDED
#include "tensor_kernel_lower.h"
/* Static dense evaluation domain, not active lengths or a serving label. */
struct VHO_TENSOR_WORKLOAD_GEOMETRY {
    bool known,update_items_known;
    UINT64 batch,heads,queries,keys,latent,rotary,update_items;
    UINT32 query_value,rotary_query_value,mask_value,latent_value,rotary_value;
    std::string source_key,reason,memory_form,source_effect;
    /* Unknown geometry has no usable dimensions or value identities. */
    VHO_TENSOR_WORKLOAD_GEOMETRY():known(false),update_items_known(false),batch(0),heads(0),queries(0),keys(0),latent(0),rotary(0),update_items(0),query_value(~0U),rotary_query_value(~0U),mask_value(~0U),latent_value(~0U),rotary_value(~0U) {}
};
/* One request in a dense-padded invocation. Offsets index the logical packed
 * query sequence only; they never change the source buffer strides. */
struct VHO_TENSOR_WORKLOAD_REQUEST {
    UINT64 request,query_begin,queries,history,valid,append_tokens,window_left;
    std::string phase,mask_policy,update;
    std::vector<UINT64> query_positions,key_positions,update_positions;
    std::vector<unsigned char> allowed;
    /* All runtime witnesses must be supplied explicitly by the owning adapter. */
    VHO_TENSOR_WORKLOAD_REQUEST():request(0),query_begin(0),queries(0),history(0),valid(0),append_tokens(0),window_left(0) {}
};
/* Ephemeral value witness supplied by an invocation owner. A file, ST hint or
 * fixture label is not a live engine binding. Q3 owns actual buffer/generation
 * joining; this Q2 API validates semantic consistency under that precondition. */
struct VHO_TENSOR_WORKLOAD_INVOCATION {
    unsigned version;
    std::string source_key,invocation_id,evidence;
    std::vector<VHO_TENSOR_WORKLOAD_REQUEST> requests;
    /* Version and evidence are explicit, not inferred from a phase string. */
    VHO_TENSOR_WORKLOAD_INVOCATION():version(0) {}
};
/* -1 means unobserved. Empty position lists do not prove a source-domain fact. */
struct VHO_TENSOR_WORKLOAD_SEGMENT {
    UINT64 request;
    INT64 query_begin,queries,history,valid,append_tokens;
    std::string phase,reason,mask_policy,update,work;
    std::vector<UINT64> query_positions,key_positions,update_positions;
    /* Preserve useful geometry while active work and serving semantics are unknown. */
    VHO_TENSOR_WORKLOAD_SEGMENT():request(0),query_begin(-1),queries(-1),history(-1),valid(-1),append_tokens(-1),phase("unknown"),reason("no_bound_serving_invocation"),mask_policy("opaque-runtime-mask"),update("unknown"),work("unknown") {}
};
struct VHO_TENSOR_WORKLOAD_FACTS {
    VHO_TENSOR_WORKLOAD_GEOMETRY geometry;
    std::string phase,workload,reason,evidence,invocation_id,binding_key;
    bool has_unknown,ragged,zero_active_queries;
    std::vector<VHO_TENSOR_WORKLOAD_SEGMENT> segments;
    /* This never certifies dispatch/reuse, native lifetime or serving publication. */
    VHO_TENSOR_WORKLOAD_FACTS():phase("unknown"),workload("unknown"),reason("unrecognized_attention_dataflow"),evidence("source-only"),has_unknown(true),ragged(false),zero_active_queries(false) {}
};
/* Read-only source recognition plus optional checked invocation witness. Error
 * clears output; valid but insufficient evidence returns explicit unknown. */
extern bool VHO_tensor_infer_workload(const VHO_TENSOR_PROGRAM &,
    const VHO_TENSOR_WORKLOAD_INVOCATION *,VHO_TENSOR_WORKLOAD_FACTS &,std::string &);
/* The invocation owner supplies its current witness again. Changed invocation,
 * source, visibility or declarations expire old facts before any consumer use. */
extern bool VHO_tensor_revalidate_workload(const VHO_TENSOR_PROGRAM &,
    const VHO_TENSOR_WORKLOAD_INVOCATION *,const VHO_TENSOR_WORKLOAD_FACTS &,std::string &);
/* Stable owned source explanation, printed through WHIRL COMMENT/whirl2c. */
extern std::string VHO_tensor_workload_explanation(const VHO_TENSOR_PROGRAM &);
/* Owned diagnostics only; no native state or phase-dependent dispatch. */
extern void VHO_tensor_print_workload(FILE *,const VHO_TENSOR_WORKLOAD_FACTS &,const char *);
#endif
