/* Copyright (C) 2026 Open64 Project */
/* PU-local tensor semantic lowering and explicit baseline GPU mapping.
 * Design: doc/AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.2.md. These transient
 * recipes contain no WHIRL pointers and add no mapped-image representation.
 * The driver owns source/device scopes, image writing and launch manifests. */
#ifndef tensor_kernel_lower_INCLUDED
#define tensor_kernel_lower_INCLUDED
#include <map>
#include <cstring>
#include <string>
#include <vector>
#include "defs.h"
#include "srcpos.h"
#include "dsl_hardware_performance.h"
struct pu_info;

struct VHO_TENSOR_VALUE {
    std::string name;
    std::string dtype;
    std::vector<UINT64> shape;
    UINT32 role;                 /* 0 temporary, 1 input, 2 result */
    UINT32 ordinal;
    UINT32 storage;              /* canonical dense logical allocation */
};
struct VHO_TENSOR_OPERATION {
    std::string name;
    UINT32 version;
    UINT32 source_node;
    UINT32 source_line;
    std::vector<UINT32> inputs;
    UINT32 output;
    std::map<std::string,std::string> attributes;
};
/* Explicit offline device envelope; architecture alone never supplies SM count.
 * Capacities/register demand are estimates under the initial profile schema. */
struct VHO_TENSOR_GPU_PROFILE {
    VHO_TENSOR_GPU_PROFILE():sms(0),registers_per_thread(0),target_ctas_per_sm(0),scratch_budget(0) {
        memset(&resources,0,sizeof(resources));
    }
    std::string id,architecture;
    UINT64 sms,registers_per_thread,target_ctas_per_sm,scratch_budget;
    DSL_HARDWARE_RESOURCE_RECORD resources;
};
struct VHO_TENSOR_CHUNK_CANDIDATE {
    VHO_TENSOR_CHUNK_CANDIDATE():width(0),threads(128),chunks(0),ctas(0),local_shared(0),merge_shared(0),
        scratch(0),resident(0),goal(0),coverage(0),eligible(false) {}
    UINT64 width,threads,chunks,ctas,local_shared,merge_shared,scratch,resident,goal,coverage;
    bool eligible;
    std::string reason;
};
struct VHO_TENSOR_CHUNK_SELECTION {
    VHO_TENSOR_CHUNK_SELECTION():width(0),short_direct(false) {}
    UINT32 width;
    bool short_direct;
    std::vector<VHO_TENSOR_CHUNK_CANDIDATE> candidates;
};
struct VHO_TENSOR_PROGRAM {
    VHO_TENSOR_PROGRAM():opt_level(0),threads(1),fuse(false),split_width(0),cooperative(false),fuse_attention(false),register_latent(false),auto_chunks(false),search_selected(false),chunk_policy(1) {}
    std::string source_pu;
    std::string source_file;
    UINT32 opt_level;
    UINT32 threads;
    bool fuse;                  /* explicit O3 producer inlining */
    UINT32 split_width;          /* 0 unsplit, otherwise selected chunk width */
    bool cooperative;           /* explicit warp trees and staged dot products */
    bool fuse_attention;        /* explicit stable local softmax / rescaled merge */
    bool register_latent;       /* four lane-private columns per warp, v5 */
    bool auto_chunks;           /* sealed resource-aware v6 selection */
    bool search_selected;        /* fixed two-phase winner; never rerun selection per kernel */
    std::string search_evidence,search_identity; /* driver-owned canonical audit JSON and opaque identity */
    UINT32 chunk_policy;        /* 1 legacy 128+, 2 expanded 32+ search, 3 narrow blocks, 4 warp registers, 5 two-warp broadcasts, 6 warp-owned partials */
    VHO_TENSOR_GPU_PROFILE gpu;
    std::vector<VHO_TENSOR_VALUE> values;
    std::vector<VHO_TENSOR_OPERATION> operations;
};

/* Deterministic PU-local launch plan. Synthetic scratch is detached storage,
 * never a new logical opcode or binary WHIRL record. IDs >=4096 are merges. */
struct VHO_TENSOR_KERNEL {
    VHO_TENSOR_KERNEL():warp_owned_partials(false),two_warp_statistics(false),register_statistics(false) {}
    bool warp_owned_partials;   /* independent 32-token distributions, one CTA tuple publication */
    bool two_warp_statistics;   /* disjoint summary slots, register broadcasts, three CTA barriers */
    bool register_statistics;   /* one token/lane; private scores/weights, shuffle broadcasts */
    UINT32 id,root,output,threads; /* selected launch size, owned by VHO */
    UINT32 split_width,chunks,stage; /* 0 direct, 1 partial, 2 merge */
    UINT64 elements;
    UINT32 mapping;             /* 0 thread, 1 warp, 2 staged dot, 3 attention CTA, 4 tuple merge */
    std::vector<UINT32> inputs;
};
/* Facts derived by the owning phase from a closed absorbed-attention DAG.
 * Bundles are physical allocations; source value identities remain unchanged. */
struct VHO_TENSOR_ATTENTION {
    VHO_TENSOR_ATTENTION():enabled(false),bundle(0),partial(0) {}
    bool enabled;
    UINT32 q,qr,cache,rope_cache,mask,latent,lse,bundle,partial,scale_bits;
    UINT64 batch,heads,length,rank,rope;
};
struct VHO_TENSOR_SCHEDULE {
    VHO_TENSOR_PROGRAM bindings;
    VHO_TENSOR_ATTENTION attention;
    VHO_TENSOR_CHUNK_SELECTION chunk_selection; /* source-scope choice, before physical bindings */
    std::vector<VHO_TENSOR_KERNEL> kernels;
};
/* Select O3 producer fusion, cooperative mapping and proved attention fusion.
 * Flags carry the explicit numerical permissions sealed by the driver. */
extern BOOL VHO_tensor_schedule(const VHO_TENSOR_PROGRAM &,VHO_TENSOR_SCHEDULE *,FILE *);

/* Pure per-PU selection; existing occupancy service supplies rounded limits.
 * Output is unchanged on error. No native tables, probing or CUDA emission. */
extern BOOL VHO_tensor_select_attention_chunks(const VHO_TENSOR_PROGRAM &,
    const VHO_TENSOR_GPU_PROFILE &,VHO_TENSOR_CHUNK_SELECTION *,FILE *);
extern BOOL VHO_tensor_gpu_profile_valid(const VHO_TENSOR_GPU_PROFILE &,FILE *);
extern const char *VHO_tensor_chunk_policy_name(const VHO_TENSOR_PROGRAM &);
extern std::string VHO_tensor_chunk_selection_identity(const VHO_TENSOR_PROGRAM &);
/* Validate all dimensions, dataflow, schemas and aliases before mutation.
 * This is also the fresh-device-context admission check for a copied recipe. */
extern BOOL VHO_tensor_validate_program(const VHO_TENSOR_PROGRAM &,FILE *);
/* Read the driver-selected source PU without mutation. Reject unsupported
 * effects/control flow; replace output only after whole-PU admission succeeds. */
extern BOOL VHO_tensor_prepare_program(struct pu_info *,UINT32 opt_level,
    VHO_TENSOR_PROGRAM *output,FILE *diagnostic);
/* Construct one operation in the driver's active empty kernel PU. Every
 * recoverable check precedes allocation. No PU switching, source lookup,
 * profitability choice, external library dependency or source emission occurs. */
extern BOOL VHO_tensor_materialize_kernel(const VHO_TENSOR_PROGRAM &,UINT32 operation,
    SRCPOS source,FILE *diagnostic);
/* Audit the active reopened kernel against its source recipe, persisted
 * launch/argument bindings and construction fingerprint. No mutation occurs. */
extern BOOL VHO_tensor_verify_kernel(const VHO_TENSOR_PROGRAM &,UINT32 operation,FILE *);
/* Add the selected explanation records as ordinary COMMENT statements in the
 * active verified kernel. Reject repeated annotation before table mutation. */
extern BOOL VHO_tensor_annotate_kernel(const VHO_TENSOR_PROGRAM &,UINT32 operation,FILE *);
/* Positive static extents must have been validated before taking the product. */
extern UINT64 VHO_tensor_elements(const VHO_TENSOR_VALUE &);
/* Derive the reviewed closed pattern from a validated recipe; throw on a
 * requested incompatible fusion, and never consult compiler tables or names. */
extern VHO_TENSOR_ATTENTION VHO_tensor_attention(const VHO_TENSOR_PROGRAM &);
/* Static shared memory selected by the owning physical lowering. */
extern UINT64 VHO_tensor_shared_bytes(const VHO_TENSOR_PROGRAM &,const VHO_TENSOR_KERNEL &);
/* Launch geometry is selected before emission and shared with native audit. */
extern UINT64 VHO_tensor_grid(const VHO_TENSOR_PROGRAM &,const VHO_TENSOR_KERNEL &);
#endif
