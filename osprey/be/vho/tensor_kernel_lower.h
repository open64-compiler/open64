/* Copyright (C) 2026 Open64 Project */
/* PU-local tensor semantic lowering and explicit baseline GPU mapping.
 * Design: doc/AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.2.md. These transient
 * recipes contain no WHIRL pointers and add no mapped-image representation.
 * The driver owns source/device scopes, image writing and launch manifests. */
#ifndef tensor_kernel_lower_INCLUDED
#define tensor_kernel_lower_INCLUDED
#include <map>
#include <string>
#include <vector>
#include "defs.h"
#include "srcpos.h"
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
struct VHO_TENSOR_PROGRAM {
    VHO_TENSOR_PROGRAM():opt_level(0),threads(1),fuse(false),split_width(0),cooperative(false),fuse_attention(false) {}
    std::string source_pu;
    std::string source_file;
    UINT32 opt_level;
    UINT32 threads;
    bool fuse;                  /* explicit O3 producer inlining */
    UINT32 split_width;          /* 0 unsplit, otherwise selected chunk width */
    bool cooperative;           /* explicit warp trees and staged dot products */
    bool fuse_attention;        /* explicit stable local softmax / rescaled merge */
    std::vector<VHO_TENSOR_VALUE> values;
    std::vector<VHO_TENSOR_OPERATION> operations;
};

/* Deterministic PU-local launch plan. Synthetic scratch is detached storage,
 * never a new logical opcode or binary WHIRL record. IDs >=4096 are merges. */
struct VHO_TENSOR_KERNEL {
    UINT32 id,root,output;
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
    std::vector<VHO_TENSOR_KERNEL> kernels;
};
/* Select O3 producer fusion, cooperative mapping and proved attention fusion.
 * Flags carry the explicit numerical permissions sealed by the driver. */
extern BOOL VHO_tensor_schedule(const VHO_TENSOR_PROGRAM &,VHO_TENSOR_SCHEDULE *,FILE *);

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
