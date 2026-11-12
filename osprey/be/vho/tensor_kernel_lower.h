/* Copyright (C) 2026 Open64 Project */
/* PU-local tensor semantic lowering and explicit baseline GPU mapping.
 * Design: doc/AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.1.md. These transient
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
    std::string source_pu;
    std::string source_file;
    UINT32 opt_level;
    UINT32 threads;
    std::vector<VHO_TENSOR_VALUE> values;
    std::vector<VHO_TENSOR_OPERATION> operations;
};

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
/* Positive static extents must have been validated before taking the product. */
extern UINT64 VHO_tensor_elements(const VHO_TENSOR_VALUE &);
#endif
