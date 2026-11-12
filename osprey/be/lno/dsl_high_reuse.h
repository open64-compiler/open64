/* Copyright (C) 2026 Open64 Project */
/* LNO-owned, invocation-local cache storage/version/access/lifetime proofs.
 * All records are copied values. No WN/ST/TY/analysis pointer, runtime address,
 * content hash, GPU measurement or grouped materialization authority escapes.
 * Design: doc/AI-COMPILER-MLA-MR1-OWNED-REUSE-v1.0.md. */
#ifndef dsl_high_reuse_INCLUDED
#define dsl_high_reuse_INCLUDED
#include "dsl_high_snapshot.h"
#include "../vho/tensor_kernel_lower.h"
#include "dsl_tensor_locality.h"

struct DSL_HIGH_CACHE_TRAFFIC {
    uint64_t payload_bytes, fused_logical_read_bytes;
    DSL_HIGH_CACHE_TRAFFIC() : payload_bytes(0), fused_logical_read_bytes(0) {}
};
/* Checked dense FP32 counts only; no transaction/cache/residency assumption.
 * Clear output on refusal. UINT64_MAX remains the shared unknown sentinel. */
extern bool LNO_DSL_cache_traffic(uint64_t batch,uint64_t heads,uint64_t tokens,
    uint64_t columns,unsigned sweeps,DSL_HIGH_CACHE_TRAFFIC &);

struct DSL_HIGH_REUSE_STORAGE {
    unsigned value, root, producer_operation;
    std::string native_storage, version, origin_version, layout, producer_group;
    std::vector<unsigned> alias_values;
    std::vector<uint64_t> shape, strides;
    std::string first_use, last_use;
    DSL_HIGH_STORAGE native;
    DSL_TENSOR_LOCALITY_FACT_RECORD locality;
    DSL_HIGH_REUSE_STORAGE();
};
struct DSL_HIGH_REUSE_ACCESS {
    std::string id, storage_version, execution_group, head_loop, purpose;
    unsigned storage, root, operation, operand;
    uint64_t read_elements, read_bytes, footprint_bytes, head_multiplicity;
    uint64_t reduction_extent, reduction_slice;
    bool sliced_reduction;
    bool head_invariant;
    DSL_HIGH_ACCESS native;
    DSL_HIGH_REUSE_ACCESS() : storage(0), root(0), operation(0), operand(0),read_elements(0),
        read_bytes(0),footprint_bytes(0),head_multiplicity(0),reduction_extent(1),reduction_slice(0),
        sliced_reduction(false),head_invariant(false) {}
};
struct DSL_HIGH_REUSE_CACHE {
    unsigned root, logical_sweeps;
    std::string role, role_provenance, version, first_use, last_use;
    uint64_t heads, payload_bytes, fused_logical_read_bytes, source_read_bytes,
        source_write_bytes, materialized_copy_bytes;
    std::vector<std::string> reads;
    DSL_HIGH_REUSE_CACHE() : root(0),logical_sweeps(0),heads(0),payload_bytes(0),
        fused_logical_read_bytes(0),source_read_bytes(0),source_write_bytes(0),materialized_copy_bytes(0) {}
};
struct DSL_HIGH_REUSE_SUMMARY {
    std::vector<DSL_HIGH_REUSE_STORAGE> storages;
    std::vector<DSL_HIGH_REUSE_ACCESS> accesses;
    std::vector<DSL_HIGH_REUSE_CACHE> caches;
    std::string key;
};
/* Analyze already-owned data only. This unbound summary is not an application
 * certificate. A complete refusal clears output and leaves legacy G=1 usable. */
extern bool LNO_DSL_analyze_cache_reuse(const std::vector<DSL_HIGH_STORAGE> &,
    const std::vector<DSL_HIGH_LOOP> &,const std::vector<DSL_HIGH_ACCESS> &,
    const std::vector<DSL_HIGH_ALIAS> &,const VHO_TENSOR_PROGRAM &,
    DSL_HIGH_REUSE_SUMMARY &,std::string &);

class DSL_HIGH_REUSE_FACTS {
    bool _valid;
    DSL_HIGH_STAMP _stamp;
    DSL_HIGH_BINDINGS _bindings;
    std::string _source, _facts, _memory, _manifest;
    DSL_HIGH_REUSE_SUMMARY _summary;
    friend bool LNO_DSL_derive_cache_reuse(const DSL_HIGH_SNAPSHOT &,const DSL_HIGH_EPOCH &,
        const DSL_HIGH_BINDINGS &,const VHO_TENSOR_PROGRAM &,DSL_HIGH_REUSE_FACTS &,std::string &);
public:
    DSL_HIGH_REUSE_FACTS() : _valid(false) {}
    bool Available() const { return _valid; }
    /* Token-only diagnostic; application additionally needs Is_current and
     * the owner's native snapshot recapture before any transformation. */
    bool Is_live(const DSL_HIGH_EPOCH &epoch) const { return _valid && epoch.Matches(_stamp); }
    bool Is_current(const DSL_HIGH_SNAPSHOT &,const DSL_HIGH_EPOCH &,
        const DSL_HIGH_BINDINGS &,const VHO_TENSOR_PROGRAM &) const;
    const DSL_HIGH_REUSE_SUMMARY &Summary() const { return _summary; }
    /* Safe after native cleanup. live=false never presents expired facts as
     * permission to apply a schedule. All hardware traffic remains unknown. */
    void Print(FILE *,const char *stage,bool live) const;
};
/* Bind complete owned proofs to the current native snapshot and source recipe.
 * No source normalization, search, lowering, profiling or reference execution. */
extern bool LNO_DSL_derive_cache_reuse(const DSL_HIGH_SNAPSHOT &,const DSL_HIGH_EPOCH &,
    const DSL_HIGH_BINDINGS &,const VHO_TENSOR_PROGRAM &,DSL_HIGH_REUSE_FACTS &,std::string &);
#endif
