/* Copyright (C) 2026 Open64 Project */
/* Owned Q1 High storage/access and local cache-scale SSA facts, PU-local.
 * Design: doc/AI-COMPILER-MLA-Q1-HIGH-OWNED-FACTS-v1.0.md. The driver supplies
 * the live High scope and existing mutation epoch. Indirect accesses remain
 * explicit; this is not an affine dependence or grouped-reuse certificate. */
#ifndef dsl_high_quantized_INCLUDED
#define dsl_high_quantized_INCLUDED
#include "dsl_high_snapshot.h"
#include "../vho/tensor_kernel_lower.h"
struct DSL_HIGH_QUANTIZED_ACCESS {
    std::string id,storage,version,group,address_key,encoding;
    unsigned order,operation;
    bool write,indirect,conditional;
    int64_t element_bytes;
    std::vector<std::string> loops;
};
struct DSL_HIGH_QUANTIZED_STORAGE {
    std::string id,value,version,scale_storage,scale_version,encoding,role;
    std::string first_use,last_use,origin_storage,origin_version,lineage,scale_pair_ready_group;
    std::vector<uint64_t> shape,strides;
    uint64_t payload_bytes;
    unsigned formal,source_root;
    bool input,output;
};
class DSL_HIGH_QUANTIZED_FACTS {
    bool _valid;
    DSL_HIGH_STAMP _stamp;
    std::string _native_key,_source_key;
    std::vector<DSL_HIGH_LOOP> _loops;
    std::vector<DSL_HIGH_QUANTIZED_STORAGE> _storages;
    std::vector<DSL_HIGH_QUANTIZED_ACCESS> _accesses;
    friend class DSL_HIGH_QUANTIZED_CAPTURE;
public:
    /* Empty or refused captures never expose a usable seal. */
    DSL_HIGH_QUANTIZED_FACTS() : _valid(false) {}
    /* Token-only freshness gate; native consumers must also recapture. */
    bool Is_live(const DSL_HIGH_EPOCH &e) const { return _valid && e.Matches(_stamp); }
    /* Canonical signed loop records use stable lexical tree paths. */
    const std::vector<DSL_HIGH_LOOP> &Loops() const { return _loops; }
    /* Owned vectors stay readable after native PU/maps/pools are destroyed. */
    const std::vector<DSL_HIGH_QUANTIZED_STORAGE> &Storages() const { return _storages; }
    const std::vector<DSL_HIGH_QUANTIZED_ACCESS> &Accesses() const { return _accesses; }
    /* Exact keys, not review hashes, establish equality on revalidation. */
    bool Same_content(const DSL_HIGH_QUANTIZED_FACTS &o) const {
        return _valid && o._valid && _native_key==o._native_key && _source_key==o._source_key;
    }
};
/* Read-only structural capture at canonical Q1 High, before ARRAY lowering.
 * Copies no native pointer. Refusal clears output and preserves fallback. */
extern bool LNO_DSL_capture_quantized_high(WN *,const VHO_TENSOR_PROGRAM &,
    const DSL_HIGH_EPOCH &,DSL_HIGH_QUANTIZED_FACTS &,std::string &);
/* Epoch gate precedes native recapture; source/scale/alias/IR changes refuse. */
extern bool LNO_DSL_revalidate_quantized_high(WN *,const VHO_TENSOR_PROGRAM &,
    const DSL_HIGH_EPOCH &,const DSL_HIGH_QUANTIZED_FACTS &,std::string &);
#endif
