/* Copyright (C) 2026 Open64 Project */
/* Owned Q1 High storage/access and local cache-scale SSA facts, PU-local.
 * Design: doc/AI-COMPILER-MLA-Q1-HIGH-OWNED-FACTS-v1.0.md. The driver supplies
 * the live High scope and existing mutation epoch. Indirect accesses remain
 * explicit; this is not an affine dependence or grouped-reuse certificate. */
#ifndef dsl_high_quantized_INCLUDED
#define dsl_high_quantized_INCLUDED
#include "dsl_high_snapshot.h"
#include "../vho/tensor_kernel_lower.h"
#include "../vho/tensor_workload.h"
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
/* Synchronous DU adapter supplied by the owning phase; never retained in facts. */
struct DSL_HIGH_Q1_RESOLVER {
    void *context;
    WN *(*definition)(void *,WN *);
    /* Absent DU preserves conservative unknown/refusal behavior. */
    inline DSL_HIGH_Q1_RESOLVER() : context(NULL), definition(NULL) {}
};
class DSL_HIGH_QUANTIZED_FACTS {
    bool _valid;
    bool _post_preopt;
    DSL_HIGH_STAMP _stamp;
    std::string _native_key,_source_key,_device_key;
    std::vector<DSL_HIGH_LOOP> _loops;
    std::vector<DSL_HIGH_QUANTIZED_STORAGE> _storages;
    std::vector<DSL_HIGH_QUANTIZED_ACCESS> _accesses;
    VHO_TENSOR_WORKLOAD_FACTS _workload;
    friend class DSL_HIGH_QUANTIZED_CAPTURE;
public:
    /* Empty or refused captures never expose a usable seal. */
    inline DSL_HIGH_QUANTIZED_FACTS() : _valid(false), _post_preopt(false) {}
    /* Token-only freshness gate; native consumers must also recapture. */
    inline bool Is_live(const DSL_HIGH_EPOCH &e) const { return _valid && e.Matches(_stamp); }
    /* Canonical signed loop records use stable lexical tree paths. */
    inline const std::vector<DSL_HIGH_LOOP> &Loops() const { return _loops; }
    /* Owned vectors stay readable after native PU/maps/pools are destroyed. */
    inline const std::vector<DSL_HIGH_QUANTIZED_STORAGE> &Storages() const { return _storages; }
    /* Borrow copied accesses; no native node views survive capture. */
    inline const std::vector<DSL_HIGH_QUANTIZED_ACCESS> &Accesses() const { return _accesses; }
    /* Source workload facts share this native snapshot's epoch; serving phase
     * stays unknown without a separately bound invocation witness. */
    inline const VHO_TENSOR_WORKLOAD_FACTS &Workload() const { return _workload; }
    /* Reports distinguish freshly rebound physical sites from source-form facts. */
    inline bool Post_preopt() const { return _post_preopt; }
    /* Exact keys, including copied H1c evidence, establish revalidation;
     * context changes refuse even when native source and scale facts match. */
    inline bool Same_content(const DSL_HIGH_QUANTIZED_FACTS &o) const {
        return _post_preopt==o._post_preopt && _valid && o._valid && _native_key==o._native_key && _source_key==o._source_key && _device_key==o._device_key;
    }
};
/* Read-only structural capture at canonical Q1 High, before ARRAY lowering.
 * Copies no native pointer. Refusal clears output and preserves fallback. */
extern bool LNO_DSL_capture_quantized_high(WN *,const VHO_TENSOR_PROGRAM &,
    const DSL_HIGH_EPOCH &,DSL_HIGH_QUANTIZED_FACTS &,std::string &);
/* Epoch gate precedes native recapture; source/scale/alias/IR changes refuse. */
extern bool LNO_DSL_revalidate_quantized_high(WN *,const VHO_TENSOR_PROGRAM &,
    const DSL_HIGH_EPOCH &,const DSL_HIGH_QUANTIZED_FACTS &,std::string &);
/* Fresh canonical sites after PREOPT; source snapshots are never reused. */
extern bool LNO_DSL_rebind_quantized_high(WN *,const VHO_TENSOR_PROGRAM &,
    const DSL_HIGH_EPOCH &,const DSL_HIGH_Q1_RESOLVER &,DSL_HIGH_QUANTIZED_FACTS &,std::string &);
/* Revalidate rebound facts with the same live native resolver; epoch gate
 * precedes every native access and stale contexts never regain authority. */
extern bool LNO_DSL_revalidate_rebound_quantized_high(WN *,const VHO_TENSOR_PROGRAM &,
    const DSL_HIGH_EPOCH &,const DSL_HIGH_Q1_RESOLVER &,const DSL_HIGH_QUANTIZED_FACTS &,std::string &);
/* Owned report only; no native manager or node access. */
extern void LNO_DSL_print_quantized_high(FILE *,const DSL_HIGH_QUANTIZED_FACTS &,const char *,bool);
#endif
