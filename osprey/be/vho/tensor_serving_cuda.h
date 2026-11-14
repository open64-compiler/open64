/* Copyright (C) 2026 Open64 Project */
/* Optional Q6d CUDA Driver owner over the existing Q3 registry. No compiler IR,
 * kernel generation, search, JIT or phase selection. Contract:
 * doc/AI-COMPILER-MLA-Q6D-DEVICE-BINDINGS-v1.0.md. */
#ifndef tensor_serving_cuda_INCLUDED
#define tensor_serving_cuda_INCLUDED
#include "tensor_serving.h"
/* Owner-authenticated fixed Q6c code; this is an internal embedding contract,
 * not a serving-engine packaging ABI or an AOT selection mechanism. */
struct VHO_MLA_CUDA_CODE {
    VHO_TENSOR_PROGRAM program;
    std::string source_key,mapping_key,stage_keys[3],names[3];
    UINT64 grid[3],block[3],shared[3],cubin_bytes;
    unsigned char sha256[32];
};
/* Trusted owner hooks establish the full three-stage audit/digest association.
 * Claimed keys or a matching image hash alone are not compiler provenance.
 * Hooks never throw; owner/context outlive the adapter. */
struct VHO_MLA_CUDA_HOOKS {
    void *context;
    bool (*authorize)(void *,const VHO_MLA_CUDA_CODE &);
    bool (*sha256)(void *,const void *,size_t,unsigned char[32]);
};
class VHO_MLA_CUDA {
    struct IMPL;
    IMPL *_impl;
    /* Only Open/Destroy own construction/teardown; failed cleanup is retryable. */
    VHO_MLA_CUDA();
    ~VHO_MLA_CUDA();
    VHO_MLA_CUDA(const VHO_MLA_CUDA &);
    VHO_MLA_CUDA &operator=(const VHO_MLA_CUDA &);
public:
    /* Explicit current context + nondefault producer/execution streams, sm_90
     * and CUDA >=12.8; owned cubin copy is hashed before load (no PTX/JIT).
     * On failure output may receive a cleanup-only handle; always retain it. */
    static bool Open(const VHO_MLA_CUDA_CODE &,const void *,size_t,void *,void *,void *,const VHO_MLA_CUDA_HOOKS &,VHO_MLA_CUDA *&,std::string &);
    /* H1d embedding owner authenticates the requirements-to-code association.
     * The explicit observer retains its budget reservation through completion;
     * legacy CODE/HOOKS layouts and Open remain unchanged. */
    static bool Open(const VHO_MLA_CUDA_CODE &,const void *,size_t,void *,void *,void *,const VHO_MLA_CUDA_HOOKS &,const DSL_GPU_SERVING_BINDING &,VHO_MLA_CUDA *&,std::string &);
    /* Refuse live jobs/readers/bundles/allocations. Failed CUDA cleanup retains
     * the object and handles for retry; no implicit context switching. Caller
     * externally excludes all concurrent API use during object destruction. */
    static bool Destroy(VHO_MLA_CUDA *&,std::string &);
    /* Token-only state; validity does not imply hardware numerical qualification. */
    bool Valid() const;
    /* Allocate owned device memory, register actual CUDA buffer identity and a
     * Q3 generation. Retire must succeed before physical free/reuse. */
    bool Allocate(UINT64,VHO_SERVING_HANDLE &,std::string &);
    /* Cooperative producer-stream upload; an owned event joins execution-stream
     * readiness and completes before caller host input may be released. */
    bool Write(const VHO_SERVING_HANDLE &,UINT64,const void *,UINT64,std::string &);
    /* Synchronous download of a bounded registered range; refuse writers and
     * retain unknown transfers before any allocation can be reclaimed. */
    bool Read(const VHO_SERVING_HANDLE &,UINT64,void *,UINT64,std::string &);
    /* Refuse pinned/published/foreign/recycled storage; preserve failed frees. */
    bool Retire(const VHO_SERVING_HANDLE &,std::string &);
    /* Import/Begin use actual device controls/scales/page maps through Q3 I/O.
     * Begin explicitly instantiates this fixed code for a fresh device snapshot;
     * the historical host compilation snapshot grants no device authority. */
    bool Import(const std::vector<VHO_SERVING_VIEW> &,const VHO_SERVING_VIEW &,UINT64 &,std::string &);
    bool Begin(const std::vector<VHO_SERVING_VIEW> &,const VHO_SERVING_VIEW &,UINT64,VHO_SERVING_FACTS &,std::string &);
    /* Queue prepare -> attention -> finalize once, then record an owned event.
     * Failure may leave a cleanup-only live ticket: Complete/Cancel must drain it. */
    bool Submit(UINT64,std::string &);
    /* Query/wait actual completion, status, controls, scale versions and parent
     * CAS before Q3 publishes. Pending/error-to-drain keeps every lease live. */
    bool Complete(UINT64,bool,UINT64 &,std::string &);
    /* Prepared cancel expires immediately; submitted cancel permanently denies
     * publication and releases only after proven completion/drain. */
    bool Cancel(UINT64,bool,std::string &);
    /* Retry uncertain I/O drains and resource cleanup without publishing jobs. */
    bool Drain(std::string &);
    /* All four cache/scale outputs, maps and metadata share a reader lease.
     * A fixed owned ready event orders a validated nondefault consumer stream. */
    bool Reader(UINT64,void *,UINT64 &,std::vector<UINT64> &,std::string &);
    /* Seal consumer work once with an owned event; pending/error retains lease.
     * Caller must enqueue no more use after the first Reader_done attempt. */
    bool Reader_done(UINT64,bool,std::string &);
    /* Retire only after all consumers and pending parent users release. */
    bool Retire_bundle(UINT64,std::string &);
    /* Read-only generation diagnostics; never a device pointer/reader lease. */
    bool Inspect_bundle(UINT64,VHO_SERVING_BUNDLE_FACTS &,std::string &);
    /* Diagnostic pending-token lookup does not acquire or prolong a lease. */
    bool Is_live(UINT64) const;
};
#endif
