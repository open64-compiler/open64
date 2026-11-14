/* Copyright (C) 2026 Open64 Project */
/* Q3 serving authority for the Q1 disjoint-storage ABI; host callbacks are the
 * default and explicit device providers use the private Q6d completion gate.
 * Runtime leases are distinct from compiler mutation epochs. No IR mutation,
 * search, JIT, device pointer dereference or asynchronous completion inference.
 * Design: doc/AI-COMPILER-MLA-Q3-SERVING-v1.0.md. */
#ifndef tensor_serving_INCLUDED
#define tensor_serving_INCLUDED
#include "tensor_serving_schema.h"
#include "dsl_gpu_contract.h"
#include "dsl_movement_facts.h"
struct VHO_SERVING_HANDLE {
    UINT64 owner,id,generation;
    /* Zero/foreign/recycled handles cannot acquire a lease. */
    VHO_SERVING_HANDLE():owner(0),id(0),generation(0) {}
};
struct VHO_SERVING_VIEW {
    VHO_SERVING_HANDLE handle;
    UINT64 offset,bytes;
    /* A root view has the exact contiguous Q1 size, within a live allocation. */
    VHO_SERVING_VIEW():offset(0),bytes(0) {}
};
struct VHO_SERVING_PAGE {
    UINT64 request,logical_page,physical_page,capacity_tokens,valid_tokens;
    UINT64 byte_offsets[4],row_bytes[4],base_alignment[4],page_alignment[4];
};
struct VHO_SERVING_FACTS {
    VHO_TENSOR_WORKLOAD_FACTS workload;
    std::vector<VHO_SERVING_PAGE> pages;
    std::vector<UINT64> allocation_ids,allocation_generations,content_versions;
    UINT64 ticket,parent_bundle;
    bool bound;
    /* Owned diagnostics never authorize use after their ticket expires. */
    VHO_SERVING_FACTS():ticket(0),parent_bundle(0),bound(false) {}
};
struct VHO_SERVING_BUNDLE_FACTS {
    UINT64 bundle;
    std::vector<UINT64> allocation_ids,allocation_generations,content_versions,valid_lengths;
    /* A missing/retired publication yields no usable record. */
    VHO_SERVING_BUNDLE_FACTS():bundle(0) {}
};
/* Optional runtime-owned device I/O. Validate returns the observed allocation
 * identity; Read/Write complete synchronously on success. Failed uncertain I/O
 * remains owned by the provider until Idle succeeds. Domain separates address
 * spaces. Callbacks/context must outlive the registry; no callback may throw. */
struct VHO_SERVING_MEMORY {
    UINT64 version,bytes;
    void *context;
    UINT64 domain;
    bool (*validate)(void *,void *,UINT64,UINT64 &,std::string &);
    bool (*read)(void *,const void *,void *,UINT64,std::string &);
    bool (*write)(void *,void *,const void *,UINT64,std::string &);
    bool (*idle)(void *);
};
class VHO_MLA_CUDA;
struct VHO_Q4_SNAPSHOT;
class VHO_TENSOR_SERVING {
    struct IMPL;
    IMPL *_impl;
    VHO_TENSOR_SERVING(const VHO_TENSOR_SERVING &);
    VHO_TENSOR_SERVING &operator=(const VHO_TENSOR_SERVING &);
    friend class VHO_MLA_CUDA;
    /* Only the owning device adapter can split execution. Armed calls keep all
     * leases until it proves completion/drain, then invokes Finish_external. */
    bool Start_external(UINT64,std::vector<void *> &,bool &,std::string &);
    bool Finish_external(UINT64,bool,UINT64 &,std::string &);
    /* Only an already acquired whole-bundle reader may resolve opaque views. */
    bool Bundle_views(UINT64,std::vector<VHO_SERVING_VIEW> &,std::string &);
public:
    /* The owner supplies a qualified Q1 program and legal allocator P set.
     * Refusal leaves no usable adapter. Only host-synchronous memory is admitted. */
    VHO_TENSOR_SERVING(const VHO_TENSOR_PROGRAM &,const std::vector<UINT64> &,std::string &);
    /* Explicit device registry; source validation/publication remain Q3. The
     * service admits actual device addresses, never host snapshot identities. */
    VHO_TENSOR_SERVING(const VHO_TENSOR_PROGRAM &,const std::vector<UINT64> &,const VHO_SERVING_MEMORY *,std::string &);
    /* Optional H1d requirements are copied; observer lifetime remains owned by
     * the caller. Recheck before arm/publication, retaining Q3 completion gates. */
    VHO_TENSOR_SERVING(const VHO_TENSOR_PROGRAM &,const std::vector<UINT64> &,const VHO_SERVING_MEMORY *,const DSL_GPU_SERVING_BINDING &,std::string &);
    /* Destroy only after all callbacks/readers finish; never frees borrowed memory. */
    ~VHO_TENSOR_SERVING();
    /* A failed constructor exposes no registry operations. */
    bool Valid() const;
    /* Teardown is permitted only after callbacks and external readers finish. */
    bool Quiescent() const;
    /* The owner retains readable/writable memory until Retire succeeds and must
     * use this registry for writes/reclamation. Overlapping registrations refuse. */
    bool Register(void *,UINT64,const std::string &,VHO_SERVING_HANDLE &,std::string &);
    /* Cooperative writes increment content versions; pinned/published buffers refuse. */
    bool Write(const VHO_SERVING_HANDLE &,UINT64,const void *,UINT64,std::string &);
    /* Synchronous read refuses active writers. Device failures retain provider
     * staging; the adapter must drain before reclamation. */
    bool Read(const VHO_SERVING_HANDLE &,UINT64,void *,UINT64,std::string &);
    /* Reject pinned or published memory; re-registration issues a fresh generation. */
    bool Retire(const VHO_SERVING_HANDLE &,std::string &);
    /* Atomically introduce an already-ready incoming host bundle. Source joins,
     * actual controls/pages/scales and complete ABI views must validate first. */
    bool Import(const std::vector<VHO_SERVING_VIEW> &,const VHO_SERVING_VIEW &,UINT64 &,std::string &);
    /* Acquire shared input and exclusive output/scratch/status leases. Facts are
     * rebuilt from actual host controls. A stale parent/refusal changes no bytes. */
    bool Begin(const std::vector<VHO_SERVING_VIEW> &,const VHO_SERVING_VIEW &,UINT64,VHO_SERVING_FACTS &,std::string &);
    /* Freeze normalized cache facts from a prepared, reobserved Q3 ticket.
     * No kernel, reference test, search or compiler mutation occurs here. */
    bool Freeze_costing(UINT64,VHO_Q4_SNAPSHOT &,std::string &);
    /* H2 captures owned registered ranges, cache/scale/page generations and
     * source workload facts. This grants no decoded-ready or device authority. */
    bool Capture_movement(UINT64,DSL_MOVEMENT_FACTS &,std::string &);
    /* Rebuild actual Q3 bindings; expired/stale tickets cannot revive a copy. */
    bool Revalidate_movement(UINT64,const DSL_MOVEMENT_FACTS &,std::string &);
    /* Reobserve before a consumer uses a study; expiration/stale controls refuse.
     * This does not turn a model winner into an executable plan. */
    bool Revalidate_costing(UINT64,const std::string &,std::string &);
    /* Host memory only: run one qualified synchronous entry; completion/status/control/parent CAS
     * gates atomic publication. Failure expires ticket and publishes nothing. */
    bool Run(UINT64,bool (*)(void *,void **,UINT32),void *,UINT64 &,std::string &);
    /* Abandon a prepared ticket. No partial result may be published afterward. */
    bool Cancel(UINT64,std::string &);
    /* Token-only freshness; copied facts remain readable after expiry. */
    bool Is_live(UINT64) const;
    /* Readers pin the complete immutable bundle, mapping and metadata together. */
    bool Acquire_reader(UINT64,UINT64 &,std::string &);
    /* Copy the complete published version tuple; this does not acquire a reader
     * lease and cannot authorize consumption after subsequent retirement. */
    bool Inspect_bundle(UINT64,VHO_SERVING_BUNDLE_FACTS &,std::string &);
    /* Copy optional H1d diagnostics from a live ticket or published bundle.
     * Keeps legacy fact/bundle layouts intact; does not acquire any lease. */
    bool Inspect_gpu(UINT64,bool,DSL_GPU_SERVING_RESULT &,std::string &);
    /* Exactly-once completion releases the reader's whole bundle lease. */
    bool Release_reader(UINT64,std::string &);
    /* Explicit retirement refuses outstanding readers or pending invocations. */
    bool Retire_bundle(UINT64,std::string &);
};
#endif
