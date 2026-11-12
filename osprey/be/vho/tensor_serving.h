/* Copyright (C) 2026 Open64 Project */
/* Native synchronous host serving adapter for the Q1 disjoint-storage ABI.
 * Runtime leases are distinct from compiler mutation epochs. No IR mutation,
 * search, JIT, device pointer dereference or asynchronous completion inference.
 * Design: doc/AI-COMPILER-MLA-Q3-SERVING-v1.0.md. */
#ifndef tensor_serving_INCLUDED
#define tensor_serving_INCLUDED
#include "tensor_serving_schema.h"
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
class VHO_TENSOR_SERVING {
    struct IMPL;
    IMPL *_impl;
    VHO_TENSOR_SERVING(const VHO_TENSOR_SERVING &);
    VHO_TENSOR_SERVING &operator=(const VHO_TENSOR_SERVING &);
public:
    /* The owner supplies a qualified Q1 program and legal allocator P set.
     * Refusal leaves no usable adapter. Only host-synchronous memory is admitted. */
    VHO_TENSOR_SERVING(const VHO_TENSOR_PROGRAM &,const std::vector<UINT64> &,std::string &);
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
    /* Reject pinned or published memory; re-registration issues a fresh generation. */
    bool Retire(const VHO_SERVING_HANDLE &,std::string &);
    /* Atomically introduce an already-ready incoming host bundle. Source joins,
     * actual controls/pages/scales and complete ABI views must validate first. */
    bool Import(const std::vector<VHO_SERVING_VIEW> &,const VHO_SERVING_VIEW &,UINT64 &,std::string &);
    /* Acquire shared input and exclusive output/scratch/status leases. Facts are
     * rebuilt from actual host controls. A stale parent/refusal changes no bytes. */
    bool Begin(const std::vector<VHO_SERVING_VIEW> &,const VHO_SERVING_VIEW &,UINT64,VHO_SERVING_FACTS &,std::string &);
    /* Run one qualified synchronous entry; completion/status/control/parent CAS
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
    /* Exactly-once completion releases the reader's whole bundle lease. */
    bool Release_reader(UINT64,std::string &);
    /* Explicit retirement refuses outstanding readers or pending invocations. */
    bool Retire_bundle(UINT64,std::string &);
};
#endif
