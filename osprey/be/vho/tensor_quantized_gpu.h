/* Copyright (C) 2026 Open64 Project */
/* VHO-owned Q6 selected quantized MLA placement and transport admission.
 * Detached records only: no WN/ST/TY mutation, serving observation or device
 * authority. Contract: doc/AI-COMPILER-MLA-Q6-GPU-STAGING-v1.0.md. */
#ifndef tensor_quantized_gpu_INCLUDED
#define tensor_quantized_gpu_INCLUDED
#include "tensor_quantized_plan.h"
#include "../com/dsl_movement_facts.h"

enum VHO_Q6_DELIVERY { VHO_Q6_SYNCHRONOUS=1, VHO_Q6_BULK_TMA=2 };
struct VHO_Q6_CONTROL {
    UINT32 delivery,arch,ptx,toolkit,max_threads;
    UINT64 shared_limit;
    /* Explicit compilation envelope, never inferred from host alignment. */
    VHO_Q6_CONTROL():delivery(0),arch(0),ptx(0),toolkit(0),max_threads(0),shared_limit(0) {}
};
struct VHO_Q6_MAPPING {
    UINT64 batch,heads,queries,keys,latent,rotary,width,group,grid,threads,tiles;
    UINT64 raw_row[4],raw_offset[4],raw_stride,decoded_offset,decoded_bytes;
    UINT64 barrier_offset,shared_bytes,slots;
    VHO_Q6_CONTROL control;
    std::string selected_key,plan_key,binding_key,key,explanation;
    /* A compiled layout is not a launch lease or a measured cost certificate. */
    VHO_Q6_MAPPING():batch(0),heads(0),queries(0),keys(0),latent(0),rotary(0),width(0),
        group(0),grid(0),threads(0),tiles(0),raw_stride(0),decoded_offset(0),decoded_bytes(0),
        barrier_offset(0),shared_bytes(0),slots(0) {
        for(unsigned i=0;i<4;++i) raw_row[i]=raw_offset[i]=0;
    }
};
struct VHO_Q6_COPY {
    UINT64 source,destination,bytes;
    bool bulk;
    /* Offsets are relative to disjoint source and destination allocations. */
    VHO_Q6_COPY(UINT64 s=0,UINT64 d=0,UINT64 n=0,bool b=false):source(s),destination(d),bytes(n),bulk(b) {}
};
struct VHO_Q6_PHYSICAL_RUN {
    UINT64 first_slot,tokens;
    /* One common physical token sequence for all four payload/scale streams.
     * Its correspondence to actual index SSA is a device-owner obligation. */
    VHO_Q6_PHYSICAL_RUN(UINT64 s=0,UINT64 n=0):first_slot(s),tokens(n) {}
};
struct VHO_Q6_TRANSFER {
    UINT32 stream;
    VHO_Q6_COPY copy;
    /* No stream independently chooses a different page or scale generation. */
    VHO_Q6_TRANSFER(UINT32 s,const VHO_Q6_COPY &c):stream(s),copy(c) {}
};
enum VHO_Q6_ACTION_KIND {
    VHO_Q6_ACQUIRE=1,VHO_Q6_ISSUE,VHO_Q6_WAIT,VHO_Q6_DEQUANTIZE,
    VHO_Q6_PUBLISH,VHO_Q6_CONSUME,VHO_Q6_RELEASE
};
struct VHO_Q6_ACTION {
    UINT32 kind,pass,slot;
    UINT64 tile;
    /* WAIT joins all payload/scale transfers, including synchronous fragments. */
    VHO_Q6_ACTION(UINT32 k=0,UINT32 p=0,UINT32 s=0,UINT64 t=0):kind(k),pass(p),slot(s),tile(t) {}
};
/* Qualify an already selected strict Q5 plan against exact native/source/Q3
 * identities. Unsupported/baseline plans fail without modifying output. This
 * is a compiler placement proposal; live device leases are still required. */
extern bool VHO_q6_mapping(const VHO_TENSOR_PROGRAM &,const VHO_Q5_PLAN &,
    const std::string &selected_key,const VHO_Q6_CONTROL &,VHO_Q6_MAPPING &,std::string &);
/* Full detached layout identity, excluding its own key/explanatory prose. */
extern std::string VHO_q6_mapping_key(const VHO_Q6_MAPPING &);
/* Partition one actual contiguous run, never crossing a page discontinuity.
 * Base residues come from device-address checks, not Q3 host alignment. Bulk
 * requires matching 16-byte alignment and exact in-bounds multiples of 16.
 * Empty/overflowing/out-of-range inputs fail without changing output. */
extern bool VHO_q6_partition(UINT64 source,UINT64 destination,UINT64 bytes,
    UINT64 source_size,UINT64 destination_size,UINT32 source_base_mod16,
    UINT32 destination_base_mod16,bool tma,std::vector<VHO_Q6_COPY> &,UINT64 &transaction_bytes,std::string &);
/* Expand a selected tile's common physical runs into four exact raw streams.
 * Live device sizes/residues must be revalidated by the eventual device owner.
 * No host pointer is inspected. Gaps/tails/overflow never become bulk overreads;
 * expected transaction bytes sum only async fragments across all four streams. */
extern bool VHO_q6_transfers(const VHO_Q6_MAPPING &,UINT64 tile,UINT32 slot,
    const std::vector<VHO_Q6_PHYSICAL_RUN> &,const UINT64 source_sizes[4],
    const UINT32 source_base_mod16[4],std::vector<VHO_Q6_TRANSFER> &,
    UINT64 &transaction_bytes,std::string &);
/* Materialize a bounded detached action trace only after selection. Three
 * ordered passes; two raw slots overlap next-tile transfer with current work.
 * The single decoded tile is released only after every head finishes. */
extern bool VHO_q6_pipeline(const VHO_Q6_MAPPING &,std::vector<VHO_Q6_ACTION> &,std::string &);
/* Independent state-machine validation rejects premature reads/overwrite,
 * missing publication/drain and cross-pass reordering; no native mutation. */
extern bool VHO_q6_verify_pipeline(const VHO_Q6_MAPPING &,const std::vector<VHO_Q6_ACTION> &,std::string &);
/* H2 projects an already qualified selected mapping/action trace. Complete
 * source/Q3/profile bindings are mandatory; representative generation events
 * supplement the full verified trace identity. No launch/search authority. */
extern bool VHO_q6_capture_movement(const VHO_Q6_MAPPING &,const std::vector<VHO_Q6_ACTION> &,
    const DSL_MOVEMENT_BINDING &,DSL_MOVEMENT_FACTS &,std::string &);
#endif
