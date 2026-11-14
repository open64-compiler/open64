/* Copyright (C) 2026 Open64 Project */
/* VHO-owned Q4 cache-service cost study. Q3 freezes live host binding evidence;
 * exploration reads only owned records. No native IR, runtime callback or GPU
 * authority. Design: doc/AI-COMPILER-MLA-Q4-QUANTIZED-MOVEMENT-v1.0.md. */
#ifndef tensor_quantized_movement_INCLUDED
#define tensor_quantized_movement_INCLUDED
#include "tensor_serving.h"
#include "tensor_attention_movement.h"
struct VHO_Q4_SNAPSHOT {
    VHO_TENSOR_SERVING_SCHEMA schema;
    VHO_SERVING_FACTS facts;
    UINT64 maintenance_bytes,maintenance_ops;
    std::string key;
    /* Cache construction/update/return maintenance remains common to all plans. */
    VHO_Q4_SNAPSHOT():maintenance_bytes(0),maintenance_ops(0) {}
};
struct VHO_Q4_PROFILE {
    VHO_MLA_MOVEMENT_PROFILE hierarchy;
    UINT64 convert_lanes,unpack_lanes,page_cycles;
    /* Explicit per-SM conversion/unpack lanes per cycle and lookup cycles. */
    VHO_Q4_PROFILE():convert_lanes(0),unpack_lanes(0),page_cycles(0) {}
};
struct VHO_Q4_ESTIMATE {
    bool modeled;
    UINT64 packed,scales,page_requests,decoded_global,decoded_shared,conversions,unpacks;
    UINT64 maintenance,partial,unique_physical,logical_capacity,logical_valid,active_queries;
    std::vector<UINT64> scenario_cycles;
    std::vector<VHO_MLA_TRAFFIC_ESTIMATE> traffic;
    std::string key,explanation;
    /* Counts are cache-service estimates; never an executable plan certificate. */
    VHO_Q4_ESTIMATE():modeled(false),packed(0),scales(0),page_requests(0),decoded_global(0),
        decoded_shared(0),conversions(0),unpacks(0),maintenance(0),partial(0),unique_physical(0),
        logical_capacity(0),logical_valid(0),active_queries(0) {}
};
/* Complete snapshot identity, excluding its own key. Not a signature/lease. */
extern std::string VHO_q4_snapshot_key(const VHO_Q4_SNAPSHOT &);
/* Exact separate finite space; no archived MR2 score or materializer changes. */
extern const char *VHO_q4_catalog();
#endif
