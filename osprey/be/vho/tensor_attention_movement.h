/* Copyright (C) 2026 Open64 Project */
/* Detached MLA hierarchy/reuse model v1. No native handles or materialization.
 * All hardware traffic/cycles are explicit scenario estimates, not counters.
 * Design: doc/AI-COMPILER-MLA-MR2-MOVEMENT-COSTING-v1.0.md. */
#ifndef tensor_attention_movement_INCLUDED
#define tensor_attention_movement_INCLUDED
#include "tensor_kernel_lower.h"
struct VHO_MLA_CACHE_SCENARIO {
    std::string id;
    UINT64 weight, cache_hit_ppm, other_hit_ppm;
    VHO_MLA_CACHE_SCENARIO():weight(0),cache_hit_ppm(0),other_hit_ppm(0) {}
};
struct VHO_MLA_MOVEMENT_PROFILE {
    std::string id, architecture, provenance, objective;
    UINT64 hbm_rate,l2_rate,sector_bytes,request_amplification_ppm,shared_conflict_ppm;
    std::vector<VHO_MLA_CACHE_SCENARIO> scenarios;
    VHO_MLA_MOVEMENT_PROFILE():hbm_rate(0),l2_rate(0),sector_bytes(0),
        request_amplification_ppm(0),shared_conflict_ppm(0) {}
};
/* Normalized proof projection supplied by the owning LNO phase after live
 * validation. It grants no grouped physical schedule/application authority. */
struct VHO_MLA_MOVEMENT_CONTEXT {
    VHO_MLA_MOVEMENT_PROFILE profile;
    std::string proof_key;
    UINT64 latent_payload,rotary_payload;
    VHO_MLA_MOVEMENT_CONTEXT():latent_payload(0),rotary_payload(0) {}
};
struct VHO_MLA_TRAFFIC_ESTIMATE {
    std::string operand,direction,scenario;
    UINT64 requested,transactions,l2,hbm,shared;
    VHO_MLA_TRAFFIC_ESTIMATE():requested(0),transactions(0),l2(0),hbm(0),shared(0) {}
};
struct VHO_MLA_MOVEMENT_STAGE {
    UINT64 valid_tokens,valid_heads,registers,rounded_registers,rounded_shared,unused_slots;
    std::vector<VHO_MLA_TRAFFIC_ESTIMATE> traffic;
    VHO_MLA_MOVEMENT_STAGE():valid_tokens(0),valid_heads(0),registers(0),
        rounded_registers(0),rounded_shared(0),unused_slots(0) {}
};
struct VHO_MLA_MOVEMENT_DESCRIPTOR {
    UINT32 group,column;
    bool retained;
    VHO_MLA_MOVEMENT_DESCRIPTOR():group(1),column(0),retained(false) {}
};
struct VHO_MLA_MOVEMENT_ESTIMATE {
    bool modeled;
    std::string descriptor_key;
    UINT64 cache_payload,cache_requests,partial_allocation,partial_requests,score_storage,head_slots;
    std::vector<UINT64> scenario_cycles;
    VHO_MLA_MOVEMENT_ESTIMATE():modeled(false),cache_payload(0),cache_requests(0),
        partial_allocation(0),partial_requests(0),score_storage(0),head_slots(0) {}
};
/* Exact versioned registry. Only unchanged G=1 materializers are verified.
 * Callers cannot enable a family through JSON, flags or a profile string. */
extern const char *VHO_mla_movement_catalog();
extern const char *VHO_mla_movement_capabilities();
extern BOOL VHO_mla_movement_profile_valid(const VHO_MLA_MOVEMENT_PROFILE &,FILE *);
extern std::string VHO_mla_movement_profile_key(const VHO_MLA_MOVEMENT_PROFILE &);
#endif
