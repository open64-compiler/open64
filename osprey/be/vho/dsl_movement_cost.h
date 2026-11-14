/* Copyright (C) 2026 Open64 Project */
/* VHO-owned H3 physical-unit costing shared by phase-owned family adapters.
 * Detached H2 facts and explicit work recipes only; no native handles, device
 * probing, materialization or runtime authority. Legacy cycle schemas are not
 * reinterpreted. Design: doc/AI-COMPILER-H3-MOVEMENT-COSTING-v1.0.md. */
#ifndef dsl_movement_cost_INCLUDED
#define dsl_movement_cost_INCLUDED
#include "../com/dsl_movement_facts.h"
#include <map>

struct DSL_H3_MODEL {
    std::string id,binding,provenance,objective,key;
    uint64_t transaction_bytes,queue_per_sm,warm_hit_ppm,contention_ppm;
    double issue_per_sm,barrier_ns,shuffle_ns,fence_ns,rsqrt_ns;
    DSL_MOVEMENT_BYTES spill_per_cta;
    uint64_t site_limit,request_limit,memory_limit,deadline_us;
    /* An empty supplement grants neither performance nor budget permission. */
    inline DSL_H3_MODEL() : transaction_bytes(0),queue_per_sm(0),warm_hit_ppm(0),contention_ppm(0),
        issue_per_sm(0),barrier_ns(0),shuffle_ns(0),fence_ns(0),rsqrt_ns(0),site_limit(0),request_limit(0),memory_limit(0),deadline_us(0) {}
};
struct DSL_H3_STAGE {
    std::string id;
    uint64_t ctas,threads,registers,shared_allocation,steps,depth;
    uint64_t read_requests,write_through_requests,reusable_reads,shared_requests,scalar_ops,issue_ops;
    uint64_t add_chain,mul_chain,exps,divs,shuffles,barriers,fences,dequant,rsqrts;
    bool asynchronous,launch;
    /* Unknown work is never silently supplied by a hardware rate. Owners must
     * explicitly complete the operation-specific recipe before evaluation. */
    inline DSL_H3_STAGE() : ctas(0),threads(0),registers(0),shared_allocation(0),steps(1),depth(1),
        read_requests(0),write_through_requests(0),reusable_reads(0),shared_requests(0),scalar_ops(0),issue_ops(0),add_chain(0),mul_chain(0),
        exps(0),divs(0),shuffles(0),barriers(0),fences(0),dequant(0),rsqrts(0),asynchronous(false),launch(true) {}
};
struct DSL_H3_PLAN {
    std::string id,capability;
    bool executable;
    uint64_t live_bytes;
    std::vector<DSL_H3_STAGE> stages;
    /* Modeled plans cannot become executable through a JSON capability flag. */
    inline DSL_H3_PLAN() : executable(false),live_bytes(~uint64_t(0)) {}
};
struct DSL_H3_SCENARIO {
    std::string name;
    double lower_ns,upper_ns,transfer_ns,compute_issue_ns,local_ns,special_ns,collective_ns,launch_ns;
    uint64_t requests,transactions,l2,hbm,shared,waves,queue_rounds,spill_requests;
    /* Defined zero accumulators require all component inputs before complete. */
    inline DSL_H3_SCENARIO() : lower_ns(0),upper_ns(0),transfer_ns(0),compute_issue_ns(0),local_ns(0),special_ns(0),
        collective_ns(0),launch_ns(0),requests(0),transactions(0),l2(0),hbm(0),shared(0),waves(0),queue_rounds(0),spill_requests(0) {}
};
struct DSL_H3_ESTIMATE {
    std::string key,binding,reason,provenance;
    bool complete,executable,cache_hit;
    uint64_t rank_ns;
    std::vector<DSL_H3_SCENARIO> scenarios;
    /* Incomplete estimates cannot supply a ranking or an application grant. */
    inline DSL_H3_ESTIMATE() : complete(false),executable(false),cache_hit(false),rank_ns(0) {}
};
/* Validate complete supplement ranges and exact imported device binding. No
 * instruction rate is derived from a device spelling or reference cycles. */
extern bool VHO_h3_model_valid(const DSL_H3_MODEL &,const DSL_GPU_PLAN_SNAPSHOT &,std::string &);
/* Full framed work identity includes every count, capability and assumption. */
extern std::string VHO_h3_plan_key(const DSL_H3_PLAN &);
/* Pure physical cost engine; false clears output and reports missing/overflow
 * evidence. Source facts remain unchanged and all physical bytes are estimates. */
extern bool VHO_h3_estimate(const DSL_MOVEMENT_FACTS &,const DSL_H3_MODEL &,const DSL_H3_PLAN &,DSL_H3_ESTIMATE &,std::string &);
/* Additive versioned diagnostics never relabel old cycle totals as ns. */
extern void VHO_h3_print(FILE *,const DSL_MOVEMENT_FACTS &,const DSL_H3_PLAN &,const DSL_H3_ESTIMATE &);

class DSL_H3_SEARCH {
    DSL_H3_MODEL _model;
    struct DSL_H3_SITE {
        DSL_MOVEMENT_FACTS facts;
        uint64_t unique;
        /* One fully owned source record per site; no compiler lifetime borrow. */
        inline DSL_H3_SITE() : unique(0) {}
    };
    std::map<std::string,DSL_H3_SITE> _sites;
    std::map<std::string,DSL_H3_ESTIMATE> _cache;
    uint64_t _unique,_proposals,_charged,_start_us;
    std::string _stop;
public:
    /* One request lifetime spans active PUs, with no per-strategy reset. */
    inline DSL_H3_SEARCH() : _unique(0),_proposals(0),_charged(0),_start_us(0) {}
    /* Configure once before source processing. Invalid input preserves owner. */
    bool Configure(const DSL_H3_MODEL &,const DSL_GPU_PLAN_SNAPSHOT &,std::string &);
    /* Shared cache charges new estimates once; each attempted lookup consumes a
     * proposal. Atomic estimates are not preempted; deadline checked at both
     * boundaries. No native/file/device operation is invoked from evaluation. */
    bool Evaluate(const DSL_MOVEMENT_FACTS &,const DSL_H3_PLAN &,DSL_H3_ESTIMATE &,std::string &);
    /* Borrow complete owned supplement for selected preflight re-estimation. */
    inline const DSL_H3_MODEL &Model() const { return _model; }
    /* Bound an owning estimator's transient work before expansion. Eight MiB
     * is reserved for the existing finite catalog/work graph; no allocation or
     * native mutation occurs here. Zero signals deadline/storage refusal. */
    uint64_t Working_items(uint64_t bytes_per_item);
    /* Reports request-wide accounting, never GPU elapsed time. */
    void Print(FILE *) const;
};
/* Driver-only pinned file import, before native input. RapidJSON is the existing
 * backend parser. Missing fields/binding/digest refuse without enabling search. */
extern bool VHO_h3_import(const char *,const char *,const DSL_GPU_PLAN_SNAPSHOT &,DSL_H3_SEARCH &,std::string &);
#endif
