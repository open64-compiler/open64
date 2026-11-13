/* Copyright (C) 2026 Open64 Project */
/* H3 production model/cache oracle over explicit synthetic H20 imports.
 * Independent fixed arithmetic, resource/queue counterexamples and budget
 * refusals; no native or GPU execution. Tests own every detached input.
 * Design: doc/AI-COMPILER-H3-MOVEMENT-COSTING-v1.0.md. */
#include "dsl_movement_cost.h"
#include "dsl_gpu_profile.h"
#include <stdlib.h>
#include <unistd.h>
#include <cmath>

/* Assertions are always active, including deliberately malformed inputs. */
static void H3_require(bool ok,const char *name) {if(!ok) {fprintf(stderr,"FAIL H3 %s\n",name);exit(1);}printf("PASS H3 %s\n",name);}
/* Source proofs use exact immutable storage/version/access and lifetime joins. */
static DSL_MOVEMENT_FACTS H3_facts(const DSL_GPU_PLAN_SNAPSHOT &gpu,uint64_t width=17) {
    DSL_MOVEMENT_CAPTURE c;c.binding.family="fixture";c.binding.owner="PU";c.binding.source_key="native-source";c.binding.proof_key="access-alias-lifetime";
    c.binding.device=gpu;c.binding.device_key=gpu.Cache_key();c.work.owner="row-owner";c.work.domain="row,feature";c.work.instances=4;c.work.width=width;c.work.active_last=width;
    c.work.axes.push_back("row");c.work.extents.push_back(4);c.work.axes.push_back("feature");c.work.extents.push_back(width);
    std::vector<uint64_t> shape;shape.push_back(4);shape.push_back(width);DSL_MOVEMENT_STORAGE s=DSL_movement_dense_storage("X","allocation-1","g0","operand",shape,4,true);c.storages.push_back(s);
    DSL_MOVEMENT_ACCESS a;a.id="read-X";a.storage=s.id;a.version=s.version;a.extent=s.bytes;a.read_bytes=s.bytes;a.counts_known=a.reuse_proved=true;a.reuse_scope="invocation";c.accesses.push_back(a);
    DSL_movement_source_lifetime(c,s.id,s.version,"invocation","existing-source-proof");DSL_MOVEMENT_FACTS facts;std::string why;H3_require(DSL_MOVEMENT_FACTS::Create(c,facts,why),"source-capture");return facts;
}
/* Supplement is an explicit synthetic model, never an architecture default. */
static DSL_H3_MODEL H3_model(const DSL_GPU_PLAN_SNAPSHOT &gpu) {
    DSL_H3_MODEL m;m.id="fixture-model";m.key="explicit-synthetic-supplement";m.binding=gpu.Cache_key();m.provenance="synthetic";m.objective="worst";
    m.transaction_bytes=32;m.queue_per_sm=4;m.warm_hit_ppm=500000;m.contention_ppm=2000000;m.spill_per_cta=DSL_MOVEMENT_BYTES(0);m.issue_per_sm=1e10;m.barrier_ns=10;m.shuffle_ns=1;m.fence_ns=2;m.rsqrt_ns=3;
    m.site_limit=128;m.request_limit=1024;m.memory_limit=67108864;m.deadline_us=60000000;return m;
}
/* One fixed recipe permits a handwritten physical-byte and ns oracle. */
static DSL_H3_PLAN H3_plan() {
    DSL_H3_PLAN p;p.id="fixed";p.capability="fixture-only";p.executable=true;p.live_bytes=272;
    DSL_H3_STAGE s;s.id="one";s.ctas=1;s.threads=32;s.registers=16;s.steps=4;s.read_requests=128;s.write_through_requests=32;s.reusable_reads=128;
    s.shared_requests=32;s.scalar_ops=s.issue_ops=64;s.add_chain=4;s.exps=2;s.divs=1;s.rsqrts=1;s.barriers=1;s.shuffles=2;s.fences=1;p.stages.push_back(s);return p;
}
/* The actual pure evaluator must succeed before comparing any numerical field. */
static DSL_H3_ESTIMATE H3_evaluate(const DSL_MOVEMENT_FACTS &f,const DSL_H3_MODEL &m,const DSL_H3_PLAN &p) {
    DSL_H3_ESTIMATE out;std::string why;bool ok=VHO_h3_estimate(f,m,p,out,why);if(!ok) fprintf(stderr,"cost refusal %s\n",why.c_str());H3_require(ok && out.complete,"complete-estimate");return out;
}
/* Authenticate the real H1b importer, then exercise production H3 services. */
int main(int argc,char **argv) {
    if(argc<5) return 2;DSL_HARDWARE_TARGET_SELECTION target;DSL_GPU_DEVICE_CONTEXT_OWNER owner;
    H3_require(DSL_hardware_target_resolve(NULL,argv[1],&target,stderr) && DSL_gpu_profile_resolve(target,argv[2],argv[3],"fixture-toolchain-v1",&owner,stderr),"device-import");
    DSL_GPU_PLAN_SNAPSHOT gpu=DSL_GPU_PLAN_SNAPSHOT::Capture(owner.Context());std::string why;
    if(std::string(argv[4])=="binding") {printf("BINDING %s\n",gpu.Cache_key().c_str());return 0;}
    if(std::string(argv[4])=="import") {
        if(argc!=8) return 2;DSL_H3_SEARCH search;bool ok=VHO_h3_import(argv[5],argv[6],gpu,search,why);printf("IMPORT %u %s\n",unsigned(ok),why.c_str());return ok==(atoi(argv[7])!=0)?0:1;
    }
    DSL_MOVEMENT_FACTS facts=H3_facts(gpu);DSL_H3_MODEL m=H3_model(gpu);DSL_H3_PLAN p=H3_plan();DSL_H3_ESTIMATE cost=H3_evaluate(facts,m,p);
    H3_require(cost.scenarios.size()==4 && cost.scenarios[0].requests==160 && cost.scenarios[0].transactions==160 && cost.scenarios[0].hbm==160 && cost.scenarios[1].hbm==96,"independent-traffic-oracle");
    H3_require(std::fabs(cost.scenarios[0].upper_ns-72.2)<1e-8 && std::fabs(cost.scenarios[1].upper_ns-65.8)<1e-8 && cost.rank_ns==105,"independent-ns-oracle");
    for(size_t i=0;i<cost.scenarios.size();++i) {const DSL_H3_SCENARIO &s=cost.scenarios[i];H3_require(std::fabs(s.transfer_ns+s.compute_issue_ns+s.local_ns+s.special_ns+s.collective_ns+s.launch_ns-s.upper_ns)<1e-8,"components-reconcile");}
    H3_require(!facts.Capture().ledger.hbm.known && !facts.Capture().ledger.spills.known,"source-physical-unknowns-preserved");
    H3_require(cost.provenance=="estimated-from-synthetic-inputs","mixed-input-provenance-conservative");
    DSL_H3_MODEL spilled=m;spilled.spill_per_cta=DSL_MOVEMENT_BYTES(32);DSL_H3_ESTIMATE paid=H3_evaluate(facts,spilled,p);
    H3_require(paid.scenarios[0].requests==192 && paid.scenarios[0].transactions==192 && paid.scenarios[0].spill_requests==32 && paid.rank_ns>cost.rank_ns,"explicit-spill-scenario-paid");
    DSL_H3_PLAN partitions=p;partitions.stages.push_back(p.stages[0]);partitions.stages[1].id="same-launch-tail";partitions.stages[1].launch=false;
    DSL_H3_ESTIMATE joined=H3_evaluate(facts,m,partitions);H3_require(std::fabs(joined.scenarios[0].upper_ns-141.9)<1e-8 && joined.scenarios[0].launch_ns==2.5,"partition-launch-not-double-counted");
    DSL_H3_MODEL changed=m;changed.shuffle_ns=2;DSL_H3_ESTIMATE rebound=H3_evaluate(facts,changed,p);
    H3_require(rebound.key!=cost.key && rebound.binding!=cost.binding,"complete-model-key-not-ID-only");
    DSL_MOVEMENT_FACTS big=H3_facts(gpu,100000);DSL_H3_ESTIMATE capacity=H3_evaluate(big,m,p);H3_require(capacity.scenarios[1].hbm==160,"capacity-exceeding-removes-warm-credit");
    DSL_H3_PLAN parallel=p;parallel.stages[0].ctas=16;parallel.stages[0].read_requests=1048576;parallel.stages[0].reusable_reads=0;
    DSL_H3_ESTIMATE a=H3_evaluate(facts,m,parallel);parallel.stages[0].registers=128;DSL_H3_ESTIMATE b=H3_evaluate(facts,m,parallel);
    H3_require(a.scenarios[0].transfer_ns==b.scenarios[0].transfer_ns,"device-bandwidth-not-multiplied-by-residency");
    DSL_H3_PLAN shallow=p;shallow.stages[0].ctas=16;shallow.stages[0].steps=100;shallow.stages[0].add_chain=1000000;shallow.stages[0].scalar_ops=1000000;shallow.stages[0].issue_ops=1000000;DSL_H3_PLAN deep=shallow;deep.stages[0].depth=3;deep.stages[0].shared_allocation=32768;
    DSL_H3_MODEL ample=m;ample.queue_per_sm=128;
    DSL_H3_ESTIMATE c=H3_evaluate(facts,ample,shallow),d=H3_evaluate(facts,ample,deep);H3_require(d.rank_ns>c.rank_ns,"larger-stage-depth-can-lose-occupancy");
    DSL_H3_MODEL small_queue=m;small_queue.queue_per_sm=1;DSL_H3_ESTIMATE q=H3_evaluate(facts,small_queue,deep);H3_require(q.scenarios[0].queue_rounds>d.scenarios[0].queue_rounds,"finite-outstanding-work");
    for(unsigned i=0;i<6;++i) {
        DSL_H3_PLAN invalid=p;DSL_H3_MODEL bad=m;
        if(i==0) invalid.stages[0].read_requests=~uint64_t(0);
        if(i==1) invalid.stages[0].registers=0;
        if(i==2) invalid.live_bytes=~uint64_t(0);
        if(i==3) bad.binding+="wrong";
        if(i==4) bad.issue_per_sm=0;
        if(i==5) invalid.stages[0].depth=4;
        DSL_H3_ESTIMATE out=cost;H3_require(!VHO_h3_estimate(facts,bad,invalid,out,why) && !out.complete,"unknown-overflow-binding-refusal");
    }
    DSL_MOVEMENT_CAPTURE unknown=facts.Capture();unknown.accesses[0].reuse_proved=false;DSL_MOVEMENT_FACTS no_reuse;H3_require(DSL_MOVEMENT_FACTS::Create(unknown,no_reuse,why),"unproved-source-retained");
    DSL_H3_ESTIMATE refused;H3_require(!VHO_h3_estimate(no_reuse,m,p,refused,why),"no-reuse-credit-without-source-proof");
    p.stages[0].reusable_reads=0;H3_evaluate(no_reuse,m,p);p=H3_plan();p.executable=false;H3_require(!H3_evaluate(facts,m,p).executable,"analysis-only-does-not-become-executable");p=H3_plan();
    DSL_H3_SEARCH search;m.site_limit=2;m.request_limit=3;H3_require(search.Configure(m,gpu,why) && !search.Configure(m,gpu,why),"request-cannot-reset-budgets");
    DSL_H3_ESTIMATE first,hit;H3_require(search.Evaluate(facts,p,first,why) && search.Evaluate(facts,p,hit,why) && hit.cache_hit && first.key==hit.key,"shared-estimate-cache");
    p.id="second";H3_require(search.Evaluate(facts,p,hit,why),"second-unique");p.id="third";H3_require(!search.Evaluate(facts,p,hit,why) && why=="h3_site_unique_budget","exact-site-budget");
    H3_require(search.Evaluate(big,p,hit,why),"new-site-shares-request-budget");p.id="fourth";H3_require(!search.Evaluate(big,p,hit,why) && why=="h3_request_unique_budget","exact-request-budget");search.Print(stdout);
    // Repeated refusal cannot advance the admitted proposal census above its
    // finite cap, even when several strategies keep offering cached entries.
    for(unsigned i=0;i<64;++i) search.Evaluate(big,p,hit,why);
    H3_require(why=="h3_request_proposal_budget","repeated-refusals-saturate-proposal-cap");search.Print(stdout);
    DSL_H3_SEARCH memory;m.memory_limit=4096;H3_require(memory.Configure(m,gpu,why) && !memory.Evaluate(facts,p,hit,why) && why=="h3_detached_memory_budget","memory-budget");
    DSL_H3_SEARCH deadline;m.memory_limit=67108864;m.deadline_us=1;H3_require(deadline.Configure(m,gpu,why),"deadline-configuration");usleep(2000);H3_require(!deadline.Evaluate(facts,p,hit,why) && why=="h3_request_deadline","deadline-fallback");
    owner.Reset();H3_require(facts.Available() && cost.complete,"detached-owner-lifetime");VHO_h3_print(stdout,facts,H3_plan(),cost);return 0;
}
