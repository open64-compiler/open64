/* Copyright (C) 2026 Open64 Project */
/* VHO-owned H3 detached physical model and request-wide bounded estimate cache.
 * Reuses H2 source ownership and H1 capacity/rate evidence. No compiler/native
 * allocation, callback, file/device observation or transformation is accepted.
 * Design: doc/AI-COMPILER-H3-MOVEMENT-COSTING-v1.0.md. */
#include "dsl_movement_cost.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <sstream>
#include <stdexcept>
#include <time.h>

namespace {
/* Model/refusal conditions remain release checks, not assertion-only guards. */
void VHO_h3_need(bool ok,const char *why) { if(!ok) throw std::runtime_error(why); }
/* Preserve the established unknown sentinel and refuse wrapped byte/work sums. */
uint64_t VHO_h3_add(uint64_t a,uint64_t b) {uint64_t v;VHO_h3_need(DSL_movement_add(a,b,v),"h3_add_overflow");return v;}
/* Checked multiplication is also used for waves and traffic amplification. */
uint64_t VHO_h3_mul(uint64_t a,uint64_t b) {uint64_t v;VHO_h3_need(DSL_movement_mul(a,b,v),"h3_mul_overflow");return v;}
/* Positive ceiling avoids addition overflow in tail and queue arithmetic. */
uint64_t VHO_h3_ceil(uint64_t a,uint64_t b) {VHO_h3_need(b!=0,"h3_zero_divisor");return a/b+(a%b!=0);}
/* Only finite, nonnegative physical costs can enter an incumbent comparison. */
bool VHO_h3_finite(double v) {return v>=0 && v<=std::numeric_limits<double>::max();}
/* Length framing prevents a plan spelling from aliasing a source/model key. */
void VHO_h3_field(std::ostringstream &out,const std::string &value) {out<<value.size()<<':'<<value<<';';}
/* Include the complete supplement, not merely its human-readable ID. */
std::string VHO_h3_model_key(const DSL_H3_MODEL &m) {
    std::ostringstream out;out.precision(17);out<<"h3-model-v1;";
    const std::string fields[]={m.id,m.binding,m.provenance,m.objective,m.key};
    for(unsigned i=0;i<5;++i) VHO_h3_field(out,fields[i]);
    out<<m.transaction_bytes<<';'<<m.queue_per_sm<<';'<<m.warm_hit_ppm<<';'<<m.contention_ppm<<';'
        <<m.issue_per_sm<<';'<<m.barrier_ns<<';'<<m.shuffle_ns<<';'<<m.fence_ns<<';'
        <<m.rsqrt_ns<<';'<<m.spill_per_cta.known<<';'<<m.spill_per_cta.value<<';'<<m.site_limit<<';'<<m.request_limit<<';'<<m.memory_limit<<';'<<m.deadline_us;
    return out.str();
}
/* Require a known physical interval only for work actually present. Missing
 * zero-work rates grant no statement about spill-free or hardware execution. */
double VHO_h3_quantity(const DSL_GPU_PLAN_SNAPSHOT &gpu,int field,bool upper,bool rate,uint64_t count) {
    if(!count) return 0;
    const DSL_GPU_EXECUTION_FACT *fact=gpu.Execution(field);
    VHO_h3_need(fact && fact->known && fact->lower>0 && VHO_h3_finite(fact->upper),"h3_unknown_physical_input");
    return rate?(upper?fact->lower:fact->upper):(upper?fact->upper:fact->lower);
}
/* Imported conservative occupancy uses one physical throughput per SM. More
 * residents hide latency but never multiply the SM or device service rates. */
uint64_t VHO_h3_residents(const DSL_GPU_PLAN_SNAPSHOT &gpu,const DSL_H3_STAGE &s,uint64_t &sms) {
    uint64_t threads,blocks,rf,shared,quantum,rf_demand;
    VHO_h3_need(gpu.Fits_CTA(s.threads,s.registers,s.shared_allocation) &&
        gpu.Resource_limit(DSL_GPU_VISIBLE_SMS,&sms) && sms &&
        gpu.Resource_limit(DSL_GPU_MAX_THREADS_PER_SM,&threads) &&
        gpu.Resource_limit(DSL_GPU_MAX_BLOCKS_PER_SM,&blocks) &&
        gpu.Resource_limit(DSL_GPU_REGISTERS_PER_SM,&rf) &&
        gpu.Register_reservation(s.threads,s.registers,&rf_demand),"h3_unknown_or_exceeded_CTA_resources");
    uint64_t resident=std::min(blocks,std::min(threads/s.threads,rf/rf_demand));
    if(s.shared_allocation) {
        VHO_h3_need(gpu.Resource_limit(DSL_GPU_SHARED_BYTES_PER_SM,&shared) &&
            gpu.Resource_limit(DSL_GPU_SHARED_QUANTUM,&quantum) && quantum,"h3_unknown_shared_capacity");
        uint64_t rounded=VHO_h3_mul(VHO_h3_ceil(s.shared_allocation,quantum),quantum);
        resident=std::min(resident,shared/rounded);
    }
    VHO_h3_need(resident!=0,"h3_no_resident_CTA");return resident;
}
/* Host monotonic time bounds compiler work only; never measures a device or
 * candidate runtime. Search's public API admits no external callback. */
uint64_t VHO_h3_clock() {
    struct timespec now;VHO_h3_need(clock_gettime(CLOCK_MONOTONIC,&now)==0 && now.tv_sec>=0,"h3_clock_failed");
    return VHO_h3_add(VHO_h3_mul(uint64_t(now.tv_sec),1000000),uint64_t(now.tv_nsec/1000));
}
/* H2 proof covers possible read reuse, not its realization. Cache scenarios
 * may use only proven immutable, known-layout accesses with owned lifetimes. */
uint64_t VHO_h3_reuse_footprint(const DSL_MOVEMENT_CAPTURE &c) {
    uint64_t bytes=0;
    for(size_t i=0;i<c.storages.size();++i) {
        const DSL_MOVEMENT_STORAGE &s=c.storages[i];bool proof=false,life=false;
        for(size_t j=0;j<c.accesses.size();++j) if(c.accesses[j].storage==s.id && c.accesses[j].reuse_proved) proof=true;
        for(size_t j=0;j<c.lifetimes.size();++j) if(c.lifetimes[j].storage==s.id && c.lifetimes[j].version==s.version) life=true;
        if(proof && life) bytes=VHO_h3_add(bytes,s.bytes);
    }
    return bytes;
}
/* Add one whole-stage interval. Engine service and dependency latency are
 * separate constraints; max joins them once. Compute/issue share a bottleneck
 * maximum. Other engines serialize conservatively: no speculative overlap. */
void VHO_h3_stage(const DSL_GPU_PLAN_SNAPSHOT &gpu,const DSL_H3_MODEL &m,const DSL_H3_STAGE &s,
    unsigned scenario,uint64_t footprint,bool upper,DSL_H3_SCENARIO &r) {
    VHO_h3_need(s.ctas && s.threads && s.depth && s.depth<=3 && s.steps && s.reusable_reads<=s.read_requests,"h3_work_shape");
    VHO_h3_need(!s.reusable_reads || footprint,"h3_reuse_without_H2_proof");
    uint64_t sms=0,resident=VHO_h3_residents(gpu,s,sms),capacity=VHO_h3_mul(sms,resident);
    uint64_t waves=VHO_h3_ceil(s.ctas,capacity),active_sms=std::min(sms,s.ctas),per_sm=VHO_h3_ceil(s.ctas,active_sms);
    uint64_t spill_requests=VHO_h3_mul(s.ctas,m.spill_per_cta.value);
    uint64_t requests=VHO_h3_mul(s.ctas,VHO_h3_add(VHO_h3_add(s.read_requests,s.write_through_requests),m.spill_per_cta.value));
    uint64_t read_tx=VHO_h3_mul(VHO_h3_ceil(s.read_requests,m.transaction_bytes),m.transaction_bytes);
    uint64_t write_tx=VHO_h3_mul(VHO_h3_ceil(VHO_h3_add(s.write_through_requests,m.spill_per_cta.value),m.transaction_bytes),m.transaction_bytes);
    uint64_t transactions=VHO_h3_mul(s.ctas,VHO_h3_add(read_tx,write_tx));
    uint64_t hits=0,l2_bytes=0;
    if(scenario==1 && s.reusable_reads) {
        VHO_h3_need(gpu.Resource_limit(DSL_GPU_L2_BYTES,&l2_bytes),"h3_unknown_L2_capacity");
        if(footprint<=l2_bytes) hits=VHO_h3_mul(s.reusable_reads,m.warm_hit_ppm)/1000000;
    }
    uint64_t hbm=VHO_h3_mul(s.ctas,VHO_h3_add(read_tx-hits,write_tx));
    uint64_t shared=VHO_h3_mul(s.ctas,s.shared_requests),scalar=VHO_h3_mul(s.ctas,s.scalar_ops),issues=VHO_h3_mul(s.ctas,s.issue_ops);
    double slowdown=scenario==3?double(m.contention_ppm)/1000000:1;
    double service=0;
    if(hbm) service+=double(hbm)/VHO_h3_quantity(gpu,DSL_GPU_HBM_MIXED_RATE,upper,true,hbm)*1e9*slowdown;
    if(transactions) service+=double(transactions)/VHO_h3_quantity(gpu,DSL_GPU_L2_RATE,upper,true,transactions)*1e9*slowdown;
    uint64_t batch_slots=std::min(VHO_h3_mul(active_sms,m.queue_per_sm),VHO_h3_mul(std::min(s.ctas,capacity),s.depth));
    uint64_t queues=requests?VHO_h3_ceil(VHO_h3_mul(s.ctas,s.steps),batch_slots):0;
    uint64_t rounds=requests?std::max(queues,VHO_h3_mul(waves,VHO_h3_ceil(s.steps,s.depth))):0;
    double readiness=double(rounds)*VHO_h3_quantity(gpu,DSL_GPU_MEMORY_STARTUP_NS,upper,false,rounds);
    double transfer=std::max(service,readiness);
    double compute=scalar?double(VHO_h3_mul(per_sm,s.scalar_ops))/VHO_h3_quantity(gpu,DSL_GPU_SIMT_RATE_PER_SM,upper,true,scalar)*1e9:0;
    double issue=issues?double(VHO_h3_mul(per_sm,s.issue_ops))/m.issue_per_sm*1e9:0;
    double chain=double(VHO_h3_mul(s.add_chain,waves))*VHO_h3_quantity(gpu,DSL_GPU_ADD_DEPENDENT_NS,upper,false,s.add_chain)+
        double(VHO_h3_mul(s.mul_chain,waves))*VHO_h3_quantity(gpu,DSL_GPU_MUL_DEPENDENT_NS,upper,false,s.mul_chain);
    double compute_issue=std::max(std::max(compute,issue),chain);
    double local=shared?double(VHO_h3_mul(per_sm,s.shared_requests))/VHO_h3_quantity(gpu,DSL_GPU_SHARED_RATE_PER_SM,upper,true,shared)*1e9:0;
    double special=double(VHO_h3_mul(per_sm,s.exps))*VHO_h3_quantity(gpu,DSL_GPU_EXP_DEPENDENT_NS,upper,false,s.exps)+
        double(VHO_h3_mul(per_sm,s.divs))*VHO_h3_quantity(gpu,DSL_GPU_DIV_DEPENDENT_NS,upper,false,s.divs);
    special+=double(VHO_h3_mul(per_sm,s.rsqrts))*m.rsqrt_ns;
    if(s.dequant) special+=double(VHO_h3_mul(per_sm,s.dequant))/VHO_h3_quantity(gpu,DSL_GPU_DEQUANT_RATE_PER_SM,upper,true,s.dequant)*1e9;
    double collective=double(per_sm)*(double(s.barriers)*m.barrier_ns+double(s.shuffles)*m.shuffle_ns+double(s.fences)*m.fence_ns);
    double launch=s.launch?VHO_h3_quantity(gpu,DSL_GPU_LAUNCH_NS,upper,false,1):0;
    if(s.asynchronous) launch+=double(waves)*VHO_h3_quantity(gpu,DSL_GPU_ASYNC_STARTUP_NS,upper,false,1);
    double total=transfer+compute_issue+local+special+collective+launch;
    VHO_h3_need(VHO_h3_finite(total) && total<9e18,"h3_nonfinite_cost");
    if(upper) {
        r.upper_ns+=total;r.transfer_ns+=transfer;r.compute_issue_ns+=compute_issue;r.local_ns+=local;r.special_ns+=special;r.collective_ns+=collective;r.launch_ns+=launch;
        r.requests=VHO_h3_add(r.requests,requests);r.transactions=VHO_h3_add(r.transactions,transactions);r.l2=VHO_h3_add(r.l2,transactions);r.hbm=VHO_h3_add(r.hbm,hbm);
        r.spill_requests=VHO_h3_add(r.spill_requests,spill_requests);r.shared=VHO_h3_add(r.shared,shared);r.waves=VHO_h3_add(r.waves,waves);r.queue_rounds=VHO_h3_add(r.queue_rounds,rounds);
    } else r.lower_ns+=total;
}
}
/* Rates are physical and model assumptions are explicit. Imported dependency
 * key includes exact device, conditions, toolchain, resource and budget joins. */
bool VHO_h3_model_valid(const DSL_H3_MODEL &m,const DSL_GPU_PLAN_SNAPSHOT &gpu,std::string &why) {
    bool valid=gpu.Bound() && m.binding==gpu.Cache_key() && !m.id.empty() && m.id.size()<=96 && !m.key.empty() &&
        (m.provenance=="synthetic" || m.provenance=="estimated" || m.provenance=="measured-input") &&
        (m.objective=="worst" || m.objective=="cold" || m.objective=="warm" || m.objective=="capacity" || m.objective=="contention") &&
        (m.transaction_bytes==32 || m.transaction_bytes==64 || m.transaction_bytes==128) && m.queue_per_sm && m.queue_per_sm<=65536 &&
        m.warm_hit_ppm<=1000000 && m.contention_ppm>=1000000 && m.contention_ppm<=8000000 &&
        VHO_h3_finite(m.issue_per_sm) && m.issue_per_sm>0 && VHO_h3_finite(m.barrier_ns) && m.barrier_ns>0 &&
        VHO_h3_finite(m.shuffle_ns) && m.shuffle_ns>0 && VHO_h3_finite(m.fence_ns) && m.fence_ns>0 && VHO_h3_finite(m.rsqrt_ns) && m.rsqrt_ns>0 &&
        m.spill_per_cta.known && m.spill_per_cta.value<=1073741824 &&
        m.site_limit && m.site_limit<=128 && m.request_limit && m.request_limit<=1024 &&
        m.memory_limit>=4096 && m.memory_limit<=67108864 && m.deadline_us && m.deadline_us<=3600000000ULL;
    why=valid?"bound":"h3_model_or_device_binding";return valid;
}
/* Exact operation-specific counts, work shape and capability bind estimates. */
std::string VHO_h3_plan_key(const DSL_H3_PLAN &p) {
    std::ostringstream out;out<<"h3-plan-v1;";VHO_h3_field(out,p.id);VHO_h3_field(out,p.capability);out<<p.executable<<';'<<p.live_bytes<<';'<<p.stages.size()<<';';
    for(size_t i=0;i<p.stages.size();++i) {
        const DSL_H3_STAGE &s=p.stages[i];VHO_h3_field(out,s.id);
        const uint64_t fields[]={s.ctas,s.threads,s.registers,s.shared_allocation,s.steps,s.depth,s.read_requests,s.write_through_requests,s.reusable_reads,s.shared_requests,s.scalar_ops,s.issue_ops,s.add_chain,s.mul_chain,s.exps,s.divs,s.shuffles,s.barriers,s.fences,s.dequant,s.rsqrts};
        for(unsigned j=0;j<21;++j) out<<fields[j]<<';';out<<s.asynchronous<<';'<<s.launch<<';';
    }
    return out.str();
}
/* All byte service is modeled, not fed back into immutable source facts.
 * Four scenarios retain separate ledgers; worst is over declared scenarios. */
bool VHO_h3_estimate(const DSL_MOVEMENT_FACTS &facts,const DSL_H3_MODEL &model,const DSL_H3_PLAN &plan,DSL_H3_ESTIMATE &out,std::string &why) {
    out=DSL_H3_ESTIMATE();
    try {
        VHO_h3_need(facts.Available() && VHO_h3_model_valid(model,facts.Capture().binding.device,why),"h3_missing_or_stale_facts");
        VHO_h3_need(!plan.id.empty() && !plan.capability.empty() && !plan.stages.empty() && plan.stages.size()<=64,"h3_plan_contract");
        VHO_h3_need(plan.live_bytes!=~uint64_t(0) && facts.Capture().binding.device.Fits_storage(plan.live_bytes),"h3_live_budget_unknown_or_exceeded");
        DSL_H3_ESTIMATE result;result.binding=facts.Key()+VHO_h3_model_key(model)+VHO_h3_plan_key(plan);result.key=DSL_movement_digest(result.binding);
        bool synthetic=model.provenance=="synthetic",measured=model.provenance=="measured-input";
        const DSL_GPU_PLAN_SNAPSHOT &gpu=facts.Capture().binding.device;
        for(unsigned i=0;i<DSL_GPU_RESOURCE_COUNT;++i) if(gpu.Resource(i)->known) synthetic=synthetic || gpu.Resource(i)->provenance==DSL_GPU_FACT_SYNTHETIC;
        for(unsigned i=0;i<DSL_GPU_EXECUTION_COUNT;++i) {
            const DSL_GPU_EXECUTION_FACT &v=*gpu.Execution(i);
            synthetic=synthetic || (v.known && v.provenance==DSL_GPU_FACT_SYNTHETIC);
            measured=measured && v.known && v.provenance==DSL_GPU_FACT_MEASURED;
        }
        result.provenance=synthetic?"estimated-from-synthetic-inputs":measured?"estimated-from-declared-measured-inputs":"estimated-from-mixed-inputs";
        result.executable=plan.executable;
        const char *names[]={"cold","warm","capacity","contention"};double rank=0;uint64_t footprint=VHO_h3_reuse_footprint(facts.Capture());
        for(unsigned scenario=0;scenario<4;++scenario) {
            DSL_H3_SCENARIO r;r.name=names[scenario];
            for(size_t i=0;i<plan.stages.size();++i) {
                VHO_h3_stage(facts.Capture().binding.device,model,plan.stages[i],scenario,footprint,false,r);
                VHO_h3_stage(facts.Capture().binding.device,model,plan.stages[i],scenario,footprint,true,r);
            }
            VHO_h3_need(VHO_h3_finite(r.upper_ns) && r.upper_ns>=r.lower_ns && r.upper_ns<9e18,"h3_interval_overflow");
            if(model.objective=="worst" || model.objective==r.name) rank=std::max(rank,r.upper_ns);
            result.scenarios.push_back(r);
        }
        result.rank_ns=uint64_t(std::ceil(rank));result.complete=true;result.reason="estimated-in-declared-space";out=result;why="complete";return true;
    } catch(const std::exception &error) {why=error.what();out.reason=why;return false;}
}
/* Print reconciled additive components and cache/profile dependency keys. */
void VHO_h3_print(FILE *file,const DSL_MOVEMENT_FACTS &facts,const DSL_H3_PLAN &plan,const DSL_H3_ESTIMATE &cost) {
    if(!file) return;
    fprintf(file,"AI_H3_COST 1 family=%s plan=%s complete=%u executable=%u unit=ns rank=%llu key=%s facts=%s cache_hit=%u provenance=%s reason=%s GPU_measured=0\n",facts.Capture().binding.family.c_str(),plan.id.c_str(),unsigned(cost.complete),unsigned(cost.executable),(unsigned long long)cost.rank_ns,cost.key.c_str(),facts.Key().c_str(),unsigned(cost.cache_hit),cost.provenance.c_str(),cost.reason.c_str());
    for(size_t i=0;i<plan.stages.size();++i) {
        const DSL_H3_STAGE &s=plan.stages[i];
        fprintf(file,"AI_H3_WORK 1 plan=%s stage=%s ctas=%llu threads=%llu registers=%llu shared_allocation=%llu steps=%llu depth=%llu launch=%u\n",plan.id.c_str(),s.id.c_str(),(unsigned long long)s.ctas,(unsigned long long)s.threads,(unsigned long long)s.registers,(unsigned long long)s.shared_allocation,(unsigned long long)s.steps,(unsigned long long)s.depth,unsigned(s.launch));
    }
    for(size_t i=0;i<cost.scenarios.size();++i) {
        const DSL_H3_SCENARIO &s=cost.scenarios[i];
        fprintf(file,"AI_H3_SCENARIO 1 plan=%s scenario=%s requested=%llu transactions=%llu L2=%llu HBM=%llu SMEM=%llu waves=%llu queue_rounds=%llu lower_ns=%.17g upper_ns=%.17g transfer_ns=%.17g compute_issue_ns=%.17g local_ns=%.17g special_ns=%.17g collective_ns=%.17g launch_ns=%.17g spill_requests_estimate=%llu source_spills=unknown evidence=estimate\n",plan.id.c_str(),s.name.c_str(),(unsigned long long)s.requests,(unsigned long long)s.transactions,(unsigned long long)s.l2,(unsigned long long)s.hbm,(unsigned long long)s.shared,(unsigned long long)s.waves,(unsigned long long)s.queue_rounds,s.lower_ns,s.upper_ns,s.transfer_ns,s.compute_issue_ns,s.local_ns,s.special_ns,s.collective_ns,s.launch_ns,(unsigned long long)s.spill_requests);
    }
}
/* Atomic configuration is separate from exploration and never resets a live
 * request's global budget. Callers create one owner per driver invocation. */
bool DSL_H3_SEARCH::Configure(const DSL_H3_MODEL &model,const DSL_GPU_PLAN_SNAPSHOT &gpu,std::string &why) {
    if(!_model.key.empty() || !VHO_h3_model_valid(model,gpu,why)) return false;
    try {uint64_t start=VHO_h3_clock();_model=model;_start_us=start;return true;}
    catch(const std::exception &error) {why=error.what();return false;}
}
/* Admission and bounded storage checks precede every copy/estimate. Cache hits
 * do not consume unique budget; proposals and deadline remain request-wide. */
bool DSL_H3_SEARCH::Evaluate(const DSL_MOVEMENT_FACTS &facts,const DSL_H3_PLAN &plan,DSL_H3_ESTIMATE &out,std::string &why) {
    out=DSL_H3_ESTIMATE();
    try {
        VHO_h3_need(!_model.key.empty() && facts.Available(),"h3_not_configured_or_no_facts");
        VHO_h3_need(_proposals<_model.request_limit*16,"h3_request_proposal_budget");
        ++_proposals;
        VHO_h3_need(VHO_h3_clock()-_start_us<_model.deadline_us,"h3_request_deadline");
        VHO_h3_need(plan.stages.size()<=64 && plan.id.size()<=1024 && plan.capability.size()<=1024,"h3_descriptor_budget");
        for(size_t i=0;i<plan.stages.size();++i) VHO_h3_need(plan.stages[i].id.size()<=1024,"h3_descriptor_budget");
        std::string body=VHO_h3_plan_key(plan);VHO_h3_need(body.size()<=65536,"h3_descriptor_budget");
        std::string key=facts.Key()+body;
        std::map<std::string,DSL_H3_SITE>::iterator site=_sites.find(facts.Key());
        VHO_h3_need(site==_sites.end() || site->second.facts.Same_content(facts),"h3_fact_digest_collision");
        std::map<std::string,DSL_H3_ESTIMATE>::const_iterator cached=_cache.find(key);
        if(cached!=_cache.end()) {out=cached->second;out.cache_hit=true;why=out.reason;return out.complete;}
        VHO_h3_need(_unique<_model.request_limit,"h3_request_unique_budget");
        VHO_h3_need(site==_sites.end() || site->second.unique<_model.site_limit,"h3_site_unique_budget");
        uint64_t charge=VHO_h3_add(VHO_h3_mul(body.size(),2),16384);
        if(site==_sites.end()) charge=VHO_h3_add(charge,facts.Storage_charge());
        const uint64_t scratch=8388608;
        VHO_h3_need(_model.memory_limit>=scratch && charge<=_model.memory_limit-scratch &&
            _charged<=_model.memory_limit-scratch-charge,"h3_detached_memory_budget");
        if(site==_sites.end()) {DSL_H3_SITE value;value.facts=facts;_sites.insert(std::make_pair(facts.Key(),value));site=_sites.find(facts.Key());}
        VHO_h3_need(site->second.facts.Same_content(facts),"h3_fact_digest_collision");
        ++_unique;++site->second.unique;_charged+=charge;
        DSL_H3_ESTIMATE estimate;bool complete=VHO_h3_estimate(facts,_model,plan,estimate,why);_cache.insert(std::make_pair(key,estimate));
        // An atomic estimate completing at the deadline remains a qualified
        // incumbent. The next proposal stops; no partially evaluated winner.
        if(VHO_h3_clock()-_start_us>=_model.deadline_us) _stop="h3_request_deadline";
        out=estimate;return complete;
    } catch(const std::exception &error) {why=error.what();_stop=why;out.reason=why;return false;}
}
/* Existing owner graphs have independent item bounds; keep their transient
 * storage within the reserved work region and the request's remaining budget. */
uint64_t DSL_H3_SEARCH::Working_items(uint64_t bytes_per_item) {
    try {
        if(_model.key.empty() || !bytes_per_item || VHO_h3_clock()-_start_us>=_model.deadline_us) {_stop="h3_request_deadline";return 0;}
        if(_charged>=_model.memory_limit) {_stop="h3_detached_memory_budget";return 0;}
        return std::min(uint64_t(8388608),_model.memory_limit-_charged)/bytes_per_item;
    } catch(const std::exception &error) {_stop=error.what();return 0;}
}
/* Request accounting is explicit and never represented as device performance. */
void DSL_H3_SEARCH::Print(FILE *file) const {
    if(file && !_model.key.empty()) fprintf(file,"AI_H3_SEARCH 1 unique=%llu proposals=%llu charged_bytes=%llu scratch_reserve=8388608 site_limit=%llu request_limit=%llu memory_limit=%llu deadline_us=%llu stop=%s conclusion=best-estimated-in-declared-space\n",(unsigned long long)_unique,(unsigned long long)_proposals,(unsigned long long)_charged,(unsigned long long)_model.site_limit,(unsigned long long)_model.request_limit,(unsigned long long)_model.memory_limit,(unsigned long long)_model.deadline_us,_stop.empty()?"catalog_or_existing_portfolio":_stop.c_str());
}
