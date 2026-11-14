/* Copyright (C) 2026 Open64 Project */
/* Q4 embedding review protocol. Typed VHO exploration has no JSON dependency;
 * this synchronous wrapper freezes/revalidates Q3 outside the search loop.
 * Design: doc/AI-COMPILER-MLA-Q4-QUANTIZED-MOVEMENT-v1.0.md. */
namespace {
/* Strict review input; reject duplicate keys recursively before any lookup. */
void q4_need(bool ok) {if(!ok) throw std::runtime_error("q4_invalid_review_request");}
/* The JSON is a test/review request, never a compiler/serving dispatch option. */
void q4_unique(const rapidjson::Value &v,unsigned depth=0) {
    q4_need(depth<=8);
    if(v.IsObject()) {
        std::set<std::string> keys;
        for(rapidjson::Value::ConstMemberIterator i=v.MemberBegin();i!=v.MemberEnd();++i) {
            q4_need(keys.insert(std::string(i->name.GetString(),i->name.GetStringLength())).second);q4_unique(i->value,depth+1);
        }
    } else if(v.IsArray()) for(unsigned i=0;i<v.Size();++i) q4_unique(v[i],depth+1);
}
/* Exact uint64 prevents signed/coerced inputs from supplying rates or budgets. */
UINT64 q4_number(const rapidjson::Value &v) {q4_need(v.IsUint64());return v.GetUint64();}
/* Reject unknown, missing and embedded-NUL string fields. */
std::string q4_text(const rapidjson::Value &v) {
    q4_need(v.IsString());std::string s(v.GetString(),v.GetStringLength());q4_need(s.find('\0')==std::string::npos);return s;
}
/* Validate the complete bounded review shape before constructing typed inputs. */
void q4_request(const char *wire,VHO_TENSOR_GPU_PROFILE &g,VHO_MLA_COST_PROFILE &m,
    VHO_Q4_PROFILE &p,VHO_MLA_SEARCH_CONTROL &c) {
    q4_need(wire && strlen(wire)<=65536);rapidjson::Document d;d.Parse(wire);q4_need(!d.HasParseError());q4_unique(d);
    const char *names[]={"schema","architecture","hardware","timings","hierarchy","scenarios","objective","provenance","strategies","stopping","seed","proposals","unique","time_us","patience","target","minimum_gain"};
    q4_need(d.IsObject() && d.MemberCount()==17);for(unsigned i=0;i<17;++i) q4_need(d.HasMember(names[i]));
    q4_need(q4_text(d["schema"])=="open64.q4-cost-study-request.v1");
    g.id="q4-review-explicit";g.architecture=q4_text(d["architecture"]);
    const rapidjson::Value &hardware=d["hardware"];q4_need(hardware.IsArray() && hardware.Size()==16);
    UINT64 *hw[]={&g.sms,&g.registers_per_thread,&g.target_ctas_per_sm,&g.scratch_budget};
    for(unsigned i=0;i<4;++i) *hw[i]=q4_number(hardware[i]);
    uint64_t *resources[]={
        &g.resources.max_blocks,&g.resources.max_warps,&g.resources.max_threads,&g.resources.max_threads_per_cta,
        &g.resources.registers_per_sm,&g.resources.max_registers_per_thread,&g.resources.register_quantum,&g.resources.register_warp_quantum,
        &g.resources.shared_capacity,&g.resources.shared_budget,&g.resources.shared_quantum,&g.resources.shared_reserved};
    for(unsigned i=0;i<12;++i) *resources[i]=q4_number(hardware[i+4]);
    const rapidjson::Value &times=d["timings"];q4_need(times.IsArray() && times.Size()==VHO_MLA_COST_FIELDS);
    m.id=g.id;m.architecture=g.architecture;m.provenance=q4_text(d["provenance"]);
    for(unsigned i=0;i<VHO_MLA_COST_FIELDS;++i) m.values[i]=q4_number(times[i]);
    VHO_MLA_MOVEMENT_PROFILE &h=p.hierarchy;h.id=g.id;h.architecture=g.architecture;h.provenance=m.provenance;h.objective=q4_text(d["objective"]);
    const rapidjson::Value &hierarchy=d["hierarchy"];q4_need(hierarchy.IsArray() && hierarchy.Size()==8);
    UINT64 *fields[]={&h.hbm_rate,&h.l2_rate,&h.sector_bytes,&h.request_amplification_ppm,&h.shared_conflict_ppm,&p.convert_lanes,&p.unpack_lanes,&p.page_cycles};
    for(unsigned i=0;i<8;++i) *fields[i]=q4_number(hierarchy[i]);
    const rapidjson::Value &scenarios=d["scenarios"];q4_need(scenarios.IsArray() && scenarios.Size()<=4);
    for(unsigned i=0;i<scenarios.Size();++i) {
        const rapidjson::Value &v=scenarios[i];q4_need(v.IsArray() && v.Size()==4);VHO_MLA_CACHE_SCENARIO s;
        s.id=q4_text(v[0]);s.weight=q4_number(v[1]);s.cache_hit_ppm=q4_number(v[2]);s.other_hit_ppm=q4_number(v[3]);h.scenarios.push_back(s);
    }
    c.space=VHO_q4_catalog();c.runs.clear();const rapidjson::Value &strategies=d["strategies"];
    q4_need(strategies.IsArray() && strategies.Size()<=16);
    for(unsigned i=0;i<strategies.Size();++i) {
        VHO_MLA_SEARCH_RUN_CONTROL run;run.strategy=q4_text(strategies[i]);run.id=run.strategy;
        run.seed=q4_number(d["seed"]);run.proposal_limit=17;c.runs.push_back(run);
    }
    c.stopping.clear();const rapidjson::Value &stopping=d["stopping"];q4_need(stopping.IsArray() && stopping.Size()<=6);
    for(unsigned i=0;i<stopping.Size();++i) c.stopping.push_back(q4_text(stopping[i]));
    c.global_proposals=q4_number(d["proposals"]);c.global_unique=q4_number(d["unique"]);c.global_time_us=q4_number(d["time_us"]);
    c.patience=q4_number(d["patience"]);c.target=q4_number(d["target"]);c.minimum_gain=q4_number(d["minimum_gain"]);
}
/* Export owned counts and explanations; application eligibility is always false. */
std::string q4_report(const VHO_Q4_SNAPSHOT &x,const VHO_MLA_SEARCH_CONTROL &control,const VHO_MLA_PRODUCTION_RESULT &r) {
    rapidjson::StringBuffer b;rapidjson::Writer<rapidjson::StringBuffer> w(b);w.StartObject();
    w.Key("schema");w.String("open64.q4-cache-service-study.v1");w.Key("space");w.String(VHO_q4_catalog());
    w.Key("snapshot_key");string(w,x.key);w.Key("controls_key");string(w,VHO_mla_search_control_key(control));
    w.Key("phase");string(w,x.facts.workload.phase);w.Key("encoding");string(w,x.schema.encoding);
    w.Key("scope");w.String("cache-service-only; no attention arithmetic or projections");
    w.Key("application_eligible");w.Bool(false);w.Key("aot_selection");w.String("deferred");
    w.Key("proposals");w.Uint64(r.proposals);w.Key("evaluations");w.Uint64(r.exploration.evaluations);w.Key("cache_hits");w.Uint64(r.exploration.cache_hits);
    w.Key("complete");w.Bool(r.exploration.complete);w.Key("termination");string(w,r.termination);
    w.Key("has_model_winner");w.Bool(r.has_winner);w.Key("model_winner");w.Uint(r.exploration.winner);
    w.Key("candidates");w.StartArray();
    for(unsigned i=0;i<r.exploration.candidates.size();++i) {
        const VHO_MLA_SEARCH_CANDIDATE &c=r.exploration.candidates[i];const VHO_Q4_ESTIMATE &e=c.quantized;
        w.StartObject();w.Key("id");w.Uint(i);w.Key("width");w.Uint(c.width);w.Key("group");w.Uint(c.descriptor.group);
        w.Key("evaluated");w.Bool(c.evaluated);w.Key("model_eligible");w.Bool(c.eligible);w.Key("application_eligible");w.Bool(false);
        w.Key("reason");string(w,c.reason);w.Key("estimated_cache_service_cycles");w.Uint64(c.total);
        const char *names[]={"packed_bytes","scale_bytes","page_requests","decoded_global_bytes","decoded_shared_bytes","conversions","unpacks","maintenance_bytes","partial_bytes","unique_physical_tokens","logical_capacity_tokens","logical_valid_tokens","active_queries"};
        const UINT64 values[]={e.packed,e.scales,e.page_requests,e.decoded_global,e.decoded_shared,e.conversions,e.unpacks,e.maintenance,e.partial,e.unique_physical,e.logical_capacity,e.logical_valid,e.active_queries};
        for(unsigned k=0;k<13;++k) {w.Key(names[k]);w.Uint64(values[k]);}
        w.Key("scenario_cycles");w.StartArray();for(unsigned k=0;k<e.scenario_cycles.size();++k) w.Uint64(e.scenario_cycles[k]);w.EndArray();
        w.Key("traffic");w.StartArray();for(unsigned k=0;k<e.traffic.size();++k) {
            const VHO_MLA_TRAFFIC_ESTIMATE &t=e.traffic[k];w.StartObject();w.Key("operand");string(w,t.operand);w.Key("scenario");string(w,t.scenario);
            w.Key("requested");w.Uint64(t.requested);w.Key("transactions");w.Uint64(t.transactions);w.Key("l2");w.Uint64(t.l2);w.Key("hbm");w.Uint64(t.hbm);w.Key("shared");w.Uint64(t.shared);w.EndObject();
        }w.EndArray();
        w.Key("model_key");string(w,e.key);w.Key("explanation");string(w,e.explanation);
        w.Key("resources");w.StartArray();for(unsigned k=0;k<c.stages.size();++k) {
            const VHO_MLA_STAGE_ESTIMATE &s=c.stages[k];w.StartArray();w.Uint64(s.ctas);w.Uint64(s.threads);w.Uint64(s.shared);w.Uint64(s.resident);w.Uint64(s.movement.rounded_registers);w.Uint64(s.movement.rounded_shared);w.EndArray();
        }w.EndArray();w.EndObject();
    }w.EndArray();w.Key("runs");w.StartArray();
    for(unsigned i=0;i<r.exploration.runs.size();++i) {
        const VHO_MLA_SEARCH_RUN &s=r.exploration.runs[i];w.StartObject();w.Key("strategy");string(w,s.strategy);w.Key("termination");string(w,s.termination);
        w.Key("visited");w.StartArray();for(unsigned k=0;k<s.visited.size();++k) w.Uint(s.visited[k]);w.EndArray();w.EndObject();
    }w.EndArray();w.EndObject();return std::string(b.GetString(),b.GetSize());
}
}
extern "C" {
/* Freeze once before exploration and validate once afterward. Candidate loops
 * cannot touch runtime buffers, run kernels or call a source reference. */
const char *open64_q4_study(void *object,UINT64 ticket,const char *request) {
    try {
        SESSION *s=(SESSION *)object;VHO_TENSOR_GPU_PROFILE g;VHO_MLA_COST_PROFILE m;VHO_Q4_PROFILE p;VHO_MLA_SEARCH_CONTROL c;
        q4_request(request,g,m,p,c);VHO_Q4_SNAPSHOT x;VHO_MLA_PRODUCTION_RESULT result;
        if(!s->adapter || !s->adapter->Freeze_costing(ticket,x,error)) return NULL;
        if(!VHO_mla_explore_quantized(g,m,c,p,x,&result,NULL)) {error="q4_study_admission";return NULL;}
        if(!s->adapter->Revalidate_costing(ticket,x.key,error)) return NULL;
        report_text=q4_report(x,c,result);error.clear();return report_text.c_str();
    } catch(const std::exception &e) {error=e.what();return NULL;}
}
/* External consumers must revalidate the original exact snapshot at use time;
 * successfully checking it grants no quantized materializer or dispatch right. */
int open64_q4_revalidate(void *object,UINT64 ticket,const char *key) {
    SESSION *s=(SESSION *)object;return key && s->adapter && s->adapter->Revalidate_costing(ticket,key,error);
}
}
