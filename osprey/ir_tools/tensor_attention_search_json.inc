/* Copyright (C) 2026 Open64 Project */
/* Standalone driver-owned JSON for two-phase MLA search. VHO receives typed
 * source/target/model/options; the canonical census is opaque archival evidence
 * to the lowerer. No JSON linkage is added to be.so. Included in driver scope.
 * Design: doc/AI-COMPILER-MLA-TWO-PHASE-SEARCH-v0.1.md. */
/* Canonical text supports legacy JsonCpp numeric spellings without coercion. */
std::string search_text(const Json::Value &j) {Json::StyledWriter w;return w.write(j);}
/* Strict bounded standalone documents share the recipe parser's JSON policy. */
Json::Value search_read(const char *path) {
    std::ifstream in(path);need(in.good(),"cannot read search document");
    in.seekg(0,std::ios::end);need(in.tellg()>=0 && in.tellg()<=16*1024*1024,"oversized search document");in.seekg(0);
    Json::CharReaderBuilder r;r["allowComments"]=false;r["collectComments"]=false;r["strictRoot"]=true;r["failIfExtra"]=true;r["rejectDupKeys"]=true;
    Json::Value j;std::string errors;need(Json::parseFromStream(r,in,&j,&errors),"invalid search JSON");return j;
}
/* The lowerer never interprets this driver-owned canonical evidence. */
Json::Value search_evidence(const VHO_TENSOR_PROGRAM &p) {
    Json::Reader reader;Json::Value j;need(reader.parse(p.search_evidence,j,false),"invalid admitted search evidence");return j;
}
/* Seal identity with the existing detached-recipe integrity checksum, not a
 * cryptographic signature; immutable artifacts additionally retain SHA256. */
std::string search_identity(const Json::Value &j) {
    const std::string text=search_text(j);UINT64 h=1469598103934665603ULL;
    for(size_t i=0;i<text.size();++i) h=(h^(unsigned char)text[i])*UINT64(1099511628211ULL);
    std::ostringstream out;out<<h;return out.str();
}
/* Record explicit timing inputs; no architecture table supplies absent costs. */
Json::Value search_cost_json(const VHO_MLA_COST_PROFILE &p) {
    Json::Value j;j["schema"]="open64.mla-cost-profile.v1";j["id"]=p.id;j["architecture"]=p.architecture;
    j["provenance"]=p.provenance;j["unit"]="reference_cycles";
    for(UINT32 i=0;i<VHO_MLA_COST_FIELDS;++i) j["parameters"][VHO_mla_cost_names[i]]=Json::UInt64(p.values[i]);return j;
}
/* Reject unknown/missing/unit-mismatched timing inputs before search. */
VHO_MLA_COST_PROFILE search_cost_decode(const Json::Value &j) {
    need(j.isObject() && j["schema"].isString() && j["schema"].asString()=="open64.mla-cost-profile.v1" &&
        j["id"].isString() && j["architecture"].isString() && j["provenance"].isString() && j["unit"].asString()=="reference_cycles" &&
        j["parameters"].isObject(),"invalid MLA cost profile");
    VHO_MLA_COST_PROFILE p;p.id=j["id"].asString();p.architecture=j["architecture"].asString();p.provenance=j["provenance"].asString();
    for(UINT32 i=0;i<VHO_MLA_COST_FIELDS;++i) {const Json::Value &v=j["parameters"][VHO_mla_cost_names[i]];need(v.isUInt64(),"missing/noninteger MLA timing");p.values[i]=v.asUInt64();}
    need(search_text(j)==search_text(search_cost_json(p)),"unknown or noncanonical MLA timing fields");return p;
}
/* Independent strategy and stop controls are sealed with the selected census. */
Json::Value search_options_json(const VHO_MLA_SEARCH_OPTIONS &o) {
    Json::Value j;j["schema"]="open64.mla-search-options.v1";j["strategy"]=o.strategy;j["stop"]=o.stop;
    j["max_evaluations"]=Json::UInt64(o.budget);j["seed"]=Json::UInt64(o.seed);j["patience"]=Json::UInt64(o.patience);
    j["minimum_gain_cycles"]=Json::UInt64(o.minimum_gain);j["target_cycles"]=Json::UInt64(o.target);j["gap_ppm"]=Json::UInt64(o.gap_ppm);
    j["remaining_invocations"]=Json::UInt64(o.remaining_invocations);j["evaluation_cost_cycles"]=Json::UInt64(o.evaluation_cost);return j;
}
/* Unavailable probabilistic certificates are rejected, not fabricated from a
 * deterministic point estimate. Numeric evidence requirements are checked in VHO. */
VHO_MLA_SEARCH_OPTIONS search_options_decode(const Json::Value &j) {
    need(j.isObject() && j["schema"].asString()=="open64.mla-search-options.v1" && j["strategy"].isString() && j["stop"].isString(),"invalid MLA search options");
    VHO_MLA_SEARCH_OPTIONS o;o.strategy=j["strategy"].asString();o.stop=j["stop"].asString();
    const char *names[]={"max_evaluations","seed","patience","minimum_gain_cycles","target_cycles","gap_ppm","remaining_invocations","evaluation_cost_cycles"};
    UINT64 *values[]={&o.budget,&o.seed,&o.patience,&o.minimum_gain,&o.target,&o.gap_ppm,&o.remaining_invocations,&o.evaluation_cost};
    for(UINT32 i=0;i<8;++i) {need(j[names[i]].isUInt64(),"missing/noninteger search option");*values[i]=j[names[i]].asUInt64();}
    need(search_text(j)==search_text(search_options_json(o)),"unknown search options");return o;
}
/* Serialize the auditable per-CTA counts and complete whole-stage components. */
Json::Value search_stage_json(const VHO_MLA_STAGE_ESTIMATE &s) {
    const char *names[]={"operation","ctas","threads","shared","resident","scalar_ops","global_bytes","shared_bytes","shuffles","exps","divs","barriers","fences","dependency","rounds","compute","memory","special","local","sync","launch","total"};
    const UINT64 values[]={s.operation,s.ctas,s.threads,s.shared,s.resident,s.scalar_ops,s.global_bytes,s.shared_bytes,s.shuffles,s.exps,s.divs,s.barriers,s.fences,s.dependency,s.rounds,s.compute,s.memory,s.special,s.local,s.sync,s.launch,s.total};
    Json::Value j;for(UINT32 i=0;i<22;++i) j[names[i]]=Json::UInt64(values[i]);return j;
}
/* Build a deterministic estimation-only report. Replaying this cheap ledger
 * validates a selected handoff once at driver admission, never per kernel. */
Json::Value search_report(const VHO_TENSOR_PROGRAM &source,const VHO_TENSOR_GPU_PROFILE &g,
    const VHO_MLA_COST_PROFILE &cost,const VHO_MLA_SEARCH_OPTIONS &options) {
    VHO_MLA_SEARCH_RESULT result;need(VHO_mla_explore(source,g,cost,options,&result,stderr),"MLA estimation-only exploration failed");
    Json::Value j;j["schema"]="open64.mla-exploration.v1";j["source_recipe"]=encode(source);j["gpu_profile"]=profile_json(g);
    j["cost_profile"]=search_cost_json(cost);j["search_options"]=search_options_json(options);j["model"]="mla-additive-waves-v1";
    j["candidates"]=Json::Value(Json::arrayValue);
    for(UINT32 i=0;i<result.candidates.size();++i) {
        const VHO_MLA_SEARCH_CANDIDATE &c=result.candidates[i];Json::Value x;
        x["ordinal"]=i;x["policy"]=c.policy;x["width"]=c.width;x["evaluated"]=c.evaluated;x["eligible"]=c.eligible;x["reason"]=c.reason;
        x["scratch_bytes"]=Json::UInt64(c.scratch);x["estimated_cycles"]=Json::UInt64(c.total);x["stages"]=Json::Value(Json::arrayValue);
        for(size_t k=0;k<c.stages.size();++k) x["stages"].append(search_stage_json(c.stages[k]));j["candidates"].append(x);
    }
    j["runs"]=Json::Value(Json::arrayValue);
    for(UINT32 i=0;i<result.runs.size();++i) {
        const VHO_MLA_SEARCH_RUN &r=result.runs[i];Json::Value x;x["strategy"]=r.strategy;x["termination"]=r.termination;x["eligible_winner"]=r.best>0;x["winner"]=r.best?Json::Value(r.winner):Json::Value(Json::nullValue);
        x["best_estimated_cycles"]=Json::UInt64(r.best);x["first_best_trial"]=Json::UInt64(r.first_best_trial);x["restarts"]=Json::UInt64(r.restarts);x["visited"]=Json::Value(Json::arrayValue);
        for(size_t k=0;k<r.visited.size();++k) x["visited"].append(r.visited[k]);j["runs"].append(x);
    }
    j["winner"]=result.winner;j["representative_strategy"]=result.best_run;j["complete_declared_space"]=result.complete;
    j["conclusion"]=result.complete?"best_estimated_in_declared_space":"best_estimated_among_explored";
    j["estimator_lower_bound"]=Json::UInt64(result.lower_bound);j["candidate_estimates"]=Json::UInt64(result.evaluations);j["cache_hits"]=Json::UInt64(result.cache_hits);
    j["native_initializations"]=Json::UInt64(native_initializations);j["materialized_kernels"]=Json::UInt64(materialized_kernels);
    j["gpu_performance_verified"]=false;j["probabilistic_certificate"]="unavailable_no_calibrated_posterior";
    j["search_fingerprint"]=search_identity(j);return j;
}
/* Admit the complete report by deterministic replay; then retain one fixed
 * winner and opaque audit evidence. No losers are materialized, compiled or tested. */
VHO_TENSOR_PROGRAM search_admit(const Json::Value &j) {
    need(j.isObject() && j["schema"].asString()=="open64.mla-exploration.v1" && j["source_recipe"]["schema"].asString()!="open64.tensor-kernel.v7", "invalid or recursive exploration handoff");
    VHO_TENSOR_PROGRAM p=decode_json(j["source_recipe"]);
    need(!p.auto_chunks && !p.search_selected && p.register_latent && p.chunk_policy==1,"search source must be detached manual register attention");
    VHO_TENSOR_GPU_PROFILE g=profile_decode(j["gpu_profile"]);VHO_MLA_COST_PROFILE cost=search_cost_decode(j["cost_profile"]);
    VHO_MLA_SEARCH_OPTIONS options=search_options_decode(j["search_options"]);
    need(search_text(j)==search_text(search_report(p,g,cost,options)),"stale/forged MLA search ledger or winner");
    const Json::Value &winner=j["candidates"][j["winner"].asUInt()];
    p.search_selected=true;p.search_evidence=search_text(j);p.search_identity=search_identity(j);p.gpu=g;
    p.chunk_policy=winner["policy"].asUInt();p.split_width=winner["width"].asUInt();
    need(VHO_tensor_validate_program(p,stderr),"selected MLA plan rejected");return p;
}
