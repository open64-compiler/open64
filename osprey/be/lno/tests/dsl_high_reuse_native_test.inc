/* Copyright (C) 2026 Open64 Project */
/* Test-only perturbations of actual owned native records. No WN/ST/TY or
 * analysis manager is edited; production has neither hook nor fault option.
 * Design: doc/AI-COMPILER-MLA-MR1-OWNED-REUSE-v1.0.md. */
#include <algorithm>

/* Keep every acceptance/refusal individually reviewable. */
static void DSL_reuse_test_require(bool ok,const char *name,FILE *trace) {
    FmtAssert(ok,("MR1 owned reuse test failed: %s",name));
    if(trace) fprintf(trace,"AI_HIGH_REUSE_TEST PASS %s\n",name);
}
/* Seed output with valid facts and prove every refusal clears stale results. */
static void DSL_reuse_test_refusal(const std::vector<DSL_HIGH_STORAGE> &s,
    const DSL_HIGH_SNAPSHOT &snapshot,const std::vector<DSL_HIGH_ACCESS> &a,
    const std::vector<DSL_HIGH_ALIAS> &aliases,const VHO_TENSOR_PROGRAM &p,
    const DSL_HIGH_REUSE_SUMMARY &good,const char *name,FILE *trace) {
    DSL_HIGH_REUSE_SUMMARY result=good;std::string reason;
    bool refused=!LNO_DSL_analyze_cache_reuse(s,snapshot.Loops(),a,aliases,p,result,reason);
    DSL_reuse_test_require(refused && !reason.empty() && result.storages.empty() &&
        result.accesses.empty() && result.caches.empty() && result.key.empty(),name,trace);
}
/* Exercise real compiler-captured arrays/alias vectors and bound lifetimes. */
static void DSL_high_reuse_test(const DSL_HIGH_SNAPSHOT &snapshot,const DSL_HIGH_EPOCH &epoch,
    const DSL_HIGH_BINDINGS &bindings,const VHO_TENSOR_PROGRAM &p,const DSL_HIGH_REUSE_FACTS &bound,
    const std::string &reason,FILE *trace) {
    FmtAssert(bound.Available(),("MR1 baseline proof unavailable: %s",reason.c_str()));
    DSL_reuse_test_require(bound.Is_current(snapshot,epoch,bindings,p),"bound_current",trace);
    const DSL_HIGH_REUSE_SUMMARY &good=bound.Summary();
    DSL_reuse_test_require(good.caches.size()==2 && good.storages.size()==snapshot.Storages().size(),
        "complete_storage_and_cache_census",trace);
    bool unknown=true;
    for(size_t i=0;i<good.storages.size();++i) {
        const DSL_HIGH_REUSE_STORAGE &s=good.storages[i];
        unknown=unknown && s.locality.working_set_bytes==DSL_TENSOR_LOCALITY_UNKNOWN_U64 &&
            s.locality.reuse_distance_statements==DSL_TENSOR_LOCALITY_UNKNOWN_U64 &&
            (s.root!=UINT32_MAX || s.locality.estimated_read_bytes==DSL_TENSOR_LOCALITY_UNKNOWN_U64);
    }
    DSL_reuse_test_require(unknown,"unproved_metrics_stay_unknown",trace);
    uint64_t b=p.values[2].shape[0],h=p.values[0].shape[1],l=p.values[2].shape[1];
    for(size_t i=0;i<good.caches.size();++i) {
        const DSL_HIGH_REUSE_CACHE &c=good.caches[i];uint64_t width=p.values[c.root].shape[2];
        unsigned sweeps=c.root==2?2:1;
        DSL_reuse_test_require(c.logical_sweeps==sweeps && c.payload_bytes==4*b*l*width &&
            c.fused_logical_read_bytes==4*b*h*l*width*sweeps &&
            c.source_read_bytes==4*b*l*width*(1+h*sweeps) &&
            c.source_write_bytes==c.payload_bytes && c.materialized_copy_bytes==c.payload_bytes,
            sweeps==2?"latent_two_sweeps_and_copy_counts":"rotary_one_sweep_and_copy_counts",trace);
    }
    size_t chosen=good.accesses.size(),derived=good.storages.size();
    for(size_t i=0;i<good.accesses.size();++i)
        if(good.accesses[i].storage==2 && !good.accesses[i].head_loop.empty()) {chosen=i;break;}
    for(size_t i=0;i<good.storages.size();++i)
        if(good.storages[i].root==2 && good.storages[i].value!=2) {derived=i;break;}
    DSL_reuse_test_require(chosen<good.accesses.size() && derived<good.storages.size(),
        "native_head_and_transpose_coverage",trace);
    const DSL_HIGH_REUSE_ACCESS &read=good.accesses[chosen];
    size_t ri=snapshot.Accesses().size(),wi=snapshot.Accesses().size();
    for(size_t i=0;i<snapshot.Accesses().size();++i) {
        const DSL_HIGH_ACCESS &a=snapshot.Accesses()[i];
        if(a.id==read.id) ri=i;
        if(a.write && a.storage!=read.native.storage) wi=i;
    }
    DSL_reuse_test_require(ri<snapshot.Accesses().size() && wi<snapshot.Accesses().size(),
        "native_read_write_coverage",trace);
    std::vector<DSL_HIGH_STORAGE> s=snapshot.Storages();
    std::vector<DSL_HIGH_ACCESS> a=snapshot.Accesses();
    std::vector<DSL_HIGH_ALIAS> aliases=snapshot.Aliases();
    size_t head=std::find(a[ri].loop_ids.begin(),a[ri].loop_ids.end(),read.head_loop)-a[ri].loop_ids.begin();
    ++a[ri].dimensions[0].loop_coefficients[head];
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"head_dependent_address",trace);
    a=snapshot.Accesses();a[ri].dimensions[0].loop_coefficients.back()*=2;
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"strided_address",trace);
    a=snapshot.Accesses();a[ri].byte_offset=4;
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"offset_view",trace);
    a=snapshot.Accesses();a[ri].array_extent++;
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"wrong_allocation_extent",trace);
    a=snapshot.Accesses();a[ri].conditional=true;
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"conditional_read_lifetime",trace);
    a=snapshot.Accesses();a[ri].dimensions[0].linear_symbols.push_back("unknown");
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"symbolic_read_footprint",trace);
    a=snapshot.Accesses();a[ri].direct_formal=false;
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"opaque_base",trace);
    a=snapshot.Accesses();a[ri].element_encoding="I4";
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"wrong_access_encoding",trace);
    a=snapshot.Accesses();a[wi].storage=read.native.storage;
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"input_cache_write",trace);
    a=snapshot.Accesses();
    for(size_t i=0;i<s.size();++i) if(s[i].id==read.native.storage) s[i].base_stable=false;
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"reassigned_base_version",trace);
    s=snapshot.Storages();
    for(size_t i=0;i<s.size();++i) if(s[i].id==read.native.storage) s[i].uses_only_as_array_base=false;
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"escaped_base_lifetime",trace);
    s=snapshot.Storages();
    for(size_t i=0;i<s.size();++i) if(s[i].id==read.native.storage) s[i].restrict_qualified=false;
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"missing_storage_contract",trace);
    s=snapshot.Storages();
    for(size_t i=0;i<s.size();++i) if(s[i].id==read.native.storage) s[i].element_encoding="I4";
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"wrong_storage_encoding",trace);
    s=snapshot.Storages();
    bool edited=false;
    for(size_t i=0;i<aliases.size();++i)
        if((aliases[i].first==read.id && aliases[i].second==a[wi].id) ||
           (aliases[i].second==read.id && aliases[i].first==a[wi].id)) {
            aliases[i].disjoint=false;edited=true;
        }
    DSL_reuse_test_require(edited,"alias_pair_coverage",trace);
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"possibly_aliased_write",trace);
    aliases.clear();
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"missing_alias_answers",trace);
    aliases=snapshot.Aliases();
    size_t producer=a.size(),consumer=a.size();
    for(size_t i=0;i<a.size();++i) if(a[i].storage==good.storages[derived].native_storage) {
        if(a[i].write) producer=i;else consumer=i;
    }
    DSL_reuse_test_require(producer<a.size() && consumer<a.size(),"derived_version_coverage",trace);
    a[consumer].execution_order=a[producer].execution_order;
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"read_before_complete_producer",trace);
    a=snapshot.Accesses();a[producer].copied_from_access.clear();
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"unproved_transpose_value_flow",trace);
    a=snapshot.Accesses();DSL_HIGH_ACCESS extra=a[producer];extra.id+=".duplicate";a.push_back(extra);
    DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"multiple_storage_versions",trace);
    a=snapshot.Accesses();
    size_t slice=good.accesses.size();
    for(size_t i=0;i<good.accesses.size();++i) if(good.accesses[i].sliced_reduction) {slice=i;break;}
    if(slice<good.accesses.size()) {
        std::string id=good.accesses[slice].id;
        for(size_t i=0;i<a.size();++i) if(a[i].id==id) {a.erase(a.begin()+i);break;}
        DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"missing_unrolled_reduction_slice",trace);
        a=snapshot.Accesses();
        const DSL_HIGH_REUSE_ACCESS &r=good.accesses[slice];
        for(size_t i=0;i<a.size();++i) if(a[i].id==r.id) a[i].dimensions[0].constant=1;
        DSL_reuse_test_refusal(s,snapshot,a,aliases,p,good,"misaligned_unrolled_slice",trace);
    }
    a=snapshot.Accesses();
    VHO_TENSOR_PROGRAM undeclared=p;
    for(size_t i=0;i<undeclared.values.size();++i) undeclared.values[i].cache_trait.clear();
    for(size_t i=0;i<s.size();++i) s[i].cache_trait.clear();
    DSL_HIGH_REUSE_SUMMARY inferred;std::string why;
    DSL_reuse_test_require(LNO_DSL_analyze_cache_reuse(s,snapshot.Loops(),a,aliases,undeclared,inferred,why) &&
        inferred.caches.size()==2 && inferred.caches[0].role_provenance=="closed-graph-derived",
        "missing_trait_graph_inference",trace);
    s=snapshot.Storages();if(s.size()>3) s[2].symbol=s[3].symbol;
    DSL_HIGH_REUSE_SUMMARY distinct;
    DSL_reuse_test_require(LNO_DSL_analyze_cache_reuse(s,snapshot.Loops(),a,aliases,p,distinct,why) &&
        distinct.caches[0].version!=distinct.caches[1].version,"equal_names_do_not_merge_storage",trace);
    DSL_HIGH_CACHE_TRAFFIC traffic;traffic.payload_bytes=1;traffic.fused_logical_read_bytes=1;
    DSL_reuse_test_require(!LNO_DSL_cache_traffic(UINT64_MAX,h,l,16,2,traffic) &&
        !traffic.payload_bytes && !traffic.fused_logical_read_bytes,"traffic_overflow_clears_output",trace);
    DSL_HIGH_EPOCH another(snapshot.Stamp().owner);
    DSL_reuse_test_require(!bound.Is_current(snapshot,another,bindings,p),"activation_ABA_rejected",trace);
    DSL_HIGH_BINDINGS changed=bindings;changed.numeric+="changed";
    DSL_reuse_test_require(!bound.Is_current(snapshot,epoch,changed,p),"binding_rebind_rejected",trace);
    VHO_TENSOR_PROGRAM changed_program=p;changed_program.values[2].name+="changed";
    DSL_reuse_test_require(!bound.Is_current(snapshot,epoch,bindings,changed_program),"manifest_rebind_rejected",trace);
    DSL_HIGH_REUSE_FACTS refused=bound;
    DSL_reuse_test_require(!LNO_DSL_derive_cache_reuse(snapshot,epoch,bindings,changed_program,refused,why) &&
        !refused.Available(),"producer_rebind_clears_output",trace);
    DSL_reuse_test_require(bound.Is_current(snapshot,epoch,bindings,p),"all_owned_edits_isolated",trace);
}
