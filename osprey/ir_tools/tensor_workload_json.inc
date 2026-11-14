/* Copyright (C) 2026 Open64 Project */
/* Standalone Q2 review wire adapter. JSON witnesses are review data, never a
 * production engine binding or persistent native phase certificate.
 * Design: doc/AI-COMPILER-MLA-Q2-WORKLOAD-v1.0.md. */
/* Exact field census; no boolean-to-integer or unknown-field coercion. */
void q2_fields(const Json::Value &j,const char *const *names,unsigned n) {
    need(j.isObject() && j.size()==n,"Q2 field census");for(unsigned i=0;i<n;++i) need(j.isMember(names[i]),"Q2 missing field");
}
/* Reject negative, boolean, nonintegral and excessive external integers. */
UINT64 q2_integer(const Json::Value &j) {need(j.isUInt64() && !j.isBool() && j.asUInt64()<=16777216,"Q2 bounded integer required");return j.asUInt64();}
/* Bound text before the native semantic checker uses it in copied records. */
std::string q2_string(const Json::Value &j) {need(j.isString() && j.asString().size()<=4*1024*1024 && j.asString().find('\0')==std::string::npos,"Q2 bounded string required");return j.asString();}
/* Preserve complete numeric witnesses without reducing them to size labels. */
std::vector<UINT64> q2_numbers(const Json::Value &j) {
    need(j.isArray() && j.size()<=16777216,"Q2 array required");std::vector<UINT64> out;
    for(unsigned i=0;i<j.size();++i) out.push_back(q2_integer(j[i]));return out;
}
/* Closed review invocation schema. The production provider receives typed data
 * from an owning caller, not a file or an ST hint with an optimistic label. */
VHO_TENSOR_WORKLOAD_INVOCATION q2_decode(const Json::Value &j) {
    const char *root[]={"schema","source_key","invocation_id","evidence","requests"};q2_fields(j,root,5);
    need(q2_string(j["schema"])=="open64.mla-workload-invocation.v1","Q2 schema version");
    VHO_TENSOR_WORKLOAD_INVOCATION in;in.version=1;in.source_key=q2_string(j["source_key"]);
    in.invocation_id=q2_string(j["invocation_id"]);in.evidence=q2_string(j["evidence"]);
    need(j["requests"].isArray() && j["requests"].size()<=16,"Q2 request bound");
    const char *fields[]={"request","query_begin","queries","history","valid","append_tokens","window_left","phase","mask_policy","update","query_positions","key_positions","update_positions","allowed"};
    for(unsigned i=0;i<j["requests"].size();++i) {
        const Json::Value &v=j["requests"][i];q2_fields(v,fields,14);VHO_TENSOR_WORKLOAD_REQUEST r;
        r.request=q2_integer(v["request"]);r.query_begin=q2_integer(v["query_begin"]);r.queries=q2_integer(v["queries"]);
        r.history=q2_integer(v["history"]);r.valid=q2_integer(v["valid"]);r.append_tokens=q2_integer(v["append_tokens"]);r.window_left=q2_integer(v["window_left"]);
        r.phase=q2_string(v["phase"]);r.mask_policy=q2_string(v["mask_policy"]);r.update=q2_string(v["update"]);
        r.query_positions=q2_numbers(v["query_positions"]);r.key_positions=q2_numbers(v["key_positions"]);r.update_positions=q2_numbers(v["update_positions"]);
        need(v["allowed"].isArray() && v["allowed"].size()<=16777216,"Q2 mask bound");
        for(unsigned k=0;k<v["allowed"].size();++k) {need(v["allowed"][k].isBool(),"Q2 mask boolean required");r.allowed.push_back(v["allowed"][k].asBool());}
        in.requests.push_back(r);
    }
    return in;
}
/* Expose owned positive/unknown facts plus the conditional authority boundary. */
Json::Value q2_encode(const VHO_TENSOR_WORKLOAD_FACTS &f) {
    Json::Value j;j["schema"]="open64.mla-workload-facts.v1";const VHO_TENSOR_WORKLOAD_GEOMETRY &g=f.geometry;
    j["source_key"]=g.source_key;j["geometry"]["known"]=g.known;j["geometry"]["reason"]=g.reason;
    j["geometry"]["batch"]=Json::UInt64(g.batch);j["geometry"]["heads"]=Json::UInt64(g.heads);
    j["geometry"]["query_capacity"]=Json::UInt64(g.queries);j["geometry"]["kv_extent"]=Json::UInt64(g.keys);
    j["geometry"]["latent"]=Json::UInt64(g.latent);j["geometry"]["rotary"]=Json::UInt64(g.rotary);
    j["geometry"]["memory_form"]=g.memory_form;j["geometry"]["source_effect"]=g.source_effect;
    j["geometry"]["update_items_known"]=g.update_items_known;j["geometry"]["update_items"]=Json::UInt64(g.update_items);
    j["phase"]=f.phase;j["workload"]=f.workload;j["reason"]=f.reason;j["evidence"]=f.evidence;
    j["invocation_id"]=f.invocation_id;j["binding_key"]=f.binding_key;j["has_unknown"]=f.has_unknown;
    j["ragged"]=f.ragged;j["zero_active_queries"]=f.zero_active_queries;j["segments"]=Json::Value(Json::arrayValue);
    for(unsigned i=0;i<f.segments.size();++i) {
        const VHO_TENSOR_WORKLOAD_SEGMENT &s=f.segments[i];Json::Value v;v["request"]=Json::UInt64(s.request);
        v["query_begin"]=Json::Int64(s.query_begin);v["queries"]=Json::Int64(s.queries);v["history"]=Json::Int64(s.history);
        v["valid"]=Json::Int64(s.valid);v["append_tokens"]=Json::Int64(s.append_tokens);v["phase"]=s.phase;v["reason"]=s.reason;
        v["mask_policy"]=s.mask_policy;v["update"]=s.update;v["work"]=s.work;
        const char *names[]={"query_positions","key_positions","update_positions"};
        const std::vector<UINT64> *lists[]={&s.query_positions,&s.key_positions,&s.update_positions};
        for(unsigned k=0;k<3;++k) {v[names[k]]=Json::Value(Json::arrayValue);for(unsigned n=0;n<lists[k]->size();++n) v[names[k]].append(Json::UInt64((*lists[k])[n]));}
        j["segments"].append(v);
    }
    j["search_invoked"]=false;j["dispatch_eligible"]=false;j["engine_buffer_binding"]=false;
    j["scope"]="source-only or conditional review invocation; Q3 must bind actual engine buffers/generations";return j;
}
/* Pure repeated inference must not allocate native tables or mutate WHIRL.
 * Optional corruption modes exercise valid-but-unrecognized and renamed DAGs. */
int q2_command(int argc,char **argv) {
    need((argc>=4 && argc<=6) && access(argv[3],F_OK)!=0,"Q2 usage: q2-workload image output [invocation|-] [rename|unrecognized]");initialize(false);
    Open_Input_Info(argv[2]);Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
    PU_Info *pu=Read_Global_Info(NULL);need(pu && !PU_Info_next(pu) && !PU_Info_child(pu),"Q2 single entry required");
    Read_Local_Info(MEM_pu_nz_pool_ptr,pu);Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);
    Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);
    WN *entry=PU_Info_tree_ptr(pu);VHO_TENSOR_PROGRAM p;std::string why;
    bool read=VHO_q1_program_read(entry,p,why);need(read,why.c_str());
    if(argc==6) {
        if(!strcmp(argv[5],"rename")) {
            std::map<std::string,std::string> names;
            for(unsigned i=0;i<p.values.size();++i) {std::ostringstream n;n<<"renamed_"<<i;names[p.values[i].name]=n.str();}
            for(unsigned i=0;i<p.values.size();++i) {VHO_TENSOR_VALUE &v=p.values[i];v.name=names[v.name];if(!v.cache_scale.empty()) v.cache_scale=names[v.cache_scale];}
            p.source_pu="not_a_phase_name";p.source_file="opaque_source";
        } else if(!strcmp(argv[5],"unrecognized")) {
            bool changed=false;for(unsigned i=0;i<p.operations.size();++i) if(p.operations[i].name=="common.exp") {p.operations[i].name="common.log";changed=true;break;}
            need(changed,"Q2 expected exponential for recognition negative");
        } else need(false,"Q2 unknown test mode");
    }
    VHO_TENSOR_WORKLOAD_INVOCATION in;const VHO_TENSOR_WORKLOAD_INVOCATION *binding=NULL;
    if(argc>=5 && strcmp(argv[4],"-")) {in=q2_decode(search_read(argv[4]));binding=&in;}
    unsigned types=TY_Table_Size(),symbols=ST_Table_Size(CURRENT_SYMTAB);std::string native=DSL_NATIVE_TREE_KEY().Build(entry);
    VHO_TENSOR_WORKLOAD_FACTS facts,again;bool inferred=VHO_tensor_infer_workload(p,binding,facts,why);need(inferred,why.c_str());
    inferred=VHO_tensor_infer_workload(p,binding,again,why);need(inferred,why.c_str());need(search_text(q2_encode(facts))==search_text(q2_encode(again)),"Q2 unstable inference");
    bool live=VHO_tensor_revalidate_workload(p,binding,facts,why);need(live,why.c_str());
    VHO_TENSOR_WORKLOAD_FACTS corrupt=facts;corrupt.phase=facts.phase=="decode"?"prefill":"decode";
    need(!VHO_tensor_revalidate_workload(p,binding,corrupt,why),"Q2 changed result phase accepted");
    if(binding) {
        VHO_TENSOR_WORKLOAD_INVOCATION stale=in;stale.invocation_id+="-next";
        need(!VHO_tensor_revalidate_workload(p,&stale,facts,why) && why=="q2_workload_binding_changed","Q2 stale invocation accepted");
        stale=in;stale.requests[0].allowed[0]=!stale.requests[0].allowed[0];
        need(!VHO_tensor_revalidate_workload(p,&stale,facts,why),"Q2 stale mask accepted");
        need(!VHO_tensor_revalidate_workload(p,NULL,facts,why),"Q2 invocation facts became source authority");
    }
    need(types==TY_Table_Size() && symbols==ST_Table_Size(CURRENT_SYMTAB) && native==DSL_NATIVE_TREE_KEY().Build(entry),"Q2 inference mutated native state");
    Json::Value result=q2_encode(facts);result["native_state_unchanged"]=true;result["deterministic"]=true;
    write_json(argv[3],result);return 0;
}
