/* Copyright (C) 2026 Open64 Project */
/* Embedding boundary for Q5 selection: Q3 observation and freshness sit outside
 * the existing detached portfolio. Native compilation/execution is subsequent.
 * Design: doc/AI-COMPILER-MLA-Q5-SELECTED-PLANS-v1.0.md. */
extern "C" {
/* Qualify all estimates without invoking an entry, compiler or IR builder. */
const char *open64_q5_study(void *object,UINT64 ticket,const char *request,UINT64 log_cycles) {
    try {
        SESSION *s=(SESSION *)object;VHO_TENSOR_GPU_PROFILE g;VHO_MLA_COST_PROFILE m;VHO_Q4_PROFILE p;VHO_MLA_SEARCH_CONTROL c;
        q4_request(request,g,m,p,c);c.space=VHO_q5_catalog();VHO_Q4_SNAPSHOT x;VHO_MLA_PRODUCTION_RESULT result;
        if(!s->adapter || !s->adapter->Freeze_costing(ticket,x,error)) return NULL;
        if(!VHO_mla_explore_q5(s->program,g,m,c,p,x,log_cycles,&result,NULL)) {error="q5_study_admission";return NULL;}
        if(!s->adapter->Revalidate_costing(ticket,x.key,error)) return NULL;
        rapidjson::Document d;std::string report=q4_report(x,c,result);d.Parse(report.c_str());
        rapidjson::Document::AllocatorType &a=d.GetAllocator();
        d["schema"].SetString("open64.q5-complete-plan-study.v1",a);d["space"].SetString(VHO_q5_catalog(),a);
        d["scope"].SetString("complete source entry; strict synchronous High plan; GPU mapping unqualified",a);
        d["application_eligible"].SetBool(result.has_winner);
        for(unsigned i=0;i<result.exploration.candidates.size();++i) {
            d["candidates"][i]["application_eligible"].SetBool(result.exploration.candidates[i].eligible);
            d["candidates"][i].FindMember("estimated_cache_service_cycles")->name.SetString("estimated_plan_cycles",a);
        }
        if(result.has_winner) {
            const VHO_MLA_SEARCH_CANDIDATE &winner=result.exploration.candidates[result.exploration.winner];VHO_Q5_PLAN plan;
            plan.binding_key=x.key;plan.width=winner.width;plan.group=winner.descriptor.group;plan.source_key=x.schema.source_key;
            std::ostringstream descriptor;descriptor<<"/selected-descriptor-v1/W="<<plan.width<<"/G="<<plan.group;
            plan.selection_key="mla-q5-selected-v1/"+VHO_mla_search_control_key(c)+"/"+winner.quantized.key+descriptor.str();
            std::string wire=VHO_q5_plan_encode(plan);rapidjson::Value value;value.SetString(wire.c_str(),wire.size(),a);d.AddMember("selected_plan",value,a);
        }
        d.AddMember("gpu_execution_eligible",false,a);d.AddMember("selected_reference_check_required",true,a);
        rapidjson::StringBuffer b;rapidjson::Writer<rapidjson::StringBuffer> w(b);d.Accept(w);report_text.assign(b.GetString(),b.GetSize());error.clear();return report_text.c_str();
    } catch(const std::exception &e) {error=e.what();return NULL;}
}
/* The owner executes a qualified selected entry only after checking its exact
 * source contract and fresh Q3 study snapshot. Q3 Run rechecks controls/parent
 * again and retains complete status/scale/publication checks. No variant choice. */
int open64_q5_run(void *object,UINT64 ticket,const char *plan_text,const char *snapshot_key,
    bool (*entry)(void *,void **,UINT32),void *context,UINT64 *bundle) {
    SESSION *s=(SESSION *)object;VHO_Q5_PLAN plan;
    if(!plan_text || !snapshot_key || !VHO_q5_plan_decode(plan_text,plan,error)) return 0;
    if(plan.binding_key!=snapshot_key) {error="q5_snapshot_plan_join";return 0;}
    if(plan.source_key!=VHO_tensor_quantized_high_key(s->program)) {error="q5_foreign_source_plan";return 0;}
    if(!s->adapter || !s->adapter->Revalidate_costing(ticket,snapshot_key,error)) return 0;
    return s->adapter->Run(ticket,entry,context,*bundle,error);
}
}

#ifdef OPEN64_SERVING_TEST
extern "C" {
/* Focused detached contract negatives use copies only. No live WN/ST/TY or
 * buffer is accessed; test mutation cannot alter a registry source program. */
const char *open64_q5_contract_test(void *object) {
    try {
        SESSION *s=(SESSION *)object;const VHO_TENSOR_PROGRAM &source=s->program;VHO_Q5_LAYOUT layout;std::string why;
        std::string original=VHO_tensor_quantized_high_key(source);unsigned checks=0;
        VHO_TENSOR_SERVING_SCHEMA schema;q4_need(VHO_tensor_serving_schema(source,schema,why));
        bool supported=!schema.encoding.empty();q4_need(VHO_q5_layout(source,layout,why)==supported);++checks;
        if(supported) {
            VHO_TENSOR_PROGRAM bad=source;bad.operations[layout.start+2].attributes["attr.permutation"]="0,1,2,3";
            VHO_Q5_LAYOUT refused;q4_need(!VHO_q5_layout(bad,refused,why));++checks;
            bad=source;UINT32 decoded=layout.schema.geometry.latent_value;
            for(unsigned i=0;i<bad.operations.size();++i) if(bad.operations[i].output==decoded)
                bad.operations[i].inputs[1]=bad.operations[i].inputs[0];
            q4_need(!VHO_q5_layout(bad,refused,why));++checks;
        }
        VHO_Q5_PLAN p;p.width=32;p.group=4;p.source_key=original;p.binding_key="synthetic-codec-only";
        p.selection_key="synthetic-test/selected-descriptor-v1/W=32/G=4";VHO_Q5_PLAN decoded;
        std::string wire=VHO_q5_plan_encode(p);q4_need(VHO_q5_plan_decode(wire,decoded,why));++checks;
        q4_need(VHO_q5_plan_key(p)==VHO_q5_plan_key(decoded));++checks;
        p.width=64;q4_need(!VHO_q5_plan_decode(VHO_q5_plan_encode(p),decoded,why));++checks;
        p.width=33;q4_need(!VHO_q5_plan_decode(VHO_q5_plan_encode(p),decoded,why));++checks;
        q4_need(!VHO_q5_plan_decode(wire+"garbage",decoded,why));++checks;
        q4_need(!VHO_q5_plan_decode("{\"width\":32,"+wire.substr(1),decoded,why));++checks;
        q4_need(original==VHO_tensor_quantized_high_key(source));++checks;
        rapidjson::StringBuffer b;rapidjson::Writer<rapidjson::StringBuffer> w(b);w.StartObject();w.Key("status");w.String("PASS");w.Key("checks");w.Uint(checks);w.Key("source_unchanged");w.Bool(true);w.EndObject();
        report_text.assign(b.GetString(),b.GetSize());return report_text.c_str();
    } catch(const std::exception &e) {error=e.what();return NULL;}
}
}
#endif
