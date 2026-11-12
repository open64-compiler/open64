/* Copyright (C) 2026 Open64 Project */
/* Isolated selected Q6c continuation. Driver owns active PU, epochs and fresh
 * artifacts; phases neither search nor switch context. Manifest is last. */
int q6c_command(int argc,char **argv) {
    bool apply=!strcmp(argv[1],"q6-device");
    if(apply) need(argc==6 && !strcmp(argv[5],"--opt-level=3"),"q6-device staged.B prefix prepare|attention|finalize --opt-level=3");
    else need(argc==5
#ifdef OPEN64_TENSOR_BRIDGE_TEST
        || (argc==6 && !strcmp(argv[5],"--test-write-mutated"))
#endif
        ,"q6-device-audit stage.B high|mid|low result.json");
    std::string prefix=argv[3],stage=argv[4];
    if(apply) {
        const char *suffixes[]={".before.B",".high.B",".mid.B",".low.B",".ptx",".json"};
        for(unsigned i=0;i<6;++i) need(access((prefix+suffixes[i]).c_str(),F_OK)!=0,"q6c_output_exists");
    } else need(access(argv[4],F_OK)!=0,"q6c_audit_output_exists");
    initialize(false);Open_Input_Info(argv[2]);Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);PU_Info *pu=Read_Global_Info(NULL);
    need(pu && !PU_Info_next(pu) && !PU_Info_child(pu),"q6c_single_PU");Read_Local_Info(MEM_pu_nz_pool_ptr,pu);
    Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);Initialize_Special_Global_Symbols();
    WN *root=PU_Info_tree_ptr(pu);ST_IDX st=WN_st_idx(root);
#ifdef OPEN64_TENSOR_BRIDGE_TEST
    if(apply) {
        const char *fault=getenv("OPEN64_Q6C_TEST_FAULT");
        if(fault && !strcmp(fault,"source-binding")) ST_tensor_bind_metadata(st,"tensor.q6.mapping","stale");
    }
#endif
    if(!apply) {
#ifdef OPEN64_TENSOR_BRIDGE_TEST
        const char *fault=getenv("OPEN64_Q6C_TEST_FAULT");
        if(fault) {
            if(!strcmp(fault,"wait")) need(q6b_test_remove(root,"__open64_q6c_wait"),"q6c_test_missing_wait");
            else if(!strcmp(fault,"sync")) need(q6b_test_remove(root,"__open64_q6c_cta_sync"),"q6c_test_missing_sync");
            else if(!strcmp(fault,"bulk")) need(q6b_test_remove(root,"__open64_q6c_bulk"),"q6c_test_missing_bulk");
            else ST_tensor_bind_metadata(st,"tensor.q6c.source_key","stale");
        }
        if(argc==6) q5_checkpoint(pu,root,(std::string(argv[4])+".B").c_str());
#endif
        VHO_q6c_verify(argv[3]);Json::Value j;j["status"]="PASS";j["key"]=VHO_q6c_key(root);write_json(argv[4],j);return 0;
    }
    const char *key=ST_tensor_metadata(st,"tensor.q6.accepted.key"),*schema=ST_tensor_metadata(st,"tensor.q6.schema"),*wire=ST_tensor_metadata(st,"tensor.q6.control");
    need(schema && !strcmp(schema,"mla-native-staging-v1") && key && wire && !ST_tensor_metadata(st,"tensor.q6c.schema"),"q6c_requires_Q6b");
    std::string source_key=key;need(VHO_q6_stage_key(root)==source_key,"q6c_stale_Q6b");
    Json::Value control;VHO_Q6_CONTROL c=q6_control(wire,control);VHO_TENSOR_PROGRAM p;VHO_Q5_PLAN plan;std::string why;
    const char *applied=ST_tensor_metadata(st,"tensor.q5.applied"),*accepted=ST_tensor_metadata(st,"tensor.q5.accepted.key");
    need(applied && accepted && VHO_q1_program_read(root,p,why) && VHO_q5_plan_decode(applied,plan,why),"q6c_source_plan");
    need(plan.binding_key==control["q3_binding_key"].asString(),"q6c_Q3_identity");VHO_Q6_MAPPING m;
    need(VHO_q6_mapping(p,plan,accepted,c,m,why),why.c_str());
    const char *mapping=ST_tensor_metadata(st,"tensor.q6.mapping");need(mapping && m.key==mapping,"q6c_mapping_binding");
    need(PU_Info_state(pu,WT_DEPGRAPH)==Subsect_Missing,"q6c_live_analysis_refusal");
    DSL_HIGH_EPOCH epoch("q6c-selected-device");q5_checkpoint(pu,root,(prefix+".before.B").c_str());epoch.Before_mutation();
    VHO_q6c_worker(m,stage,source_key);q5_checkpoint(pu,PU_Info_tree_ptr(pu),(prefix+".high.B").c_str());
    VHO_q6c_memory();q5_checkpoint(pu,PU_Info_tree_ptr(pu),(prefix+".mid.B").c_str());
#ifdef OPEN64_TENSOR_BRIDGE_TEST
    need(!getenv("OPEN64_Q6C_TEST_REJECT_AFTER_MEMORY"),"q6c_test_reject_after_memory");
#endif
    VHO_q6c_gpu();q5_checkpoint(pu,PU_Info_tree_ptr(pu),(prefix+".low.B").c_str());
    std::string ptx=CG_tensor_selected_ptx();int fd=open((prefix+".ptx").c_str(),O_WRONLY|O_CREAT|O_EXCL,0600);need(fd>=0,"q6c_ptx_output_exists");
    size_t position=0;while(position<ptx.size()) {ssize_t n=write(fd,ptx.data()+position,ptx.size()-position);need(n>0,"q6c_ptx_write");position+=n;}
    need(close(fd)==0,"q6c_ptx_close");epoch.Close();
    Json::Value j;j["schema"]="open64.q6c.device-stage.v1";j["stage"]=stage;j["function"]=ST_name(WN_st(PU_Info_tree_ptr(pu)));
    j["source_key"]=source_key;j["q3_binding_key"]=plan.binding_key;j["mapping_key"]=m.key;j["native_key"]=VHO_q6c_key(PU_Info_tree_ptr(pu));
    j["recipe"]=q1_encode(p);j["buffers"]=Json::Value(Json::arrayValue);for(unsigned i=0;i<p.values.size();++i) if(p.values[i].storage==i) j["buffers"].append(i);
    j["grid_x"]=Json::UInt64(stage=="attention"?m.grid:1);j["block_x"]=Json::UInt64(stage=="attention"?m.threads:1);j["shared_bytes"]=Json::UInt64(stage=="attention"?m.shared_bytes:0);
    j["launch_order"]="prepare -> attention -> finalize; same stream; no host synchronization required between launches";
    j["original_formal_ABI_preserved"]=true;j["source_fallback_retained"]=true;j["search_invocations"]=0;j["alternate_candidates_materialized"]=0;
    j["analyses_expired"]=true;j["gpu_execution_eligible"]=false;j["device_bindings_implemented"]=false;j["stage_operations_lowered"]=true;j["aot_selection"]="deferred";
    write_json((prefix+".json").c_str(),j);return 0;
}
