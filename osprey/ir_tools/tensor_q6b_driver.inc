/* Copyright (C) 2026 Open64 Project */
/* Process-isolated selected Q6b High application and independent native audit.
 * Only this driver establishes scope, expires analyses and writes checkpoints.
 * Contract: doc/AI-COMPILER-MLA-Q6B-NATIVE-STAGING-v1.0.md. */
/* Explain actual native actions at function entry, where the established
 * PREOPT workaround can restore them without perturbing inner-loop admission. */
std::string q6b_explanation(const VHO_Q6_MAPPING &m) {
    std::ostringstream s;s<<"Q6b selected native MLA staging: B="<<m.batch<<", H="<<m.heads<<", Q="<<m.queries<<", L="<<m.keys
        <<", R="<<m.latent<<", Ro="<<m.rotary<<", W="<<m.width<<", G="<<m.group<<".\n"
        <<"Prefix quantization, functional updates and query projection run before staging. All paged index bounds are checked before any copy is pending.\n"
        <<"A "<<m.shared_bytes<<"-byte arena with a shared role and alignment 16 contains "<<m.slots<<" raw slot(s), four separate payload/scale streams per slot, and one decoded F32 tile. "
        <<"Contiguous physical token runs come from original index SSA; all four streams share each run. Exact byte counts guard tails without reading padding.\n"
        <<"Each source-ordered maximum, denominator and normalized-weighted-latent pass acquires/issues a raw tile and waits before dequantization. ";
    if(m.slots==2) s<<"After waiting for tile n, issue n+1 into the other raw slot before dequantizing and computing n; startup and final drain are explicit.\n";
    else s<<"Synchronous one-slot delivery stages the next tile only after current consumers complete and release.\n";
    s<<"S8 sign extension or even-low/odd-high U4 unpack and F32 offset-eight subtraction precedes multiplication by the paired F32 token scale.\n"
        <<"Publish decoded writes before all logical head consumers; join consumer completion before release/overwrite; end every slot after the last pass drains.\n"
        <<"Head consumers are logical ordered High loops here; physical GPU participants and barrier instructions are Q6c work. The typed High copy permits synchronous fragments; it does not claim every run is TMA-aligned.\n"
        <<"Global max/denominator/token folds and per-token division retain Q5 source F32 order, including finite/zero guards and masked nonfinite multiplication. No online-softmax reassociation or split sums are introduced.\n"
        <<"Value projection, statistics and all four cache/scale output copies remain after staging. Original formals/ABI allocations remain; intermediate decoded-cache/score/probability traffic stays eliminated.\n"
        <<"This is executable High WHIRL with abstract staging operations, not a mapped device kernel or measured pipeline. A qualified host callback retains exact Q3 source/snapshot, buffer/page/cache/scale generations and publication rules. Device authority remains unimplemented. "
        <<"Search was estimation-only; only the selected fixed plan was transformed and reference-tested. AOT selection remains deferred.";return s.str();
}
#ifdef OPEN64_TENSOR_BRIDGE_TEST
/* Corrupt one actual WN operation, without updating the accepted key. Native
 * audit must reject before publishing any output; no test hook is in production. */
bool q6b_test_remove(WN *n,const std::string &name) {
    if(WN_operator(n)==OPR_BLOCK) for(WN *s=WN_first(n);s;s=WN_next(s)) {
        if(WN_operator(s)==OPR_CALL && name==ST_name(WN_st(s))) {WN_EXTRACT_FromBlock(n,s);return true;}
        if(q6b_test_remove(s,name)) return true;
    }
    if(WN_operator(n)!=OPR_BLOCK) for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i) && q6b_test_remove(WN_kid(n,i),name)) return true;
    return false;
}
/* A same-width pointer-view change must alter the complete native key. */
bool q6b_test_view(WN *n,TY_IDX type) {
    if(WN_operator(n)==OPR_TAS && WN_ty(n)!=type) {WN_set_ty(n,type);return true;}
    if(WN_operator(n)==OPR_BLOCK) {for(WN *s=WN_first(n);s;s=WN_next(s)) if(q6b_test_view(s,type)) return true;}
    else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i) && q6b_test_view(WN_kid(n,i),type)) return true;
    return false;
}
#endif
/* Read one accepted Q5 or Q6b image using the existing native reader. Isolated
 * construction failure leaves input and the immediately preceding image intact. */
int q6b_command(int argc,char **argv) {
    bool apply=!strcmp(argv[1],"q6-apply");
    bool reject_after_build=false;
#ifdef OPEN64_TENSOR_BRIDGE_TEST
    if(apply && argc==9 && !strcmp(argv[8],"--test-reject-after-build")) {reject_after_build=true;--argc;}
#endif
    if(apply) {
        need(argc==8 && !strcmp(argv[7],"--opt-level=3"),"q6-apply selected.B control.json before.B after.B manifest.json --opt-level=3");
        need(access(argv[4],F_OK)!=0 && access(argv[5],F_OK)!=0 && access(argv[6],F_OK)!=0,"q6_application_output_exists");
    } else need(argc==4
#ifdef OPEN64_TENSOR_BRIDGE_TEST
        || (argc==6 && !strcmp(argv[4],"--test-remove"))
#endif
        ,"q6-audit high.B keys.json");
    if(!apply) need(access(argv[3],F_OK)!=0,"q6_audit_output_exists");
    Json::Value control;VHO_Q6_CONTROL c;std::string control_wire;
    if(apply) {control_wire=q5_wire(argv[3]);c=q6_control(control_wire,control);}initialize(false);
    Open_Input_Info(argv[2]);Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);PU_Info *pu=Read_Global_Info(NULL);
    need(pu && !PU_Info_next(pu) && !PU_Info_child(pu),"q6b_single_PU");Read_Local_Info(MEM_pu_nz_pool_ptr,pu);
    Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);Initialize_Special_Global_Symbols();
    WN *root=PU_Info_tree_ptr(pu);ST_IDX st=WN_st_idx(root);const char *applied=ST_tensor_metadata(st,"tensor.q5.applied"),*accepted=ST_tensor_metadata(st,"tensor.q5.accepted.key");
    need(applied && accepted,"q6b_requires_accepted_Q5");VHO_TENSOR_PROGRAM p;VHO_Q5_PLAN plan;std::string why;
    need(VHO_q1_program_read(root,p,why) && VHO_q5_plan_decode(applied,plan,why),"q6b_source_or_descriptor");
    if(!apply) {
        const char *schema=ST_tensor_metadata(st,"tensor.q6.schema"),*stamp=ST_tensor_metadata(st,"tensor.q6.accepted.key"),*wire=ST_tensor_metadata(st,"tensor.q6.control");
        need(schema && !strcmp(schema,"mla-native-staging-v1") && stamp && wire,"q6b_unsealed_native_image");
#ifdef OPEN64_TENSOR_BRIDGE_TEST
        if(argc==6) need(!strcmp(argv[5],"view-type")?q6b_test_view(root,ST_type(WN_st(WN_formal(root,0)))):q6b_test_remove(root,std::string("__open64_mla_stage_")+argv[5]),"q6b_test_operation_missing");
#endif
        std::string key=VHO_q6_stage_key(root);need(key==stamp,"q6b_stale_native_key");c=q6_control(wire,control);
        VHO_Q6_MAPPING m;need(VHO_q6_mapping(p,plan,accepted,c,m,why),why.c_str());
        const char *mapping=ST_tensor_metadata(st,"tensor.q6.mapping");need(mapping && m.key==mapping && plan.binding_key==control["q3_binding_key"].asString(),"q6b_stale_mapping_or_Q3");
        Verify_SYMTAB(CURRENT_SYMTAB);need(WN_verifier(root),"q6b_native_WN_verifier");Json::Value j;j["schema"]="open64.q6b.native-audit.v1";
        j["key"]=key;j["gpu_execution_eligible"]=false;j["aot_selection"]="deferred";write_json(argv[3],j);return 0;
    }
    need(!ST_tensor_metadata(st,"tensor.q6.schema"),"q6b_already_applied");
    std::string native=DSL_NATIVE_TREE_KEY().Build(root);need(native==accepted,"q6b_stale_Q5_native_key");
    need(plan.binding_key==control["q3_binding_key"].asString(),"q6b_stale_Q3_snapshot");
    VHO_Q6_MAPPING mapping;need(VHO_q6_mapping(p,plan,native,c,mapping,why),why.c_str());std::vector<VHO_Q6_ACTION> actions;
    need(VHO_q6_pipeline(mapping,actions,why) && VHO_q6_verify_pipeline(mapping,actions,why),why.c_str());
    need(PU_Info_state(pu,WT_DEPGRAPH)==Subsect_Missing,"q6b_live_analysis_refusal");
    DSL_HIGH_EPOCH epoch("q6b-native-staging");DSL_HIGH_QUANTIZED_FACTS rejected;
    need(!LNO_DSL_capture_quantized_high(root,p,epoch,rejected,why) && why=="q1_source_facts_do_not_describe_selected_q5","q6b_stale_source_facts_refusal");
    q5_checkpoint(pu,root,argv[4]);unsigned types=TY_Table_Size(),locals=ST_Table_Size(CURRENT_SYMTAB),globals=ST_Table_Size(GLOBAL_SYMTAB),tcons=TCON_Table_Size();epoch.Before_mutation();
    BOOL simplify=WN_Simplifier_Enable(FALSE);WN *staged=WN_COPY_Tree(root);WN_func_body(staged)=VHO_q6_build_high(p,plan,mapping,root);
    need(!reject_after_build,"q6b_test_reject_after_build");
    need(TCON_Table_Size()==tcons,"q6b_source_constant_reuse");WN_Simplifier_Enable(simplify);
    std::string explanation=q6b_explanation(mapping);WN *comment=WN_CreateComment(explanation.c_str());WN_Set_Linenum(comment,WN_Get_Linenum(root));
    Set_ST_Srcpos(*WN_st(comment),WN_Get_Linenum(root));
    WN_INSERT_BlockBefore(WN_func_body(staged),WN_first(WN_func_body(staged)),comment);
    ST_tensor_bind_metadata(st,"tensor.q6.schema","mla-native-staging-v1");ST_tensor_bind_metadata(st,"tensor.q6.mapping",mapping.key.c_str());
    ST_tensor_bind_metadata(st,"tensor.q6.control",control_wire.c_str());ST_tensor_bind_metadata(st,"tensor.q6.explanation",explanation.c_str());
    Set_PU_Info_tree_ptr(pu,staged);Verify_SYMTAB(CURRENT_SYMTAB);need(WN_verifier(staged) && DSL_Gatekeeper_Verify_No_Native_PU(pu,stderr),"q6b_native_verification");
    std::string key=VHO_q6_stage_key(staged);ST_tensor_bind_metadata(st,"tensor.q6.accepted.key",key.c_str());epoch.Close();q5_checkpoint(pu,staged,argv[5]);
    Json::Value j;j["schema"]="open64.q6b.native-staging.v1";j["function"]=ST_name(st);j["recipe"]=q1_encode(p);j["buffers"]=Json::Value(Json::arrayValue);
    for(unsigned i=0;i<p.values.size();++i) if(p.values[i].storage==i) j["buffers"].append(i);
    j["selected_plan"]=VHO_q5_plan_encode(plan);j["q3_binding_key"]=plan.binding_key;j["mapping_key"]=mapping.key;j["native_key"]=key;
    j["shared_bytes"]=Json::UInt64(mapping.shared_bytes);j["raw_slots"]=Json::UInt64(mapping.slots);j["delivery"]=c.delivery;
    j["added_types"]=(unsigned)(TY_Table_Size()-types);j["added_locals"]=(unsigned)(ST_Table_Size(CURRENT_SYMTAB)-locals);j["added_globals"]=(unsigned)(ST_Table_Size(GLOBAL_SYMTAB)-globals);j["new_TCONs"]=0;
    j["candidates_materialized"]=1;j["exploration_native_mutations"]=0;j["analyses_expired"]=true;j["analysis_policy"]="isolated active PU; no live managers admitted; normal backend rebuilds analyses";
    j["gpu_execution_eligible"]=false;j["device_bindings_implemented"]=false;j["stage_operations_lowered"]=false;
    j["requires_stage_operation_implementation"]=true;j["aot_selection"]="deferred";write_json(argv[6],j);return 0;
}
