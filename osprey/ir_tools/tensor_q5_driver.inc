/* Copyright (C) 2026 Open64 Project */
/* Process-owned selected-only Q5 High application. Exact native input/source
 * gates precede construction; only a verified result is published. Fresh compiler
 * process owns all state and no live DU/alias/dependence manager is admitted.
 * Design: doc/AI-COMPILER-MLA-Q5-SELECTED-PLANS-v1.0.md. */
/* Read a bounded canonical native plan string, not a runtime authority file. */
std::string q5_wire(const char *path) {
    std::ifstream f(path);need(f.good(),"q5_plan_file");f.seekg(0,std::ios::end);need(f.tellg()>=0 && f.tellg()<=16*1024*1024,"q5_plan_extent");f.seekg(0);
    return std::string((std::istreambuf_iterator<char>(f)),std::istreambuf_iterator<char>());
}
/* Reopened PUs are reader/driver-owned, not builder-owned. Copy subsection
 * state in a child and write only the active PU; never select another scope. */
void q5_checkpoint(PU_Info *pu,WN *root,const char *path) {
    std::string temporary=std::string(path)+".tmp";
    need(access(path,F_OK)!=0 && access(temporary.c_str(),F_OK)!=0,"q5_checkpoint_exists");
    fflush(NULL);pid_t child=fork();need(child>=0,"q5_checkpoint_fork");
    if(!child) {
        PU_Info image=*pu;PU_Info_next(&image)=NULL;PU_Info_child(&image)=NULL;
        Set_PU_Info_tree_ptr(&image,root);Set_PU_Info_state(&image,WT_TREE,Subsect_InMem);
        // Reader-only/unloaded optimizer sections are not current analyses.
        // Match the established standalone Q1 rebind snapshot protocol.
        for(unsigned i=0;i<WT_SUBSECTIONS;++i) if(i!=WT_TREE && i!=WT_SYMTAB && i!=WT_REGIONS) {
            PU_Info_subsect_ptr(&image,i)=NULL;Set_PU_Info_state(&image,i,Subsect_Missing);
        }
        Irb_File_Name=const_cast<char *>(temporary.c_str());
        if(!Open_Output_Info(Irb_File_Name)) _exit(1);
        Write_PU_Info(&image);Write_Global_Info(&image);Close_Output_Info();
        if(link(temporary.c_str(),path)) _exit(1);_exit(unlink(temporary.c_str())?1:0);
    }
    int status=0;need(waitpid(child,&status,0)==child && WIFEXITED(status) && !WEXITSTATUS(status),"q5_checkpoint_failed");
}
/* Native source identity is returned separately for the owner to seal into a
 * selected request after normal reader/type-cache bootstrap. No algorithm is
 * constructed and the input file is unchanged. */
int q5_command(int argc,char **argv) {
    bool apply=!strcmp(argv[1],"q5-apply");need((apply?argc==8:argc==4),"q5 usage: key input.B output.json | apply input.B plan.json before.B after.B manifest.json");
    if(apply) need(!strcmp(argv[7],"--opt-level=3"),"q5_requires_explicit_O3");
    if(apply) need(access(argv[4],F_OK)!=0 && access(argv[5],F_OK)!=0 && access(argv[6],F_OK)!=0,"q5_output_exists");
    else need(access(argv[3],F_OK)!=0,"q5_key_output_exists");initialize(false);
    Open_Input_Info(argv[2]);Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);PU_Info *pu=Read_Global_Info(NULL);
    need(pu && !PU_Info_next(pu) && !PU_Info_child(pu),"q5_single_PU");Read_Local_Info(MEM_pu_nz_pool_ptr,pu);
    Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);
    // Reading types does not initialize the backend's MTYPE-to-TY cache.
    // Use the normal backend service before creating any typed local/LOAD.
    Initialize_Special_Global_Symbols();
    WN *root=PU_Info_tree_ptr(pu);VHO_TENSOR_PROGRAM p;std::string why;need(VHO_q1_program_read(root,p,why),why.c_str());
    need(!ST_tensor_metadata(WN_st_idx(root),"tensor.q5.applied"),"q5_already_applied");
    std::string native=DSL_NATIVE_TREE_KEY().Build(root),source=VHO_tensor_quantized_high_key(p);
    if(!apply) {Json::Value j;j["source_key"]=source;j["native_key"]=native;write_json(argv[3],j);return 0;}
    VHO_Q5_PLAN plan;need(VHO_q5_plan_decode(q5_wire(argv[3]),plan,why),why.c_str());
    need(plan.source_key==source && !plan.native_key.empty() && plan.native_key==native,"q5_stale_native_or_source");
    VHO_Q5_LAYOUT layout;if(plan.width) need(VHO_q5_layout(p,layout,why),why.c_str());
    need(PU_Info_state(pu,WT_DEPGRAPH)==Subsect_Missing,"q5_live_dependences_not_supported");
    DSL_HIGH_EPOCH epoch("q5-selected-application");DSL_HIGH_QUANTIZED_FACTS facts;
    need(LNO_DSL_capture_quantized_high(root,p,epoch,facts,why) && LNO_DSL_revalidate_quantized_high(root,p,epoch,facts,why),why.c_str());
    q5_checkpoint(pu,root,argv[4]);
    unsigned types=TY_Table_Size(),tcons=TCON_Table_Size();epoch.Before_mutation();need(!facts.Is_live(epoch),"q5_fact_invalidation");
    // No native construction is performed for unselected alternatives. Process
    // failure abandons all staged memory; original input/before image survives.
    BOOL simplify=WN_Simplifier_Enable(FALSE);
    WN *staged=plan.width?WN_COPY_Tree(root):root;
    if(plan.width) WN_func_body(staged)=VHO_q5_build_high(p,plan,root);
    else WN_INSERT_BlockBefore(WN_func_body(staged),WN_first(WN_func_body(staged)),WN_CreateComment(VHO_q5_explanation(plan,&p).c_str()));
    std::string selected_key=DSL_NATIVE_TREE_KEY().Build(staged);
    need(types==TY_Table_Size() && tcons==TCON_Table_Size(),"q5_global_interning_boundary");
    WN_Simplifier_Enable(simplify);
    Set_PU_Info_tree_ptr(pu,staged);need(DSL_Gatekeeper_Verify_No_Native_PU(pu,stderr),"q5_native_verification");
    ST_tensor_bind_metadata(WN_st_idx(staged),"tensor.q5.applied",VHO_q5_plan_encode(plan).c_str());
    ST_tensor_bind_metadata(WN_st_idx(staged),"tensor.q5.accepted.key",selected_key.c_str());
    DSL_HIGH_QUANTIZED_FACTS rejected;
    need(!LNO_DSL_capture_quantized_high(staged,p,epoch,rejected,why) && why=="q1_source_facts_do_not_describe_selected_q5","q5_source_fact_refusal");
    epoch.Close();q5_checkpoint(pu,staged,argv[5]);Json::Value j;
    j["schema"]="open64.q5.selected-high-result.v1";j["function"]=ST_name(WN_st(staged));j["recipe"]=q1_encode(p);
    j["buffers"]=Json::Value(Json::arrayValue);for(unsigned i=0;i<p.values.size();++i) if(p.values[i].storage==i) j["buffers"].append(i);
    j["minimum_optimization_level"]=3;j["selected_plan"]=VHO_q5_plan_encode(plan);j["candidates_materialized"]=1;j["exploration_native_mutations"]=0;
    j["added_pointer_types"]=(unsigned)(TY_Table_Size()-types);j["new_TCONs"]=0;j["old_facts_expired"]=true;j["analysis_policy"]="no live managers admitted; normal backend rebuilds DU/alias/access/dependences";
    j["native_key"]=selected_key;j["gpu_mapping_qualified"]=false;write_json(argv[6],j);return 0;
}
