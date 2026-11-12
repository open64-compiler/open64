/* Copyright (C) 2026 Open64 Project */
/* Driver-owned source admission, fresh PU contexts and selected checkpoints.
 * Included only by the separately linked bridge executable. Native lowering
 * phases never traverse/switch PUs; search is never called by this command.
 * Contract: doc/AI-COMPILER-SEARCH-M5-DEVICE-BRIDGE-v1.0.md. */

#ifdef OPEN64_TENSOR_BRIDGE_TEST
std::string bridge_test_fault;
/* Native mutation controls exist only in the separately built test driver. */
void bridge_test_mutation(bool source) {
    WN *root=PU_Info_tree_ptr(Current_PU_Info);ST_IDX st=WN_st_idx(root);
    if(bridge_test_fault=="tree") {WN *loop=find_loop(root);need(loop!=NULL,"test loop missing");WN_const_val(WN_kid1(WN_end(loop)))+=1;}
    if(bridge_test_fault=="comment") {
        WN *comment=WN_first(WN_func_body(root));need(comment&&WN_operator(comment)==OPR_COMMENT,"test comment missing");
        Set_ST_name(*WN_st(comment),Save_Str("stale selected explanation"));
    }
    if(bridge_test_fault=="abi") {
        TY_IDX function=ST_pu_type(WN_st(root));Set_TYLIST_type(Tylist_Table[TY_tylist(function)],MTYPE_To_TY(MTYPE_F4));
    }
    if(source) {
        const char *names[]={"width","policy","resources","model","controls","numerical","scratch","source","facts"};
        for(unsigned i=0;i<9;++i) if(bridge_test_fault==names[i])
            ST_tensor_bind_metadata(st,(std::string("tensor.high.selected.")+names[i]).c_str(),"stale");
    } else {
        if(bridge_test_fault=="barrier") remove_barrier(root);
        if(bridge_test_fault=="mask") {
            WN *call=find_collective(root,"__shfl_down_sync");
            if(call) WN_const_val(WN_kid0(WN_kid0(call)))=0xffff;
        }
        if(bridge_test_fault=="collective_abi") {
            WN *call=find_collective(root,"__syncthreads");
            if(call) Set_TYLIST_type(Tylist_Table[TY_tylist(ST_pu_type(WN_st(call)))],MTYPE_To_TY(MTYPE_F4));
        }
        if(bridge_test_fault=="argument") ST_tensor_bind_metadata(st,"tensor.argument.0.bytes","stale");
        if(bridge_test_fault=="extra_search") ST_tensor_bind_metadata(st,"tensor.kernel.attention_search","stale");
        const ST *symbol;ST_IDX idx;
        if(bridge_test_fault=="shared_bound") FOREACH_SYMBOL(CURRENT_SYMTAB,symbol,idx)
            if(ST_device_symbol_kind(*symbol)==DEVICE_SYMBOL_SHARED) {
                ARB_HANDLE bound=TY_arb(ST_type(symbol));Set_ARB_ubnd_val(bound,ARB_ubnd_val(bound)+1);break;
            }
        if(bridge_test_fault=="coordinate_type") FOREACH_SYMBOL(GLOBAL_SYMTAB,symbol,idx)
            if(!strcmp(ST_name(symbol),"threadIdx")) {Set_TY_size(ST_type(symbol),16);break;}
        const char *names[]={"block_x","grid_x","shared_bytes","attention_layout","split_width"};
        for(unsigned i=0;i<5;++i) if(bridge_test_fault==names[i])
            ST_tensor_bind_metadata(st,(std::string("tensor.kernel.")+names[i]).c_str(),"stale");
    }
}
/* Select the designated fresh PU under test-driver ownership and invalidate
 * precisely the product that the next real production phase must admit. */
void bridge_test_stage_gate(const std::vector<PU_Info *> &pus,const char *gate,const char *mutation) {
    if(bridge_test_fault!=gate) return;
    need(!pus.empty() && DSL_Builder_Select_PU(pus[0]),"test stage PU missing");
    std::string saved=bridge_test_fault;bridge_test_fault=mutation;
    bridge_test_mutation(false);bridge_test_fault=saved;
}
#endif

/* Extract only verified native Low argument/launch records for existing host
 * adapters. This allocates JSON facts, never a new compiler scheduling choice. */
Json::Value bridge_launch_kernel(std::map<UINT32,Json::Value> &storages) {
    WN *root=PU_Info_tree_ptr(Current_PU_Info);ST_IDX st=WN_st_idx(root);Json::Value kernel;
    kernel["name"]=ST_name(st);const char *fields[]={"grid_x","block_x","shared_bytes","mapping"};
    for(unsigned i=0;i<4;++i) kernel[fields[i]]=metadata(st,(std::string("tensor.kernel.")+fields[i]).c_str());
    kernel["arguments"]=Json::Value(Json::arrayValue);
    for(INT a=0;a<WN_num_formals(root);++a) {
        std::ostringstream prefix;prefix<<"tensor.argument."<<a<<'.';Json::Value arg,storage;
        const char *fields[]={"storage","role","ordinal","elements","bytes","dtype","name"};
        for(unsigned i=0;i<7;++i) storage[fields[i]]=metadata(st,(prefix.str()+fields[i]).c_str());
        UINT32 id=strtoul(storage["storage"].asCString(),NULL,10);
        need(!storages.count(id)||storages[id]==storage,"bridge inconsistent storage join");storages[id]=storage;
        arg["storage"]=storage["storage"];arg["dtype"]=storage["dtype"];kernel["arguments"].append(arg);
    }return kernel;
}

/* Read one sealed source in a separate compiler process. Parent starts its
 * fresh device context only after this child has validated the entire source. */
Json::Value selected_admission(const char *image,const VHO_TENSOR_PROGRAM &original) {
    initialize(false);Open_Input_Info(const_cast<char *>(image));Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
    PU_Info *pu=Read_Global_Info(NULL);need(pu&&!PU_Info_next(pu)&&!PU_Info_child(pu),"bridge requires one selected source PU");
    Read_Local_Info(MEM_pu_nz_pool_ptr,pu);Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);
    Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);
    WN *root=PU_Info_tree_ptr(pu);ST_IDX st=WN_st_idx(root);
#ifdef OPEN64_TENSOR_BRIDGE_TEST
    bridge_test_mutation(true);
#endif
    need(metadata(st,"tensor.high.accepted.schema")=="mla-selected-native-v1" &&
        metadata(st,"tensor.high.selected.schema")=="lno-selected-mla-v1","bridge unsealed source");
    UINT32 symbols=ST_Table_Size(CURRENT_SYMTAB),types=TY_Table_Size();
    std::string key=VHO_tensor_selected_key(root);
    need(key==metadata(st,"tensor.high.accepted.key"),"bridge stale selected source/key/COMMENT");
    WN *source_comment=WN_first(WN_func_body(root));
    need(source_comment && WN_operator(source_comment)==OPR_COMMENT,"bridge source explanation missing");
    need(!original.fuse&&!original.auto_chunks&&!original.search_selected &&
        VHO_tensor_recipe_key(original)==metadata(st,"tensor.high.recipe_key"),"bridge semantic recipe mismatch");
    need(metadata(st,"tensor.high.buffer_contract")=="disjoint-storage-formals" &&
        metadata(st,"tensor.high.selected.scratch")=="private-role0-formals; capacity-proved","bridge storage contract");
    VHO_TENSOR_PROGRAM p=original;
    const char *names[]={"tensor.high.selected.width","tensor.high.selected.policy"};UINT32 values[2];
    for(unsigned i=0;i<2;++i) {
        std::string value=metadata(st,names[i]);
        need(!value.empty() && value.size()<6 && value.find_first_not_of("0123456789")==std::string::npos,"bridge fixed decision syntax");
        values[i]=strtoul(value.c_str(),NULL,10);std::ostringstream canonical;canonical<<values[i];
        need(canonical.str()==value,"bridge noncanonical decision");
    }
    p.fuse=p.cooperative=p.fuse_attention=p.register_latent=true;p.split_width=values[0];p.chunk_policy=values[1];
    VHO_TENSOR_SCHEDULE plan;need(VHO_tensor_schedule(p,&plan,stderr),"bridge fixed schedule invalid");
    for(size_t i=0;i<plan.kernels.size();++i) {
        UINT64 grid=VHO_tensor_grid(p,plan.kernels[i]);
        need(grid && grid<=UINT64(0xffffffffU) && plan.kernels[i].threads && plan.kernels[i].threads<=1024,
            "bridge participant domain exceeds U4/CTA ABI");
        need(VHO_tensor_shared_bytes(p,plan.kernels[i])<=48*1024,"bridge static shared memory requires unsupported opt-in ABI");
    }
    std::string resources=metadata(st,"tensor.high.selected.resources");
    size_t colon=resources.find(':');need(colon!=std::string::npos && colon>0 && colon<10,"bridge resource identity");
    std::string length=resources.substr(0,colon);need(length.find_first_not_of("0123456789")==std::string::npos,"bridge resource ID length");
    size_t id_size=strtoul(length.c_str(),NULL,10);
    need(id_size<=resources.size() && colon+1+id_size<resources.size() &&
        resources.compare(colon+1+id_size,7,"/sm_90/")==0,"bridge PTX requires accepted sm_90 resources");
    need(symbols==ST_Table_Size(CURRENT_SYMTAB)&&types==TY_Table_Size(),"bridge admission mutated native tables");
    Json::Value result;result["schema"]="open64.selected-mla-bridge.v1";result["source_image"]=image;
    result["accepted_key"]=key;result["selected"]=encode(p);result["resources"]=resources;
    result["source_comment"]=Index_To_Str(WN_GetComment(source_comment));
    result["model"]=metadata(st,"tensor.high.selected.model");result["controls"]=metadata(st,"tensor.high.selected.controls");
    result["numerical"]=metadata(st,"tensor.high.selected.numerical");result["search_invocations"]=0;
    result["source_unchanged"]=true;return result;
}
/* Driver owns all PU contexts/checkpoints. Every stage consumes the preceding
 * verified native product; failure leaves source and legacy fallback untouched. */
int selected_bridge_command(int argc,char **argv) {
    if(!strcmp(argv[1],"bridge-audit") || !strcmp(argv[1],"bridge-ptx") || !strcmp(argv[1],"bridge-code-key")) {
        need(argc>=4,"bridge audit/ptx requires an image and level/output");
        bool emit=!strcmp(argv[1],"bridge-ptx"),code_key=!strcmp(argv[1],"bridge-code-key");const char *level=(emit||code_key)?"low":argv[3];
#ifdef OPEN64_TENSOR_BRIDGE_TEST
        if(argc==6 && !strcmp(argv[4],"--test-fault")) {bridge_test_fault=argv[5];argc=4;}
#endif
        need(argc==4 || (!emit && !code_key && argc==5 && !strcmp(level,"low")),"bridge-audit image.B high|mid|low [launch.json] | bridge-ptx low.B output.ptx | bridge-code-key image.B keys.json");initialize(false);
        Open_Input_Info(argv[2]);Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
        PU_Info *pu=Read_Global_Info(NULL);need(pu!=NULL,"bridge empty image");unsigned count=0;std::string ptx;
        Json::Value result;result["schema"]="open64.device-tensor-launch.v1";result["kernels"]=Json::Value(Json::arrayValue);
        std::map<UINT32,Json::Value> storages;
        for(;pu;pu=PU_Info_next(pu)) {
            need(!PU_Info_child(pu),"bridge nested image");Read_Local_Info(MEM_pu_nz_pool_ptr,pu);
            Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);
#ifdef OPEN64_TENSOR_BRIDGE_TEST
            bridge_test_mutation(false);
#endif
            if(code_key) result["code_keys"][ST_name(PU_Info_proc_sym(pu))]=DSL_NATIVE_TREE_KEY(false).Build(PU_Info_tree_ptr(pu));
            else VHO_tensor_verify_bridge(level);
            if(emit) {std::string text=CG_tensor_selected_ptx();if(count) text=text.substr(text.find(".visible .entry"));ptx+=text;}
            if(argc==5) result["kernels"].append(bridge_launch_kernel(storages));++count;
        }
        if(emit) write_source(argv[3],ptx);
        if(code_key) {result["schema"]="open64.native-executable-keys.v1";write_json(argv[3],result);}
        if(argc==5) {
            result["storages"]=Json::Value(Json::arrayValue);
            for(std::map<UINT32,Json::Value>::const_iterator i=storages.begin();i!=storages.end();++i) result["storages"].append(i->second);
            write_json(argv[4],result);
        }
        printf("bridge_%s=PASS level=%s kernels=%u\n",code_key?"code_extraction":"audit",level,count);return 0;
    }
#ifdef OPEN64_TENSOR_BRIDGE_TEST
    if(argc==7 && !strcmp(argv[5],"--test-fault")) {bridge_test_fault=argv[6];argc=5;}
#endif
    need(argc==5,"bridge selected-high.B original-recipe.json output-prefix");
    std::string prefix=argv[4],admission=prefix+".admission.json";
    const char *suffixes[]={".admission.json",".high.B",".mid.B",".low.B",".ptx",".math.cu",".selected.json"};
    for(unsigned i=0;i<7;++i) need(access((prefix+suffixes[i]).c_str(),F_OK)!=0,"bridge output exists");
    VHO_TENSOR_PROGRAM original=decode(argv[3]);fflush(NULL);pid_t child=fork();need(child>=0,"bridge admission fork");
    if(!child) {
        try {write_json(admission.c_str(),selected_admission(argv[2],original));fflush(NULL);_exit(0);}
        catch(const std::exception &e) {fprintf(stderr,"bridge admission: %s\n",e.what());fflush(NULL);_exit(1);}
    }
    int status=0;need(waitpid(child,&status,0)==child&&WIFEXITED(status)&&!WEXITSTATUS(status),"bridge source refused; original fallback retained");
    Json::Value accepted=search_read(admission.c_str());VHO_TENSOR_PROGRAM p=decode_json(accepted["selected"]);
    VHO_TENSOR_SCHEDULE plan;need(VHO_tensor_schedule(p,&plan,stderr),"bridge schedule");
    initialize(true);Handle_Signals();BE_symtab_initialize_be_scopes();BE_symtab_alloc_scope_level(GLOBAL_SYMTAB);
    Scope_tab[GLOBAL_SYMTAB].st_tab->Register(*Be_scope_tab[GLOBAL_SYMTAB].be_st_tab);
    std::vector<PU_Info *> pus;
    for(size_t i=0;i<plan.kernels.size();++i) {
        PU_Info *pu=DSL_Builder_Create_Minimal_PU(kernel_symbol(p,plan.kernels[i].id).c_str());
        need(pu&&DSL_Builder_Select_PU(pu),"bridge create worker");
        /* Builder-only checkpoints use the malloc sentinel pool. Native
         * lowering/simplifier map growth requires a real initialized pool.
         * Replace the still-empty map under driver ownership before building. */
        WN_MAP_TAB *builder_map=Current_Map_Tab;
        Current_Map_Tab=WN_MAP_TAB_Create(MEM_pu_pool_ptr);PU_Info_maptab(pu)=Current_Map_Tab;
        WN_MAP_TAB_Delete(builder_map);
        ST &function=St_Table[PU_Info_proc_sym(pu)];Set_ST_device_symbol_kind(function,DEVICE_SYMBOL_KERNEL);Set_ST_export(function,EXPORT_PREEMPTIBLE);
        UINT32 file=DSL_Builder_Register_Source_File(pu,p.source_file.c_str());USRCPOS source;USRCPOS_clear(source);
        USRCPOS_filenum(source)=file;USRCPOS_linenum(source)=p.operations[plan.kernels[i].root].source_line;
        if(!USRCPOS_linenum(source)) USRCPOS_linenum(source)=1;
        VHO_tensor_selected_worker(p,plan.kernels[i].id,USRCPOS_srcpos(source),accepted["accepted_key"].asString(),accepted["source_comment"].asString());pus.push_back(pu);
    }
    checkpoint(pus,(prefix+".high.B").c_str());
#ifdef OPEN64_TENSOR_BRIDGE_TEST
    bridge_test_stage_gate(pus,"mid_gate","tree");
#endif
    for(size_t i=0;i<pus.size();++i) {
        need(DSL_Builder_Select_PU(pus[i]),"bridge select memory PU");
        BE_symtab_alloc_scope_level(CURRENT_SYMTAB);
        Scope_tab[CURRENT_SYMTAB].st_tab->Register(*Be_scope_tab[CURRENT_SYMTAB].be_st_tab);
        VHO_tensor_selected_memory();
        Scope_tab[CURRENT_SYMTAB].st_tab->Un_register(*Be_scope_tab[CURRENT_SYMTAB].be_st_tab);
        Be_scope_tab[CURRENT_SYMTAB].be_st_tab->Clear();
    }
    checkpoint(pus,(prefix+".mid.B").c_str());
#ifdef OPEN64_TENSOR_BRIDGE_TEST
    bridge_test_stage_gate(pus,"low_gate","comment");
#endif
    std::string ptx;
    for(size_t i=0;i<pus.size();++i) {
        need(DSL_Builder_Select_PU(pus[i]),"bridge select GPU PU");VHO_tensor_selected_gpu();
#ifdef OPEN64_TENSOR_BRIDGE_TEST
        if(!i) bridge_test_stage_gate(pus,"ptx_gate","comment");
#endif
        std::string entry=CG_tensor_selected_ptx();
        if(i) {size_t pos=entry.find(".visible .entry");need(pos!=std::string::npos,"bridge PTX entry");entry=entry.substr(pos);}
        ptx+=entry;
    }
    checkpoint(pus,(prefix+".low.B").c_str());
    write_json((prefix+".selected.json").c_str(),encode(p));
    write_source((prefix+".math.cu").c_str(),"// Math ABI only: native PTX owns all MLA arithmetic, memory and scheduling.\n#include <math.h>\nextern \"C\" __device__ __noinline__ float __open64_exp_f32(float x) { return expf(x); }\nextern \"C\" __device__ __noinline__ float __open64_log_f32(float x) { return logf(x); }\n");
    write_source((prefix+".ptx").c_str(),ptx);
    printf("selected_bridge=PASS kernels=%lu search_invocations=0 source_unchanged=true high=verified mid=native_ARRAY_SCF low=GPU_mapped ptx=native_scalar\n",(unsigned long)pus.size());return 0;
}
