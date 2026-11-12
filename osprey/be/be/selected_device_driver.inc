/* Copyright (C) 2026 Open64 Project */
/* Backend-driver active-PU capture and selected-only process orchestration.
 * Reuses native mapped writers and the M5 bridge without adding a be.so phase
 * dependency or moving PU contexts inside a phase. Contract:
 * doc/AI-COMPILER-SEARCH-M6-PRODUCTION-ROUTE-v1.0.md. */

/* Query existing file-scope option descriptors, without a new public backend
 * global or parsing a second command language. Absent options stay disabled. */
static std::string BE_DSL_device_option(const char *name) {
    OPTION_GROUP *group=Get_Command_Line_Group(Common_Option_Groups,"LNO");
    if(group) for(OPTION_DESC *option=OGROUP_options(group);ODESC_kind(option)!=OVK_COUNT;++option)
        if(ODESC_name(option) && !strcmp(ODESC_name(option),name) && ODESC_kind(option)==OVK_NAME) {
            const char *value=*(const char **)ODESC_variable(option);return value?value:"";
        }
    return "";
}

/* Wait for this child only. Interrupted waits never change native PU state;
 * crashes and nonzero status refuse device publication, preserving host work. */
static BOOL BE_DSL_device_wait(pid_t child) {
    if(child<0) return FALSE;
    int status=0;pid_t result;
    do {result=waitpid(child,&status,0);} while(result<0 && errno==EINTR);
    return result==child && WIFEXITED(status) && WEXITSTATUS(status)==0;
}

/* Snapshot the current accepted PU in a copy-on-write process: native writer
 * subsection updates and global interning cannot reach the compiling parent.
 * A fresh directory owns all paths, and no existing file is replaced. */
static BOOL BE_DSL_device_capture(PU_Info *current_pu,WN *root,const std::string &path) {
    if(access(path.c_str(),F_OK)==0) return FALSE;
    fflush(NULL);pid_t child=fork();
    if(!child) {
        PU_Info image=*current_pu;PU_Info_next(&image)=NULL;PU_Info_child(&image)=NULL;
        Set_PU_Info_tree_ptr(&image,root);Set_PU_Info_state(&image,WT_TREE,Subsect_InMem);
        Set_PU_Info_depgraph_ptr(&image,NULL);Set_PU_Info_state(&image,WT_DEPGRAPH,Subsect_Missing);
        Write_BE_Maps=FALSE;Write_ALIAS_CLASS_Map=FALSE;Write_ALIAS_CGNODE_Map=FALSE;
        std::string temporary=path+".tmp";
        if(access(temporary.c_str(),F_OK)==0 || !Open_Output_Info(const_cast<char *>(temporary.c_str()))) _exit(1);
        Write_PU_Info(&image);Write_Global_Info(&image);Close_Output_Info();
        if(link(temporary.c_str(),path.c_str())) _exit(1);
        _exit(unlink(temporary.c_str())?1:0);
    }
    return BE_DSL_device_wait(child);
}

/* Run a fixed argv in a separate native tool, never a shell or PATH lookup.
 * Diagnostics remain in private staging even on a signal or exec failure. */
static BOOL BE_DSL_device_lower(const std::string &tool,const std::string &image,
    const std::string &bindings,const std::string &prefix,const std::string &staging) {
    fflush(NULL);pid_t child=fork();
    if(!child) {
        int output=open((staging+"/bridge.stdout").c_str(),O_WRONLY|O_CREAT|O_EXCL,0600);
        int error=open((staging+"/bridge.stderr").c_str(),O_WRONLY|O_CREAT|O_EXCL,0600);
        if(output<0 || error<0 || dup2(output,STDOUT_FILENO)<0 || dup2(error,STDERR_FILENO)<0) _exit(1);
        close(output);close(error);
        char *arguments[]={const_cast<char *>(tool.c_str()),const_cast<char *>("bridge-production"),
            const_cast<char *>(image.c_str()),const_cast<char *>(bindings.c_str()),const_cast<char *>(prefix.c_str()),NULL};
        execv(tool.c_str(),arguments);fprintf(stderr,"selected bridge exec failed: %s\n",strerror(errno));_exit(127);
    }
    return BE_DSL_device_wait(child);
}

/* Publish only this PU's verified complete product via no-replace links. The
 * completion manifest is last; on failure remove only links created here.
 * Failed staging and host output remain available for diagnosis and fallback. */
static BOOL BE_DSL_device_publish(const std::string &staging,const std::string &destination) {
    if(mkdir(destination.c_str(),0700)) return FALSE;
    const char *files[]={"source.B","bridge.stdout","bridge.stderr","selected.admission.json","selected.high.B",
        "selected.mid.B","selected.low.B","selected.ptx","selected.math.cu","selected.selected.json",
        "selected.launch.json","selected.optimized.cu","selected.complete.json"};
    unsigned i=0;
    for(;i<sizeof(files)/sizeof(files[0]);++i)
        if(link((staging+"/"+files[i]).c_str(),(destination+"/"+files[i]).c_str())) break;
    if(i==sizeof(files)/sizeof(files[0])) return TRUE;
    while(i) {--i;unlink((destination+"/"+files[i]).c_str());}
    rmdir(destination.c_str());return FALSE;
}

/* Driver-owned post-LNO boundary. The exact accepted source is mandatory;
 * no option can materialize an estimate, infer a missing plan, or replace the
 * ordinary host path. Each PU receives an independent reserved output family. */
static void BE_DSL_selected_device(PU_Info *current_pu,WN *root) {
    std::string output=BE_DSL_device_option("high_device_output");if(output.empty()) return;
    static BOOL reserved=FALSE,disabled=FALSE;static unsigned ordinal=0;
    if(disabled) return;
    std::string reason,staging,destination;
    if(!root || WN_operator(root)!=OPR_FUNC_ENTRY || !ST_tensor_metadata(WN_st_idx(root),"tensor.high.accepted.schema")) {
        fprintf(stderr,"AI_HIGH_DEVICE 1 SKIP reason=no_selected_high fallback=ordinary_host\n");return;
    }
    std::string tool=BE_DSL_device_option("high_device_tool"),bindings=BE_DSL_device_option("high_search_bindings");
    if(!Run_lno || Opt_Level<3 || Run_ipl || Instrumentation_Enabled ||
        BE_DSL_device_option("high_search")!="on" || BE_DSL_device_option("high_apply")!="on" ||
        PU_lexical_level(*Current_pu)!=GLOBAL_SYMTAB+1) reason="production_scope_or_controls";
    else if(tool.empty() || bindings.empty() || access(tool.c_str(),X_OK)) reason="helper_or_bindings_unavailable";
    else {
        try {
            const char *stamp=ST_tensor_metadata(WN_st_idx(root),"tensor.high.accepted.key");
            if(!stamp || VHO_tensor_selected_key(root)!=stamp) reason="stale_high_exit";
        } catch(const std::exception &) {reason="invalid_high_exit";}
    }
    if(reason.empty() && !reserved) {
        if(mkdir(output.c_str(),0700)) {reason="output_not_fresh";disabled=TRUE;}
        else reserved=TRUE;
    }
    unsigned current=ordinal++;
    if(reason.empty()) {
        char suffix[64];snprintf(suffix,sizeof(suffix),"/pu-%u",current);destination=output+suffix;
        std::string pattern=output+"/.pending-pu-"+std::string(suffix+4)+"-XXXXXX";
        std::vector<char> name(pattern.begin(),pattern.end());name.push_back(0);
        if(!mkdtemp(&name[0])) reason="staging_unavailable";
        else {
            staging=&name[0];
            if(!BE_DSL_device_capture(current_pu,root,staging+"/source.B")) reason="source_checkpoint";
            else if(!BE_DSL_device_lower(tool,staging+"/source.B",bindings,staging+"/selected",staging)) reason="selected_lowering";
            else if(!BE_DSL_device_publish(staging,destination)) reason="publication";
        }
    }
    FILE *trace=VHO_DSL_AI_Opt_Trace && Get_Trace(TP_WOPT2,AI_OPT_TRACE_FLAG)?TFile:NULL;
    if(reason.empty()) {
        fprintf(stderr,"AI_HIGH_DEVICE 1 PUBLISHED pu=%u path=%s search_invocations=0 host_fallback=retained\n",current,destination.c_str());
        if(trace) fprintf(trace,"AI_HIGH_DEVICE 1 PUBLISHED pu=%u path=%s search_invocations=0 host_fallback=retained\n",current,destination.c_str());
    } else {
        fprintf(stderr,"AI_HIGH_DEVICE 1 REFUSED reason=%s staging=%s fallback=ordinary_host\n",reason.c_str(),staging.c_str());
        if(trace) fprintf(trace,"AI_HIGH_DEVICE 1 REFUSED reason=%s staging=%s fallback=ordinary_host\n",reason.c_str(),staging.c_str());
    }
}
