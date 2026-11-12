/* Copyright (C) 2026 Open64 Project */
/* Corruption-only regression in the standalone process, never in be/search.
 * Design: doc/AI-COMPILER-MLA-Q1-PRODUCTION-v1.0.md. */
/* PREOPT may discard every COMMENT; copied operation facts must not depend on
 * explanation text or positions. Keep detached nodes for process lifetime. */
void q1_strip_comments(WN *node) {
    if(WN_operator(node)==OPR_BLOCK) {
        for(WN *s=WN_first(node);s;) {WN *next=WN_next(s);
            if(WN_operator(s)==OPR_COMMENT) WN_EXTRACT_FromBlock(node,s);
            else q1_strip_comments(s);s=next;
        }
    } else for(INT i=0;i<WN_kid_count(node);++i) if(WN_kid(node,i)) q1_strip_comments(WN_kid(node,i));
}
/* Build fresh untagged induction declarations to model metadata loss without
 * relying on a non-existent metadata erase API or changing mapped layout. */
void q1_untagged_declarations(WN *node,std::map<ST_IDX,ST_IDX> &renames) {
    if(WN_operator(node)==OPR_DO_LOOP) {
        ST_IDX old=WN_st_idx(WN_index(node));
        if(ST_tensor_metadata(old,"tensor.q1.operation") && !renames.count(old)) {
            ST *s=New_ST(CURRENT_SYMTAB);ST_Init(s,Save_Str("q1_untagged_axis"),CLASS_VAR,SCLASS_AUTO,EXPORT_LOCAL,ST_type(St_Table[old]));
            renames[old]=ST_st_idx(s);
        }
    }
    if(WN_operator(node)==OPR_BLOCK) {for(WN *s=WN_first(node);s;s=WN_next(s)) q1_untagged_declarations(s,renames);}
    else for(INT i=0;i<WN_kid_count(node);++i) if(WN_kid(node,i)) q1_untagged_declarations(WN_kid(node,i),renames);
}
/* Rename all native references consistently, preserving the scalar algorithm. */
void q1_rename(WN *node,const std::map<ST_IDX,ST_IDX> &renames) {
    if(OPCODE_has_sym(WN_opcode(node)) && renames.count(WN_st_idx(node))) WN_st_idx(node)=renames.find(WN_st_idx(node))->second;
    if(WN_operator(node)==OPR_BLOCK) {for(WN *s=WN_first(node);s;s=WN_next(s)) q1_rename(s,renames);}
    else for(INT i=0;i<WN_kid_count(node);++i) if(WN_kid(node,i)) q1_rename(WN_kid(node,i),renames);
}
/* Collect every axis of one operation; an outer axis alone can be
 * shadowed by an unchanged inner join at every observed memory site. */
void q1_operation_axes(WN *node,const std::string &tag,std::set<ST_IDX> &axes) {
    if(WN_operator(node)==OPR_DO_LOOP) {
        ST_IDX st=WN_st_idx(WN_index(node));const char *text=ST_tensor_metadata(st,"tensor.q1.operation");
        if(text && tag==text) axes.insert(st);
    }
    if(WN_operator(node)==OPR_BLOCK) {for(WN *s=WN_first(node);s;s=WN_next(s)) q1_operation_axes(s,tag,axes);}
    else for(INT i=0;i<WN_kid_count(node);++i) if(WN_kid(node,i)) q1_operation_axes(WN_kid(node,i),tag,axes);
}
/* Actual independently reopened High/PREOPT image, fresh epoch and refusal
 * tests. This command never participates in candidate exploration. */
int q1_rebind_test(int argc,char **argv) {
    need(argc==6 && access(argv[3],F_OK)!=0 && access(argv[4],F_OK)!=0 && access(argv[5],F_OK)!=0,"Q1 rebind test outputs exist");initialize(false);
    std::string prefix=argv[3];need(prefix.size()>9 && prefix.substr(prefix.size()-9)==".before.B","Q1 before snapshot suffix");
    prefix.resize(prefix.size()-9);need(prefix+".after.B"==argv[4],"Q1 paired snapshot prefix");
    VHO_DSL_Q1_Checkpoint=const_cast<char *>(prefix.c_str());
    Open_Input_Info(argv[2]);Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
    PU_Info *pu=Read_Global_Info(NULL);need(pu && !PU_Info_next(pu) && !PU_Info_child(pu),"Q1 rebind requires one PU");
    Read_Local_Info(MEM_pu_nz_pool_ptr,pu);Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);
    Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);
    WN *entry=PU_Info_tree_ptr(pu);VHO_TENSOR_PROGRAM p;std::string why;
    need(VHO_q1_program_read(entry,p,why),why.c_str());DSL_HIGH_EPOCH epoch(p.source_pu);
    DSL_HIGH_Q1_RESOLVER resolver;DSL_HIGH_QUANTIZED_FACTS facts;
    unsigned types=TY_Table_Size(),symbols=ST_Table_Size(CURRENT_SYMTAB);
    need(LNO_DSL_rebind_quantized_high(entry,p,epoch,resolver,facts,why),why.c_str());
    need(LNO_DSL_revalidate_rebound_quantized_high(entry,p,epoch,resolver,facts,why),why.c_str());
    need(types==TY_Table_Size() && symbols==ST_Table_Size(CURRENT_SYMTAB),"Q1 rebind changed native tables");
    epoch.Before_mutation();q1_strip_comments(entry);
    need(!facts.Is_live(epoch),"Q1 rebind failed epoch expiry");
    DSL_HIGH_QUANTIZED_FACTS stripped;
    need(LNO_DSL_rebind_quantized_high(entry,p,epoch,resolver,stripped,why),why.c_str());
    need(stripped.Accesses().size()==facts.Accesses().size(),"Q1 comments changed access census");
    for(unsigned i=0;i<facts.Accesses().size();++i) need(stripped.Accesses()[i].operation==facts.Accesses()[i].operation,"Q1 COMMENT supplied proof");
    WN *loop=find_loop(entry);ST_IDX induction=WN_st_idx(WN_index(loop));
    std::string tag=ST_tensor_metadata(induction,"tensor.q1.operation");
    std::ostringstream other_tag;other_tag<<(strtoul(tag.c_str(),NULL,10)+1)%p.operations.size();
    std::set<ST_IDX> axes;q1_operation_axes(entry,tag,axes);
    for(std::set<ST_IDX>::const_iterator i=axes.begin();i!=axes.end();++i)
        ST_tensor_bind_metadata(*i,"tensor.q1.operation",other_tag.str().c_str());
    need(!LNO_DSL_revalidate_rebound_quantized_high(entry,p,epoch,resolver,stripped,why),"Q1 changed operation join accepted");
    for(std::set<ST_IDX>::const_iterator i=axes.begin();i!=axes.end();++i)
        ST_tensor_bind_metadata(*i,"tensor.q1.operation",tag.c_str());
    VHO_TENSOR_PROGRAM changed=p;changed.values[0].cache_scale="stale-scale";
    need(!LNO_DSL_revalidate_rebound_quantized_high(entry,changed,epoch,resolver,stripped,why),"Q1 rebind accepted stale scale");
    ST *formal=WN_st(WN_formal(entry,0));TY_IDX original=ST_type(formal),alias=original;Clear_TY_is_restrict(alias);Set_ST_type(formal,alias);
    DSL_HIGH_QUANTIZED_FACTS unknown;
    need(!LNO_DSL_rebind_quantized_high(entry,p,epoch,resolver,unknown,why) && why=="q1_alias_unknown","Q1 rebind accepted unknown alias");Set_ST_type(formal,original);
    std::string wire=VHO_q1_program_encode(p);VHO_TENSOR_PROGRAM decoded;
    need(VHO_q1_program_decode(wire,decoded,why),"Q1 program did not round-trip");
    need(!VHO_q1_program_decode(wire+"garbage",decoded,why),"Q1 trailing metadata accepted");
    need(!VHO_q1_program_decode("{\"schema\":0,"+wire.substr(1),decoded,why),"Q1 duplicate metadata accepted");
    ST_IDX fn=WN_st_idx(entry);std::string seal=ST_tensor_metadata(fn,"tensor.high.recipe_key");
    ST_tensor_bind_metadata(fn,"tensor.high.recipe_key","stale");
    need(!VHO_q1_program_read(entry,decoded,why),"Q1 stale program seal accepted");ST_tensor_bind_metadata(fn,"tensor.high.recipe_key",seal.c_str());
    // This standalone process deliberately mutates the reopened tree. Discard
    // unavailable/stale optimizer maps before publishing its audit snapshots;
    // preserve tree, symbols and region semantics. No analysis manager is live.
    for(unsigned i=0;i<WT_SUBSECTIONS;++i) if(i!=WT_TREE && i!=WT_SYMTAB && i!=WT_REGIONS) {
        PU_Info_subsect_ptr(pu,i)=NULL;Set_PU_Info_state(pu,i,Subsect_Missing);
    }
    need(VHO_q1_checkpoint(entry,"before",true),"Q1 reopened before checkpoint failed");
    epoch.Before_mutation();std::map<ST_IDX,ST_IDX> renames;q1_untagged_declarations(entry,renames);need(!renames.empty(),"Q1 no surviving operation tags");q1_rename(entry,renames);
    need(LNO_DSL_rebind_quantized_high(entry,p,epoch,resolver,unknown,why),why.c_str());
    unsigned unknown_sites=0;for(unsigned i=0;i<unknown.Accesses().size();++i) unknown_sites+=unknown.Accesses()[i].operation==~0U;
    need(unknown_sites==unknown.Accesses().size(),"Q1 missing tags inferred source groups");
    ST_tensor_bind_metadata(renames.begin()->second,"tensor.q1.operation","");
    need(!LNO_DSL_rebind_quantized_high(entry,p,epoch,resolver,unknown,why) && why=="q1_operation_join","Q1 empty tag became operation zero");
    need(VHO_q1_checkpoint(entry,"after",true),"Q1 reopened after checkpoint failed");epoch.Close();
    need(!LNO_DSL_revalidate_rebound_quantized_high(NULL,p,epoch,resolver,stripped,why) && why=="q1_stale_rebound_epoch","Q1 rebind touched expired native scope");
    Json::Value report;report["status"]="PASS";report["fresh_reopened_capture"]=true;report["read_only_tables"]=true;
    report["comments_not_proof"]=true;report["unknown_operation_sites"]=unknown_sites;report["malformed_operation_refused"]=true;
    report["stale_scale_alias_metadata_refused"]=true;report["epoch_and_scope_expiry"]=true;report["search_invoked"]=false;
    write_json(argv[5],report);return 0;
}
