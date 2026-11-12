/* Copyright (C) 2026 Open64 Project */
/* Standalone exporter of native serving schema and canonical Q1 source metadata.
 * Never imports JSON as allocation/publication authority. Design:
 * doc/AI-COMPILER-MLA-Q3-SERVING-v1.0.md. */
/* Independently reopen one genuine High image; discovery is native read-only. */
int q3_schema_command(int argc,char **argv) {
    need(argc==4 && access(argv[3],F_OK)!=0,"Q3 schema usage: image.B output.json");initialize(false);
    Open_Input_Info(argv[2]);Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
    PU_Info *pu=Read_Global_Info(NULL);need(pu && !PU_Info_next(pu) && !PU_Info_child(pu),"Q3 single entry required");
    Read_Local_Info(MEM_pu_nz_pool_ptr,pu);Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);
    WN *entry=PU_Info_tree_ptr(pu);VHO_TENSOR_PROGRAM p;std::string why;bool read=VHO_q1_program_read(entry,p,why);need(read,why.c_str());
    unsigned types=TY_Table_Size(),symbols=ST_Table_Size(CURRENT_SYMTAB);std::string key=DSL_NATIVE_TREE_KEY().Build(entry);
    VHO_TENSOR_SERVING_SCHEMA s;bool discovered=VHO_tensor_serving_schema(p,s,why);need(discovered,why.c_str());
    Json::Value j;j["schema"]="open64.mla-serving-source.v1";j["program"]=VHO_q1_program_encode(p);j["source_key"]=s.source_key;
    j["roots"]=Json::Value(Json::arrayValue);for(unsigned i=0;i<s.roots.size();++i) j["roots"].append(s.roots[i]);
    j["incoming"]=Json::Value(Json::arrayValue);j["returned"]=Json::Value(Json::arrayValue);
    for(unsigned i=0;i<4;++i) {j["incoming"].append(s.incoming[i]);j["returned"].append(s.returned[i]);}
    j["table"]=s.table;j["tokens"]=s.tokens;j["slots"]=s.slots;j["mask"]=s.geometry.mask_value;
    j["page_tokens"]=Json::UInt64(s.page_tokens);j["pages"]=Json::UInt64(s.pages);j["metadata_columns"]=Json::UInt64(s.metadata_columns);
    j["paged"]=s.paged;j["encoding"]=s.encoding;j["geometry"]["B"]=Json::UInt64(s.geometry.batch);j["geometry"]["H"]=Json::UInt64(s.geometry.heads);j["geometry"]["Q"]=Json::UInt64(s.geometry.queries);j["geometry"]["L"]=Json::UInt64(s.geometry.keys);j["geometry"]["A"]=Json::UInt64(s.geometry.update_items);
    need(types==TY_Table_Size() && symbols==ST_Table_Size(CURRENT_SYMTAB) && key==DSL_NATIVE_TREE_KEY().Build(entry),"Q3 schema mutated native state");
    j["native_state_unchanged"]=true;j["search_invoked"]=false;write_json(argv[3],j);return 0;
}
