/* Copyright (C) 2026 Open64 Project */
/* Driver-owned reopened selected-PU Q6 placement audit. No kernel construction,
 * PU switching, serving observation, search or launch; failure publishes no
 * contract. Design: doc/AI-COMPILER-MLA-Q6-GPU-STAGING-v1.0.md. */
#ifdef OPEN64_TENSOR_BRIDGE_TEST
#include "tensor_q6_contract_test.inc"
#endif
/* Require explicit unsigned fields; negative/FP/truncated sizes cannot fit. */
UINT64 q6_unsigned(const Json::Value &j,const char *name) {
    need(j.isMember(name) && j[name].isUInt64(),"q6_control_unsigned");return j[name].asUInt64();
}
/* Decode the same explicit offline envelope for admission and construction. */
VHO_Q6_CONTROL q6_control(const std::string &wire,Json::Value &control) {
    Json::Reader parser(Json::Features::strictMode());
    need(parser.parse(wire,control,false) && control.isObject() && control.size()==9,"q6_control_shape");
    Json::FastWriter writer;need(writer.write(control)==wire,"q6_control_requires_canonical_sorted_JSON");
    const char *names[]={"schema","delivery","arch","ptx","toolkit","max_threads","shared_limit","q3_binding_key","scope"};
    for(unsigned i=0;i<9;++i) need(control.isMember(names[i]),"q6_missing_control_field");
    need(control["schema"].isString() && control["schema"].asString()=="open64.q6.placement-control.v1" &&
        control["scope"].isString() && control["scope"].asString()=="offline-selected-contract" &&
        control["q3_binding_key"].isString(),"q6_control_identity");
    VHO_Q6_CONTROL c;UINT32 *fields[]={&c.delivery,&c.arch,&c.ptx,&c.toolkit,&c.max_threads};
    const char *ints[]={"delivery","arch","ptx","toolkit","max_threads"};
    for(unsigned i=0;i<5;++i) {UINT64 value=q6_unsigned(control,ints[i]);need(value<=0xffffffffULL,"q6_control_U4_overflow");*fields[i]=value;}
    c.shared_limit=q6_unsigned(control,"shared_limit");return c;
}
/* Inspect exactly one existing selected High image. Native reader/type-cache
 * initialization is outside the measured read-only admission boundary. */
int q6_command(int argc,char **argv) {
    need(argc==5,"q6-contract selected.B control.json output.json");need(access(argv[4],F_OK)!=0,"q6_output_exists");
    Json::Value control;VHO_Q6_CONTROL c=q6_control(q5_wire(argv[3]),control);initialize(false);
    Open_Input_Info(argv[2]);Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);PU_Info *pu=Read_Global_Info(NULL);
    need(pu && !PU_Info_next(pu) && !PU_Info_child(pu),"q6_single_PU");Read_Local_Info(MEM_pu_nz_pool_ptr,pu);
    Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);Initialize_Special_Global_Symbols();
    WN *root=PU_Info_tree_ptr(pu);ST_IDX st=WN_st_idx(root);const char *applied=ST_tensor_metadata(st,"tensor.q5.applied"),*accepted=ST_tensor_metadata(st,"tensor.q5.accepted.key");
    need(applied && accepted,"q6_requires_accepted_Q5_High");
    unsigned locals=ST_Table_Size(CURRENT_SYMTAB),globals=ST_Table_Size(GLOBAL_SYMTAB),types=TY_Table_Size(),tcons=TCON_Table_Size();
    std::string native=DSL_NATIVE_TREE_KEY().Build(root);need(native==accepted,"q6_stale_selected_native_key");
    VHO_Q5_PLAN plan;std::string why;need(VHO_q5_plan_decode(applied,plan,why),"q6_selected_descriptor");
    need(plan.binding_key==control["q3_binding_key"].asString(),"q6_Q3_snapshot_identity_mismatch");
    VHO_TENSOR_PROGRAM p;need(VHO_q1_program_read(root,p,why),why.c_str());VHO_Q6_MAPPING m;
    need(VHO_q6_mapping(p,plan,native,c,m,why),why.c_str());std::vector<VHO_Q6_ACTION> trace;
    need(VHO_q6_pipeline(m,trace,why) && VHO_q6_verify_pipeline(m,trace,why),why.c_str());
    DSL_MOVEMENT_BINDING movement_binding;movement_binding.owner=ST_name(st);movement_binding.source_key=native;
    movement_binding.serving_key=m.binding_key;movement_binding.device=DSL_GPU_PLAN_SNAPSHOT::Capture(VHO_DSL_GPU_Context);
    movement_binding.device_key=movement_binding.device.Cache_key();DSL_MOVEMENT_FACTS movement;
    need(VHO_q6_capture_movement(m,trace,movement_binding,movement,why),why.c_str());movement.Print(stdout,"SELECTED_Q6");
    Json::Value j;j["schema"]="open64.q6.selected-placement-contract.v1";
    j["mapping_key"]=m.key;j["q3_binding_key"]=m.binding_key;j["explanation"]=m.explanation;
    j["grid_x"]=Json::UInt64(m.grid);j["block_x"]=Json::UInt64(m.threads);j["tiles_per_pass"]=Json::UInt64(m.tiles);
    j["raw_slots"]=Json::UInt64(m.slots);j["raw_stride"]=Json::UInt64(m.raw_stride);
    j["decoded_offset"]=Json::UInt64(m.decoded_offset);j["decoded_bytes"]=Json::UInt64(m.decoded_bytes);
    j["barrier_offset"]=Json::UInt64(m.barrier_offset);j["shared_bytes"]=Json::UInt64(m.shared_bytes);j["delivery"]=c.delivery;
    j["streams"]=Json::Value(Json::arrayValue);
    for(unsigned i=0;i<4;++i) {Json::Value s;s["ordinal"]=i;s["row_bytes"]=Json::UInt64(m.raw_row[i]);s["offset"]=Json::UInt64(m.raw_offset[i]);j["streams"].append(s);}
    j["pipeline"]=Json::Value(Json::arrayValue);
    const char *actions[]={"invalid","acquire","issue","wait","dequantize","publish","consume","release"};
    for(unsigned i=0;i<trace.size();++i) {
        Json::Value a;a["action"]=actions[trace[i].kind];a["pass"]=trace[i].pass;a["slot"]=trace[i].slot;a["tile"]=Json::UInt64(trace[i].tile);j["pipeline"].append(a);
    }
#ifdef OPEN64_TENSOR_BRIDGE_TEST
    j["contract_checks"]=q6_contract_test(p,plan,native,c,m,trace);
#endif
    need(native==DSL_NATIVE_TREE_KEY().Build(root) && locals==ST_Table_Size(CURRENT_SYMTAB) && globals==ST_Table_Size(GLOBAL_SYMTAB) &&
        types==TY_Table_Size() && tcons==TCON_Table_Size(),"q6_admission_mutated_native_state");
    j["native_mutations"]=0;j["search_invocations"]=0;j["source_unchanged"]=true;
    j["gpu_execution_eligible"]=false;j["device_bindings_implemented"]=false;j["kernel_emitted"]=false;
    j["Q5_cost_reused_for_async_pipeline"]=false;j["aot_selection"]= "deferred";
    write_json(argv[4],j);return 0;
}
