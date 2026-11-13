/* Copyright (C) 2026 Open64 Project */
/* Driver-owned H6 native source/search/device fixtures; production services do
 * all admission, cost and construction. No CUDA algorithm is duplicated here.
 * Design: doc/AI-COMPILER-H6-NORM-REDUCTION-v1.0.md. */
#define DSL_NORMALIZATION_COST_NO_MAIN
#include "dsl_normalization_cost_test.cxx"
#include "opt_dsl_h6.h"
#include "dsl_gpu_profile.h"
#include "config_dsl.h"
#include "dsl_native_tree_key.h"
#include <fstream>
#include "ir_bread.h"
#include "ir_reader.h"

/* Real typed source creation, using existing R2 v2 declaration semantics. */
static WN *H6_test_reduction(UINT64 rows,UINT64 width) {
    Initialize_Context();if(!DSL_Builder_Begin_Program()) return NULL;
    PU_Info *pu=DSL_Builder_Create_Minimal_PU("h6_ordered_source");if(!pu || !DSL_Builder_Select_PU(pu)) return NULL;
    char shape[80];snprintf(shape,sizeof(shape),"[%llu,%llu]",(unsigned long long)rows,(unsigned long long)width);TY_IDX x=Tensor("h6_x",2,shape);
    snprintf(shape,sizeof(shape),"[%llu]",(unsigned long long)rows);TY_IDX y=Tensor("h6_y",1,shape);
    DSL_BUILDER_SOURCE_POSITION pos;memset(&pos,0,sizeof(pos));pos.file_id=DSL_Builder_Register_Source_File(pu,__FILE__);pos.line=__LINE__+1;
    DSL_BUILDER_VALUE input=DSL_Builder_Declare_PU_Formal(pu,"x",0,x,&pos);
    if(!input || !DSL_Builder_Declare_PU_Result(pu,"y",0,y,DSL_PU_RESULT_TENSOR,&pos)) return NULL;
    DSL_Opcode_Register_Common_Substrate();DSL_OPCODE_ID op=DSL_Opcode_Find(DSL_Domain_Find("common"),"common.reduce_sum",2);
    DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[]={{"attr.axis","1"},{"attr.keepdim","false"},{"attr.accum_dtype","float32"},{"attr.reassociate","false"},{"attr.order","ascending_left_fold"},{"attr.identity_bits","00000000"},{"attr.fp_environment","rne_gradual_no_traps"},{"attr.nan_policy","class_only"}};
    WN *value=DSL_Builder_Create_Operator_With_Result(op,2,&input,1,attrs,8,"h6_sum",y);
    if(!value || !DSL_Builder_Set_Value_Source_Position(value,&pos) || !DSL_Builder_Append_PU_Value(pu,value) || !DSL_Builder_Return_PU_Values(pu,&value,1)) return NULL;return value;
}
/* Ordinary separate-process reader; source legality is independently owned,
 * while exact native identity catches body/metadata changes after mapping. */
static PU_Info *H6_test_read(char *path) {
    MEM_Initialize();Set_Error_Tables(Phases,host_errlist);Init_Error_Handler(10);
    Set_Error_File(NULL);Set_Error_Line(ERROR_LINE_UNKNOWN);Preconfigure();Init_Controls_Tbl();
    ABI_Name="n64";Configure();IR_reader_init();
    if(!Open_Input_Info(path)) return NULL;
    Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
    PU_Info *pu=Read_Global_Info(NULL);if(!pu || PU_Info_next(pu)) return NULL;
    Read_Local_Info(MEM_pu_nz_pool_ptr,pu);Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);
    Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);return pu;
}
/* Compare retained complete Reduction identity after independent mapping. */
static int H6_test_reopen(char *path,const char *key_path) {
    PU_Info *pu=H6_test_read(path);if(!pu) return 1;
    std::ifstream input(key_path);std::string key((std::istreambuf_iterator<char>(input)),std::istreambuf_iterator<char>());
    return input && WOPT_DSL_verify_h6_reduction(PU_Info_tree_ptr(pu),key)?0:1;
}
/* Same-build review-only numeric handoffs are never a portable file format. */
static BOOL H6_test_binary(const char *path,void *value,size_t bytes,BOOL write) {
    FILE *f=fopen(path,write?"wx":"rb");if(!f) return FALSE;
    BOOL ok=write?fwrite(value,1,bytes,f)==bytes:fread(value,1,bytes,f)==bytes && fgetc(f)==EOF;
    return fclose(f)==0 && ok;
}
/* LN4 verifies its actual native scalar operations/collectives under the
 * exact reimported H20 and H3 bindings, independent of producer process state. */
static int H6_test_reopen_ln(char **argv) {
    PU_Info *pu=H6_test_read(argv[2]);if(!pu) return 1;
    DSL_HARDWARE_TARGET_SELECTION target;DSL_GPU_DEVICE_CONTEXT_OWNER owner;
    if(!DSL_hardware_target_resolve(NULL,argv[5],&target,stderr) || !DSL_gpu_profile_resolve(target,argv[6],argv[7],"fixture-toolchain-v1",&owner,stderr)) return 1;
    VHO_DSL_GPU_Context=owner.Context();DSL_H3_SEARCH search;std::string why;
    if(!VHO_h3_import(argv[8],argv[9],DSL_GPU_PLAN_SNAPSHOT::Capture(owner.Context()),search,why)) return 1;VHO_DSL_H3_Search=&search;
    WOPT_DSL_NORMALIZATION_LOWERING plan;WOPT_DSL_NORMALIZATION_DEVICE product;
    if(!H6_test_binary(argv[3],&plan,sizeof(plan),FALSE) || !H6_test_binary(argv[4],&product,sizeof(product),FALSE)) return 1;
    FILE *trace=fopen(argv[10],"wx");if(!trace) return 1;
    BOOL ok=WOPT_DSL_verify_normalization_device(&plan,&product,stderr,trace);fclose(trace);return ok?0:1;
}
/* Find a real executable node for mutation checks; never mutate during search. */
static WN *H6_test_find(WN *node,OPERATOR op) {
    if(!node) return NULL;if(WN_operator(node)==op) return node;
    if(WN_operator(node)==OPR_BLOCK) {for(WN *s=WN_first(node);s;s=WN_next(s)) {WN *p=H6_test_find(s,op);if(p) return p;}}
    else for(INT i=0;i<WN_kid_count(node);++i) {WN *p=H6_test_find(WN_kid(node,i),op);if(p) return p;}return NULL;
}
/* Exercise optional transaction semantics through the real pure engine. Logical
 * requests stay fixed; malformed/overflow envelopes refuse and spill traffic
 * remains separately charged. The original H3 model/source owner is untouched. */
static BOOL H6_test_envelopes(const DSL_MOVEMENT_FACTS &source,FILE *trace) {
    DSL_H3_PLAN p;p.id="test-envelope";p.capability="test-only";p.executable=true;p.live_bytes=1;
    DSL_H3_STAGE stage;stage.id="one";stage.ctas=1;stage.threads=32;stage.registers=16;stage.read_requests=4;stage.write_through_requests=4;p.stages.push_back(stage);
    DSL_H3_MODEL model=VHO_DSL_H3_Search->Model();model.spill_per_cta=DSL_MOVEMENT_BYTES(0);
    DSL_H3_ESTIMATE base,explicit_cost,value;std::string why;
    if(model.transaction_bytes!=32 || !VHO_h3_estimate(source,model,p,base,why) || base.scenarios[0].transactions!=64 || base.scenarios[0].requests!=8) return FALSE;
    const std::string old_key=VHO_h3_plan_key(p);
    if(old_key.find("transactions-v1;")!=std::string::npos) return FALSE;
    p.stages[0].read_transactions=64;p.stages[0].write_transactions=64;p.stages[0].transaction_proof="test-owned-envelope";
    if(!VHO_h3_estimate(source,model,p,explicit_cost,why) || explicit_cost.scenarios[0].transactions!=128 || explicit_cost.scenarios[0].requests!=8 || explicit_cost.binding==base.binding) return FALSE;
    for(unsigned fault=0;fault<4;++fault) {
        DSL_H3_PLAN bad=p;if(fault==0) bad.stages[0].transaction_proof="";
        if(fault==1) bad.stages[0].read_transactions=16;
        if(fault==2) bad.stages[0].read_transactions=65;
        if(fault==3) bad.stages[0].read_transactions=~UINT64(0)-31;
        if(VHO_h3_estimate(source,model,bad,value,why)) return FALSE;
    }
    model.spill_per_cta=DSL_MOVEMENT_BYTES(4);
    if(!VHO_h3_estimate(source,model,p,value,why) || value.scenarios[0].transactions!=160 || value.scenarios[0].requests!=12 || value.scenarios[0].spill_requests!=4) return FALSE;
    p.stages[0].read_transactions=0;p.stages[0].write_transactions=0;p.stages[0].transaction_proof="";
    if(VHO_h3_plan_key(p)!=old_key) return FALSE;
    fprintf(trace,"AI_H6_ENVELOPES 1 logical_requests=8 legacy_transactions=64 explicit_transactions=128 spill_transactions=160 malformed_refused=4 legacy_key_restored=true\n");return TRUE;
}
/* One driver process owns source and fresh-device PUs. Search cannot select a
 * PU or escape its read-only boundary. Keep both native checkpoint boundaries. */
int main(int argc,char **argv) {
    if(argc==4 && !strcmp(argv[1],"reopen")) return H6_test_reopen(argv[2],argv[3]);
    if(argc==11 && !strcmp(argv[1],"reopen-ln")) return H6_test_reopen_ln(argv);
    if(argc!=11) return 2;
    const std::string prefix=argv[1];bool normalization=!strcmp(argv[2],"ln");WN *node=NULL;
    if(normalization) {
        char *source_args[]={argv[0],argv[1],argv[3]};if(LN1b_Source_Producer(3,source_args)) return 1;
        for(node=WN_first(WN_func_body(PU_Info_tree_ptr(Current_PU_Info)));node;node=WN_next(node)) if(WN_operator(node)==OPR_STID && WN_operator(WN_kid0(node))==OPR_DSL) break;
    } else node=H6_test_reduction(strtoull(argv[9],NULL,10),strtoull(argv[10],NULL,10));
    if(!node) return 1;WN *scope=PU_Info_tree_ptr(Current_PU_Info);
    Opt_Level=3;OPT_Fast_Math=FALSE;Roundoff_Level=ROUNDOFF_NONE;IEEE_Arithmetic=IEEE_ACCURATE;
    DSL_HARDWARE_TARGET_SELECTION target;DSL_GPU_DEVICE_CONTEXT_OWNER owner;
    if(!DSL_hardware_target_resolve(NULL,argv[4],&target,stderr) || !DSL_gpu_profile_resolve(target,argv[5],argv[6],"fixture-toolchain-v1",&owner,stderr)) return 1;
    VHO_DSL_GPU_Context=owner.Context();DSL_H3_SEARCH search;std::string why;
    if(!VHO_h3_import(argv[7],argv[8],DSL_GPU_PLAN_SNAPSHOT::Capture(owner.Context()),search,why)) {fprintf(stderr,"H6 model: %s\n",why.c_str());return 1;}VHO_DSL_H3_Search=&search;
    FILE *trace=fopen((prefix+".phase.t").c_str(),"wx");if(!trace) return 1;
    DSL_BUILDER_MAPPED_IMAGE_REQUEST image={};std::string before=prefix+".source.before.B";image.path=before.c_str();if(!LN1c_Source_Checkpoint(&image)) return 1;
    const std::string source_key=DSL_NATIVE_TREE_KEY(true,true).Build(node);
    const UINT32 symbols=ST_Table_Size(CURRENT_SYMTAB),global_symbols=ST_Table_Size(GLOBAL_SYMTAB),types=TY_Table_Size();DSL_IR_IMAGE_HEADER native_before,native_after;DSL_IR_Image_Get_Header(&native_before);
    WOPT_DSL_NORMALIZATION_COST_REQUEST ln;memset(&ln,0,sizeof(ln));WOPT_DSL_REDUCTION_DIRECT_REQUEST reduction;memset(&reduction,0,sizeof(reduction));
    if(normalization) {if(!LN3_Request(scope,node,"sm_90",1,&ln)) return 1;}
    else {
        if(!WOPT_DSL_characterize_reduction(scope,node,&reduction.expected_source,stderr)) return 1;
        reduction.architecture="sm_90";reduction.instruction=WOPT_DSL_REDUCE_PTX_ADD_RN_F32;reduction.runtime_contract_id=31;reduction.context_id=41;
        reduction.input.allocation_id=1;reduction.input.allocation_bytes=4*reduction.expected_source.rows*reduction.expected_source.width+64;reduction.input.base_alignment=16;reduction.input.lease=1;reduction.input.memory_space=1;
        reduction.output.allocation_id=2;reduction.output.allocation_bytes=4*reduction.expected_source.rows+64;reduction.output.base_alignment=16;reduction.output.lease=2;reduction.output.memory_space=1;
        reduction.row_mapping=1;reduction.launched_threads=((reduction.expected_source.rows+127)/128)*128;
        reduction.before[0]=2;reduction.before[1]=4;reduction.before[2]=8|16;reduction.before[3]=32;
    }
    WOPT_DSL_H6_SELECTION selection;
    for(unsigned level=0;level<3;++level) {
        Opt_Level=level;if(normalization?WOPT_DSL_select_h6_normalization(scope,node,&ln,selection,trace):WOPT_DSL_select_h6_reduction(scope,node,&reduction,selection,trace)) return 1;
    }Opt_Level=3;
    BOOL selected=normalization?WOPT_DSL_select_h6_normalization(scope,node,&ln,selection,trace):WOPT_DSL_select_h6_reduction(scope,node,&reduction,selection,trace);
    DSL_IR_Image_Get_Header(&native_after);
    if(symbols!=ST_Table_Size(CURRENT_SYMTAB) || types!=TY_Table_Size() || global_symbols!=ST_Table_Size(GLOBAL_SYMTAB) || source_key!=DSL_NATIVE_TREE_KEY(true,true).Build(node) || memcmp(&native_before,&native_after,sizeof(native_before))) return 1;
    fprintf(trace,"AI_H6_ISOLATION 1 WN=unchanged ST=unchanged TY=unchanged managed=unchanged\n");
    std::string after=prefix+".source.after.B";image.path=after.c_str();if(!LN1c_Source_Checkpoint(&image)) return 1;
    search.Print(trace);if(!selected) {fclose(trace);return 0;}
    if(!H6_test_envelopes(selection.source,trace)) return 1;
    WOPT_DSL_NORMALIZATION_LOWERING plan;WOPT_DSL_H6_REDUCTION rp;
    WOPT_DSL_H6_SELECTION bad=selection;bad.candidates[bad.winner].estimate.rank_ns++;
    if(normalization?WOPT_DSL_prepare_h6_normalization(scope,node,&ln,bad,plan):WOPT_DSL_prepare_h6_reduction(scope,node,&reduction,bad,rp)) return 1;
    if(!(normalization?WOPT_DSL_prepare_h6_normalization(scope,node,&ln,selection,plan):WOPT_DSL_prepare_h6_reduction(scope,node,&reduction,selection,rp))) return 1;
    fprintf(trace,"AI_H6_FIXED 1 selected=%u alternative_materializations=0 stale_score_refused=true\n",selection.winner);
    unsigned refusals=1;
    for(unsigned fault=0;fault<8;++fault) {
        WOPT_DSL_H6_SELECTION changed=selection;
        WOPT_DSL_NORMALIZATION_COST_REQUEST lc=ln;WOPT_DSL_REDUCTION_DIRECT_REQUEST rc=reduction;
        if(fault==0) changed.complete=false;
        if(fault==1) changed.model+="stale";
        if(fault==2) changed.candidates[changed.winner].estimate.key+="stale";
        if(fault==3) changed.candidates[changed.winner].estimate.complete=false;
        if(fault==4) changed.candidates[changed.winner].kind=0;
        if(fault==5) {if(normalization) lc.plans[1].ranges[0].lease++;else rc.input.lease++;}
        if(fault==6) {if(normalization) lc.plans[2].before[0]=0;else rc.before[0]=0;}
        if(fault==7) {if(normalization) lc.plans[0].target.expected_source.width++;else rc.expected_source.width++;}
        if(normalization?WOPT_DSL_prepare_h6_normalization(scope,node,&lc,changed,plan):WOPT_DSL_prepare_h6_reduction(scope,node,&rc,changed,rp)) return 1;
        ++refusals;
    }
    if(!(normalization?WOPT_DSL_prepare_h6_normalization(scope,node,&ln,selection,plan):WOPT_DSL_prepare_h6_reduction(scope,node,&reduction,selection,rp))) return 1;
    fprintf(trace,"AI_H6_REFUSALS 1 selected_faults=%u O0_O1_O2=true\n",refusals);
    if(normalization) {FILE *g=fopen((prefix+".guard").c_str(),"wx");if(!g || fwrite(&plan.guard,1,sizeof(plan.guard),g)!=sizeof(plan.guard) || fclose(g)) return 1;}
    // Source images and owned handoffs are complete. The driver now ends the
    // source-only managed interface lifetime before creating an unrelated
    // scalar PU at the same lexical level; no optimizer clears source state.
    DSL_PU_Interface_Image_Reset();
    PU_Info *device=DSL_Builder_Create_Minimal_PU(normalization?"h6_layernorm":"h6_reduction");if(!device || !DSL_Builder_Select_PU(device)) return 1;
    ST &function=St_Table[PU_Info_proc_sym(device)];Set_ST_device_symbol_kind(function,DEVICE_SYMBOL_KERNEL);Set_ST_export(function,EXPORT_PREEMPTIBLE);
    UINT32 file=DSL_Builder_Register_Source_File(device,__FILE__);USRCPOS source;USRCPOS_clear(source);USRCPOS_filenum(source)=file;USRCPOS_linenum(source)=__LINE__;
    if(!LN1c_Scalar_Image(device,(prefix+".device.before.B").c_str())) return 1;
    std::string key;WOPT_DSL_NORMALIZATION_DEVICE product;
    if(normalization) {if(!WOPT_DSL_materialize_normalization_device(&plan,USRCPOS_srcpos(source),&product,stderr,trace) || !WOPT_DSL_verify_normalization_device(&plan,&product,stderr,trace) ||
        !H6_test_binary((prefix+".plan").c_str(),&plan,sizeof(plan),TRUE) || !H6_test_binary((prefix+".product").c_str(),&product,sizeof(product),TRUE)) return 1;}
    else {WOPT_DSL_H6_REDUCTION invalid=rp;invalid.chunk++;UINT32 count=ST_Table_Size(CURRENT_SYMTAB);if(WOPT_DSL_materialize_h6_reduction(invalid,USRCPOS_srcpos(source),key) || count!=ST_Table_Size(CURRENT_SYMTAB) || !WOPT_DSL_materialize_h6_reduction(rp,USRCPOS_srcpos(source),key)) return 1;
        WN *entry=PU_Info_tree_ptr(device);
        if(!WOPT_DSL_verify_h6_reduction(entry,key) || WOPT_DSL_verify_h6_reduction(entry,key+"stale")) return 1;
        WN *loop=H6_test_find(entry,OPR_DO_LOOP),*bound=loop?WN_kid1(WN_end(loop)):NULL;
        if(!bound || WN_operator(bound)!=OPR_INTCONST) return 1;
        INT64 n=WN_const_val(bound);WN_const_val(bound)=n+1;
        if(WOPT_DSL_verify_h6_reduction(entry,key)) return 1;WN_const_val(bound)=n;
        const char *grid=ST_tensor_metadata(ST_st_idx(function),"tensor.kernel.grid_x");std::string saved=grid?grid:"";
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.grid_x","999999");
        if(WOPT_DSL_verify_h6_reduction(entry,key)) return 1;ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.grid_x",saved.c_str());
        WN *barrier=H6_test_find(entry,OPR_CALL);
        if(barrier) {UINT32 flags=WN_call_flag(barrier);WN_call_flag(barrier)^=WN_CALL_NON_DATA_MOD;
            if(WOPT_DSL_verify_h6_reduction(entry,key)) return 1;WN_call_flag(barrier)=flags;}
        if(!WOPT_DSL_verify_h6_reduction(entry,key)) return 1;
        fprintf(trace,"AI_H6_NATIVE_VERIFY 1 body=true metadata=true stale_loop_refused=true stale_grid_refused=true stale_barrier_refused=%s\n",barrier?"true":"not-applicable");
        std::ofstream output((prefix+".native-key").c_str());output<<key;
    }
    const WOPT_DSL_H6_CANDIDATE &c=selection.candidates[selection.winner];
    fprintf(trace,"AI_H6_KERNEL 1 family=%s rows=%llu width=%llu kind=%u threads=%u chunk=%u rank_ns=%llu\n",normalization?"LayerNorm":"Reduction",(unsigned long long)selection.source.Capture().work.instances,(unsigned long long)selection.source.Capture().work.width,c.kind,c.threads,c.chunk,(unsigned long long)c.estimate.rank_ns);
    if(!DSL_Gatekeeper_Verify_PU(device,stderr,NULL) || !LN1c_Scalar_Image(device,(prefix+".device.after.B").c_str())) return 1;
    fclose(trace);return 0;
}
