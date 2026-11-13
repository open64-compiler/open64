/* Copyright (C) 2026 Open64 Project */
/* H1c actual WOPT owner queries with H1b-imported immutable evidence. Reuses
 * LN1b/LN2/LN3 native fixture construction and the staged-reduction fixture's
 * ordinary builder/request protocol. The test driver selects its own PUs;
 * no production probe, candidate materialization or runtime ABI change.
 * Design: doc/AI-COMPILER-H1C-PLANNER-BINDINGS-v1.0.md. */
#define DSL_NORMALIZATION_COST_NO_MAIN
#include "dsl_normalization_cost_test.cxx"
#include "opt_dsl_normalization_lower.h"
#include "opt_dsl_movement.h"
#include "opt_dsl_reduction_staged.h"
#include "dsl_gpu_profile.h"
#include "config_dsl.h"
#include <stddef.h>
#include <algorithm>

/* Retain the historical handoff prefix independently of the extended record.
 * Compiler-only sidecars require rebuild; native/runtime layouts do not change. */
struct DSL_GPU_OWNER_LEGACY_LOWERING {
  UINT64 version, revision, registers_per_thread, max_jobs, margin;
  DSL_SCALAR_EXECUTION_PROFILE profile;
  OPEN64_NORMALIZATION_GUARD guard;
  WOPT_DSL_NORMALIZATION_COST cost;
  UINT64 seal;
};
typedef char DSL_GPU_OWNER_NORM_ABI_UNCHANGED[sizeof(OPEN64_NORMALIZATION_GUARD)==83*8?1:-1];
typedef char DSL_GPU_OWNER_REDUCE_ABI_UNCHANGED[sizeof(OPEN64_REDUCTION_GUARD)==208?1:-1];

/* Exercise the original byte-prefix seal algorithm, excluding new POD fields. */
static UINT64 DSL_gpu_owner_legacy_seal(const WOPT_DSL_NORMALIZATION_LOWERING &p)
{
  const unsigned char *bytes=reinterpret_cast<const unsigned char *>(&p);
  UINT64 hash=1469598103934665603ULL;
  for (size_t i=0;i<offsetof(DSL_GPU_OWNER_LEGACY_LOWERING,seal);++i)
    hash=(hash^bytes[i])*UINT64(1099511628211ULL);
  return hash;
}

/* Resolve pinned input only through the production H1b parser and publisher.
 * Test owners keep prior contexts alive while stale bindings are exercised. */
static BOOL DSL_gpu_owner_import(const char *device,const char *file,const char *digest,
    const char *toolchain,DSL_GPU_DEVICE_CONTEXT_OWNER *owner)
{
  DSL_HARDWARE_TARGET_SELECTION target;
  return DSL_hardware_target_resolve(NULL,device,&target,stderr) &&
      DSL_gpu_profile_resolve(target,file,digest,toolchain,owner,stderr);
}

std::string H2_review_prefix;
/* Driver-owned COW checkpoint uses the existing mapped writer; it cannot
 * mutate the source/maps/tables under the following facts-only observation. */
static BOOL H2_checkpoint(const char *stage) {
  std::string path=H2_review_prefix+"."+stage+".B";DSL_BUILDER_MAPPED_IMAGE_REQUEST request={};request.path=path.c_str();
  return LN1c_Source_Checkpoint(&request);
}
/* H2 uses the actual native owner APIs. Detached output survives later source
 * teardown but grants no epoch permission; changed canonical ranges/profile
 * bindings require a new complete capture even when shape/cost stay equal. */
static BOOL H2_normalization_facts(WN *scope,WN *value,const WOPT_DSL_NORMALIZATION_COST_REQUEST &request,
    const DSL_GPU_DEVICE_CONTEXT *context,const DSL_GPU_DEVICE_CONTEXT *changed,FILE *trace)
{
  if(!H2_checkpoint("normalization.before")) return FALSE;
  DSL_MOVEMENT_FACTS source;std::string reason;
  BOOL captured_source=WOPT_DSL_capture_normalization_movement(scope,value,&request.plans[0],source,reason);
  if(!captured_source) fprintf(stderr,"H2 normalization capture: %s\n",reason.c_str());
  if(!Check(captured_source,"H2_normalization_capture")) return FALSE;
  if(!Check(source.Available() && source.Capture().storages.size()==4 &&
      !source.Is_current(source.Capture().binding,true) && !source.Capture().ledger.hbm.known && !source.Capture().ledger.spills.known,"H2_normalization_owned_source_unknown_hardware")) return FALSE;
  source.Print(trace,"NATIVE_TEST");
  for(UINT32 i=1;i<3;++i) {DSL_MOVEMENT_FACTS other;
    if(!Check(WOPT_DSL_capture_normalization_movement(scope,value,&request.plans[i],other,reason) && !source.Same_content(other) &&
        other.Capture().storages.size()==6+i,"H2_normalization_plan_event_binding")) return FALSE;
    if(i==2 && !Check(other.Capture().storages[4].allocation==other.Capture().storages[5].allocation &&
        other.Capture().storages[4].version!=other.Capture().storages[5].version,"H2_C2_same_slot_distinct_generations")) return FALSE;
  }
  WOPT_DSL_NORMALIZATION_PLAN_REQUEST altered=request.plans[0];altered.ranges[0].allocation_id+=10000;
  DSL_MOVEMENT_FACTS other;
  if(!Check(WOPT_DSL_capture_normalization_movement(scope,value,&altered,other,reason) && !source.Same_content(other),"H2_same_shape_other_allocation")) return FALSE;
  altered=request.plans[0];altered.ranges[0].offset_bytes+=4;altered.ranges[0].allocation_bytes+=4;
  if(!Check(WOPT_DSL_capture_normalization_movement(scope,value,&altered,other,reason) && !source.Same_content(other),"H2_changed_view_offset")) return FALSE;
  altered=request.plans[0];altered.before[0]=0;
  if(!Check(!WOPT_DSL_capture_normalization_movement(scope,value,&altered,other,reason) && !other.Available(),"H2_unknown_event_proof_refused")) return FALSE;
  VHO_DSL_GPU_Context=changed;
  BOOL captured=WOPT_DSL_capture_normalization_movement(scope,value,&request.plans[0],other,reason);
  if(!Check(!captured || !source.Same_content(other),"H2_stale_H20_profile")) return FALSE;
  VHO_DSL_GPU_Context=context;INT32 previous=Opt_Level;Opt_Level=2;
  BOOL below=WOPT_DSL_capture_normalization_movement(scope,value,&request.plans[0],other,reason);Opt_Level=previous;
  return Check(!below && !other.Available(),"H2_O2_no_new_capture") && H2_checkpoint("normalization.after");
}
/* R2 binding supplies the one-slot, exact-tail event proof. H2 does not create
 * a reduction kernel or replace ascending accumulator updates with a tree. */
static BOOL H2_reduction_facts(WN *scope,WN *value,const WOPT_DSL_REDUCTION_STAGED_REQUEST &request,FILE *trace)
{
  if(!H2_checkpoint("reduction.before")) return FALSE;
  DSL_MOVEMENT_FACTS direct,staged;std::string reason;
  if(!Check(WOPT_DSL_capture_reduction_movement(scope,value,&request.base,direct,reason),"H2_reduction_capture") ||
      !Check(WOPT_DSL_capture_staged_movement(scope,value,&request,staged,reason),"H2_staged_capture")) return FALSE;
  direct.Print(trace,"NATIVE_TEST");staged.Print(trace,"NATIVE_TEST");
  if(!Check(!direct.Same_content(staged) && staged.Capture().lifetimes.size()>=2 &&
      !staged.Capture().ledger.transactions.known && !staged.Capture().ledger.hbm.known,"H2_staged_generation_and_unknown_service")) return FALSE;
  WOPT_DSL_REDUCTION_STAGED_REQUEST altered=request;altered.staging.next[4]=0;
  if(!Check(!WOPT_DSL_capture_staged_movement(scope,value,&altered,staged,reason) && !staged.Available(),"H2_readers_complete_before_slot_reuse")) return FALSE;
  altered=request;altered.staging.row_stride++;
  BOOL available=WOPT_DSL_capture_staged_movement(scope,value,&altered,staged,reason);
  return Check(!available || !staged.Same_content(direct),"H2_changed_staging_stride") && H2_checkpoint("reduction.after");
}

/* Query real LayerNorm owners without native mutation. Positive imports prove
 * capacity replacement, separate ns service, stamp equality and stale prepare
 * refusal; deliberately incomplete imports must never produce a selection. */
static BOOL DSL_gpu_owner_normalization(WN *scope,WN *value,
    const DSL_GPU_DEVICE_CONTEXT *context,const DSL_GPU_DEVICE_CONTEXT *changed,
    BOOL expect_pass,FILE *trace)
{
  WOPT_DSL_NORMALIZATION_COST_REQUEST request;
  if (!LN3_Request(scope,value,"sm_90",2,&request)) return FALSE;
  VHO_DSL_GPU_Context=NULL;
  DSL_H3_SEARCH *saved_h3=VHO_DSL_H3_Search;VHO_DSL_H3_Search=NULL;
  WOPT_DSL_NORMALIZATION_COST_RESULT legacy_choice;
  WOPT_DSL_NORMALIZATION_LOWERING legacy;
  if (!WOPT_DSL_select_normalization_plan(scope,value,&request,&legacy_choice,stderr,NULL) ||
      !WOPT_DSL_prepare_normalization_lowering(scope,value,&request,&legacy_choice,&legacy,stderr,NULL) ||
      !Check(!legacy.gpu_stamp.bound && offsetof(DSL_GPU_OWNER_LEGACY_LOWERING,seal)==
          offsetof(WOPT_DSL_NORMALIZATION_LOWERING,seal) && legacy.seal==DSL_gpu_owner_legacy_seal(legacy),
          "gpu_owner_legacy_raw_seal_unchanged")) return FALSE;
  VHO_DSL_GPU_Context=context;VHO_DSL_H3_Search=saved_h3;
  const DSL_GPU_PLAN_SNAPSHOT gpu=DSL_GPU_PLAN_SNAPSHOT::Capture(context);
  DSL_IR_IMAGE_HEADER before,after;DSL_IR_Image_Get_Header(&before);
  UINT32 symbols=ST_Table_Size(CURRENT_SYMTAB),types=TY_Table_Size();
  WOPT_DSL_NORMALIZATION_COST_RESULT choice,quiet;
  if (!WOPT_DSL_select_normalization_plan(scope,value,&request,&choice,stderr,trace) ||
      !WOPT_DSL_select_normalization_plan(scope,value,&request,&quiet,NULL,NULL) ||
      !Check(!memcmp(&choice,&quiet,sizeof(choice)) &&
          DSL_gpu_plan_stamp_equal(choice.gpu_stamp,gpu.Stamp()),"gpu_owner_normalization_quiet_stamp")) return FALSE;
  if (!expect_pass) {
    if (!Check(!choice.selected_kind && !choice.common_selection.selected_plan_id,
        "gpu_owner_incomplete_normalization_no_selection")) return FALSE;
  } else {
    if(!H2_normalization_facts(scope,value,request,context,changed,trace)) return FALSE;
    WOPT_DSL_NORMALIZATION_LOWERING plan,marker,saved;
    memset(&marker,0x5a,sizeof(marker));saved=marker;
    if (!Check(choice.selected_kind && choice.gpu_live_bytes &&
        WOPT_DSL_prepare_normalization_lowering(scope,value,&request,&choice,&plan,stderr,trace),
        "gpu_owner_imported_normalization_prepare")) return FALSE;
    if(saved_h3) {
      // Selected-only same-ABI handoff to the existing fresh-device driver.
      // File output never constructs a candidate PU or changes source tables.
      std::string path=H2_review_prefix+".h3-normalization.plan";FILE *file=fopen(path.c_str(),"wx");
      if(!file) return FALSE;bool written=fwrite(&plan,1,sizeof(plan),file)==sizeof(plan);
      if(fclose(file)!=0 || !written) return FALSE;
    }
    WOPT_DSL_NORMALIZATION_PLAN_BINDING binding;
    if (!WOPT_DSL_bind_normalization_plan(scope,value,&request.plans[1],&binding,stderr,NULL)) return FALSE;
    uint64_t cta_shared,sm_shared,quantum;
    if (!gpu.Resource_limit(DSL_GPU_SHARED_BYTES_PER_CTA_DEFAULT,&cta_shared) ||
        !gpu.Resource_limit(DSL_GPU_SHARED_BYTES_PER_SM,&sm_shared) ||
        !gpu.Resource_limit(DSL_GPU_SHARED_QUANTUM,&quantum) ||
        !Check(!binding.status && binding.shared_limit==std::min(cta_shared,sm_shared) &&
            binding.shared_quantum==quantum,"gpu_owner_normalization_imported_limits")) return FALSE;
    WOPT_DSL_NORMALIZATION_PLAN_REQUEST bad=request.plans[0];
    bad.ranges[0].allocation_bytes=~UINT64(0);
    if (!Check(WOPT_DSL_bind_normalization_plan(scope,value,&bad,&binding,NULL,NULL) && binding.status,
        "gpu_owner_normalization_backing_overflow_refused")) return FALSE;
    uint64_t visible,threads;
    if (!gpu.Resource_limit(DSL_GPU_VISIBLE_HBM_BYTES,&visible) ||
        !gpu.Resource_limit(DSL_GPU_MAX_THREADS_PER_CTA,&threads) || threads>~UINT32(0)-32) return FALSE;
    bad=request.plans[0];bad.ranges[0].allocation_bytes=visible;
    if (!Check(WOPT_DSL_bind_normalization_plan(scope,value,&bad,&binding,NULL,NULL) && binding.status,
        "gpu_owner_normalization_budget_refused")) return FALSE;
    bad=request.plans[0];bad.threads_per_cta=UINT32(threads)+32;
    if (!Check(WOPT_DSL_bind_normalization_plan(scope,value,&bad,&binding,NULL,NULL) && binding.status,
        "gpu_owner_normalization_threads_refused")) return FALSE;
    VHO_DSL_GPU_Context=changed;
    if (!Check(!WOPT_DSL_prepare_normalization_lowering(scope,value,&request,&choice,&marker,NULL,NULL) &&
        !memcmp(&marker,&saved,sizeof(marker)),"gpu_owner_stale_prepare_untouched")) return FALSE;
    if (changed && changed->Resource_sha256()==context->Resource_sha256() &&
        changed->Budget_sha256()==context->Budget_sha256()) {
      WOPT_DSL_NORMALIZATION_COST_RESULT calibrated;
      if (!Check(WOPT_DSL_select_normalization_plan(scope,value,&request,&calibrated,stderr,NULL) &&
          calibrated.selected_kind==(saved_h3?1:choice.selected_kind) &&
          (!saved_h3 || (!calibrated.h3_complete[0] && !calibrated.h3_complete[1] && !calibrated.h3_complete[2])) &&
          !memcmp(calibrated.costs,choice.costs,sizeof(choice.costs)) &&
          !DSL_gpu_plan_stamp_equal(calibrated.gpu_stamp,choice.gpu_stamp),
          saved_h3?"H3_profile_drift_refuses_physical_comparison":"gpu_owner_calibration_invalidates_evidence_not_cycle_scenario")) return FALSE;
      const DSL_GPU_PLAN_SNAPSHOT other=DSL_GPU_PLAN_SNAPSHOT::Capture(changed);
      double low,high,other_low,other_high;
      uint64_t bytes=choice.costs[choice.selected_kind-1].global_sector_bytes;
      BOOL physical=gpu.HBM_service_ns(bytes,&low,&high),other_physical=other.HBM_service_ns(bytes,&other_low,&other_high);
      if (physical && other_physical && bytes &&
          context->Execution(DSL_GPU_HBM_MIXED_RATE)->value!=changed->Execution(DSL_GPU_HBM_MIXED_RATE)->value &&
          !Check(low!=other_low || high!=other_high,"gpu_owner_calibration_changes_only_physical_ns")) return FALSE;
    }
    VHO_DSL_GPU_Context=context;
    if (!Check(WOPT_DSL_prepare_normalization_lowering(scope,value,&request,&choice,&marker,NULL,NULL) &&
        !memcmp(&marker,&plan,sizeof(plan)),"gpu_owner_prepare_restored")) return FALSE;
    double lower=-1,upper=-1;
    BOOL service=gpu.HBM_service_ns(choice.costs[choice.selected_kind-1].global_sector_bytes,&lower,&upper);
    if (!Check(!service || (lower>=0 && upper>=lower),"gpu_owner_physical_ns_separate")) return FALSE;
  }
  DSL_IR_Image_Get_Header(&after);
  return Check(!memcmp(&before,&after,sizeof(before)) && symbols==ST_Table_Size(CURRENT_SYMTAB) &&
      types==TY_Table_Size(),"gpu_owner_normalization_native_unchanged");
}

/* Construct a real ordered reduction PU using the existing staged fixture's
 * builder protocol. Test-driver ownership, never an optimizer PU traversal. */
static BOOL DSL_gpu_owner_reduction_source(PU_Info **pu,WN **value,
    WOPT_DSL_REDUCTION_STAGED_REQUEST *request)
{
  *pu=DSL_Builder_Create_Minimal_PU("gpu_owner_ordered_sum");
  if (!*pu || !DSL_Builder_Select_PU(*pu)) return FALSE;
  Current_PU_Info=*pu;
  TY_IDX input_ty=Tensor("gpu_owner_input",2,"[3,17]"),output_ty=Tensor("gpu_owner_output",1,"[3]");
  DSL_BUILDER_SOURCE_POSITION pos={};
  pos.file_id=DSL_Builder_Register_Source_File(*pu,__FILE__);pos.line=__LINE__;pos.column=1;
  DSL_BUILDER_VALUE input=DSL_Builder_Declare_PU_Formal(*pu,"x",0,input_ty,&pos);
  if (!input || !DSL_Builder_Declare_PU_Result(*pu,"y",0,output_ty,DSL_PU_RESULT_TENSOR,&pos)) return FALSE;
  DSL_OPCODE_ID op=DSL_Opcode_Find(DSL_Domain_Find("common"),"common.reduce_sum",2);
  DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[8]={
    {"attr.axis","1"},{"attr.keepdim","false"},{"attr.accum_dtype","float32"},{"attr.reassociate","false"},
    {"attr.order","ascending_left_fold"},{"attr.identity_bits","00000000"},
    {"attr.fp_environment","rne_gradual_no_traps"},{"attr.nan_policy","class_only"}};
  *value=DSL_Builder_Create_Operator_With_Result(op,2,&input,1,attrs,8,"sum",output_ty);
  if (!*value || !DSL_Builder_Set_Value_Source_Position(*value,&pos) ||
      !DSL_Builder_Append_PU_Value(*pu,*value) || !DSL_Builder_Return_PU_Values(*pu,value,1)) return FALSE;
  memset(request,0,sizeof(*request));
  if (!WOPT_DSL_characterize_reduction(PU_Info_tree_ptr(*pu),*value,&request->base.expected_source,stderr)) return FALSE;
  WOPT_DSL_REDUCTION_DIRECT_REQUEST &r=request->base;
  r.architecture="sm_90";r.instruction=WOPT_DSL_REDUCE_PTX_ADD_RN_F32;
  r.runtime_contract_id=301;r.context_id=201;r.row_mapping=WOPT_DSL_REDUCE_ROW_LINEAR_THREAD;r.launched_threads=32;
  r.input.allocation_id=1;r.input.allocation_bytes=1024;r.input.base_alignment=16;
  r.input.lease=WOPT_DSL_REDUCE_INPUT_SNAPSHOT;r.input.memory_space=WOPT_DSL_REDUCE_BUFFER_DEVICE_GLOBAL;
  r.output=r.input;r.output.allocation_id=2;r.output.lease=WOPT_DSL_REDUCE_OUTPUT_EXCLUSIVE;
  r.before[0]=2;r.before[1]=4;r.before[2]=8|16;r.before[3]=32;
  WOPT_DSL_REDUCTION_STAGING &s=request->staging;
  s.chunk_elements=8;s.scratch_object=401;s.scratch_offset=16;s.row_stride=36;s.allocation_bytes=16+32*36;
  s.per_row_budget=32;s.cta_budget=32768;s.other_shared_bytes=128;s.rows_per_cta=32;s.cta_count=1;
  s.slots=1;s.memory_space=WOPT_DSL_REDUCE_STAGED_SHARED;s.copy_mode=WOPT_DSL_REDUCE_COPY_SYNC_BITS32;
  s.visibility=WOPT_DSL_REDUCE_VISIBLE_SAME_THREAD;s.tail_policy=WOPT_DSL_REDUCE_TAIL_EXACT_COPY_READ;
  for (UINT32 i=0;i<4;++i) s.before[i]=1U<<(i+1);
  s.next[4]=1;
  return TRUE;
}

/* Exercise direct export and staged slicing against current, over-budget and
 * stale imports. Requests contain no RF estimate, so success proves only the
 * resource prefixes actually represented by these APIs. No native mutation. */
static BOOL DSL_gpu_owner_reduction(const DSL_GPU_DEVICE_CONTEXT *context,
    const DSL_GPU_DEVICE_CONTEXT *changed,BOOL expect_pass,FILE *trace)
{
  PU_Info *pu;WN *value;WOPT_DSL_REDUCTION_STAGED_REQUEST request;
  if (!DSL_gpu_owner_reduction_source(&pu,&value,&request)) return FALSE;
  WN *scope=PU_Info_tree_ptr(pu);VHO_DSL_GPU_Context=context;
  DSL_IR_IMAGE_HEADER before,after;DSL_IR_Image_Get_Header(&before);
  UINT32 symbols=ST_Table_Size(CURRENT_SYMTAB),types=TY_Table_Size();
  WOPT_DSL_REDUCTION_STAGED_BINDING binding,quiet;
  if (!WOPT_DSL_bind_reduction_staged(scope,value,&request,&binding,stderr,trace) ||
      !WOPT_DSL_bind_reduction_staged(scope,value,&request,&quiet,NULL,NULL) ||
      !Check(!memcmp(&binding,&quiet,sizeof(binding)),"gpu_owner_reduction_quiet_parity")) return FALSE;
  if (!expect_pass) {
    if (!Check(binding.status && !binding.runtime_obligations,
        "gpu_owner_incomplete_reduction_refused")) return FALSE;
  } else {
    const DSL_GPU_PLAN_SNAPSHOT gpu=DSL_GPU_PLAN_SNAPSHOT::Capture(context);
    uint64_t cta_shared,sm_shared,quantum;
    if (!gpu.Resource_limit(DSL_GPU_SHARED_BYTES_PER_CTA_DEFAULT,&cta_shared) ||
        !gpu.Resource_limit(DSL_GPU_SHARED_BYTES_PER_SM,&sm_shared) ||
        !gpu.Resource_limit(DSL_GPU_SHARED_QUANTUM,&quantum) ||
        !Check(!binding.status && binding.base.live_bytes==2048 &&
            DSL_gpu_plan_stamp_equal(binding.base.gpu_stamp,gpu.Stamp()) &&
            binding.hardware_budget==std::min(cta_shared,sm_shared) && binding.quantum==quantum,
            "gpu_owner_reduction_imported_limits")) return FALSE;
    if(!H2_reduction_facts(scope,value,request,trace)) return FALSE;
    OPEN64_REDUCTION_GUARD guard,guard_marker,guard_saved;
    memset(&guard_marker,0x5a,sizeof(guard_marker));guard_saved=guard_marker;
    WOPT_DSL_REDUCTION_STAGED_SLICE slice,marker,saved;
    memset(&marker,0x5a,sizeof(marker));saved=marker;
    if (!Check(WOPT_DSL_export_reduction_direct_guard(scope,value,&binding.base,&guard,stderr,trace) &&
        WOPT_DSL_reduction_staged_slice(scope,value,&binding,0,0,&slice,stderr,trace),
        "gpu_owner_current_guard_and_slice")) return FALSE;
    VHO_DSL_GPU_Context=changed;
    if (!Check(!WOPT_DSL_export_reduction_direct_guard(scope,value,&binding.base,&guard_marker,NULL,NULL) &&
        !WOPT_DSL_reduction_staged_slice(scope,value,&binding,0,0,&marker,NULL,NULL) &&
        !memcmp(&guard_marker,&guard_saved,sizeof(guard_marker)) && !memcmp(&marker,&saved,sizeof(marker)),
        "gpu_owner_stale_guard_slice_untouched")) return FALSE;
    VHO_DSL_GPU_Context=context;
    if (!Check(WOPT_DSL_reduction_staged_slice(scope,value,&binding,0,0,&marker,NULL,NULL) &&
        !memcmp(&marker,&slice,sizeof(slice)),"gpu_owner_slice_restored")) return FALSE;
    WOPT_DSL_REDUCTION_STAGED_REQUEST bad=request;
    bad.base.input.allocation_bytes=~UINT64(0);
    if (!Check(WOPT_DSL_bind_reduction_staged(scope,value,&bad,&quiet,NULL,NULL) &&
        quiet.base.status==WOPT_DSL_REDUCE_DIRECT_RESOURCE_REFUSED && !quiet.runtime_obligations,
        "gpu_owner_reduction_backing_overflow_refused")) return FALSE;
    bad=request;bad.staging.other_shared_bytes=~UINT64(0);
    if (!Check(WOPT_DSL_bind_reduction_staged(scope,value,&bad,&quiet,NULL,NULL) &&
        quiet.status==WOPT_DSL_REDUCE_STAGED_CAPACITY,"gpu_owner_reduction_shared_refused")) return FALSE;
  }
  DSL_IR_Image_Get_Header(&after);
  return Check(!memcmp(&before,&after,sizeof(before)) && symbols==ST_Table_Size(CURRENT_SYMTAB) &&
      types==TY_Table_Size(),"gpu_owner_reduction_native_unchanged");
}

/* One fresh process per H1b fixture. Optional second pinned profile tests a
 * distinct calibration/resource/budget identity without mutable test contexts.
 * Without it, NULL supplies a deterministic stale imported-context boundary.
 * All source checkpoints use the ordinary mapped writer; no GPU runs occur. */
int main(int argc,char **argv)
{
  if (argc!=8 && argc!=10 && argc!=12) {
    fprintf(stderr,"usage: %s source.B trace DEVICE PROFILE SHA TOOLCHAIN pass|refuse|partial [CHANGED_PROFILE SHA [H3_MODEL SHA]]\n",argv[0]);
    return 2;
  }
  BOOL expect_pass=!strcmp(argv[7],"pass"), partial=!strcmp(argv[7],"partial");
  if (!expect_pass && !partial && strcmp(argv[7],"refuse")) return 2;
  DSL_GPU_DEVICE_CONTEXT_OWNER owner,other;
  if (!DSL_gpu_owner_import(argv[3],argv[4],argv[5],argv[6],&owner)) return 1;
  const DSL_GPU_DEVICE_CONTEXT *changed=NULL;
  if (argc>=10) {
    if (!DSL_gpu_owner_import(argv[3],argv[8],argv[9],argv[6],&other) ||
        DSL_gpu_plan_stamp_equal(DSL_gpu_plan_stamp(owner.Context()),DSL_gpu_plan_stamp(other.Context()))) return 1;
    changed=other.Context();
  }
  DSL_H3_SEARCH h3_search;std::string h3_reason;
  if(argc==12) {
    if(!VHO_h3_import(argv[10],argv[11],DSL_GPU_PLAN_SNAPSHOT::Capture(owner.Context()),h3_search,h3_reason)) return 1;
    VHO_DSL_H3_Search=&h3_search;
  }
  H2_review_prefix=std::string(argv[1])+".H2";
  char variant[]="default";char *source_args[]={argv[0],argv[1],variant};
  if (LN1b_Source_Producer(3,source_args)) return 1;
  WN *scope=PU_Info_tree_ptr(Current_PU_Info),*value=WN_first(WN_func_body(scope));
  while (value && (WN_operator(value)!=OPR_STID || WN_operator(WN_kid0(value))!=OPR_DSL)) value=WN_next(value);
  if (!value) return 1;
  Opt_Level=3;OPT_Fast_Math=FALSE;Roundoff_Level=ROUNDOFF_NONE;IEEE_Arithmetic=IEEE_ACCURATE;
  FILE *trace=fopen(argv[2],"wx");if (!trace) return 1;
  BOOL ok=DSL_gpu_owner_normalization(scope,value,owner.Context(),changed,expect_pass,trace) &&
      DSL_gpu_owner_reduction(owner.Context(),changed,expect_pass || partial,trace);
  h3_search.Print(trace);VHO_DSL_H3_Search=NULL;
  VHO_DSL_GPU_Context=NULL;
  if (fclose(trace)) ok=FALSE;
  if (ok) {
    std::string path=std::string(argv[1])+".preserved.B";
    DSL_BUILDER_MAPPED_IMAGE_REQUEST checkpoint={};checkpoint.path=path.c_str();
    ok=LN1c_Source_Checkpoint(&checkpoint);
  }
  return ok?0:1;
}
