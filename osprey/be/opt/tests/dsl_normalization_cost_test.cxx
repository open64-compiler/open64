/* Copyright (C) 2026 Open64 Project */

/* LN3 native cost/selection controls and source preservation, no reduction code.
 * Design: doc/AI-COMPILER-LAYERNORM-PROFITABILITY-v0.1.md. */
#define DSL_NORMALIZATION_PLAN_NO_MAIN
#include "dsl_normalization_plan_test.cxx"
#include "opt_dsl_normalization_cost.h"

static const char *ln3_scenarios[]={"builtin","balanced","global_slow","shared_slow","uncertain","missing",
    "wrong_revision","budget","rf_unknown","rf_exhausted","overflow","margin","tie","rf_pressure"};

/* Explicit synthetic hardware-extension inputs, never NVIDIA specification or
 * private optimizer constants. Built-in descriptions remain incomplete. */
static BOOL LN3_Request(WN *scope,WN *value,const char *arch,UINT32 scenario,WOPT_DSL_NORMALIZATION_COST_REQUEST *r)
{
  memset(r,0,sizeof(*r));
  for (UINT32 i=0;i<3;++i) if (!LN2_Request(scope,value,arch,i+1,&r->plans[i])) return FALSE;
  DSL_HARDWARE_DESCRIPTION_RECORD hw;
  if (!DSL_hardware_description_find_arch(arch,&hw) || !DSL_hardware_scalar_execution_profile_get_arch(hw.arch_id,&r->profile)) return FALSE;
  r->registers_per_thread[0]=r->registers_per_thread[1]=r->registers_per_thread[2]=32;r->max_jobs=500000;
  if (!scenario) return TRUE;
  r->profile.model_id=1000+scenario;
  const UINT64 values[DSL_SCALAR_FIELD_COUNT]={32,128,128,100,2,120,4,4,32,32,1,8,16,32,0,8,32,16,16};
  for (UINT32 f=0;f<DSL_SCALAR_FIELD_COUNT;++f) {
    r->profile.provenance[f]=3;
    for (UINT32 c=0;c<3;++c) r->profile.values[f][c]=values[f];
  }
  if (scenario==2) for (UINT32 c=0;c<3;++c) {
    r->profile.values[DSL_SCALAR_GLOBAL_LATENCY][c]=100000;r->profile.values[DSL_SCALAR_SHARED_LATENCY][c]=2;
  }
  if (scenario==3) for (UINT32 c=0;c<3;++c) {
    r->profile.values[DSL_SCALAR_GLOBAL_LATENCY][c]=1;r->profile.values[DSL_SCALAR_SHARED_LATENCY][c]=100000;
  }
  if (scenario==4) for (UINT32 f=0;f<DSL_SCALAR_SECTOR_BYTES;++f) {
    r->profile.values[f][0]=f<=DSL_SCALAR_COMPUTE_RATE?1:0;r->profile.values[f][2]=100000;
  }
  if (scenario==5) {r->profile.provenance[DSL_SCALAR_DIV_LATENCY]=0;memset(r->profile.values[DSL_SCALAR_DIV_LATENCY],0,sizeof(r->profile.values[0]));}
  if (scenario==6) ++r->profile.description_revision;
  if (scenario==7) r->max_jobs=1;
  if (scenario==8) r->registers_per_thread[1]=0;
  if (scenario==9) r->registers_per_thread[1]=256;
  if (scenario==10) for (UINT32 c=0;c<3;++c) r->profile.values[DSL_SCALAR_GLOBAL_LATENCY][c]=~UINT64(0);
  if (scenario==11) r->margin=~UINT64(0);
  if (scenario==12) {
    for (UINT32 f=0;f<DSL_SCALAR_FIELD_COUNT;++f) for (UINT32 c=0;c<3;++c)
      r->profile.values[f][c]=f<3?1000000:f==DSL_SCALAR_SECTOR_BYTES?32:0;
    for (UINT32 c=0;c<3;++c) {r->profile.values[DSL_SCALAR_GLOBAL_LATENCY][c]=3;r->profile.values[DSL_SCALAR_BARRIER_CYCLES][c]=1;}
  }
  if (scenario==13) r->registers_per_thread[1]=r->registers_per_thread[2]=200;
  return TRUE;
}

/* Repeated quiet queries must preserve every native/table fact and yield the
 * same selected guard/costs/IDs. Unknown costs never grant a common selection. */
static BOOL LN3_Query(WN *scope,WN *value,const char *arch,UINT32 scenario,FILE *trace)
{
  WOPT_DSL_NORMALIZATION_COST_REQUEST r;if (!LN3_Request(scope,value,arch,scenario,&r)) return FALSE;
  WOPT_DSL_NORMALIZATION_COST_RESULT a,b;DSL_IR_IMAGE_HEADER before,after;DSL_IR_Image_Get_Header(&before);
  UINT32 symbols=ST_Table_Size(CURRENT_SYMTAB),types=TY_Table_Size();
  fprintf(trace,"AI_NORM_EXPERIMENT 1 CASE %s %u\n",ln3_scenarios[scenario],r.plans[0].target.expected_source.source_line);
  if (!WOPT_DSL_select_normalization_plan(scope,value,&r,&a,stderr,trace) ||
      !WOPT_DSL_select_normalization_plan(scope,value,&r,&b,NULL,NULL)) return FALSE;
  DSL_IR_Image_Get_Header(&after);
  if (!Check(!memcmp(&a,&b,sizeof(a)) && !memcmp(&before,&after,sizeof(before)) &&
      symbols==ST_Table_Size(CURRENT_SYMTAB) && types==TY_Table_Size(),"LN3_quiet_read_only_parity")) return FALSE;
  for (UINT32 i=0;i<3;++i) if (!a.costs[i].status) {
    UINT64 sum=0;for (UINT32 t=0;t<DSL_OPT_COST_TERM_COUNT;++t) sum+=a.costs[i].terms[t];
    if (!Check(sum==a.costs[i].total && a.costs[i].lower<=sum && sum<=a.costs[i].upper,"LN3_complete_components_bounds")) return FALSE;
  }
  if (a.selected_kind && !Check(a.fallback_kind==1 && a.common_selection.selected_plan_id &&
      a.selected_guard.source_version==2 && a.selected_guard.kind==a.selected_kind,"LN3_analysis_selection_guard")) return FALSE;
  return TRUE;
}

/* Exercise invalid API/scope/comparability without any output/trace writes. */
static BOOL LN3_Negatives(WN *scope,WN *value,const char *arch,FILE *trace)
{
  WOPT_DSL_NORMALIZATION_COST_REQUEST r;if (!LN3_Request(scope,value,arch,1,&r)) return FALSE;
  WOPT_DSL_NORMALIZATION_COST_RESULT out,saved;memset(&out,0x5a,sizeof(out));saved=out;long length=ftell(trace);
  if (!Check(!WOPT_DSL_select_normalization_plan(WN_func_body(scope),value,&r,&out,NULL,trace) &&
      !WOPT_DSL_select_normalization_plan(scope,WN_COPY_Tree(value),&r,&out,NULL,trace) &&
      !WOPT_DSL_select_normalization_plan(scope,value,NULL,&out,NULL,trace) &&
      !WOPT_DSL_select_normalization_plan(scope,value,&r,NULL,NULL,trace),"LN3_invalid_api")) return FALSE;
  ++r.plans[1].context_id;
  if (!Check(!WOPT_DSL_select_normalization_plan(scope,value,&r,&out,NULL,trace) &&
      !memcmp(&out,&saved,sizeof(out)) && ftell(trace)==length,"LN3_invalid_no_publication")) return FALSE;
  for (UINT32 f=0;f<DSL_SCALAR_FIELD_COUNT;++f) for (UINT32 fault=0;fault<3;++fault) {
    if (!LN3_Request(scope,value,arch,1,&r)) return FALSE;
    if (!fault) {r.profile.provenance[f]=0;memset(r.profile.values[f],0,sizeof(r.profile.values[f]));}
    if (fault==1) r.profile.provenance[f]=4;
    if (fault==2) {r.profile.values[f][0]=1;r.profile.values[f][1]=r.profile.values[f][2]=0;}
    if (!WOPT_DSL_select_normalization_plan(scope,value,&r,&out,NULL,NULL) || out.selected_kind || out.common_selection.selected_plan_id) return FALSE;
    for (UINT32 i=0;i<3;++i) if (!Check(out.costs[i].status==(out.costs[i].plan_status?1U:fault?3U:2U),"LN3_each_profile_unknown_or_invalid")) return FALSE;
  }
  return Check(ftell(trace)==length,"LN3_quiet_profile_refusals");
}

/* Publish actual native cost traces and paired source checkpoints. Selection
 * is analysis-only; no normalization kernel/provider is invoked or emitted. */
int main(int argc,char **argv)
{
  if (argc!=5) return 2;
  char *args[]={argv[0],argv[1],argv[4]};if (LN1b_Source_Producer(3,args)) return 1;
  WN *scope=PU_Info_tree_ptr(Current_PU_Info);OPT_Fast_Math=FALSE;Roundoff_Level=ROUNDOFF_NONE;IEEE_Arithmetic=IEEE_ACCURATE;
  FILE *trace=fopen(argv[2],"wx");if (!trace) return 1;
  BOOL first=TRUE;
  for (WN *s=WN_first(WN_func_body(scope));s;s=WN_next(s)) {
    if (WN_operator(s)!=OPR_STID || WN_operator(WN_kid0(s))!=OPR_DSL) continue;
    for (UINT32 scenario=0;scenario<sizeof(ln3_scenarios)/sizeof(ln3_scenarios[0]);++scenario)
      if (!LN3_Query(scope,s,argv[3],scenario,trace)) return 1;
    if (first && !LN3_Negatives(scope,s,argv[3],trace)) return 1;first=FALSE;
  }
  if (fclose(trace)) return 1;
  char path[4096];if (snprintf(path,sizeof(path),"%s.preserved.B",argv[1])>=(int)sizeof(path)) return 1;
  DSL_BUILDER_MAPPED_IMAGE_REQUEST checkpoint={};checkpoint.path=path;
  return LN1c_Source_Checkpoint(&checkpoint)?0:1;
}
