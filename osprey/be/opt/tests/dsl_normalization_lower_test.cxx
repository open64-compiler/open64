/* Copyright (C) 2026 Open64 Project */

/* LN4 native driver fixture: select in analysis scope, explicitly select an
 * empty device PU, materialize and reopen its mapped image. No reduction code.
 * Design: doc/AI-COMPILER-LAYERNORM-EXECUTABLE-LOWERING-v0.1.md. */
#define DSL_NORMALIZATION_COST_NO_MAIN
#include "dsl_normalization_cost_test.cxx"
#include "opt_dsl_normalization_lower.h"
#include "dsl_gpu_profile.h"
#include "config_dsl.h"
#include "ir_bread.h"
#include "ir_reader.h"

/* Persist native review-only structs without pretending to define a portable
 * file format. Existing paths are never replaced. */
static BOOL LN4_Binary(const char *path,void *data,size_t bytes,BOOL write)
{
  FILE *f=fopen(path,write?"wx":"rb");if (!f) return FALSE;
  BOOL ok=write?fwrite(data,1,bytes,f)==bytes:fread(data,1,bytes,f)==bytes && fgetc(f)==EOF;
  return fclose(f)==0 && ok;
}

/* Find one actual nested instruction/loop for negative identity controls. */
static WN *LN4_Find(WN *n,OPERATOR op)
{
  if (!n) return NULL;if (WN_operator(n)==op) return n;
  if (WN_operator(n)==OPR_BLOCK) {for (WN *s=WN_first(n);s;s=WN_next(s)) {WN *v=LN4_Find(s,op);if (v) return v;}}
  else for (INT32 i=0;i<WN_kid_count(n);++i) {WN *v=LN4_Find(WN_kid(n,i),op);if (v) return v;}
  return NULL;
}

/* Reopen through the ordinary mapped reader in a separate process. The
 * driver's reader, not the lowering service, establishes active PU context. */
static int LN4_Reopen(char **argv)
{
  MEM_Initialize();Set_Error_Tables(Phases,host_errlist);Init_Error_Handler(10);
  Set_Error_File(NULL);Set_Error_Line(ERROR_LINE_UNKNOWN);Preconfigure();Init_Controls_Tbl();
  ABI_Name="n64";Configure();IR_reader_init();
  WOPT_DSL_NORMALIZATION_LOWERING plan;WOPT_DSL_NORMALIZATION_DEVICE product;
  if (!LN4_Binary(argv[3],&plan,sizeof(plan),FALSE) || !LN4_Binary(argv[4],&product,sizeof(product),FALSE)) return 1;
  if (!Open_Input_Info(argv[2])) return 1;
  Initialize_Symbol_Tables(FALSE);New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
  PU_Info *pu=Read_Global_Info(NULL);if (!pu || PU_Info_next(pu)) return 1;
  Read_Local_Info(MEM_pu_nz_pool_ptr,pu);Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);
  Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);
  FILE *trace=fopen(argv[5],"wx");if (!trace) return 1;
  BOOL ok=WOPT_DSL_verify_normalization_device(&plan,&product,stderr,trace);
  fclose(trace);return ok?0:1;
}

/* Actual WHIRL mutations must fail even when the numeric selected plan is
 * unchanged. Restoring each mutation must recover the same verified body. */
static BOOL LN4_Mutations(const WOPT_DSL_NORMALIZATION_LOWERING &plan,
    const WOPT_DSL_NORMALIZATION_DEVICE &product,FILE *trace)
{
  long length=ftell(trace);WN *root=PU_Info_tree_ptr(Current_PU_Info);
  WN *asm_node=LN4_Find(root,OPR_ASM_STMT),*loop=LN4_Find(root,OPR_DO_LOOP);
  if (!asm_node || !loop) return FALSE;
  UINT32 flags=WN_asm_flag(asm_node);WN_asm_flag(asm_node)=0;
  if (WOPT_DSL_verify_normalization_device(&plan,&product,NULL,trace)) return FALSE;WN_asm_flag(asm_node)=flags;
  WN *bound=WN_kid1(WN_end(loop));INT64 extent=WN_const_val(bound);WN_const_val(bound)=extent+1;
  if (WOPT_DSL_verify_normalization_device(&plan,&product,NULL,trace)) return FALSE;WN_const_val(bound)=extent;
  ST_ATTR *attribute=NULL;
  for (ST_ATTR_IDX i=1;i<ST_ATTR_Table_Size(GLOBAL_SYMTAB);++i) {
    ST_ATTR &a=St_Attr_Table(GLOBAL_SYMTAB,i);
    if (ST_ATTR_st_idx(a)==PU_Info_proc_sym(Current_PU_Info) && ST_ATTR_kind(a)==ST_ATTR_DEVICE_SYMBOL) attribute=&a;
  }
  if (!attribute) return FALSE;attribute->Set_value(DEVICE_SYMBOL_FUNCTION);
  if (WOPT_DSL_verify_normalization_device(&plan,&product,NULL,trace)) return FALSE;attribute->Set_value(DEVICE_SYMBOL_KERNEL);
  WN *input=WN_kid0(WN_kid(asm_node,2));ST_IDX symbol=WN_st_idx(input);WN_st_idx(input)=WN_st_idx(WN_kid0(WN_kid(asm_node,3)));
  if (WOPT_DSL_verify_normalization_device(&plan,&product,NULL,trace)) return FALSE;WN_st_idx(input)=symbol;
  WN *body=WN_func_body(root),*barrier=NULL;
  for (WN *s=WN_first(body);s;s=WN_next(s)) if (WN_operator(s)==OPR_CALL) {barrier=s;break;}
  if (barrier) {
    UINT32 flags=WN_call_flag(barrier);WN_call_flag(barrier)^=WN_CALL_NON_DATA_MOD;
    if (WOPT_DSL_verify_normalization_device(&plan,&product,NULL,trace)) return FALSE;WN_call_flag(barrier)=flags;
    WN *previous=WN_prev(barrier);WN_EXTRACT_FromBlock(body,barrier);
    if (WOPT_DSL_verify_normalization_device(&plan,&product,NULL,trace)) return FALSE;
    WN_INSERT_BlockAfter(body,previous,barrier);
  }
  const ST *array;ST_IDX index;
  FOREACH_SYMBOL(CURRENT_SYMTAB,array,index) if (ST_class(array)==CLASS_VAR && TY_kind(ST_type(array))==KIND_ARRAY) {
    ARB_HANDLE bounds=TY_arb(ST_type(array));INT64 upper=ARB_ubnd_val(bounds);Set_ARB_ubnd_val(bounds,upper+1);
    if (WOPT_DSL_verify_normalization_device(&plan,&product,NULL,trace)) return FALSE;Set_ARB_ubnd_val(bounds,upper);
  }
  return Check(ftell(trace)==length && WOPT_DSL_verify_normalization_device(&plan,&product,NULL,NULL),"LN4_mutations_refused_restored_identity");
}

static int LN4_Device(char **argv);
/* Native source-before/after images remain identical. A successful selected
 * handoff becomes a separate executable device image, not a source rewrite. */
int main(int argc,char **argv)
{
  if (argc==5 && (!strcmp(argv[1],"device") || !strcmp(argv[1],"device-quiet"))) return LN4_Device(argv);
  if (argc==9 && !strcmp(argv[1],"device-gpu")) return LN4_Device(argv);
  if (argc==6 && !strcmp(argv[1],"reopen")) return LN4_Reopen(argv);
  if (argc!=7) return 2;
  char mixed[]="mixed";char *source_args[]={argv[0],argv[1],!strcmp(argv[5],"legacy")?mixed:argv[5]};if (LN1b_Source_Producer(3,source_args)) return 1;
  WN *scope=PU_Info_tree_ptr(Current_PU_Info),*value=WN_first(WN_func_body(scope));
  while (value && (WN_operator(value)!=OPR_STID || WN_operator(WN_kid0(value))!=OPR_DSL)) value=WN_next(value);
  if (!strcmp(argv[5],"legacy")) {
    while (value) {
      WOPT_DSL_NORMALIZATION_CHARACTERIZATION s;
      if (WN_operator(value)==OPR_STID && WOPT_DSL_characterize_normalization(scope,value,&s,NULL) && s.recipe==1) break;
      value=WN_next(value);
    }
  }
  if (!value) return 1;
  OPT_Fast_Math=FALSE;Roundoff_Level=ROUNDOFF_NONE;IEEE_Arithmetic=IEEE_ACCURATE;
  UINT32 scenario=atoi(argv[6]);if (scenario>=sizeof(ln3_scenarios)/sizeof(ln3_scenarios[0])) return 2;
  WOPT_DSL_NORMALIZATION_COST_REQUEST request;if (!LN3_Request(scope,value,argv[4],scenario,&request)) return 1;
  FILE *trace=fopen(argv[3],"wx");if (!trace) return 1;
  fprintf(trace,"AI_NORM_EXPERIMENT 1 CASE %s %u\n",ln3_scenarios[scenario],request.plans[0].target.expected_source.source_line);
  WOPT_DSL_NORMALIZATION_COST_RESULT choice;
  if (!WOPT_DSL_select_normalization_plan(scope,value,&request,&choice,stderr,trace)) return 1;
  WOPT_DSL_NORMALIZATION_LOWERING plan,quiet,saved;memset(&plan,0x5a,sizeof(plan));saved=plan;
  long length=ftell(trace);UINT32 types=TY_Table_Size(),symbols=ST_Table_Size(CURRENT_SYMTAB);
  WOPT_DSL_NORMALIZATION_COST_RESULT bad=choice;bad.selected_kind=bad.selected_kind==1?2:1;
  if (!Check(!WOPT_DSL_prepare_normalization_lowering(scope,value,&request,&bad,&plan,NULL,trace) &&
      !WOPT_DSL_prepare_normalization_lowering(WN_func_body(scope),value,&request,&choice,&plan,NULL,trace) &&
      !memcmp(&plan,&saved,sizeof(plan)) && ftell(trace)==length,"LN4_stale_selection_scope_no_publication")) return 1;
  BOOL admitted=WOPT_DSL_prepare_normalization_lowering(scope,value,&request,&choice,&plan,stderr,trace);
  if (!Check(admitted==(choice.selected_kind!=0),"LN4_only_complete_selected_plan")) return 1;
  if (admitted && !Check(WOPT_DSL_prepare_normalization_lowering(scope,value,&request,&choice,&quiet,NULL,NULL) &&
      !memcmp(&plan,&quiet,sizeof(plan)) && symbols==ST_Table_Size(CURRENT_SYMTAB) && types==TY_Table_Size(),"LN4_prepare_quiet_read_only")) return 1;
  char path[4096];snprintf(path,sizeof(path),"%s.source.after.B",argv[2]);
  DSL_BUILDER_MAPPED_IMAGE_REQUEST checkpoint={};checkpoint.path=path;if (!LN1c_Source_Checkpoint(&checkpoint)) return 1;
  if (!admitted) {fclose(trace);return 0;}
  snprintf(path,sizeof(path),"%s.plan",argv[2]);if (!LN4_Binary(path,&plan,sizeof(plan),TRUE)) return 1;
  fclose(trace);
  /* Fresh driver context avoids copying analysis DSL formal identities into
   * pointer-kernel metadata. This is a same-ABI numeric handoff, not IPC ABI. */
  char *device_args[]={argv[0],(char *)"device",argv[2],path,argv[3],NULL};
  execv(argv[0],device_args);return 1;
}

/* A separate driver invocation owns the device translation unit and writer.
 * Optional pinned H1 import reestablishes the selected compiler context, without
 * search or source-policy selection. No analysis tables are reset inside the
 * lowering service; old no-import modes retain their protocol. */
static int LN4_Device(char **argv)
{
  Initialize_Context();if (!DSL_Builder_Begin_Program()) return 1;
  WOPT_DSL_NORMALIZATION_LOWERING plan;if (!LN4_Binary(argv[3],&plan,sizeof(plan),FALSE)) return 1;
  DSL_GPU_DEVICE_CONTEXT_OWNER gpu_owner;
  if(!strcmp(argv[1],"device-gpu")) {
    DSL_HARDWARE_TARGET_SELECTION target;
    if(!DSL_hardware_target_resolve(NULL,argv[5],&target,stderr) ||
        !DSL_gpu_profile_resolve(target,argv[6],argv[7],argv[8],&gpu_owner,stderr)) return 1;
    VHO_DSL_GPU_Context=gpu_owner.Context();
  }
  FILE *trace=fopen(argv[4],"a");if (!trace) return 1;
  FILE *actions=!strcmp(argv[1],"device-quiet")?NULL:trace;
  OPT_Fast_Math=FALSE;Roundoff_Level=ROUNDOFF_NONE;IEEE_Arithmetic=IEEE_ACCURATE;
  char path[4096];UINT32 symbols,types;long length;
  char kernel_name[80];snprintf(kernel_name,sizeof(kernel_name),"ln_ref_c%u",(UINT32)plan.guard.kind-1);
  PU_Info *device=DSL_Builder_Create_Minimal_PU(kernel_name);if (!device || !DSL_Builder_Select_PU(device)) return 1;
  ST &kernel=St_Table[PU_Info_proc_sym(device)];Set_ST_device_symbol_kind(kernel,DEVICE_SYMBOL_KERNEL);Set_ST_export(kernel,EXPORT_PREEMPTIBLE);
  char source_file[4096];const char *slash=strrchr(__FILE__,'/');
  snprintf(source_file,sizeof(source_file),"%.*s/dsl_normalization_ref_test.cxx",slash?(int)(slash-__FILE__):1,slash?__FILE__:".");
  UINT32 file=DSL_Builder_Register_Source_File(device,source_file);USRCPOS source;USRCPOS_clear(source);
  USRCPOS_filenum(source)=file;USRCPOS_linenum(source)=plan.guard.source_line;
  snprintf(path,sizeof(path),"%s.device.before.B",argv[2]);if (!LN1c_Scalar_Image(device,path)) return 1;
  WOPT_DSL_NORMALIZATION_DEVICE product,untouched;memset(&product,0x5a,sizeof(product));untouched=product;
  WOPT_DSL_NORMALIZATION_LOWERING corrupt=plan;corrupt.guard.kind=corrupt.guard.kind==1?2:1;
  symbols=ST_Table_Size(CURRENT_SYMTAB);types=TY_Table_Size();length=ftell(trace);
  if (!Check(!WOPT_DSL_materialize_normalization_device(&corrupt,USRCPOS_srcpos(source),&product,NULL,trace) &&
      !WOPT_DSL_materialize_normalization_device(&plan,0,&product,NULL,trace) &&
      symbols==ST_Table_Size(CURRENT_SYMTAB) && types==TY_Table_Size() && ftell(trace)==length &&
      !memcmp(&product,&untouched,sizeof(product)),"LN4_invalid_materialization_no_mutation")) return 1;
  OPT_Fast_Math=TRUE;
  BOOL unsafe=!WOPT_DSL_materialize_normalization_device(&plan,USRCPOS_srcpos(source),&product,NULL,trace);
  OPT_Fast_Math=FALSE;
  if (!Check(unsafe && symbols==ST_Table_Size(CURRENT_SYMTAB) && types==TY_Table_Size() && ftell(trace)==length,
      "LN4_unsafe_FP_no_mutation") ||
      !WOPT_DSL_materialize_normalization_device(&plan,USRCPOS_srcpos(source),&product,stderr,actions) ||
      !WOPT_DSL_verify_normalization_device(&plan,&product,stderr,actions) || !LN4_Mutations(plan,product,trace)) return 1;
  if (!Check(!WOPT_DSL_materialize_normalization_device(&plan,USRCPOS_srcpos(source),&untouched,NULL,NULL),"LN4_nonempty_kernel_refused")) return 1;
  snprintf(path,sizeof(path),"%s.product",argv[2]);if (!LN4_Binary(path,&product,sizeof(product),TRUE)) return 1;
  snprintf(path,sizeof(path),"%s.device.after.B",argv[2]);
  if (!DSL_Gatekeeper_Verify_PU(device,stderr,NULL) || !LN1c_Scalar_Image(device,path)) return 1;
  fclose(trace);return 0;
}
