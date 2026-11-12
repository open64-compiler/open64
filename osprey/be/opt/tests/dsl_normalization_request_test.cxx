/* Copyright (C) 2026 Open64 Project */

/* LN4 request fixture: explicit synthetic input, typed-JIT parity and ordinary
 * mapped-image reopen. No reduction implementation or GPU execution.
 * Design: doc/AI-COMPILER-LAYERNORM-DRIVER-REQUEST-v0.1.md. */
#define DSL_NORMALIZATION_COST_NO_MAIN
#include "dsl_normalization_cost_test.cxx"
#include "opt_dsl_normalization_request.h"
#include "ir_bread.h"
#include "ir_bcom.h"
#include "../../../rapidjson/document.h"
#include "../../../rapidjson/stringbuffer.h"
#include "../../../rapidjson/writer.h"
#include <fenv.h>
#if defined(__SSE__)
#include <xmmintrin.h>
#endif

typedef rapidjson::Writer<rapidjson::StringBuffer> LN4R_WRITER;

/* Test input uses a structured JSON writer, never a compiler-side source parser. */
static void LN4RT_Word(LN4R_WRITER &w, const char *key, UINT64 value)
{
  w.Key(key); w.Uint64(value);
}

/* Fixed native identity arrays retain exact slot order. */
static void LN4RT_Array(LN4R_WRITER &w, const char *key, const UINT32 *words, UINT32 count)
{
  w.Key(key); w.StartArray(); for (UINT32 i = 0; i < count; ++i) w.Uint(words[i]); w.EndArray();
}

/* Export a versioned review request from fixture facts only. Synthetic timing
 * is preserved with provenance 3 and never installed into builtin profiles. */
static std::string LN4RT_JSON(const WOPT_DSL_NORMALIZATION_REQUEST &r)
{
  rapidjson::StringBuffer buffer; LN4R_WRITER w(buffer);
  w.StartObject(); w.Key("schema"); w.String("open64.layernorm.request.v1");
  w.Key("source"); w.StartObject(); const WOPT_DSL_NORMALIZATION_CHARACTERIZATION &s = r.source;
  LN4RT_Word(w,"recipe",s.recipe); LN4RT_Word(w,"flags",s.flags); LN4RT_Word(w,"owner_pu",s.owner_pu);
  LN4RT_Word(w,"node_id",s.node_id); LN4RT_Array(w,"operand_value_ids",s.operand_value_ids,3);
  LN4RT_Word(w,"result_value_id",s.result_value_id); LN4RT_Array(w,"operand_types",s.operand_types,3);
  LN4RT_Word(w,"result_ty",s.result_ty); LN4RT_Word(w,"rows",s.rows); LN4RT_Word(w,"width",s.width);
  LN4RT_Word(w,"epsilon_bits",s.epsilon_bits); LN4RT_Word(w,"source_line",s.source_line); w.EndObject();
  w.Key("target"); w.StartObject(); w.Key("architecture"); w.String(r.architecture);
  LN4RT_Word(w,"route",r.route); LN4RT_Word(w,"toolkit",r.toolkit); LN4RT_Word(w,"ptx",r.ptx);
  LN4RT_Word(w,"environment",r.environment); LN4RT_Word(w,"fp_options",r.fp_options); w.EndObject();
  w.Key("invocation"); w.StartObject(); LN4RT_Word(w,"runtime_contract_id",r.runtime_contract_id);
  LN4RT_Word(w,"context_id",r.context_id); LN4RT_Word(w,"launched_ctas",r.launched_ctas);
  LN4RT_Word(w,"threads_per_cta",r.threads_per_cta); w.Key("ranges"); w.StartArray();
  for (UINT32 i = 0; i < 4; ++i) {
    const WOPT_DSL_NORMALIZATION_RANGE &a = r.ranges[i]; w.StartObject();
    LN4RT_Word(w,"allocation_id",a.allocation_id); LN4RT_Word(w,"allocation_bytes",a.allocation_bytes);
    LN4RT_Word(w,"offset_bytes",a.offset_bytes); LN4RT_Word(w,"base_alignment",a.base_alignment);
    LN4RT_Word(w,"lease",a.lease); w.EndObject();
  }
  w.EndArray(); w.EndObject(); w.Key("profile"); w.StartObject();
  LN4RT_Word(w,"version",r.profile.version); LN4RT_Word(w,"arch_id",r.profile.arch_id);
  LN4RT_Word(w,"description_revision",r.profile.description_revision); LN4RT_Word(w,"model_id",r.profile.model_id);
  w.Key("fields"); w.StartArray();
  for (UINT32 f = 0; f < DSL_SCALAR_FIELD_COUNT; ++f) {
    w.StartObject(); LN4RT_Word(w,"id",f); LN4RT_Word(w,"provenance",r.profile.provenance[f]);
    w.Key("bounds"); w.StartArray(); for (UINT32 c = 0; c < 3; ++c) w.Uint64(r.profile.values[f][c]);
    w.EndArray(); w.EndObject();
  }
  w.EndArray(); w.EndObject(); LN4RT_Array(w,"registers_per_thread",r.registers_per_thread,3);
  LN4RT_Word(w,"max_jobs",r.max_jobs); LN4RT_Word(w,"margin",r.margin); w.EndObject();
  return buffer.GetString();
}

/* Quiet typed and decoded requests must yield the identical handoff; malformed
 * requests and unsafe compilation sessions leave sentinel outputs untouched. */
static BOOL LN4RT_Controls(WN *scope, const WOPT_DSL_NORMALIZATION_REQUEST &r, const std::string &json)
{
  const WOPT_DSL_NORMALIZATION_DRIVER_API &api = WOPT_DSL_normalization_driver_api;
  WOPT_DSL_NORMALIZATION_REQUEST decoded, saved; memset(&decoded,0x5a,sizeof(decoded)); saved=decoded;
  const char *bad[] = {"{}", "[]", "{\"schema\":1,\"schema\":1}", "{", "true"};
  for (UINT32 i=0;i<sizeof(bad)/sizeof(bad[0]);++i)
    if (api.decode(bad[i],strlen(bad[i]),&decoded,NULL) || memcmp(&decoded,&saved,sizeof(saved))) return FALSE;
  if (!api.decode(json.data(),json.size(),&decoded,stderr) || memcmp(&decoded,&r,sizeof(r))) return FALSE;
  WOPT_DSL_NORMALIZATION_LOWERING a,b,unchanged; memset(&a,0x5a,sizeof(a)); unchanged=a; SRCPOS pos=123;
  for (UINT32 level=0;level<3;++level) if (api.prepare(scope,&r,level,&a,&pos,NULL,NULL) ||
      memcmp(&a,&unchanged,sizeof(a)) || pos!=123) return FALSE;
  int mode=fegetround(); if (fesetround(FE_DOWNWARD)) return FALSE;
  BOOL refused=!api.prepare(scope,&r,3,&a,&pos,NULL,NULL); fesetround(mode);
  if (!refused || memcmp(&a,&unchanged,sizeof(a)) || pos!=123) return Check(FALSE,"LN4_request_rounding_gate");
  INT pointer_size=Pointer_Size; TYPE_ID pointer_type=Pointer_Mtype;
  Pointer_Size=4; Pointer_Mtype=MTYPE_U4;
  refused=!api.prepare(scope,&r,3,&a,&pos,NULL,NULL); Pointer_Size=pointer_size; Pointer_Mtype=pointer_type;
  if (!refused || memcmp(&a,&unchanged,sizeof(a)) || pos!=123) return Check(FALSE,"LN4_request_pointer_gate");
  OPT_Fast_Math=TRUE; refused=!api.prepare(scope,&r,3,&a,&pos,NULL,NULL); OPT_Fast_Math=FALSE;
  if (!refused || memcmp(&a,&unchanged,sizeof(a)) || pos!=123) return Check(FALSE,"LN4_request_fast_math_gate");
#if defined(__SSE__)
  UINT32 control=_mm_getcsr(); _mm_setcsr(control | (1U<<15) | (1U<<6));
  refused=!api.prepare(scope,&r,3,&a,&pos,NULL,NULL); _mm_setcsr(control);
  if (!refused || memcmp(&a,&unchanged,sizeof(a)) || pos!=123) return Check(FALSE,"LN4_request_ftz_daz_gate");
  _mm_setcsr((control & ~0x6000U) | 0x2000U);
  refused=!api.prepare(scope,&r,3,&a,&pos,NULL,NULL); _mm_setcsr(control);
  if (!refused || memcmp(&a,&unchanged,sizeof(a)) || pos!=123) return Check(FALSE,"LN4_request_independent_SSE_rounding_gate");
  _mm_setcsr(control & ~(1U<<7)); UINT32 trap_control=_mm_getcsr();
  if (trap_control==(control & ~(1U<<7))) {
    refused=!api.prepare(scope,&r,3,&a,&pos,NULL,NULL); _mm_setcsr(control);
    if (!refused || memcmp(&a,&unchanged,sizeof(a)) || pos!=123) return Check(FALSE,"LN4_request_sse_trap_gate");
  } else {
    _mm_setcsr(control);
    if (trap_control!=control) return Check(FALSE,"LN4_request_unexpected_host_control_readback");
    printf("contract LN4_request_sse_trap_mask_change UNSUPPORTED requested=%x readback=%x\n",control & ~(1U<<7),trap_control);
  }
#endif
  BOOL first=api.prepare(scope,&r,3,&a,&pos,NULL,NULL), second=api.prepare(scope,&decoded,3,&b,&pos,NULL,NULL);
  return Check(first==second && (!first || !memcmp(&a,&b,sizeof(a))),"LN4_request_typed_JSON_parity_session_gates");
}

/* Read review JSON only; production requests always use the bounded decoder. */
static std::string LN4RT_Read(const char *path)
{
  FILE *f=fopen(path,"rb"); if (!f) return "";
  std::string text; char b[4096]; size_t n;
  while ((n=fread(b,1,sizeof(b),f))) text.append(b,n);
  fclose(f); return text;
}

/* Negative container fixture: an opaque obsolete subsection uses the existing
 * mapped writer, not a fabricated tree or new payload schema. Its presence
 * must be refused; no consumer is asked to interpret the opaque byte. */
static BOOL LN4RT_Auxiliary_Image(PU_Info *pu,const char *path)
{
  std::string temporary=std::string(path)+".tmp";
  if (access(path,F_OK)==0 || access(temporary.c_str(),F_OK)==0) return FALSE;
  fflush(NULL); pid_t child=fork(); if (child<0) return FALSE;
  if (!child) {
    Output_File *file=Open_Output_Info((char *)temporary.c_str()); if (!file) _exit(1);
    Write_PU_Info(pu); unsigned char opaque=0;
    off_t offset=ir_b_save_buf(&opaque,1,1,0,file);
    PU_Info_subsect_offset(pu,WT_FREQ)=offset-file->cur_section->shdr.sh_offset;
    PU_Info_subsect_size(pu,WT_FREQ)=1; Set_PU_Info_state(pu,WT_FREQ,Subsect_Written);
    Write_Global_Info(pu); Close_Output_Info();
    if (link(temporary.c_str(),path)) _exit(1);
    _exit(unlink(temporary.c_str())==0?0:1);
  }
  int status=0;return waitpid(child,&status,0)==child && WIFEXITED(status) && WEXITSTATUS(status)==0;
}

/* Independent reader establishes each active scope. Recompute selection from
 * retained request and compare the actual mapped device tree with its product. */
static int LN4RT_Reopen(char **argv)
{
  MEM_Initialize(); Set_Error_Tables(Phases,host_errlist); Init_Error_Handler(10);
  Set_Error_File(NULL); Set_Error_Line(ERROR_LINE_UNKNOWN); Preconfigure(); Init_Controls_Tbl();
  ABI_Name="n64"; Configure(); IR_reader_init();
  std::string text=LN4RT_Read(argv[3]); rapidjson::Document m; m.Parse(text.c_str());
  if (m.HasParseError() || !m.IsObject() || !m.HasMember("request")) return 1;
  rapidjson::StringBuffer json; LN4R_WRITER w(json); m["request"].Accept(w);
  WOPT_DSL_NORMALIZATION_REQUEST r;
  if (!WOPT_DSL_normalization_driver_api.decode(json.GetString(),json.GetSize(),&r,stderr) || !Open_Input_Info(argv[2])) return 1;
  Initialize_Symbol_Tables(FALSE); New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
  PU_Info *program=Read_Global_Info(NULL),*device=NULL; WOPT_DSL_NORMALIZATION_LOWERING plan; BOOL prepared=FALSE;
  OPT_Fast_Math=FALSE; Roundoff_Level=ROUNDOFF_NONE; IEEE_Arithmetic=IEEE_ACCURATE;
  for (PU_Info *pu=program;pu;pu=PU_Info_next(pu)) {
    Read_Local_Info(MEM_pu_nz_pool_ptr,pu); Set_PU_Info_symtab_ptr(pu,NULL); Save_Local_Symtab(CURRENT_SYMTAB,pu);
    Current_PU_Info=pu; Current_pu=&PU_Info_pu(pu); Current_Map_Tab=PU_Info_maptab(pu);
    if (PU_Info_proc_sym(pu)==r.source.owner_pu) {
      SRCPOS source; prepared=WOPT_DSL_normalization_driver_api.prepare(PU_Info_tree_ptr(pu),&r,3,&plan,&source,stderr,NULL);
    }
    if (!strcmp(ST_name(St_Table[PU_Info_proc_sym(pu)]),m["kernel"].GetString())) device=pu;
  }
  if (!prepared || !device || plan.seal!=m["plan_seal"].GetUint64()) return 1;
  Restore_Local_Symtab(device); Current_scope=PU_lexical_level(PU_Info_pu(device));
  Current_PU_Info=device; Current_pu=&PU_Info_pu(device); Current_Map_Tab=PU_Info_maptab(device);
  WOPT_DSL_NORMALIZATION_DEVICE product={plan.seal,m["tree_seal"].GetUint64(),m["nodes"].GetUint64(),
      m["loops"].GetUint64(),m["scalar_operations"].GetUint64(),m["barriers"].GetUint64()};
  FILE *trace=fopen(argv[4],"wx"); if (!trace) return 1;
  BOOL ok=WOPT_DSL_verify_normalization_device(&plan,&product,stderr,trace); fclose(trace); return ok?0:1;
}

/* Produce source and explicit request independently of actual backend driver.
 * Review controls exercise typed-JIT calls; subprocess tests exercise be. */
int main(int argc,char **argv)
{
  if (argc==4 && !strcmp(argv[1],"decode")) {
    std::string json=LN4RT_Read(argv[2]); WOPT_DSL_NORMALIZATION_REQUEST output,saved;
    memset(&output,0x5a,sizeof(output)); saved=output;
    BOOL admitted=WOPT_DSL_normalization_driver_api.decode(json.data(),json.size(),&output,NULL);
    return admitted==atoi(argv[3]) && (admitted || !memcmp(&output,&saved,sizeof(output)))?0:1;
  }
  if (argc==5 && !strcmp(argv[1],"reopen")) return LN4RT_Reopen(argv);
  if (argc!=6) return 2;
  char mixed[]="mixed";
  char normal[]="default"; BOOL multiple=!strcmp(argv[4],"multiple-pu");
  BOOL unmanaged=!strcmp(argv[4],"unmanaged-formals"), auxiliary=!strcmp(argv[4],"unsupported-aux");
  BOOL special=multiple || unmanaged || auxiliary;
  std::string initial=std::string(argv[1])+".initial.B";
  char *source_args[]={argv[0],special?(char *)initial.c_str():argv[1],!strcmp(argv[4],"legacy")?mixed:special?normal:argv[4]};
  if (LN1b_Source_Producer(3,source_args)) return 1;
  if (multiple) {
    PU_Info *analysis=Current_PU_Info;
    PU_Info *other=DSL_Builder_Create_Minimal_PU("ln_request_unrelated");
    if (!other || !DSL_Builder_Select_PU(other)) return 1;
    // Empty scopes still carry reserved mapped rows; initialize only this
    // newly created scope, never normalize an input image during comparison.
    SCOPE &local=Scope_tab[CURRENT_SYMTAB];
    memset(&local.st_tab->Entry(0),0,sizeof(ST)); memset(&local.label_tab->Entry(0),0,sizeof(LABEL));
    memset(&local.preg_tab->Entry(0),0,sizeof(PREG)); memset(&local.inito_tab->Entry(0),0,sizeof(INITO));
    memset(&local.st_attr_tab->Entry(0),0,sizeof(ST_ATTR));
    if (!DSL_Builder_Return_PU_Values(other,NULL,0) ||
        !DSL_Builder_Select_PU(analysis)) return 1;
    Current_PU_Info=analysis;
    DSL_BUILDER_MAPPED_IMAGE_REQUEST checkpoint={}; checkpoint.path=argv[1];
    if (!LN1c_Source_Checkpoint(&checkpoint)) return 1;
  }
  if (unmanaged || auxiliary) {
    if (unmanaged) DSL_PU_Interface_Image_Reset();
    if (auxiliary ? !LN4RT_Auxiliary_Image(Current_PU_Info,argv[1]) : !LN1c_Scalar_Image(Current_PU_Info,argv[1])) return 1;
  }
  WN *scope=PU_Info_tree_ptr(Current_PU_Info),*value=WN_first(WN_func_body(scope));
  while (value && (WN_operator(value)!=OPR_STID || WN_operator(WN_kid0(value))!=OPR_DSL)) value=WN_next(value);
  if (!value) return 1;
  if (!strcmp(argv[4],"legacy")) {
    while (value) {
      WOPT_DSL_NORMALIZATION_CHARACTERIZATION s;
      if (WN_operator(value)==OPR_STID && WOPT_DSL_characterize_normalization(scope,value,&s,NULL) && s.recipe==1) break;
      value=WN_next(value);
    }
    if (!value) return 1;
  }
  UINT32 scenario=atoi(argv[5]); if (scenario>=sizeof(ln3_scenarios)/sizeof(ln3_scenarios[0])) return 2;
  WOPT_DSL_NORMALIZATION_COST_REQUEST cost; if (!LN3_Request(scope,value,argv[3],scenario,&cost)) return 1;
  WOPT_DSL_NORMALIZATION_REQUEST r; memset(&r,0,sizeof(r)); r.version=1;
  const WOPT_DSL_NORMALIZATION_PLAN_REQUEST &p=cost.plans[0]; r.source=p.target.expected_source;
  strcpy(r.architecture,argv[3]); r.route=p.target.route; r.toolkit=p.target.toolkit; r.ptx=p.target.ptx;
  r.environment=p.target.environment; r.fp_options=p.target.fp_options; r.runtime_contract_id=p.runtime_contract_id;
  r.context_id=p.context_id; r.launched_ctas=p.launched_ctas; r.threads_per_cta=p.threads_per_cta;
  memcpy(r.ranges,p.ranges,sizeof(r.ranges)); r.profile=cost.profile;
  memcpy(r.registers_per_thread,cost.registers_per_thread,sizeof(r.registers_per_thread)); r.max_jobs=cost.max_jobs; r.margin=cost.margin;
  std::string json=LN4RT_JSON(r); OPT_Fast_Math=FALSE; Roundoff_Level=ROUNDOFF_NONE; IEEE_Arithmetic=IEEE_ACCURATE;
  if (!LN4RT_Controls(scope,r,json)) return 1;
  WOPT_DSL_NORMALIZATION_COST_RESULT reference; WOPT_DSL_NORMALIZATION_LOWERING expected,actual; SRCPOS source;
  if (!WOPT_DSL_select_normalization_plan(scope,value,&cost,&reference,stderr,NULL)) return 1;
  BOOL selected=WOPT_DSL_prepare_normalization_lowering(scope,value,&cost,&reference,&expected,NULL,NULL);
  BOOL bound=WOPT_DSL_normalization_driver_api.prepare(scope,&r,3,&actual,&source,NULL,NULL);
  if (!Check(selected==bound && (!selected || !memcmp(&expected,&actual,sizeof(actual))),
      "LN4_request_recipes_match_native_LN3_reference")) return 1;
  FILE *f=fopen(argv[2],"wx"); if (!f) return 1;
  BOOL ok=fwrite(json.data(),1,json.size(),f)==json.size(); return fclose(f)==0 && ok?0:1;
}
