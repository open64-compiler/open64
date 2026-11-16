/* Copyright (C) 2026 Open64 Project */

/* Explicit LN4 backend checkpoint: source/PU lifetime and image publication.
 * Design: doc/AI-COMPILER-LAYERNORM-DRIVER-REQUEST-v0.1.md.
 * WOPT owns preparation/materialization. This driver does not choose a plan,
 * reinterpret source v1 or reset managed source interfaces for device output. */
#include <fcntl.h>
#include <sys/stat.h>
#if !defined(__MINGW32__)
#include <sys/wait.h>
#endif
#include <string>
#include <vector>
#include <unistd.h>
#include <string.h>
#include "defs.h"
#include "wn.h"
#include "wn_util.h"
#include "pu_info.h"
#include "stab.h"
#include "strtab.h"
#include "config.h"
#include "config_opt.h"
#include "config_dsl.h"
#include "tracing.h"
#include "ir_bwrite.h"
#include "dwarf_DST.h"
#include "region_util.h"
#include "region_main.h"
#include "be_symtab.h"
#include "dsl_pu_transaction.h"
#include "dsl_gatekeeper.h"
#include "dsl_image_service.h"
#include "../com/dsl_pu_interface_verify.h"
#include "opt_dsl_normalization_request.h"
#include "normalization_driver.h"

namespace {
/* One exit diagnostic; failed staging remains inspectable but unpublished. */
static BOOL LN4D_Refuse(FILE *diagnostic, const char *message)
{
  if (diagnostic) fprintf(diagnostic, "LN4-DRIVER: %s\n", message);
  return FALSE;
}

/* Only the driver establishes a resident PU's scope/map/current identities. */
static void LN4D_Select(PU_Info *pu)
{
  Restore_Local_Symtab(pu);
  Current_scope = PU_lexical_level(PU_Info_pu(pu));
  Current_pu = &PU_Info_pu(pu);
  Current_Map_Tab = PU_Info_maptab(pu);
  Current_PU_Info = pu;
}

/* This bounded route must not silently drop maps/auxiliary subsections whose
 * writer controls are not enabled. SSA is supported by the standard writer. */
static BOOL LN4D_Input(PU_Info *program, FILE *diagnostic)
{
  if (!program) return LN4D_Refuse(diagnostic, "empty analysis image");
  for (PU_Info *pu = program; pu; pu = PU_Info_next(pu)) {
    if (PU_Info_child(pu) || ST_device_symbol_kind(St_Table[PU_Info_proc_sym(pu)]) != DEVICE_SYMBOL_NONE)
      return LN4D_Refuse(diagnostic, "requires flat analysis image without existing device entries");
    for (UINT32 section = 0; section < WT_SUBSECTIONS; ++section)
      if (section != WT_SYMTAB && section != WT_TREE && section != WT_SSA &&
          PU_Info_state(pu, section) != Subsect_Missing)
        return LN4D_Refuse(diagnostic, "unsupported input auxiliary PU subsection");
  }
  return TRUE;
}

/* The driver validates each retained scope independently, not through hidden
 * cross-PU analysis. Appending kernel rows must not expose missing coverage. */
static BOOL LN4D_Interfaces(PU_Info *program, FILE *diagnostic)
{
  for (PU_Info *pu = program; pu; pu = PU_Info_next(pu)) {
    LN4D_Select(pu);
    if ((!DSL_PU_Interface_Image_Has_Records() && WN_num_formals(PU_Info_tree_ptr(pu))) ||
        !DSL_PU_Interface_Image_Validate_PU(pu, diagnostic) || !DSL_Call_ABI_Image_Validate_PU(pu, diagnostic) ||
        !DSL_Gatekeeper_Verify_PU(pu, diagnostic, NULL))
      return LN4D_Refuse(diagnostic, "retained PU requires complete native/physical formal coverage");
  }
  return TRUE;
}

/* Terminal standard writer: subsection offsets replace pointers and writing
 * SSA consumes its manager. Never reuse these live PUs after this operation. */
static BOOL LN4D_Write(PU_Info *program, const std::string &path)
{
  if (!Open_Output_Info((char *)path.c_str())) return FALSE;
  for (PU_Info *pu = program; pu; pu = PU_Info_next(pu)) {
    LN4D_Select(pu); Write_PU_Info(pu);
  }
  Write_Global_Info(program); Close_Output_Info();
  return TRUE;
}

/* A CLI-only isolated snapshot preserves live symbol/tree/SSA ownership before
 * materialization. The typed JIT preparation API does not fork or write files. */
static BOOL LN4D_Before(PU_Info *program, const std::string &path)
{
#if defined(__MINGW32__)
  return FALSE;
#else
  fflush(NULL);
  pid_t child = fork();
  if (child < 0) return FALSE;
  if (child == 0) _exit(LN4D_Write(program, path) ? 0 : 1);
  int status = 0;
  return waitpid(child, &status, 0) == child && WIFEXITED(status) && WEXITSTATUS(status) == 0;
#endif
}

/* A new ordinary empty PU has no analysis DSL formal-interface metadata.
 * Creation is driver-owned and uses standard ST/PU/WN/scope/map services. */
static PU_Info *LN4D_Device(const char *name, SRCPOS source, BOOL host = FALSE)
{
  const ST *existing; ST_IDX idx;
  FOREACH_SYMBOL(GLOBAL_SYMTAB, existing, idx) if (!strcmp(ST_name(existing), name)) return NULL;
  TY_IDX type = Make_Function_Type(MTYPE_To_TY(host ? MTYPE_I4 : MTYPE_V));
  PU_IDX pi; PU &p = New_PU(pi); PU_Init(p, type, GLOBAL_SYMTAB + 1); Set_PU_c_lang(p); Set_PU_no_inline(p);
  ST *kernel = New_ST(GLOBAL_SYMTAB);
  ST_Init(kernel, Save_Str(name), CLASS_FUNC, SCLASS_TEXT, EXPORT_PREEMPTIBLE, type);
  Set_ST_pu(kernel, pi);
  if (!host) Set_ST_device_symbol_kind(*kernel, DEVICE_SYMBOL_KERNEL);
  Set_ST_Srcpos(*kernel, source);
  New_Scope(GLOBAL_SYMTAB + 1, MEM_pu_nz_pool_ptr, TRUE);
  // New_Scope reserves but does not initialize row zero. The mapped writer
  // includes these nonsemantic rows; never publish old allocator contents.
  SCOPE &scope = Scope_tab[CURRENT_SYMTAB];
  memset(&scope.st_tab->Entry(0), 0, sizeof(ST));
  memset(&scope.label_tab->Entry(0), 0, sizeof(LABEL));
  memset(&scope.preg_tab->Entry(0), 0, sizeof(PREG));
  memset(&scope.inito_tab->Entry(0), 0, sizeof(INITO));
  memset(&scope.st_attr_tab->Entry(0), 0, sizeof(ST_ATTR));
  Scope_tab[CURRENT_SYMTAB].st = kernel;
  PU_Info *pu = TYPE_MEM_POOL_ALLOC(PU_Info, MEM_pu_nz_pool_ptr); PU_Info_init(pu);
  PU_Info_proc_sym(pu) = ST_st_idx(kernel);
  Set_PU_Info_tree_ptr(pu, WN_CreateEntry(0, kernel, WN_CreateBlock(), NULL, NULL));
  WN_Set_Linenum(PU_Info_tree_ptr(pu), source);
  Set_PU_Info_state(pu, WT_SYMTAB, Subsect_InMem); Set_PU_Info_state(pu, WT_TREE, Subsect_InMem);
  Set_PU_Info_state(pu, WT_PROC_SYM, Subsect_InMem); Set_PU_Info_flags(pu, PU_IS_COMPILER_GENERATED);
  PU_Info_maptab(pu) = WN_MAP_TAB_Create(MEM_pu_nz_pool_ptr);
  PU_Info_pu_dst(pu) = DST_INVALID_IDX; PU_Info_cu_dst(pu) = DST_INVALID_IDX;
  Current_pu = &p; Current_PU_Info = pu; Current_Map_Tab = PU_Info_maptab(pu);
  Set_PU_Info_symtab_ptr(pu, NULL);
  Save_Local_Symtab(CURRENT_SYMTAB, pu);
  return pu;
}

/* Mixed images require complete physical-formal coverage, including ordinary
 * pointer kernels. Append owner-qualified rows without rewriting source rows. */
static BOOL LN4D_Formals(PU_Info *pu)
{
  WN *entry = PU_Info_tree_ptr(pu);
  if (WN_num_formals(entry) != 4 && WN_num_formals(entry) != 2) return FALSE;
  const std::string owner = std::string("owner_pu=") + ST_name(St_Table[PU_Info_proc_sym(pu)]);
  for (UINT32 i = 0; i < WN_num_formals(entry); ++i) {
    ST_IDX st = WN_st_idx(WN_formal(entry, i));
    DSL_IR_VALUE_RECORD value; DSL_IR_Value_Record_Init(&value);
    value.value_kind = DSL_IR_VALUE_SYMBOL; value.st = st; value.ty = ST_type(St_Table[st]);
    value.name = Save_Str(ST_name(St_Table[st])); value.metadata = Save_Str(owner.c_str());
    DSL_PU_FORMAL_RECORD formal; memset(&formal, 0, sizeof(formal));
    formal.owner_pu_st = PU_Info_proc_sym(pu); formal.formal_ordinal = i;
    formal.formal_value_id = DSL_IR_Image_Add_Value(&value); formal.formal_st = st; formal.formal_ty = value.ty;
    if (!formal.formal_value_id || !DSL_PU_Interface_Image_Add_Formal(&formal)) return FALSE;
  }
  return DSL_IR_Image_Validate(stderr) && DSL_PU_Interface_Image_Validate(stderr) &&
      DSL_PU_Interface_Image_Validate_PU(pu, stderr);
}

/* Only construct in the driver-selected empty host scope. The body is ordinary
 * WHIRL, with conservative call effects and an immediate return-preg capture. */
static BOOL LN4D_Host(PU_Info *pu, UINT64 plan, UINT64 tree, SRCPOS source)
{
  TY_IDX pointer = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
  TY_IDX args[] = {pointer, MTYPE_To_TY(MTYPE_U8), MTYPE_To_TY(MTYPE_U8), pointer};
  const char *runtime = "open64_normalization_execution_run_selected";
  const ST *old; ST_IDX idx;
  FOREACH_SYMBOL(GLOBAL_SYMTAB, old, idx)
    if (!strcmp(ST_name(old), runtime)) return FALSE;
  TY_IDX type = Make_Function_Type(MTYPE_To_TY(MTYPE_I4));
  TYLIST_IDX list; Set_TYLIST_type(New_TYLIST(list), MTYPE_To_TY(MTYPE_I4));
  Set_TY_tylist(Ty_Table[type], list);
  for (UINT32 i = 0; i < 4; ++i) Set_TYLIST_type(New_TYLIST(list), args[i]);
  Set_TYLIST_type(New_TYLIST(list), TY_IDX_ZERO);
  Set_TY_has_prototype(Ty_Table[type]);
  PU_IDX pi; PU &p = New_PU(pi); PU_Init(p, type, GLOBAL_SYMTAB + 1); Set_PU_c_lang(p);
  ST *callee = New_ST(GLOBAL_SYMTAB);
  ST_Init(callee, Save_Str(runtime), CLASS_FUNC, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, type);
  Set_ST_pu(callee, pi); Set_ST_Srcpos(*callee, source);
  ST *function = &St_Table[PU_Info_proc_sym(pu)];
  type = Make_Function_Type(MTYPE_To_TY(MTYPE_I4));
  Set_TYLIST_type(New_TYLIST(list), MTYPE_To_TY(MTYPE_I4)); Set_TY_tylist(Ty_Table[type], list);
  for (UINT32 i = 0; i < 2; ++i) Set_TYLIST_type(New_TYLIST(list), pointer);
  Set_TYLIST_type(New_TYLIST(list), TY_IDX_ZERO); Set_TY_has_prototype(Ty_Table[type]);
  Set_PU_prototype(Pu_Table[ST_pu(function)], type);
  ST *formals[2]; const char *names[] = {"execution", "root_requests"};
  WN *entry = WN_CreateEntry(2, function, WN_CreateBlock(), NULL, NULL);
  for (UINT32 i = 0; i < 2; ++i) {
    formals[i] = New_ST(CURRENT_SYMTAB);
    ST_Init(formals[i], Save_Str(names[i]), CLASS_VAR, SCLASS_FORMAL, EXPORT_LOCAL, pointer);
    Set_ST_is_value_parm(formals[i]); Set_ST_Srcpos(*formals[i], source);
    WN_kid(entry, i) = WN_CreateIdname(0, formals[i]);
  }
  WN *call = WN_Call(MTYPE_I4, MTYPE_V, 4, ST_st_idx(callee)); WN_Set_Call_Default_Flags(call);
  WN *values[] = {WN_Ldid(Pointer_Mtype, 0, formals[0], pointer), WN_Intconst(MTYPE_U8, plan),
      WN_Intconst(MTYPE_U8, tree), WN_Ldid(Pointer_Mtype, 0, formals[1], pointer)};
  for (UINT32 i = 0; i < 4; ++i) WN_kid(call, i) = WN_CreateParm(MTYPE_U8, values[i], args[i], WN_PARM_BY_VALUE);
  ST *status = New_ST(CURRENT_SYMTAB);
  ST_Init(status, Save_Str("dispatch_status"), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, MTYPE_To_TY(MTYPE_I4));
  Set_ST_Srcpos(*status, source);
  WN *capture = WN_Stid(MTYPE_I4, 0, status, ST_type(status),
      WN_Ldid(MTYPE_I4, -1, Return_Val_Preg, ST_type(status)));
  WN *ret = WN_CreateReturn_Val(OPR_RETURN_VAL, MTYPE_I4, MTYPE_V,
      WN_Ldid(MTYPE_I4, 0, status, ST_type(status)));
  WN_INSERT_BlockLast(WN_func_body(entry), call); WN_INSERT_BlockLast(WN_func_body(entry), capture);
  WN_INSERT_BlockLast(WN_func_body(entry), ret);
  WN_Set_Linenum(entry, source); WN_Set_Linenum(call, source);
  WN_Set_Linenum(capture, source); WN_Set_Linenum(ret, source);
  Set_PU_Info_tree_ptr(pu, entry);
  return LN4D_Formals(pu) && BE_DSL_normalization_host_matches(pu, ST_name(function));
}

/* Bound the byte stream before passing it to the shared typed decoder. */
static BOOL LN4D_Read(const char *path, std::string *json)
{
  FILE *f = fopen(path, "rb"); if (!f) return FALSE;
  char block[4096]; size_t n;
  while ((n = fread(block, 1, sizeof(block), f)) != 0) {
    if (json->size() + n > 1048576) { fclose(f); return FALSE; }
    json->append(block, n);
  }
  BOOL valid = !ferror(f); return fclose(f) == 0 && valid;
}

/* Keep publication paths in one positional trace column even when they
 * contain whitespace, percent signs, control characters or UTF-8 bytes. */
static std::string LN4D_Path_Token(const std::string &path)
{
  const char *hex = "0123456789ABCDEF"; std::string token;
  for (size_t i = 0; i < path.size(); ++i) {
    unsigned char c = path[i];
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
        c == '/' || c == '.' || c == '_' || c == '-' || c == '~') token += c;
    else { token += '%'; token += hex[c >> 4]; token += hex[c & 15]; }
  }
  return token;
}

/* Review-only JSON binds the request to the actual constructed device body;
 * H6 additionally publishes the selected launch rather than reusing the input
 * request width. Native guard words remain review-only, not a portable ABI. */
static BOOL LN4D_Manifest(const std::string &path, const std::string &json,
    const char *kernel, const char *host, const WOPT_DSL_NORMALIZATION_LOWERING &p, const WOPT_DSL_NORMALIZATION_DEVICE &d)
{
  FILE *f = fopen(path.c_str(), "wx"); if (!f) return FALSE;
  fprintf(f, "{\"schema\":\"open64.layernorm.driver-review.v1\",\"request\":%s,\"kernel\":\"%s\","
      "\"selected_kind\":%llu,\"cost\":%llu,\"plan_seal\":%llu,\"tree_seal\":%llu,"
      "\"nodes\":%llu,\"loops\":%llu,\"scalar_operations\":%llu,\"barriers\":%llu,"
      "\"runtime_authorized\":false,\"gpu_verified\":false,\"host_dispatch\":\"%s\",\"execution_mode\":\"%s\"", json.c_str(), kernel,
      (unsigned long long)p.guard.kind, (unsigned long long)p.cost.total, (unsigned long long)p.seal,
      (unsigned long long)d.tree_seal, (unsigned long long)d.nodes, (unsigned long long)d.loops,
      (unsigned long long)d.scalar_operations, (unsigned long long)d.barriers, host, *host ? "standalone" : "service");
  if (p.h6_model_key[0]) fprintf(f,
      ",\"selected_launch\":{\"grid_x\":%llu,\"block_x\":%llu,\"shared_bytes\":%llu},"
      "\"h6\":{\"model_key\":\"%s\",\"estimate_key\":\"%s\",\"rank_ns\":%llu,\"estimated\":true}",
      (unsigned long long)p.guard.ctas,(unsigned long long)p.guard.threads,(unsigned long long)p.guard.reserved_shared,
      p.h6_model_key,p.h6_estimate_key,(unsigned long long)p.h6_rank_ns);
  fprintf(f,"}\n");
  BOOL valid = !ferror(f); return fclose(f) == 0 && valid;
}
}

/* Validate the entire generated adapter, not just its name, before extraction.
 * Compiler-created constants are still checked against the opened runtime entry. */
BOOL BE_DSL_normalization_host_matches(PU_Info *pu, const char *name)
{
  if (!pu || !name || strcmp(ST_name(St_Table[PU_Info_proc_sym(pu)]), name) ||
      ST_device_symbol_kind(St_Table[PU_Info_proc_sym(pu)]) != DEVICE_SYMBOL_NONE ||
      !Is_Set_PU_Info_flags(pu, PU_IS_COMPILER_GENERATED)) return FALSE;
  WN *entry = PU_Info_tree_ptr(pu);
  if (!entry || WN_num_formals(entry) != 2) return FALSE;
  TYLIST_IDX host_types = TY_tylist(ST_pu_type(PU_Info_proc_sym(pu)));
  if (!host_types || TYLIST_type(Tylist_Table[host_types]) != MTYPE_To_TY(MTYPE_I4) ||
      TYLIST_type(Tylist_Table[host_types + 3])) return FALSE;
  for (UINT32 i = 0; i < 2; ++i) {
    ST *formal = WN_st(WN_formal(entry, i));
    if (ST_sclass(formal) != SCLASS_FORMAL || !ST_is_value_parm(formal) ||
        TY_kind(ST_type(formal)) != KIND_POINTER || TY_pointed(ST_type(formal)) != MTYPE_To_TY(MTYPE_V) ||
        ST_type(formal) != TYLIST_type(Tylist_Table[host_types + i + 1])) return FALSE;
  }
  WN *call = WN_first(WN_func_body(entry)), *capture = call ? WN_next(call) : NULL;
  WN *ret = capture ? WN_next(capture) : NULL;
  if (!call || WN_operator(call) != OPR_CALL || WN_rtype(call) != MTYPE_I4 || WN_kid_count(call) != 4 ||
      strcmp(ST_name(WN_st(call)), "open64_normalization_execution_run_selected") ||
      !capture || WN_operator(capture) != OPR_STID || !ret || WN_operator(ret) != OPR_RETURN_VAL || WN_next(ret) ||
      WN_operator(WN_kid0(capture)) != OPR_LDID || WN_st(WN_kid0(capture)) != Return_Val_Preg ||
      WN_offset(WN_kid0(capture)) != -1 || WN_operator(WN_kid0(ret)) != OPR_LDID ||
      WN_st_idx(WN_kid0(ret)) != WN_st_idx(capture)) return FALSE;
  TYLIST_IDX types = TY_tylist(ST_pu_type(WN_st(call)));
  if (!types || TYLIST_type(Tylist_Table[types]) != MTYPE_To_TY(MTYPE_I4) ||
      TYLIST_type(Tylist_Table[types + 5]) || WN_call_flag(call) != WN_CALL_CONSERVATIVE ||
      WN_desc(capture) != MTYPE_I4 || WN_rtype(WN_kid0(capture)) != MTYPE_I4 ||
      WN_rtype(ret) != MTYPE_I4 || ST_type(WN_st(capture)) != MTYPE_To_TY(MTYPE_I4)) return FALSE;
  for (UINT32 i = 0; i < 4; ++i) {
    WN *parm = WN_kid(call, i);
    if (!parm || WN_operator(parm) != OPR_PARM || !WN_Parm_By_Value(parm) || WN_rtype(parm) != MTYPE_U8 ||
        WN_ty(parm) != TYLIST_type(Tylist_Table[types + i + 1])) return FALSE;
    WN *actual = WN_kid0(parm);
    if (i == 1 || i == 2) {
      if (WN_operator(actual) != OPR_INTCONST || !WN_const_val(actual) || WN_rtype(actual) != MTYPE_U8 ||
          WN_ty(parm) != MTYPE_To_TY(MTYPE_U8)) return FALSE;
    }
    else if (WN_operator(actual) != OPR_LDID || WN_st_idx(actual) != WN_st_idx(WN_formal(entry, i ? 1 : 0))) return FALSE;
  }
  return TRUE;
}

/* Driver owns resident program lifetime throughout PREOPT, device construction
 * and publication. WOPT callbacks never traverse or select another PU. */
BOOL BE_DSL_normalization_checkpoint(PU_Info *program, const WOPT_DSL_NORMALIZATION_DRIVER_API *api,
    WN *(*preopt)(WN *, WN *), FILE *diagnostic)
{
  if (!api || !api->decode || !api->prepare || !api->materialize || !api->verify ||
      !api->environment_ready || !api->environment_ready() || !preopt ||
      !VHO_DSL_Normalization_Request || !VHO_DSL_Normalization_Output ||
      !VHO_DSL_GPU_Arch || Opt_Level < 3 || Pointer_Size != 8 || Pointer_Mtype != MTYPE_U8)
    return LN4D_Refuse(diagnostic, "requires request/output/gpu_arch, O3, n64 and strict compiler FP environment");
  const char *mode = VHO_DSL_Normalization_Mode ? VHO_DSL_Normalization_Mode : "service";
  if (strcmp(mode, "service") && strcmp(mode, "standalone"))
    return LN4D_Refuse(diagnostic, "normalization_mode must be service or standalone; performance harness is separate");
  std::string json;
  WOPT_DSL_NORMALIZATION_REQUEST request;
  if (!LN4D_Read(VHO_DSL_Normalization_Request, &json) || !api->decode(json.data(), json.size(), &request, diagnostic))
    return LN4D_Refuse(diagnostic, "request ingestion failed");
  if (strcmp(request.architecture, VHO_DSL_GPU_Arch)) return LN4D_Refuse(diagnostic, "gpu_arch/request mismatch");
  std::string output = VHO_DSL_Normalization_Output;
  std::string stem = output;
  if (stem.size() >= 8 && stem.substr(stem.size() - 8) == ".after.B") stem.resize(stem.size() - 8);
  else if (stem.size() >= 2 && stem.substr(stem.size() - 2) == ".B") stem.resize(stem.size() - 2);
  const std::string finals[] = {stem + ".before.B", output + ".manifest.json", output};
  for (UINT32 i = 0; i < 3; ++i) {
    struct stat status;
    if (lstat(finals[i].c_str(), &status) == 0) return LN4D_Refuse(diagnostic, "output already exists");
  }
  if (!LN4D_Input(program, diagnostic)) return FALSE;
  WN_Mem_Push(); MEM_POOL_Push(MEM_pu_nz_pool_ptr); MEM_POOL_Push(MEM_pu_pool_ptr);
  if (!DSL_PU_Transaction_Load_Mapped_Program(program, MEM_pu_nz_pool_ptr, diagnostic))
    return LN4D_Refuse(diagnostic, "could not retain mapped program");
  if (!LN4D_Interfaces(program, diagnostic)) return FALSE;
  PU_Info *analysis = NULL, *last = NULL;
  for (PU_Info *pu = program; pu; pu = PU_Info_next(pu)) {
    last = pu;
    if (PU_Info_proc_sym(pu) == request.source.owner_pu) {
      if (analysis) return LN4D_Refuse(diagnostic, "duplicate analysis PU");
      analysis = pu;
    }
  }
  if (!analysis) return LN4D_Refuse(diagnostic, "requested analysis PU unavailable");
  LN4D_Select(analysis);
  BE_symtab_alloc_scope_level(CURRENT_SYMTAB);
  Scope_tab[CURRENT_SYMTAB].st_tab->Register(*Be_scope_tab[CURRENT_SYMTAB].be_st_tab);
  WN *scope = PU_Info_tree_ptr(analysis);
  if (!DSL_Gatekeeper_Verify_PU(analysis, diagnostic, NULL)) return FALSE;
  REGION_Initialize(scope, PU_has_region(*Current_pu));
  scope = preopt(scope, scope); Set_PU_Info_tree_ptr(analysis, scope); REGION_Finalize();
  FILE *trace = VHO_DSL_AI_Opt_Trace && Get_Trace(TP_WOPT2, AI_OPT_TRACE_FLAG) ? TFile : NULL;
  WOPT_DSL_NORMALIZATION_LOWERING plan; SRCPOS source;
  if (!api->prepare(scope, &request, Opt_Level, &plan, &source, diagnostic, trace)) return FALSE;
  Scope_tab[CURRENT_SYMTAB].st_tab->Un_register(*Be_scope_tab[CURRENT_SYMTAB].be_st_tab);
  Be_scope_tab[CURRENT_SYMTAB].be_st_tab->Clear();
  char kernel[80]; snprintf(kernel, sizeof(kernel), "open64_ln_ref_%u_%u_c%llu", request.source.owner_pu,
      request.source.node_id, (unsigned long long)(plan.guard.kind - 1));
  PU_Info *device = LN4D_Device(kernel, source);
  if (!device) return LN4D_Refuse(diagnostic, "device entry name collision");
  PU_Info_next(last) = device;
  char host_name[100]; host_name[0] = 0; PU_Info *host = NULL;
  if (!strcmp(mode, "standalone")) {
    snprintf(host_name, sizeof(host_name), "open64_ln_dispatch_%u_%u_c%llu", request.source.owner_pu,
        request.source.node_id, (unsigned long long)(plan.guard.kind - 1));
    host = LN4D_Device(host_name, source, TRUE);
    if (!host) return LN4D_Refuse(diagnostic, "host entry name collision");
    PU_Info_next(device) = host;
  }
  std::vector<char> staging(output.begin(), output.end());
  const char suffix[] = ".tmp.XXXXXX"; staging.insert(staging.end(), suffix, suffix + sizeof(suffix));
  if (!mkdtemp(&staging[0])) return LN4D_Refuse(diagnostic, "could not reserve staging directory");
  std::string directory = &staging[0];
  const std::string temporary[] = {directory + "/before.B", directory + "/binding.json", directory + "/after.B"};
  if (!LN4D_Before(program, temporary[0])) return LN4D_Refuse(diagnostic, "before image failed");
  LN4D_Select(device);
  WOPT_DSL_NORMALIZATION_DEVICE product;
  if (!api->materialize(&plan, source, &product, diagnostic, trace) ||
      !api->verify(&plan, &product, diagnostic, trace) || !LN4D_Formals(device) ||
      !DSL_Gatekeeper_Verify_PU(device, diagnostic, NULL))
    return LN4D_Refuse(diagnostic, "device construction or verification failed");
  Set_PU_Info_symtab_ptr(device, NULL);
  Save_Local_Symtab(CURRENT_SYMTAB, device);
  if (host) {
    LN4D_Select(host);
    if (!LN4D_Host(host, plan.seal, product.tree_seal, source)) return LN4D_Refuse(diagnostic, "host construction failed");
    Set_PU_Info_symtab_ptr(host, NULL); Save_Local_Symtab(CURRENT_SYMTAB, host);
  }
  if (trace) fprintf(trace, "AI_NORM_HOST 1 DISPATCH %s %s %s %llu %llu %u\n", mode,
      host ? host_name : "none", kernel, (unsigned long long)plan.seal,
      (unsigned long long)product.tree_seal, request.source.source_line);
  if (!LN4D_Interfaces(program, diagnostic)) return FALSE;
  if (!LN4D_Write(program, temporary[2]) || !LN4D_Manifest(temporary[1], json, kernel, host_name, plan, product))
    return LN4D_Refuse(diagnostic, "selected image or binding manifest failed");
  // Hard-link publication is atomic and refuses pre-existing files/symlinks.
  UINT32 published = 0;
  for (; published < 3; ++published) if (link(temporary[published].c_str(), finals[published].c_str())) break;
  if (published != 3) {
    while (published) unlink(finals[--published].c_str());
    return LN4D_Refuse(diagnostic, "no-replace publication failed");
  }
  for (UINT32 i = 0; i < 3; ++i) unlink(temporary[i].c_str());
  rmdir(directory.c_str());
  if (trace) fprintf(trace, "AI_NORM_REQUEST 1 PUBLISH %u %u %s %llu %s %u\n", request.source.owner_pu,
      request.source.node_id, kernel, (unsigned long long)product.tree_seal,
      LN4D_Path_Token(output).c_str(), request.source.source_line);
  MEM_POOL_Pop(MEM_pu_pool_ptr); MEM_POOL_Pop(MEM_pu_nz_pool_ptr); WN_Mem_Pop();
  return TRUE;
}
