/* Copyright (C) 2026 Open64 Project */

/* Strict bounded LN4 request ingestion and active-PU planning, not a driver or
 * runtime. Design: doc/AI-COMPILER-LAYERNORM-DRIVER-REQUEST-v0.1.md.
 * Reuses repository header-only RapidJSON and existing LN1/LN2/LN3/LN4 gates;
 * never switches PU, invents timing defaults or changes source semantics. */
#include <limits.h>
#include <set>
#include <string>
#include <string.h>
#include "opt_dsl_normalization_request.h"
#include "../../rapidjson/document.h"
#include "config.h"
#include "config_opt.h"
#include "wn.h"
#include "pu_info.h"

namespace {
/* Bounded SAX preflight prevents deeply nested rejected documents from
 * reaching the DOM allocator/destructor. Parsing remains a structured API. */
struct LN4R_JSON_DEPTH : rapidjson::BaseReaderHandler<rapidjson::UTF8<>, LN4R_JSON_DEPTH> {
  UINT32 depth;
  /* Start a fresh bounded request parse. */
  LN4R_JSON_DEPTH() : depth(0) {}
  /* No valid schema path needs more than eight containers. */
  bool StartObject() { return ++depth <= 8; }
  /* Mirror objects for array nesting, including rejected unknown members. */
  bool StartArray() { return ++depth <= 8; }
  /* Close one successfully admitted object. */
  bool EndObject(rapidjson::SizeType) { --depth; return true; }
  /* Close one successfully admitted array. */
  bool EndArray(rapidjson::SizeType) { --depth; return true; }
};
/* One diagnostic class covers malformed requests, never a successful choice. */
static BOOL LN4R_Refuse(FILE *diagnostic, const char *message)
{
  if (diagnostic) fprintf(diagnostic, "LN4-REQUEST: %s\n", message);
  return FALSE;
}

/* Closed objects reject missing, duplicate, unknown and embedded-NUL keys. */
static BOOL LN4R_Keys(const rapidjson::Value &object, const char *const *keys, size_t count)
{
  if (!object.IsObject() || object.MemberCount() != count) return FALSE;
  std::set<std::string> seen;
  for (rapidjson::Value::ConstMemberIterator m = object.MemberBegin(); m != object.MemberEnd(); ++m) {
    std::string name(m->name.GetString(), m->name.GetStringLength());
    BOOL allowed = FALSE;
    for (size_t i = 0; i < count; ++i) if (name == keys[i]) allowed = TRUE;
    if (!allowed || !seen.insert(name).second) return FALSE;
  }
  return TRUE;
}

/* Never accept booleans, rounded floating JSON numbers or signed negatives. */
static BOOL LN4R_U64(const rapidjson::Value &value, UINT64 *output)
{
  if (!value.IsUint64()) return FALSE;
  *output = value.GetUint64();
  return TRUE;
}

/* Native source/type identities and enum fields have explicit 32-bit bounds. */
static BOOL LN4R_U32(const rapidjson::Value &value, UINT32 *output)
{
  if (!value.IsUint()) return FALSE;
  *output = value.GetUint();
  return TRUE;
}

/* Fixed arrays preserve positional native identities without accepting extras. */
static BOOL LN4R_Array32(const rapidjson::Value &value, UINT32 *output, UINT32 count)
{
  if (!value.IsArray() || value.Size() != count) return FALSE;
  for (UINT32 i = 0; i < count; ++i) if (!LN4R_U32(value[i], output + i)) return FALSE;
  return TRUE;
}

/* Decode the entire immutable native source snapshot, not only shape/name. */
static BOOL LN4R_Source(const rapidjson::Value &v, WOPT_DSL_NORMALIZATION_CHARACTERIZATION *s)
{
  const char *keys[] = {"recipe", "flags", "owner_pu", "node_id", "operand_value_ids", "result_value_id",
      "operand_types", "result_ty", "rows", "width", "epsilon_bits", "source_line"};
  return LN4R_Keys(v, keys, 12) && LN4R_U32(v["recipe"], &s->recipe) &&
      LN4R_U32(v["flags"], &s->flags) && LN4R_U32(v["owner_pu"], &s->owner_pu) &&
      LN4R_U32(v["node_id"], &s->node_id) && LN4R_Array32(v["operand_value_ids"], s->operand_value_ids, 3) &&
      LN4R_U32(v["result_value_id"], &s->result_value_id) && LN4R_Array32(v["operand_types"], s->operand_types, 3) &&
      LN4R_U32(v["result_ty"], &s->result_ty) && LN4R_U64(v["rows"], &s->rows) &&
      LN4R_U64(v["width"], &s->width) && LN4R_U32(v["epsilon_bits"], &s->epsilon_bits) &&
      LN4R_U32(v["source_line"], &s->source_line);
}

/* Profile fields use the existing versioned 19-field native contract. Unknown
 * values remain unknown and are refused by LN3, never defaulted to free. */
static BOOL LN4R_Profile(const rapidjson::Value &v, DSL_SCALAR_EXECUTION_PROFILE *p)
{
  const char *keys[] = {"version", "arch_id", "description_revision", "model_id", "fields"};
  if (!LN4R_Keys(v, keys, 5) || !LN4R_U32(v["version"], &p->version) ||
      !LN4R_U32(v["arch_id"], &p->arch_id) || !LN4R_U32(v["description_revision"], &p->description_revision) ||
      !LN4R_U32(v["model_id"], &p->model_id) || !v["fields"].IsArray() ||
      v["fields"].Size() != DSL_SCALAR_FIELD_COUNT) return FALSE;
  const char *field_keys[] = {"id", "provenance", "bounds"};
  for (UINT32 f = 0; f < DSL_SCALAR_FIELD_COUNT; ++f) {
    const rapidjson::Value &field = v["fields"][f]; UINT32 id;
    if (!LN4R_Keys(field, field_keys, 3) || !LN4R_U32(field["id"], &id) || id != f ||
        !LN4R_U32(field["provenance"], &p->provenance[f]) || !field["bounds"].IsArray() ||
        field["bounds"].Size() != 3) return FALSE;
    for (UINT32 c = 0; c < 3; ++c) {
      UINT64 word;
      if (!LN4R_U64(field["bounds"][c], &word)) return FALSE;
      p->values[f][c] = word;
    }
  }
  return TRUE;
}

/* Context-free transactional decode: reject malformed/oversized byte streams,
 * publish only a fully decoded owned request, and emit no optimization trace. */
static BOOL LN4R_Decode(const char *json, size_t bytes, WOPT_DSL_NORMALIZATION_REQUEST *output, FILE *diagnostic)
{
  if (!json || !output || !bytes || bytes > 1048576 || memchr(json, 0, bytes))
    return LN4R_Refuse(diagnostic, "invalid request byte stream");
  rapidjson::MemoryStream input(json, bytes); rapidjson::Reader reader; LN4R_JSON_DEPTH depth;
  if (!reader.Parse<rapidjson::kParseIterativeFlag | rapidjson::kParseValidateEncodingFlag>(input, depth))
    return LN4R_Refuse(diagnostic, "invalid JSON or excessive nesting");
  rapidjson::Document root;
  root.Parse<rapidjson::kParseIterativeFlag | rapidjson::kParseValidateEncodingFlag>(json, bytes);
  const char *keys[] = {"schema", "source", "target", "invocation", "profile", "registers_per_thread", "max_jobs", "margin"};
  if (root.HasParseError() || !LN4R_Keys(root, keys, 8) || !root["schema"].IsString() ||
      root["schema"].GetStringLength() != strlen("open64.layernorm.request.v1") ||
      strcmp(root["schema"].GetString(), "open64.layernorm.request.v1"))
    return LN4R_Refuse(diagnostic, "invalid request schema or JSON");
  WOPT_DSL_NORMALIZATION_REQUEST r; memset(&r, 0, sizeof(r)); r.version = 1;
  const rapidjson::Value &t = root["target"], &v = root["invocation"];
  const char *target_keys[] = {"architecture", "route", "toolkit", "ptx", "environment", "fp_options"};
  const char *invocation_keys[] = {"runtime_contract_id", "context_id", "launched_ctas", "threads_per_cta", "ranges"};
  if (!LN4R_Source(root["source"], &r.source) || !LN4R_Keys(t, target_keys, 6) ||
      !t["architecture"].IsString() || t["architecture"].GetStringLength() >= sizeof(r.architecture) ||
      strlen(t["architecture"].GetString()) != t["architecture"].GetStringLength() ||
      !LN4R_U32(t["route"], &r.route) || !LN4R_U32(t["toolkit"], &r.toolkit) ||
      !LN4R_U32(t["ptx"], &r.ptx) || !LN4R_U32(t["environment"], &r.environment) ||
      !LN4R_U32(t["fp_options"], &r.fp_options) || !LN4R_Keys(v, invocation_keys, 5) ||
      !LN4R_U64(v["runtime_contract_id"], &r.runtime_contract_id) || !LN4R_U64(v["context_id"], &r.context_id) ||
      !LN4R_U64(v["launched_ctas"], &r.launched_ctas) || !LN4R_U32(v["threads_per_cta"], &r.threads_per_cta) ||
      !v["ranges"].IsArray() || v["ranges"].Size() != 4 || !LN4R_Profile(root["profile"], &r.profile) ||
      !LN4R_Array32(root["registers_per_thread"], r.registers_per_thread, 3) ||
      !LN4R_U64(root["max_jobs"], &r.max_jobs) || !LN4R_U64(root["margin"], &r.margin))
    return LN4R_Refuse(diagnostic, "invalid source, target, invocation or profile fields");
  strcpy(r.architecture, t["architecture"].GetString());
  const char *range_keys[] = {"allocation_id", "allocation_bytes", "offset_bytes", "base_alignment", "lease"};
  for (UINT32 i = 0; i < 4; ++i) {
    const rapidjson::Value &a = v["ranges"][i]; WOPT_DSL_NORMALIZATION_RANGE &b = r.ranges[i];
    if (!LN4R_Keys(a, range_keys, 5) || !LN4R_U64(a["allocation_id"], &b.allocation_id) ||
        !LN4R_U64(a["allocation_bytes"], &b.allocation_bytes) || !LN4R_U64(a["offset_bytes"], &b.offset_bytes) ||
        !LN4R_U32(a["base_alignment"], &b.base_alignment) || !LN4R_U32(a["lease"], &b.lease))
      return LN4R_Refuse(diagnostic, "invalid canonical backing range");
  }
  *output = r;
  return TRUE;
}

/* Match only within the established active scope; duplicate identities refuse
 * rather than allowing an incidental traversal order to select the member. */
static void LN4R_Find(WN *n, WN *scope, UINT32 node, WN **found, UINT32 *count)
{
  if (!n) return;
  if (WN_operator(n) == OPR_STID && WN_kid_count(n) && WN_operator(WN_kid0(n)) == OPR_DSL) {
    WOPT_DSL_NORMALIZATION_CHARACTERIZATION source;
    if (WOPT_DSL_characterize_normalization(scope, n, &source, NULL) && source.node_id == node) {
      *found = n; ++*count;
    }
  }
  if (WN_operator(n) == OPR_BLOCK) {
    for (WN *s = WN_first(n); s; s = WN_next(s)) LN4R_Find(s, scope, node, found, count);
  } else for (INT32 i = 0; i < WN_kid_count(n); ++i) LN4R_Find(WN_kid(n, i), scope, node, found, count);
}

/* Build fixed C0/C1/C2 storage/event recipes in the owning phase. The driver
 * supplies facts, not emitter-inferred barriers or an externally forced plan. */
static BOOL LN4R_Cost_Request(const WOPT_DSL_NORMALIZATION_REQUEST &r, WOPT_DSL_NORMALIZATION_COST_REQUEST *c)
{
  DSL_HARDWARE_DESCRIPTION_RECORD hw; DSL_MEMORY_TIER_RECORD tier;
  if (!DSL_hardware_description_find_arch(r.architecture, &hw) ||
      !DSL_memory_hierarchy_find_tier(hw.target_profile_id, DSL_MEMORY_TIER_SHARED, &tier) ||
      !tier.minimum_alignment || r.source.width > 16777216) return FALSE;
  memset(c, 0, sizeof(*c)); c->profile = r.profile; c->max_jobs = r.max_jobs; c->margin = r.margin;
  for (UINT32 i = 0; i < 3; ++i) {
    c->registers_per_thread[i] = r.registers_per_thread[i];
    WOPT_DSL_NORMALIZATION_PLAN_REQUEST &p = c->plans[i];
    p.target.expected_source = r.source; p.target.architecture = r.architecture;
    p.target.route = r.route; p.target.toolkit = r.toolkit; p.target.ptx = r.ptx;
    p.target.environment = r.environment; p.target.fp_options = r.fp_options;
    p.kind = i + 1; p.layout = WOPT_DSL_NORM_CONTIGUOUS_ROW_MAJOR_F32;
    p.runtime_contract_id = r.runtime_contract_id; p.context_id = r.context_id;
    memcpy(p.ranges, r.ranges, sizeof(p.ranges));
    p.ownership = WOPT_DSL_NORM_CTA_PER_ROW_ORDERED_OWNER_ZERO;
    p.tail_policy = WOPT_DSL_NORM_PREDICATE_VALID_BEFORE_ACCESS_ARITHMETIC;
    p.threads_per_cta = r.threads_per_cta; p.launched_ctas = r.launched_ctas;
    if (i) {
      p.row_stride_bytes = ((r.source.width * 4 + tier.minimum_alignment - 1) / tier.minimum_alignment) * tier.minimum_alignment;
      p.shared_bytes = p.row_stride_bytes + 8; p.barrier_participants = r.threads_per_cta;
      p.cta_barriers = (1U << WOPT_DSL_NORM_MU_VISIBLE) | (1U << WOPT_DSL_NORM_CENTERED_READY) |
          (1U << WOPT_DSL_NORM_Q_VISIBLE) | (1U << WOPT_DSL_NORM_READERS_DONE);
      if (i == 2) p.cta_barriers |= 1U << WOPT_DSL_NORM_ROW_READY;
    }
    for (UINT32 e = 0; e + 1 < WOPT_DSL_NORM_EVENT_COUNT; ++e) p.before[e] = 1U << (e + 1);
  }
  return TRUE;
}

/* CLI and JIT share this exact typed binding. Revalidate current native source,
 * costs and guard; no source/table mutation, PU switch or runtime grant. */
static BOOL LN4R_Prepare(WN *scope, const WOPT_DSL_NORMALIZATION_REQUEST *r, UINT32 level,
    WOPT_DSL_NORMALIZATION_LOWERING *output, SRCPOS *position, FILE *diagnostic, FILE *trace)
{
  if (!r || !output || !position || r->version != 1 || !memchr(r->architecture, 0, sizeof(r->architecture)) ||
      level < 3 || !WOPT_DSL_normalization_compiler_environment_ready() || !Current_PU_Info ||
      scope != PU_Info_tree_ptr(Current_PU_Info) || !scope || WN_operator(scope) != OPR_FUNC_ENTRY ||
      PU_Info_proc_sym(Current_PU_Info) != r->source.owner_pu)
    return LN4R_Refuse(diagnostic, "requires active canonical whole PU, O3, n64 and strict FP controls");
  WN *member = NULL; UINT32 count = 0; LN4R_Find(scope, scope, r->source.node_id, &member, &count);
  WOPT_DSL_NORMALIZATION_COST_REQUEST cost;
  if (count != 1 || !LN4R_Cost_Request(*r, &cost)) return LN4R_Refuse(diagnostic, "source member or target unavailable");
  WOPT_DSL_NORMALIZATION_COST_RESULT choice;
  if (trace) fprintf(trace, "AI_NORM_REQUEST 1 BIND %u %u %s %u %u %u\n", r->source.owner_pu,
      r->source.node_id, r->architecture, r->profile.model_id, r->profile.description_revision, r->source.source_line);
  WOPT_DSL_NORMALIZATION_LOWERING plan;
  if (!WOPT_DSL_select_normalization_plan(scope, member, &cost, &choice, diagnostic, trace) ||
      !WOPT_DSL_prepare_normalization_lowering(scope, member, &cost, &choice, &plan, diagnostic, trace))
    return LN4R_Refuse(diagnostic, "no complete legal cost-gated v2 plan");
  /* Static CUDA arenas above 48 KiB need a separately designed opt-in/dynamic
   * path. Refuse here; never publish a winner different from the cost decision. */
  if (plan.guard.reserved_shared > 49152 || !WN_Get_Linenum(member))
    return LN4R_Refuse(diagnostic, "selected static arena or source coordinate unsupported");
  *output = plan; *position = WN_Get_Linenum(member);
  if (trace) fprintf(trace, "AI_NORM_REQUEST 1 SELECT %u %u %llu %llu %llu %u\n", r->source.owner_pu,
      r->source.node_id, (unsigned long long)plan.guard.kind, (unsigned long long)plan.seal,
      (unsigned long long)plan.cost.total, r->source.source_line);
  return TRUE;
}
}

const WOPT_DSL_NORMALIZATION_DRIVER_API WOPT_DSL_normalization_driver_api = {
  LN4R_Decode, LN4R_Prepare, WOPT_DSL_materialize_normalization_device, WOPT_DSL_verify_normalization_device,
  WOPT_DSL_normalization_compiler_environment_ready
};
