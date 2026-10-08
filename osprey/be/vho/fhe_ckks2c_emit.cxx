/*
 * Copyright (C) 2026 Open64 Project
 *
 * Provider-independent CKKS2C source generation from already-certified event
 * plans.  This file owns textual C emission only; WHIRL traversal, grouped
 * public-ABI lowering, checkpoint publication, and provider implementation
 * remain separate stages.  See doc/FHE-SYNC6-ACE-CKKS-C-STAGING.md and
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md.
 */

#include "fhe_ckks2c_emit.h"

#include <algorithm>
#include <cerrno>
#include <climits>
#include <cstdlib>
#include <limits>
#include <map>
#include <set>
#include <sstream>

#include "dsl_opcode.h"

namespace {

const size_t kMaxGroups = 4096;
const size_t kMaxModuleBytes = 64 * 1024 * 1024;

/* Emit one stable diagnostic and preserve all caller-owned output objects. */
bool Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHECKKS2C-001: %s\n", message);
  return false;
}

/* C symbols are supplied by the image-to-plan collector and must never be
 * repaired or uniqued implicitly by the emitter. */
bool C_Identifier(const std::string &value)
{
  if (value.empty() ||
      !((value[0] >= 'A' && value[0] <= 'Z') ||
        (value[0] >= 'a' && value[0] <= 'z') || value[0] == '_'))
    return false;
  for (size_t i = 1; i < value.size(); ++i)
    if (!((value[i] >= 'A' && value[i] <= 'Z') ||
          (value[i] >= 'a' && value[i] <= 'z') ||
          (value[i] >= '0' && value[i] <= '9') || value[i] == '_'))
      return false;
  return true;
}

/* Authenticated group identities are lowercase SHA-256 strings. */
bool SHA256(const std::string &value)
{
  if (value.size() != 64)
    return false;
  for (size_t i = 0; i < value.size(); ++i)
    if (!((value[i] >= '0' && value[i] <= '9') ||
          (value[i] >= 'a' && value[i] <= 'f')))
      return false;
  return true;
}

/* Escape a validated semantic string for a generated C literal. */
std::string C_String(const std::string &value)
{
  std::ostringstream text;
  text << '"';
  for (size_t i = 0; i < value.size(); ++i) {
    const unsigned char ch = static_cast<unsigned char>(value[i]);
    if (ch == '\\' || ch == '"')
      text << '\\' << static_cast<char>(ch);
    else if (ch >= 32 && ch <= 126)
      text << static_cast<char>(ch);
    else {
      const char hex[] = "0123456789abcdef";
      text << "\\x" << hex[ch >> 4] << hex[ch & 15];
    }
  }
  text << '"';
  return text.str();
}

/* Read one named operation attribute without depending on producer order. */
bool Attribute(const VHO_FHE_CKKS_PLAN_STEP &step, const char *name,
               std::string *value)
{
  for (size_t i = 0; i < step.attributes.size(); ++i)
    if (step.attributes[i].name == name) {
      *value = step.attributes[i].value;
      return true;
    }
  return false;
}

/* Parse one exact base-10 signed 32-bit attribute. */
bool Signed_Attribute(const VHO_FHE_CKKS_PLAN_STEP &step, const char *name,
                      int32_t *value)
{
  std::string text;
  if (!Attribute(step, name, &text) || text.empty())
    return false;
  errno = 0;
  char *end = NULL;
  const long parsed = strtol(text.c_str(), &end, 10);
  if (errno != 0 || end == NULL || *end != 0 ||
      parsed < INT_MIN || parsed > INT_MAX)
    return false;
  *value = static_cast<int32_t>(parsed);
  return true;
}

/* Parse one positive base-10 32-bit attribute. */
bool Unsigned_Attribute(const VHO_FHE_CKKS_PLAN_STEP &step, const char *name,
                        uint32_t *value)
{
  int32_t parsed = 0;
  if (!Signed_Attribute(step, name, &parsed) || parsed <= 0)
    return false;
  *value = static_cast<uint32_t>(parsed);
  return true;
}

/* Resolve the C expression naming one preloaded source/bound value or a prior
 * owned result. The maps are complete products of group preflight. */
std::string Operand_Expression(
    const VHO_FHE_CKKS_PLAN_OPERAND &operand,
    const std::map<uint32_t, size_t> &sources,
    const std::map<std::string, size_t> &bounds)
{
  std::ostringstream text;
  if (operand.kind == VHO_FHE_CKKS_PLAN_PRIOR_STEP)
    text << "step_" << operand.reference;
  else if (operand.kind == VHO_FHE_CKKS_PLAN_SOURCE_VALUE)
    text << "source_" << sources.find(operand.reference)->second;
  else
    text << "bound_" << bounds.find(operand.bound_role)->second;
  return text.str();
}

/* Validate one operation's facade mapping. Generic plan serialization has
 * already checked state completeness and operand topology. */
bool Operation_Valid(const VHO_FHE_CKKS_PLAN_STEP &step, FILE *diagnostic)
{
  if (step.operator_version != 1)
    return Report(diagnostic, "unsupported CKKS operator version");
  int32_t signed_value = 0;
  uint32_t unsigned_value = 0;
  std::string reason;
  switch (step.logical_operator) {
    case OPR_DSLCKKSENCODE:
      if (step.operands.size() != 1 ||
          step.operands[0].kind != VHO_FHE_CKKS_PLAN_SOURCE_VALUE ||
          !SHA256(step.plaintext_asset_sha256))
        return Report(diagnostic, "encode lacks one authenticated source asset");
      return true;
    case OPR_DSLCKKSADD:
    case OPR_DSLCKKSSUB:
    case OPR_DSLCKKSMUL:
      if (step.operands.size() != 2)
        return Report(diagnostic, "binary CKKS operation lacks two operands");
      return true;
    case OPR_DSLCKKSROTATE:
      if (step.operands.size() != 1 || step.signed_rotations.size() != 1 ||
          step.required_keys.size() != 1 ||
          !Signed_Attribute(step, "attr.signed_steps", &signed_value) ||
          signed_value != step.signed_rotations[0])
        return Report(diagnostic, "rotate requirement is incomplete");
      return true;
    case OPR_DSLCKKSRESCALE:
      if (step.operands.size() != 1 ||
          !Unsigned_Attribute(step, "attr.levels", &unsigned_value) ||
          !Signed_Attribute(step, "attr.target_scale_bits", &signed_value) ||
          signed_value != step.result_state.scale_bits)
        return Report(diagnostic, "rescale target state is inconsistent");
      return true;
    case OPR_DSLCKKSMODSWITCH:
      if (step.operands.size() != 1 ||
          !Signed_Attribute(step, "attr.target_level", &signed_value) ||
          signed_value != step.result_state.level)
        return Report(diagnostic, "modswitch target state is inconsistent");
      return true;
    case OPR_DSLCKKSRELIN:
      if (step.operands.size() != 1 || step.required_keys.size() != 1)
        return Report(diagnostic, "relinearization key is missing");
      return true;
    case OPR_DSLCKKSBOOTSTRAP:
      if (step.operands.size() != 1 || step.required_keys.size() != 1 ||
          !Signed_Attribute(step, "attr.target_level", &signed_value) ||
          signed_value != step.result_state.level ||
          !Attribute(step, "attr.reason", &reason) || reason.empty())
        return Report(diagnostic, "bootstrap target, reason, or key is missing");
      return true;
    default:
      return Report(diagnostic, "unsupported CKKS operation");
  }
}

/* Preflight one group and collect deterministic borrowed-input tables. */
bool Prepare_Group(const VHO_FHE_CKKS2C_GROUP &group,
                   std::map<uint32_t, size_t> *sources,
                   std::map<std::string, size_t> *bounds,
                   FILE *diagnostic)
{
  if (!C_Identifier(group.function_name) || !SHA256(group.identity_sha256) ||
      group.owner_pu_st == 0 || group.source_value_id == 0 ||
      group.context_pu_identity_id == 0 ||
      group.plan.group_output_steps.size() != 1 ||
      group.plan.source_final_step_index ==
          std::numeric_limits<uint32_t>::max())
    return Report(diagnostic, "group identity or single-output contract is invalid");
  std::vector<unsigned char> canonical;
  if (!VHO_FHE_CKKS_Serialize_Event_Plan(
          group.plan, &canonical, diagnostic))
    return false;
  std::set<uint32_t> source_set;
  std::set<std::string> bound_set;
  for (size_t i = 0; i < group.plan.steps.size(); ++i) {
    const VHO_FHE_CKKS_PLAN_STEP &step = group.plan.steps[i];
    if (!Operation_Valid(step, diagnostic))
      return false;
    for (size_t j = 0; j < step.operands.size(); ++j) {
      const VHO_FHE_CKKS_PLAN_OPERAND &operand = step.operands[j];
      /* Encode resolves its external tensor by value ID plus digest through
       * the asset API; it is not an already-materialized runtime CKKS value. */
      if (operand.kind == VHO_FHE_CKKS_PLAN_SOURCE_VALUE &&
          step.logical_operator != OPR_DSLCKKSENCODE)
        source_set.insert(operand.reference);
      else if (operand.kind == VHO_FHE_CKKS_PLAN_BOUND_FORMAL)
        bound_set.insert(operand.bound_role);
    }
  }
  size_t index = 0;
  for (std::set<uint32_t>::const_iterator value = source_set.begin();
       value != source_set.end(); ++value)
    (*sources)[*value] = index++;
  index = 0;
  for (std::set<std::string>::const_iterator role = bound_set.begin();
       role != bound_set.end(); ++role)
    (*bounds)[*role] = index++;
  return true;
}

/* Emit one expected result-state object with no hidden provider defaults. */
void Emit_State(std::ostringstream *text, size_t index,
                const VHO_FHE_CKKS_PLAN_STATE &state)
{
  *text << "  static const open64_fhe_ckks2c_state_v1 state_" << index
        << " = {\n"
        << "    OPEN64_FHE_CKKS2C_FACADE_VERSION_V1, sizeof(state_" << index
        << "), " << state.encryption_descriptor_id << "u, " << state.scheme
        << "u, " << state.value_class << "u, " << state.level << ", "
        << state.scale_bits << ", " << state.component_count << ", "
        << state.precision_bits << ", " << state.slot_count << "u, "
        << state.alignment_group << "u, " << state.pending_actions << "u, "
        << state.pending_bootstrap_reason << "u, "
        << C_String(state.encrypted_layout) << "\n  };\n";
}

/* Emit one checked primitive call after its operands and state are named. */
void Emit_Operation(std::ostringstream *text,
                    const VHO_FHE_CKKS_PLAN_STEP &step, size_t index,
                    const std::map<uint32_t, size_t> &sources,
                    const std::map<std::string, size_t> &bounds)
{
  const std::string left = step.operands.empty() ? "" :
      Operand_Expression(step.operands[0], sources, bounds);
  const std::string right = step.operands.size() < 2 ? "" :
      Operand_Expression(step.operands[1], sources, bounds);
  int32_t signed_value = 0;
  uint32_t unsigned_value = 0;
  std::string reason;
  *text << "  /* logical_operator=" << step.logical_operator
        << " version=" << step.operator_version;
  for (size_t i = 0; i < step.attributes.size(); ++i)
    *text << " " << step.attributes[i].name << "="
          << step.attributes[i].value;
  *text << " */\n";
  *text << "  status = ";
  switch (step.logical_operator) {
    case OPR_DSLCKKSENCODE:
      *text << "open64_fhe_ckks2c_encode_asset_v1(execution, "
            << step.operands[0].reference << "u, "
            << C_String(step.plaintext_asset_sha256);
      break;
    case OPR_DSLCKKSADD:
      *text << "open64_fhe_ckks2c_add_v1(execution, " << left << ", "
            << right;
      break;
    case OPR_DSLCKKSSUB:
      *text << "open64_fhe_ckks2c_sub_v1(execution, " << left << ", "
            << right;
      break;
    case OPR_DSLCKKSMUL:
      *text << "open64_fhe_ckks2c_mul_v1(execution, " << left << ", "
            << right;
      break;
    case OPR_DSLCKKSROTATE:
      Signed_Attribute(step, "attr.signed_steps", &signed_value);
      *text << "open64_fhe_ckks2c_rotate_v1(execution, " << left << ", "
            << signed_value << ", " << C_String(step.required_keys[0]);
      break;
    case OPR_DSLCKKSRESCALE:
      Unsigned_Attribute(step, "attr.levels", &unsigned_value);
      Signed_Attribute(step, "attr.target_scale_bits", &signed_value);
      *text << "open64_fhe_ckks2c_rescale_v1(execution, " << left << ", "
            << unsigned_value << "u, " << signed_value;
      break;
    case OPR_DSLCKKSMODSWITCH:
      Signed_Attribute(step, "attr.target_level", &signed_value);
      *text << "open64_fhe_ckks2c_modswitch_v1(execution, " << left << ", "
            << signed_value;
      break;
    case OPR_DSLCKKSRELIN:
      *text << "open64_fhe_ckks2c_relin_v1(execution, " << left << ", "
            << C_String(step.required_keys[0]);
      break;
    case OPR_DSLCKKSBOOTSTRAP:
      Signed_Attribute(step, "attr.target_level", &signed_value);
      Attribute(step, "attr.reason", &reason);
      *text << "open64_fhe_ckks2c_bootstrap_v1(execution, " << left << ", "
            << signed_value << ", " << C_String(reason) << ", "
            << C_String(step.required_keys[0]);
      break;
  }
  *text << ", &state_" << index << ", &step_" << index << ");\n"
        << "  if (status != OPEN64_FHE_CKKS2C_STATUS_OK_V1)\n"
        << "    goto cleanup;\n";
}

/* Emit one evaluator with borrowed inputs, owned intermediates, one transferred
 * result, and uniform cleanup on every provider failure. */
void Emit_Group(std::ostringstream *text, const VHO_FHE_CKKS2C_GROUP &group,
                const std::map<uint32_t, size_t> &sources,
                const std::map<std::string, size_t> &bounds)
{
  *text << "/* group_sha256=" << group.identity_sha256
        << " owner_pu=" << group.owner_pu_st
        << " source_value=" << group.source_value_id
        << " context_identity=" << group.context_pu_identity_id
        << " callsite=" << group.context_callsite_id << " */\n"
        << "open64_fhe_ckks2c_status_v1\n" << group.function_name
        << "(open64_fhe_ckks2c_execution_v1 execution,\n"
        << " " << "open64_fhe_ckks2c_value_v1 *out_value)\n{\n"
        << "  open64_fhe_ckks2c_status_v1 status = "
        << "OPEN64_FHE_CKKS2C_STATUS_OK_V1;\n";
  for (size_t i = 0; i < sources.size(); ++i)
    *text << "  open64_fhe_ckks2c_value_v1 source_" << i << " = 0;\n";
  for (size_t i = 0; i < bounds.size(); ++i)
    *text << "  open64_fhe_ckks2c_value_v1 bound_" << i << " = 0;\n";
  for (size_t i = 0; i < group.plan.steps.size(); ++i)
    *text << "  open64_fhe_ckks2c_value_v1 step_" << i << " = 0;\n";
  *text << "  if (execution == 0 || out_value == 0)\n"
        << "    return UINT32_C(1);\n"
        << "  *out_value = 0;\n";
  for (std::map<uint32_t, size_t>::const_iterator source = sources.begin();
       source != sources.end(); ++source)
    *text << "  status = open64_fhe_ckks2c_source_value_v1(execution, "
          << source->first << "u, &source_" << source->second << ");\n"
          << "  if (status != OPEN64_FHE_CKKS2C_STATUS_OK_V1)\n"
          << "    goto cleanup;\n";
  for (std::map<std::string, size_t>::const_iterator bound = bounds.begin();
       bound != bounds.end(); ++bound)
    *text << "  status = open64_fhe_ckks2c_bound_value_v1(execution, "
          << C_String(bound->first) << ", &bound_" << bound->second << ");\n"
          << "  if (status != OPEN64_FHE_CKKS2C_STATUS_OK_V1)\n"
          << "    goto cleanup;\n";
  for (size_t i = 0; i < group.plan.steps.size(); ++i) {
    Emit_State(text, i, group.plan.steps[i].result_state);
    Emit_Operation(text, group.plan.steps[i], i, sources, bounds);
  }
  const size_t final_step = group.plan.source_final_step_index;
  *text << "  *out_value = step_" << final_step << ";\n"
        << "  step_" << final_step << " = 0;\n"
        << "cleanup:\n";
  for (size_t i = group.plan.steps.size(); i != 0; --i)
    *text << "  if (step_" << (i - 1) << " != 0)\n"
          << "    open64_fhe_ckks2c_value_release_v1(execution, step_"
          << (i - 1) << ");\n";
  *text << "  return status;\n}\n\n";
}

}  // namespace

/* Validate every group before constructing candidate text, then atomically
 * replace caller output and counters. */
bool VHO_FHE_CKKS2C_Emit_Module(
    const std::string &module_name,
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    std::string *output, VHO_FHE_CKKS2C_EMIT_RESULT *result,
    FILE *diagnostic)
{
  if (output == NULL || result == NULL || !C_Identifier(module_name) ||
      groups.empty() || groups.size() > kMaxGroups)
    return Report(diagnostic, "module or group set is invalid");
  std::set<std::string> function_names;
  std::set<std::string> identities;
  std::vector<std::map<uint32_t, size_t> > sources(groups.size());
  std::vector<std::map<std::string, size_t> > bounds(groups.size());
  VHO_FHE_CKKS2C_EMIT_RESULT candidate_result = {0, 0, 0, 0};
  for (size_t i = 0; i < groups.size(); ++i) {
    if (!function_names.insert(groups[i].function_name).second ||
        !identities.insert(groups[i].identity_sha256).second ||
        !Prepare_Group(groups[i], &sources[i], &bounds[i], diagnostic))
      return Report(diagnostic, "duplicate or invalid CKKS2C group");
    ++candidate_result.group_count;
    candidate_result.operation_count += groups[i].plan.steps.size();
    candidate_result.source_value_count += sources[i].size();
    candidate_result.bound_value_count += bounds[i].size();
  }

  std::ostringstream text;
  text << "/* Generated by Open64 CKKS2C from module " << module_name
       << ". Do not edit. */\n"
       << "#include <stdint.h>\n"
       << "#include \"open64_fhe_ckks2c_facade.h\"\n\n";
  for (size_t i = 0; i < groups.size(); ++i)
    Emit_Group(&text, groups[i], sources[i], bounds[i]);
  const std::string candidate = text.str();
  if (candidate.size() > kMaxModuleBytes)
    return Report(diagnostic, "generated module exceeds bounded size");
  *output = candidate;
  *result = candidate_result;
  return true;
}
