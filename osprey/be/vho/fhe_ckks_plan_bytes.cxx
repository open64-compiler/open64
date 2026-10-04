/*
 * Copyright (C) 2026 Open64 Project
 *
 * Portable, process-local CKKS event-plan serialization for PU signatures.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#include "fhe_ckks_plan_bytes.h"

#include <algorithm>
#include <limits>

namespace {

const size_t kMaxPlanBytes = 4 * 1024 * 1024;
const size_t kMaxSteps = 65535;
const size_t kMaxOperands = 8;
const size_t kMaxRequirements = 64;
const size_t kMaxStringBytes = 4096;

/* Keep diagnostics stable and never publish a partial encoding. */
bool Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-PLAN-001: %s\n", message);
  return false;
}

/* Encode every integer explicitly so host endianness and struct padding do
 * not affect whole-PU signature identity. */
void U32(std::vector<unsigned char> *bytes, uint32_t value)
{
  for (unsigned shift = 0; shift < 32; shift += 8)
    bytes->push_back(static_cast<unsigned char>(value >> shift));
}

/* Length-prefix a semantic string; no C++ object representation is copied. */
void String(std::vector<unsigned char> *bytes, const std::string &value)
{
  U32(bytes, static_cast<uint32_t>(value.size()));
  bytes->insert(bytes->end(), value.begin(), value.end());
}

/* Reject unbounded or ambiguous strings before writing the candidate. */
bool Valid_String(const std::string &value, bool allow_empty)
{
  if ((!allow_empty && value.empty()) || value.size() > kMaxStringBytes)
    return false;
  for (size_t i = 0; i < value.size(); ++i)
    if (value[i] < ' ' || value[i] > '~')
      return false;
  return true;
}

/* Payload identity is a lowercase SHA-256, never a platform path. */
bool Valid_SHA256(const std::string &value)
{
  if (value.empty())
    return true;
  if (value.size() != 64)
    return false;
  for (size_t i = 0; i < value.size(); ++i)
    if (!((value[i] >= '0' && value[i] <= '9') ||
          (value[i] >= 'a' && value[i] <= 'f')))
      return false;
  return true;
}

/* Keep attribute ordering portable to the backend's older C++ dialect. */
struct Attribute_Name_Less {
  /* Order semantic names without depending on producer row order. */
  bool operator()(const VHO_FHE_CKKS_PLAN_ATTRIBUTE &left,
                  const VHO_FHE_CKKS_PLAN_ATTRIBUTE &right) const
  {
    return left.name < right.name;
  }
};

/* Sort attributes by name and reject aliases with duplicate names. */
bool Prepare_Attributes(const VHO_FHE_CKKS_PLAN_STEP &step,
                        std::vector<VHO_FHE_CKKS_PLAN_ATTRIBUTE> *result)
{
  *result = step.attributes;
  std::sort(result->begin(), result->end(), Attribute_Name_Less());
  for (size_t i = 0; i < result->size(); ++i)
    if (!Valid_String((*result)[i].name, false) ||
        !Valid_String((*result)[i].value, false) ||
        (i != 0 && (*result)[i - 1].name == (*result)[i].name))
      return false;
  return true;
}

/* Sort key requirements and reject a duplicate or unnamed key. */
bool Prepare_Keys(const VHO_FHE_CKKS_PLAN_STEP &step,
                  std::vector<std::string> *result)
{
  *result = step.required_keys;
  std::sort(result->begin(), result->end());
  for (size_t i = 0; i < result->size(); ++i)
    if (!Valid_String((*result)[i], false) ||
        (i != 0 && (*result)[i - 1] == (*result)[i]))
      return false;
  return true;
}

/* Signed rotations form a set, not a sequence of unrelated effects. */
bool Prepare_Rotations(const VHO_FHE_CKKS_PLAN_STEP &step,
                       std::vector<int32_t> *result)
{
  *result = step.signed_rotations;
  std::sort(result->begin(), result->end());
  for (size_t i = 0; i < result->size(); ++i)
    if ((*result)[i] == 0 ||
        (i != 0 && (*result)[i - 1] == (*result)[i]))
      return false;
  return true;
}

/* Encode one step only after its structural and bounded-state preflight. */
bool Encode_Step(const VHO_FHE_CKKS_PLAN_STEP &step, size_t index,
                 std::vector<unsigned char> *bytes, FILE *diagnostic)
{
  const VHO_FHE_CKKS_PLAN_STATE &state = step.result_state;
  if (step.logical_operator == 0 || step.operator_version == 0 ||
      step.result_ty == 0 || step.operands.size() > kMaxOperands ||
      step.attributes.size() > kMaxRequirements ||
      step.required_keys.size() > kMaxRequirements ||
      step.signed_rotations.size() > kMaxRequirements ||
      state.encryption_descriptor_id == 0 || state.scheme == 0 ||
      state.value_class == 0 || state.level < 0 || state.scale_bits <= 0 ||
      state.component_count <= 0 || state.precision_bits <= 0 ||
      state.slot_count == 0 || !Valid_String(state.encrypted_layout, false) ||
      !Valid_SHA256(step.plaintext_asset_sha256))
    return Report(diagnostic, "incomplete operation or result state");

  std::vector<VHO_FHE_CKKS_PLAN_ATTRIBUTE> attributes;
  std::vector<std::string> keys;
  std::vector<int32_t> rotations;
  if (!Prepare_Attributes(step, &attributes) ||
      !Prepare_Keys(step, &keys) ||
      !Prepare_Rotations(step, &rotations))
    return Report(diagnostic, "duplicate or malformed semantic requirement");

  U32(bytes, step.logical_operator);
  U32(bytes, step.operator_version);
  U32(bytes, step.result_ty);
  U32(bytes, static_cast<uint32_t>(step.operands.size()));
  for (size_t i = 0; i < step.operands.size(); ++i) {
    const VHO_FHE_CKKS_PLAN_OPERAND &operand = step.operands[i];
    if ((operand.kind == VHO_FHE_CKKS_PLAN_SOURCE_VALUE &&
         (operand.reference == 0 || !operand.bound_role.empty())) ||
        (operand.kind == VHO_FHE_CKKS_PLAN_PRIOR_STEP &&
         (operand.reference >= index || !operand.bound_role.empty())) ||
        (operand.kind == VHO_FHE_CKKS_PLAN_BOUND_FORMAL &&
         (operand.reference != 0 ||
          !Valid_String(operand.bound_role, false))) ||
        (operand.kind != VHO_FHE_CKKS_PLAN_SOURCE_VALUE &&
         operand.kind != VHO_FHE_CKKS_PLAN_PRIOR_STEP &&
         operand.kind != VHO_FHE_CKKS_PLAN_BOUND_FORMAL))
      return Report(diagnostic, "operand is not a prior, source, or B formal");
    U32(bytes, operand.kind);
    U32(bytes, operand.reference);
    String(bytes, operand.bound_role);
  }
  U32(bytes, static_cast<uint32_t>(attributes.size()));
  for (size_t i = 0; i < attributes.size(); ++i) {
    String(bytes, attributes[i].name);
    String(bytes, attributes[i].value);
  }
  U32(bytes, state.encryption_descriptor_id);
  U32(bytes, state.scheme);
  U32(bytes, state.value_class);
  U32(bytes, static_cast<uint32_t>(state.level));
  U32(bytes, static_cast<uint32_t>(state.scale_bits));
  U32(bytes, static_cast<uint32_t>(state.component_count));
  U32(bytes, static_cast<uint32_t>(state.precision_bits));
  U32(bytes, state.slot_count);
  U32(bytes, state.alignment_group);
  String(bytes, state.encrypted_layout);
  U32(bytes, state.pending_actions);
  U32(bytes, state.pending_bootstrap_reason);
  U32(bytes, static_cast<uint32_t>(keys.size()));
  for (size_t i = 0; i < keys.size(); ++i)
    String(bytes, keys[i]);
  U32(bytes, static_cast<uint32_t>(rotations.size()));
  for (size_t i = 0; i < rotations.size(); ++i)
    U32(bytes, static_cast<uint32_t>(rotations[i]));
  String(bytes, step.plaintext_asset_sha256);
  return true;
}

}  // namespace

/* Validate the entire event and replace output only with its complete v1
 * encoding. Native step legality and value-state transfer are separate gates. */
bool VHO_FHE_CKKS_Serialize_Event_Plan(
    const VHO_FHE_CKKS_EVENT_PLAN &plan,
    std::vector<unsigned char> *bytes, FILE *diagnostic)
{
  if (bytes == NULL || plan.source_static_ordinal == 0 ||
      plan.steps.empty() || plan.steps.size() > kMaxSteps ||
      plan.group_output_steps.empty() ||
      plan.group_output_steps.size() > plan.steps.size())
    return Report(diagnostic, "event or group shape is incomplete");
  uint32_t previous = 0;
  for (size_t i = 0; i < plan.group_output_steps.size(); ++i) {
    uint32_t output = plan.group_output_steps[i];
    if (output >= plan.steps.size() || (i != 0 && output <= previous))
      return Report(diagnostic, "group outputs are missing or reordered");
    previous = output;
  }
  if (plan.group_output_steps.back() != plan.steps.size() - 1 ||
      (plan.source_final_step_index !=
           std::numeric_limits<uint32_t>::max() &&
       plan.source_final_step_index != plan.group_output_steps.back()))
    return Report(diagnostic, "source final is not the terminal group result");

  std::vector<unsigned char> candidate;
  const char magic[] = {'F', 'C', 'K', 'P', 'L', 'A', 'N', '1'};
  candidate.insert(candidate.end(), magic, magic + sizeof(magic));
  U32(&candidate, plan.source_static_ordinal);
  U32(&candidate, static_cast<uint32_t>(plan.steps.size()));
  U32(&candidate, static_cast<uint32_t>(plan.group_output_steps.size()));
  for (size_t i = 0; i < plan.group_output_steps.size(); ++i)
    U32(&candidate, plan.group_output_steps[i]);
  U32(&candidate, plan.source_final_step_index);
  for (size_t i = 0; i < plan.steps.size(); ++i) {
    if (!Encode_Step(plan.steps[i], i, &candidate, diagnostic))
      return false;
    if (candidate.size() > kMaxPlanBytes)
      return Report(diagnostic, "event exceeds canonical encoding budget");
  }
  bytes->swap(candidate);
  return true;
}
