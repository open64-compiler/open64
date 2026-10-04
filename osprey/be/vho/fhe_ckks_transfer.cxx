/*
 * Copyright (C) 2026 Open64 Project
 *
 * Verify explicit unary CKKS state transitions before native expansion.
 * These rules do not infer provider behavior or mutate canonical tensor TY.
 * See doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "fhe_ckks_transfer.h"

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

/* Return the first exact logical attribute; common/com later proves schema. */
static const char *
VHO_FHE_CKKS_Transfer_Attribute(
    const DSL_CKKS_EXPANSION_STEP &step, const char *name)
{
  if (step.attribute_count != 0 && step.attributes == NULL)
    return NULL;
  for (UINT32 i = 0; i < step.attribute_count; ++i) {
    if (step.attributes[i].name != NULL &&
        strcmp(step.attributes[i].name, name) == 0)
      return step.attributes[i].value;
  }
  return NULL;
}

/* Reject whitespace, plus signs, overflow, and numeric suffixes. */
BOOL
VHO_FHE_CKKS_Step_Integer(
    const DSL_CKKS_EXPANSION_STEP &step, const char *name, INT32 *value)
{
  const char *text = VHO_FHE_CKKS_Transfer_Attribute(step, name);
  if (text == NULL || text[0] == '\0' || value == NULL)
    return FALSE;
  const char *digit = text[0] == '-' ? text + 1 : text;
  if (*digit == '\0')
    return FALSE;
  for (const char *cursor = digit; *cursor != '\0'; ++cursor) {
    if (*cursor < '0' || *cursor > '9')
      return FALSE;
  }
  errno = 0;
  char *end = NULL;
  long parsed = strtol(text, &end, 10);
  if (errno != 0 || end == text || *end != '\0' ||
      parsed < INT_MIN || parsed > INT_MAX)
    return FALSE;
  *value = (INT32)parsed;
  return TRUE;
}

/* One diagnostic for a failed transfer, distinct from row/attribute checks. */
static BOOL
VHO_FHE_CKKS_Transfer_Report(FILE *diagnostic, const char *reason)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-TRANSFER-001: %s\n", reason);
  return FALSE;
}

/* All unary operations preserve representation identity and packed layout;
 * only bootstrap may restore level, scale, precision, and noise capacity. */
BOOL
VHO_FHE_CKKS_Verify_Unary_State_Transfer(
    const DSL_CKKS_EXPANSION_STEP &step,
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &input,
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &output,
    FILE *diagnostic)
{
  if (input.scheme != DSL_FHE_SCHEME_CKKS ||
      input.value_class != DSL_FHE_VALUE_CLASS_CIPHERTEXT ||
      output.scheme != DSL_FHE_SCHEME_CKKS ||
      output.value_class != DSL_FHE_VALUE_CLASS_CIPHERTEXT ||
      input.encryption_descriptor_id == 0 ||
      input.encryption_descriptor_id != output.encryption_descriptor_id ||
      input.level < 0 || input.scale_bits <= 0 ||
      input.component_count < 2 || input.precision_bits <= 0 ||
      input.slot_count == 0 || input.slot_count != output.slot_count ||
      input.encrypted_layout_name == 0 ||
      input.encrypted_layout_name != output.encrypted_layout_name ||
      input.alignment_group != output.alignment_group)
    return VHO_FHE_CKKS_Transfer_Report(
        diagnostic, "input is not a concrete compatible ciphertext");

  INT32 levels = 0;
  switch (step.dsl_operator) {
  case OPR_DSLCKKSBOOTSTRAP:
    if (input.pending_actions != DSL_FHE_CKKS_PENDING_BOOTSTRAP ||
        input.pending_bootstrap_reason !=
            DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH ||
        output.pending_actions != 0 ||
        output.pending_bootstrap_reason !=
            DSL_FHE_BOOTSTRAP_REASON_NONE ||
        output.component_count != 2 || output.level <= input.level)
      return VHO_FHE_CKKS_Transfer_Report(
          diagnostic, "bootstrap did not restore the requested state");
    return TRUE;
  case OPR_DSLCKKSROTATE:
    if (output.level != input.level ||
        output.scale_bits != input.scale_bits ||
        output.component_count != input.component_count ||
        output.precision_bits != input.precision_bits ||
        output.pending_actions != input.pending_actions ||
        output.pending_bootstrap_reason !=
            input.pending_bootstrap_reason)
      return VHO_FHE_CKKS_Transfer_Report(
          diagnostic, "rotation changed ciphertext state");
    return TRUE;
  case OPR_DSLCKKSRELIN:
    if (input.component_count <= 2 ||
        (input.pending_actions &
             DSL_FHE_CKKS_PENDING_RELINEARIZE) == 0 ||
        output.component_count != 2 ||
        output.level != input.level ||
        output.scale_bits != input.scale_bits ||
        output.precision_bits > input.precision_bits ||
        output.pending_actions !=
            (input.pending_actions &
             ~DSL_FHE_CKKS_PENDING_RELINEARIZE) ||
        output.pending_bootstrap_reason !=
            input.pending_bootstrap_reason)
      return VHO_FHE_CKKS_Transfer_Report(
          diagnostic, "relinearization did not consume widened state");
    return TRUE;
  case OPR_DSLCKKSRESCALE:
    if (!VHO_FHE_CKKS_Step_Integer(step, "attr.levels", &levels) ||
        levels <= 0 || input.level < levels ||
        (input.pending_actions & DSL_FHE_CKKS_PENDING_RESCALE) == 0 ||
        output.level != input.level - levels ||
        output.scale_bits >= input.scale_bits ||
        output.component_count != input.component_count ||
        output.precision_bits > input.precision_bits ||
        output.pending_actions !=
            (input.pending_actions & ~DSL_FHE_CKKS_PENDING_RESCALE) ||
        output.pending_bootstrap_reason !=
            input.pending_bootstrap_reason)
      return VHO_FHE_CKKS_Transfer_Report(
          diagnostic, "rescale did not consume declared level and scale");
    return TRUE;
  case OPR_DSLCKKSMODSWITCH:
    if (output.level < 0 || output.level >= input.level ||
        output.scale_bits != input.scale_bits ||
        output.component_count != input.component_count ||
        output.precision_bits > input.precision_bits ||
        output.pending_actions != input.pending_actions ||
        output.pending_bootstrap_reason !=
            input.pending_bootstrap_reason)
      return VHO_FHE_CKKS_Transfer_Report(
          diagnostic, "modswitch did not lower the modulus level");
    return TRUE;
  default:
    return VHO_FHE_CKKS_Transfer_Report(
        diagnostic, "operator has no unary CKKS transfer rule");
  }
}
