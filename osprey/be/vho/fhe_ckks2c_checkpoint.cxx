/*
 * Copyright (C) 2026 Open64 Project
 *
 * All-PU bridge between mapped CKKS event collection and the shared atomic
 * FHE checkpoint lifecycle. Collection and grouped standard-WHIRL lowering
 * occur with each owning PU active; finalization authenticates the all-PU
 * public join and stages private outputs once. See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D3-D4.
 */

#include "fhe_ckks2c_checkpoint.h"

#include <cstring>
#include <string>
#include <vector>

#include "dsl_ckks_event.h"
#include "fhe_ckks2c_collect.h"
#include "fhe_ckks2c_lower.h"
#include "fhe_ckks2c_publish.h"
#include "fhe_ckks2c_public_join.h"
#include "pu_info.h"

namespace {

struct CKKS2C_Checkpoint_Session {
  bool active;
  bool failed;
  std::string binary_final_path;
  VHO_FHE_CKKS2C_COLLECTION collection;
  VHO_FHE_CKKS2C_PUBLISH_RESULT published;
  std::vector<VHO_FHE_CKKS2C_PUBLIC_ROUTE> public_routes;
  VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT public_route_result;
  VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN public_join;
  VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT public_join_result;
  VHO_FHE_CKKS2C_LOWER_RESULT lowered;

  CKKS2C_Checkpoint_Session() : active(false), failed(false)
  {
    memset(&public_route_result, 0, sizeof(public_route_result));
    memset(&public_join_result, 0, sizeof(public_join_result));
    memset(&lowered, 0, sizeof(lowered));
  }
};

CKKS2C_Checkpoint_Session session;

/* Emit one session diagnostic and mark the current transaction terminal. */
bool Report(FILE *diagnostic, const char *message)
{
  session.failed = true;
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHECKKS2C-004: %s\n", message);
  return false;
}

/* Authenticate every preliminary image route against the final full join. */
bool Public_Routes_Match_Join()
{
  if (session.public_routes.size() !=
      session.public_join_result.private_group_binding_count)
    return false;
  std::vector<bool> consumed(session.public_routes.size(), false);
  uint32_t matched = 0;
  for (uint32_t binding_index = 0;
       binding_index < session.public_join.bindings.size(); ++binding_index) {
    const VHO_FHE_CKKS2C_PUBLIC_BINDING &binding =
        session.public_join.bindings[binding_index];
    if (binding.binding_kind != VHO_FHE_CKKS2C_PUBLIC_PRIVATE_GROUP)
      continue;
    uint32_t matches = 0;
    for (uint32_t route_index = 0;
         route_index < session.public_routes.size(); ++route_index) {
      const VHO_FHE_CKKS2C_PUBLIC_ROUTE &route =
          session.public_routes[route_index];
      if (route.specialized_owner_pu_st !=
              binding.specialized_owner_pu_st ||
          route.specialized_source_value_id !=
              binding.specialized_source_value_id ||
          route.context_pu_identity_id != binding.context_pu_identity_id ||
          route.context_callsite_id != binding.context_callsite_id ||
          route.public_static_ordinal != binding.static_ordinal ||
          route.operation_kind != binding.operation_kind)
        continue;
      if (consumed[route_index])
        return false;
      consumed[route_index] = true;
      ++matches;
    }
    if (matches != 1)
      return false;
    ++matched;
  }
  return matched == session.public_routes.size();
}

}  // namespace

/* Lazily start one session and collect the current active PU. */
bool VHO_FHE_CKKS2C_Checkpoint_Collect_Active_PU(
    uint32_t owner_pu_st, const char *binary_final_path, FILE *diagnostic)
{
  if (DSL_CKKS_Event_Image_Count() == 0)
    return true;
  if (binary_final_path == NULL || binary_final_path[0] == 0)
    return Report(diagnostic, "conversion checkpoint output is absent");
  if (!session.active) {
    session.active = true;
    session.binary_final_path = binary_final_path;
  } else if (session.binary_final_path != binary_final_path) {
    return Report(diagnostic, "checkpoint output changed across PUs");
  }
  if (session.failed || !VHO_FHE_CKKS2C_Collect_Active_PU(
                            owner_pu_st, &session.collection, diagnostic))
    return Report(diagnostic, "active-PU CKKS2C collection failed");
  return true;
}

/* Collect and atomically lower every CKKS group in the current active PU. */
bool VHO_FHE_CKKS2C_Checkpoint_Lower_Active_PU(
    PU_Info *pu_info, const char *binary_final_path, FILE *diagnostic,
    VHO_FHE_CKKS2C_LOWER_RESULT *result)
{
  if (result == NULL)
    return Report(diagnostic, "active-PU lowering result is absent");
  memset(result, 0, sizeof(*result));
  if (DSL_CKKS_Event_Image_Count() == 0)
    return true;
  if (pu_info == NULL || pu_info != Current_PU_Info)
    return Report(diagnostic, "active-PU lowering owner is invalid");
  if (session.public_routes.empty() &&
      !VHO_FHE_CKKS2C_Build_Public_Routes_From_Image(
          &session.public_routes, &session.public_route_result,
          diagnostic))
    return Report(diagnostic, "public descriptor route preparation failed");
  const size_t first_group = session.collection.groups.size();
  if (!VHO_FHE_CKKS2C_Checkpoint_Collect_Active_PU(
          PU_Info_proc_sym(pu_info), binary_final_path, diagnostic))
    return false;
  std::vector<VHO_FHE_CKKS2C_GROUP> groups(
      session.collection.groups.begin() + first_group,
      session.collection.groups.end());
  VHO_FHE_CKKS2C_LOWER_RESULT lowered;
  memset(&lowered, 0, sizeof(lowered));
  if (!groups.empty() &&
      !VHO_FHE_CKKS2C_Lower_Active_PU(
          pu_info, groups, session.public_routes, diagnostic, &lowered))
    return Report(diagnostic, "active-PU grouped lowering failed");
  session.lowered.group_count += lowered.group_count;
  session.lowered.replacement_block_count +=
      lowered.replacement_block_count;
  session.lowered.retired_member_count += lowered.retired_member_count;
  session.lowered.inserted_statement_count += lowered.inserted_statement_count;
  session.lowered.standard_call_count += lowered.standard_call_count;
  session.lowered.output_handle_count += lowered.output_handle_count;
  session.lowered.status_check_count += lowered.status_check_count;
  *result = lowered;
  return true;
}

/* Finish exact coverage and stage the private output family once. */
bool VHO_FHE_CKKS2C_Checkpoint_Finalize(
    uint32_t expected_pu_count, FILE *diagnostic)
{
  if (!session.active)
    return true;
  if (session.failed)
    return Report(diagnostic, "checkpoint session already failed");
  std::vector<VHO_FHE_CKKS2C_GROUP> groups;
  VHO_FHE_CKKS2C_COLLECT_RESULT collected = {0, 0, 0, 0, 0, 0};
  if (!VHO_FHE_CKKS2C_Finish_Collection(
          &session.collection, &groups, &collected, diagnostic) ||
      session.lowered.group_count != collected.group_count ||
      session.lowered.retired_member_count != collected.event_count ||
      !VHO_FHE_CKKS2C_Build_Public_Join(
          groups, &session.public_join, &session.public_join_result,
          diagnostic) ||
      session.public_join_result.private_group_binding_count !=
          collected.group_count ||
      session.public_join_result.dynamic_binding_count !=
          session.public_join_result.private_group_binding_count +
              session.public_join_result.representation_binding_count ||
      session.public_route_result.canonical_pu_count !=
          session.public_join_result.canonical_pu_count ||
      session.public_route_result.canonical_source_count !=
          session.public_join_result.canonical_source_count ||
      session.public_route_result.static_descriptor_count !=
          session.public_join_result.static_descriptor_count ||
      session.public_route_result.dynamic_binding_count !=
          session.public_join_result.dynamic_binding_count ||
      !Public_Routes_Match_Join() ||
      !VHO_FHE_CKKS2C_Stage_Private_Artifacts(
          session.binary_final_path.c_str(), expected_pu_count, groups,
          collected, &session.published, diagnostic))
    return Report(diagnostic, "private-output finalization failed");
  if (diagnostic != NULL)
    fprintf(diagnostic,
            "FHE-CKKS2C-CHECKPOINT: pu=%u groups=%u events=%u "
            "assets=%u keys=%u replacements=%u public_calls=%u "
            "retired=%u canonical_pus=%u descriptors=%u bindings=%u "
            "join_sha256=%s c_sha256=%s\n",
            collected.pu_count, collected.group_count, collected.event_count,
            collected.external_asset_count, collected.key_use_count,
            session.lowered.replacement_block_count,
            session.lowered.standard_call_count,
            session.lowered.retired_member_count,
            session.public_join_result.canonical_pu_count,
            session.public_join_result.static_descriptor_count,
            session.public_join_result.dynamic_binding_count,
            session.public_join.identity_sha256.c_str(),
            session.published.private_c_sha256.c_str());
  return true;
}

/* Drop process-local state after the shared lifecycle commits or aborts. */
void VHO_FHE_CKKS2C_Checkpoint_Complete(bool committed)
{
  (void)committed;
  session = CKKS2C_Checkpoint_Session();
}
