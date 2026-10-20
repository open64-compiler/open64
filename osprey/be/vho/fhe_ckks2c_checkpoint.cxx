/*
 * Copyright (C) 2026 Open64 Project
 *
 * All-PU bridge between mapped CKKS event collection and the shared atomic
 * FHE checkpoint lifecycle.  Collection is read-only and occurs with each
 * owning PU active; finalization runs once after the driver's complete image
 * validation and stages private outputs without changing WHIRL.  See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D3.
 */

#include "fhe_ckks2c_checkpoint.h"

#include <string>
#include <vector>

#include "dsl_ckks_event.h"
#include "fhe_ckks2c_collect.h"
#include "fhe_ckks2c_publish.h"

namespace {

struct CKKS2C_Checkpoint_Session {
  bool active;
  bool failed;
  std::string binary_final_path;
  VHO_FHE_CKKS2C_COLLECTION collection;
  VHO_FHE_CKKS2C_PUBLISH_RESULT published;

  CKKS2C_Checkpoint_Session() : active(false), failed(false) {}
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
      !VHO_FHE_CKKS2C_Stage_Private_Artifacts(
          session.binary_final_path.c_str(), expected_pu_count, groups,
          collected, &session.published, diagnostic))
    return Report(diagnostic, "private-output finalization failed");
  if (diagnostic != NULL)
    fprintf(diagnostic,
            "FHE-CKKS2C-CHECKPOINT: pu=%u groups=%u events=%u "
            "assets=%u keys=%u c_sha256=%s\n",
            collected.pu_count, collected.group_count, collected.event_count,
            collected.external_asset_count, collected.key_use_count,
            session.published.private_c_sha256.c_str());
  return true;
}

/* Drop process-local state after the shared lifecycle commits or aborts. */
void VHO_FHE_CKKS2C_Checkpoint_Complete(bool committed)
{
  (void)committed;
  session = CKKS2C_Checkpoint_Session();
}
