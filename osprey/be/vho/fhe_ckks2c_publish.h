/*
 * Copyright (C) 2026 Open64 Project
 *
 * Deterministic staging of the provider-private CKKS2C output family.  This
 * service converts a complete, already-certified group collection into the
 * private C evaluator, facade manifest, group report, and compile command,
 * then registers their temporary/final path pairs with the shared FHE
 * checkpoint transaction.  It does not publish files or mutate WHIRL.  See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D3.
 */

#ifndef fhe_ckks2c_publish_INCLUDED
#define fhe_ckks2c_publish_INCLUDED

#include <stdint.h>
#include <stdio.h>
#include <string>
#include <vector>

#include "fhe_ckks2c_collect.h"

struct VHO_FHE_CKKS2C_PUBLISH_RESULT {
  VHO_FHE_CKKS2C_COLLECT_RESULT collected;
  VHO_FHE_CKKS2C_EMIT_RESULT emitted;
  std::string private_c_sha256;
  std::string private_c_temporary_path;
  std::string private_c_final_path;
  std::string facade_manifest_temporary_path;
  std::string facade_manifest_final_path;
  std::string group_report_temporary_path;
  std::string group_report_final_path;
  std::string compile_command_temporary_path;
  std::string compile_command_final_path;
};

/* Validate the whole-program census, build all four auxiliary files in
 * memory, register their no-replace path pairs, and write only temporary
 * files.  The caller must abort the active shared checkpoint after failure.
 * Success leaves publication and the final binary commit marker to the
 * established checkpoint lifecycle. */
bool VHO_FHE_CKKS2C_Stage_Private_Artifacts(
    const char *binary_final_path, uint32_t expected_pu_count,
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    const VHO_FHE_CKKS2C_COLLECT_RESULT &collected,
    VHO_FHE_CKKS2C_PUBLISH_RESULT *result, FILE *diagnostic);

#endif /* fhe_ckks2c_publish_INCLUDED */
