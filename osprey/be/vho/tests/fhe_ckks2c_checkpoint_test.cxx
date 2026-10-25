/*
 * Copyright (C) 2026 Open64 Project
 *
 * Focused S6-0d D3 transaction test for private CKKS2C artifacts.  It drives
 * the production staging API through the shared checkpoint lifecycle and
 * proves success, stale-output preservation, write/compile failure rollback,
 * missing-PU rejection, and explicit abort cleanup.  See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D3/S6D-T3.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include <fstream>
#include <string>
#include <vector>

#include "defs.h"
#include "dsl_opcode.h"
#include "fhe_checkpoint.h"
#include "fhe_ckks2c_publish.h"

static std::vector<VHO_FHE_CKKS2C_GROUP> groups;
static VHO_FHE_CKKS2C_COLLECT_RESULT collected;
static VHO_FHE_CKKS2C_PUBLISH_RESULT published;
static std::string binary_final;
static std::string compiler_path;
static std::string include_path;
static bool compile_should_fail;
static uint32_t expected_pus;
static uint32_t completion_count;
static bool completion_committed;

/* Quote a test-only compiler argument for the POSIX shell. */
static std::string Shell_Quote(const std::string &value)
{
  std::string result("'");
  for (size_t i = 0; i < value.size(); ++i)
    result += value[i] == '\'' ? "'\\''" : std::string(1, value[i]);
  result += '\'';
  return result;
}

/* Test the staged generated C with the real C compiler before publication. */
static bool Compile_Temporary_C()
{
  if (compile_should_fail)
    return false;
  const std::string object = published.private_c_temporary_path + ".o";
  const std::string command =
      Shell_Quote(compiler_path) +
      " -x c -std=c11 -Wall -Wextra -Werror -I" +
      Shell_Quote(include_path) +
      " -c " + Shell_Quote(published.private_c_temporary_path) +
      " -o " + Shell_Quote(object);
  const int status = system(command.c_str());
  const bool valid = status == 0 && access(object.c_str(), F_OK) == 0;
  remove(object.c_str());
  return valid;
}

/* Stage all production auxiliaries, then run the independent compile gate. */
static BOOL Finalize(const void *aggregate, FILE *diagnostic)
{
  if (aggregate == NULL ||
      *static_cast<const uint32_t *>(aggregate) != expected_pus)
    return FALSE;
  if (!VHO_FHE_CKKS2C_Stage_Private_Artifacts(
          binary_final.c_str(), expected_pus, groups, collected, &published,
          diagnostic))
    return FALSE;
  return Compile_Temporary_C() ? TRUE : FALSE;
}

/* Record whether the transaction committed and prove one callback per run. */
static void Complete(BOOL committed)
{
  ++completion_count;
  completion_committed = committed != FALSE;
}

/* Build one complete nine-primitive group accepted by the D1 emitter. */
static VHO_FHE_CKKS2C_GROUP Build_Group(uint32_t owner, char digest_digit)
{
  VHO_FHE_CKKS_PLAN_STATE state = {
    1, 1, 2, 15, 56, 2, 40, 32768, 1, "ckks_slots_v1", 0, 0
  };
  VHO_FHE_CKKS_EVENT_PLAN plan;
  plan.source_static_ordinal = owner;
  plan.steps.push_back({
    OPR_DSLCKKSENCODE, 1, 1002,
    {{VHO_FHE_CKKS_PLAN_SOURCE_VALUE, 20, ""}}, {}, state, {}, {},
    std::string(64, 'a')
  });
  plan.steps.push_back({
    OPR_DSLCKKSADD, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_SOURCE_VALUE, 10, ""},
     {VHO_FHE_CKKS_PLAN_PRIOR_STEP, 0, ""}}, {}, state, {}, {}, ""
  });
  plan.steps.push_back({
    OPR_DSLCKKSSUB, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 1, ""},
     {VHO_FHE_CKKS_PLAN_BOUND_FORMAL, 0, "relu_bound_0"}},
    {}, state, {}, {}, ""
  });
  plan.steps.push_back({
    OPR_DSLCKKSMUL, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 2, ""},
     {VHO_FHE_CKKS_PLAN_PRIOR_STEP, 0, ""}}, {}, state, {}, {}, ""
  });
  plan.steps.push_back({
    OPR_DSLCKKSROTATE, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 3, ""}},
    {{"attr.signed_steps", "-3"}}, state,
    {"rotation-key--3"}, {-3}, ""
  });
  state.level = 14;
  state.scale_bits = 50;
  plan.steps.push_back({
    OPR_DSLCKKSRESCALE, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 4, ""}},
    {{"attr.levels", "1"}, {"attr.target_scale_bits", "50"}},
    state, {}, {}, ""
  });
  state.level = 13;
  plan.steps.push_back({
    OPR_DSLCKKSMODSWITCH, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 5, ""}},
    {{"attr.target_level", "13"}}, state, {}, {}, ""
  });
  plan.steps.push_back({
    OPR_DSLCKKSRELIN, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 6, ""}}, {}, state,
    {"relin-key"}, {}, ""
  });
  state.level = 18;
  state.scale_bits = 56;
  plan.steps.push_back({
    OPR_DSLCKKSBOOTSTRAP, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 7, ""}},
    {{"attr.reason", "pre_relu_refresh"},
     {"attr.target_level", "18"}},
    state, {"bootstrap-key"}, {}, ""
  });
  plan.group_output_steps.push_back(8);
  plan.source_final_step_index = 8;

  VHO_FHE_CKKS2C_GROUP group;
  group.function_name = owner == 101 ? "open64_fhe_eval_root" :
                                      "open64_fhe_eval_callee";
  group.identity_sha256 = std::string(64, digest_digit);
  group.owner_pu_st = owner;
  group.source_value_id = owner + 100;
  group.context_pu_identity_id = owner + 200;
  group.context_callsite_id = owner == 101 ? 0 : 1;
  group.plan = plan;
  return group;
}

/* Populate a deterministic two-PU, two-group whole-program census. */
static void Initialize_Fixture()
{
  groups.clear();
  groups.push_back(Build_Group(101, 'b'));
  groups.push_back(Build_Group(102, 'c'));
  memset(&collected, 0, sizeof(collected));
  collected.pu_count = 2;
  collected.source_count = 2;
  collected.group_count = 2;
  collected.event_count = 18;
  collected.external_asset_count = 2;
  collected.key_use_count = 6;
  published = VHO_FHE_CKKS2C_PUBLISH_RESULT();
  compile_should_fail = false;
  expected_pus = 2;
  completion_count = 0;
  completion_committed = false;
}

/* Return one derived auxiliary path for endpoint and cleanup assertions. */
static std::string Artifact_Path(const std::string &binary,
                                 const char *suffix)
{
  std::string stem = binary;
  if (stem.size() > 2 && stem.compare(stem.size() - 2, 2, ".B") == 0)
    stem.erase(stem.size() - 2);
  return stem + suffix;
}

/* Remove only test-owned files before constructing one fresh scenario. */
static void Clean_Files(const std::string &binary)
{
  const char *suffixes[] = {".c", "-facade-manifest.txt", "-report.txt",
                            "-compile-command.txt"};
  remove(binary.c_str());
  remove((binary + ".tmp").c_str());
  for (size_t i = 0; i < sizeof(suffixes) / sizeof(suffixes[0]); ++i) {
    const std::string final = Artifact_Path(binary, suffixes[i]);
    remove(final.c_str());
    remove((final + ".tmp").c_str());
  }
}

/* Create the binary temporary marker after all auxiliary gates pass. */
static bool Write_Binary_Temporary()
{
  std::ofstream output((binary_final + ".tmp").c_str(),
                       std::ios::out | std::ios::binary | std::ios::trunc);
  output << "miniature two-PU CKKS checkpoint\n";
  output.close();
  return !!output;
}

/* Begin one transaction using the production generic lifecycle. */
static bool Begin()
{
  return VHO_FHE_Checkpoint_Register_Lifecycle(Finalize, Complete) &&
         VHO_FHE_Checkpoint_Begin((binary_final + ".tmp").c_str(),
                                  binary_final.c_str(),
                                  "CFHECKKS2C-CHECKPOINT", stderr);
}

/* Prove all auxiliaries precede the binary and remain after completion. */
static void Check_Success(const std::string &directory)
{
  Initialize_Fixture();
  binary_final = directory + "/success/fixture.ckks2c.B";
  mkdir((directory + "/success").c_str(), 0777);
  Clean_Files(binary_final);
  assert(Begin());
  assert(VHO_FHE_Checkpoint_Finalize(&expected_pus, stderr));
  assert(VHO_FHE_Checkpoint_Artifact_Count() == 4);
  assert(VHO_FHE_Checkpoint_Publish_Artifacts(stderr));
  assert(Write_Binary_Temporary());
  assert(VHO_FHE_Checkpoint_Publish_Binary(stderr));
  VHO_FHE_Checkpoint_Complete();
  assert(completion_count == 1 && completion_committed);
  assert(access(binary_final.c_str(), F_OK) == 0);
  assert(access(Artifact_Path(binary_final, ".c").c_str(), F_OK) == 0);
  assert(access(Artifact_Path(binary_final,
                              "-facade-manifest.txt").c_str(), F_OK) == 0);
  assert(access(Artifact_Path(binary_final, "-report.txt").c_str(), F_OK) == 0);
  assert(access(Artifact_Path(binary_final,
                              "-compile-command.txt").c_str(), F_OK) == 0);
}

/* Prove a stale final is preserved and no sibling output is created. */
static void Check_Stale_Final(const std::string &directory)
{
  Initialize_Fixture();
  binary_final = directory + "/stale/fixture.ckks2c.B";
  mkdir((directory + "/stale").c_str(), 0777);
  Clean_Files(binary_final);
  const std::string stale = Artifact_Path(binary_final, ".c");
  std::ofstream output(stale.c_str());
  output << "foreign\n";
  output.close();
  assert(Begin());
  assert(!VHO_FHE_Checkpoint_Finalize(&expected_pus, NULL));
  VHO_FHE_Checkpoint_Abort();
  std::ifstream input(stale.c_str());
  std::string line;
  getline(input, line);
  assert(line == "foreign" && completion_count == 1 &&
         !completion_committed);
  remove(stale.c_str());
}

/* Prove an unwritable/nonexistent destination leaves no valid output. */
static void Check_Write_Failure(const std::string &directory)
{
  Initialize_Fixture();
  binary_final = directory + "/missing/fixture.ckks2c.B";
  assert(Begin());
  assert(!VHO_FHE_Checkpoint_Finalize(&expected_pus, NULL));
  VHO_FHE_Checkpoint_Abort();
  assert(access(binary_final.c_str(), F_OK) != 0 &&
         access((binary_final + ".tmp").c_str(), F_OK) != 0 &&
         completion_count == 1 && !completion_committed);
}

/* Prove a failed generated-C compile gate rolls back every temporary. */
static void Check_Compile_Failure(const std::string &directory)
{
  Initialize_Fixture();
  compile_should_fail = true;
  binary_final = directory + "/compile-fail/fixture.ckks2c.B";
  mkdir((directory + "/compile-fail").c_str(), 0777);
  Clean_Files(binary_final);
  assert(Begin());
  assert(!VHO_FHE_Checkpoint_Finalize(&expected_pus, NULL));
  VHO_FHE_Checkpoint_Abort();
  assert(access(Artifact_Path(binary_final, ".c").c_str(), F_OK) != 0 &&
         access((Artifact_Path(binary_final, ".c") + ".tmp").c_str(),
                F_OK) != 0 &&
         completion_count == 1 && !completion_committed);
}

/* Prove incomplete all-PU coverage is rejected before artifact registration. */
static void Check_Missing_PU(const std::string &directory)
{
  Initialize_Fixture();
  expected_pus = 3;
  binary_final = directory + "/missing-pu/fixture.ckks2c.B";
  mkdir((directory + "/missing-pu").c_str(), 0777);
  Clean_Files(binary_final);
  assert(Begin());
  assert(!VHO_FHE_Checkpoint_Finalize(&expected_pus, NULL));
  assert(VHO_FHE_Checkpoint_Artifact_Count() == 0);
  VHO_FHE_Checkpoint_Abort();
  assert(completion_count == 1 && !completion_committed);
}

/* Model signal cleanup by aborting after finalization but before publication. */
static void Check_Abort_Cleanup(const std::string &directory)
{
  Initialize_Fixture();
  binary_final = directory + "/abort/fixture.ckks2c.B";
  mkdir((directory + "/abort").c_str(), 0777);
  Clean_Files(binary_final);
  assert(Begin());
  assert(VHO_FHE_Checkpoint_Finalize(&expected_pus, stderr));
  assert(access((Artifact_Path(binary_final, ".c") + ".tmp").c_str(),
                F_OK) == 0);
  VHO_FHE_Checkpoint_Abort();
  assert(access(Artifact_Path(binary_final, ".c").c_str(), F_OK) != 0 &&
         access((Artifact_Path(binary_final, ".c") + ".tmp").c_str(),
                F_OK) != 0 &&
         completion_count == 1 && !completion_committed);
}

/* Run every D3 transaction case using explicit compiler/include inputs. */
int main(int argc, char **argv)
{
  assert(argc == 4);
  const std::string directory = argv[1];
  compiler_path = argv[2];
  include_path = argv[3];
  mkdir(directory.c_str(), 0777);
  Check_Success(directory);
  Check_Stale_Final(directory);
  Check_Write_Failure(directory);
  Check_Compile_Failure(directory);
  Check_Missing_PU(directory);
  Check_Abort_Cleanup(directory);
  puts("FHE CKKS2C checkpoint publication passed: cases=6 artifacts=4");
  return 0;
}
