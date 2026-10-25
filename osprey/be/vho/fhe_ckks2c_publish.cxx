/*
 * Copyright (C) 2026 Open64 Project
 *
 * Transactional staging for provider-private CKKS2C artifacts.  Text is
 * fully prepared before file creation; temporary files are written with
 * exclusive creation and are published only by fhe_checkpoint.cxx.  The
 * binary WHIRL checkpoint remains the last-published commit marker.  See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D3.
 */

#include "fhe_ckks2c_publish.h"

#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#include <map>
#include <sstream>

#include "dsl_opcode.h"
#include "fhe_checkpoint.h"
#include "fhe_sha256.h"

namespace {

const uint32_t kArtifactCount = 4;

/* Emit one stable D3 diagnostic without modifying caller-owned results. */
bool Report(FILE *diagnostic, const char *message, const char *path)
{
  if (diagnostic != NULL) {
    fprintf(diagnostic, "CFHECKKS2C-003: %s", message);
    if (path != NULL)
      fprintf(diagnostic, ": %s", path);
    fputc('\n', diagnostic);
  }
  return false;
}

/* Strip only the conventional binary suffix so sibling names share a stem. */
std::string Output_Stem(const char *binary_final_path)
{
  if (binary_final_path == NULL || binary_final_path[0] == 0)
    return std::string();
  std::string stem(binary_final_path);
  if (stem.size() > 2 && stem.compare(stem.size() - 2, 2, ".B") == 0)
    stem.erase(stem.size() - 2);
  return stem;
}

/* Render a logical operator as the facade primitive it requires. */
const char *Primitive_Name(uint32_t logical_operator)
{
  switch (logical_operator) {
    case OPR_DSLCKKSENCODE: return "encode_asset";
    case OPR_DSLCKKSADD: return "add";
    case OPR_DSLCKKSSUB: return "sub";
    case OPR_DSLCKKSMUL: return "mul";
    case OPR_DSLCKKSROTATE: return "rotate";
    case OPR_DSLCKKSRESCALE: return "rescale";
    case OPR_DSLCKKSMODSWITCH: return "modswitch";
    case OPR_DSLCKKSRELIN: return "relin";
    case OPR_DSLCKKSBOOTSTRAP: return "bootstrap";
    default: return NULL;
  }
}

/* Return one portable leaf name for the relocatable compile receipt. */
std::string Basename(const std::string &value)
{
  const size_t slash = value.find_last_of('/');
  return slash == std::string::npos ? value : value.substr(slash + 1);
}

/* Reject an occupied or uninspectable endpoint before registering any pair. */
bool Endpoint_Available(const std::string &path, FILE *diagnostic)
{
  errno = 0;
  if (access(path.c_str(), F_OK) == 0 || errno != ENOENT)
    return Report(diagnostic,
                  "artifact endpoint already exists or cannot be inspected",
                  path.c_str());
  return true;
}

/* Write exact bytes to a newly created temporary file. */
bool Write_Exclusive(const std::string &path, const std::string &content,
                     FILE *diagnostic)
{
  const int descriptor = open(path.c_str(), O_WRONLY | O_CREAT | O_EXCL,
                              0666);
  if (descriptor < 0)
    return Report(diagnostic, "could not create temporary artifact",
                  path.c_str());
  size_t offset = 0;
  bool valid = true;
  while (offset < content.size()) {
    const ssize_t written = write(descriptor, content.data() + offset,
                                  content.size() - offset);
    if (written <= 0) {
      valid = false;
      break;
    }
    offset += static_cast<size_t>(written);
  }
  if (close(descriptor) != 0)
    valid = false;
  if (!valid) {
    remove(path.c_str());
    return Report(diagnostic, "could not finalize temporary artifact",
                  path.c_str());
  }
  return true;
}

/* Construct the facade capability manifest and its primitive census. */
std::string Facade_Manifest(
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    const VHO_FHE_CKKS2C_COLLECT_RESULT &collected,
    const VHO_FHE_CKKS2C_EMIT_RESULT &emitted,
    const std::string &private_c_sha256)
{
  std::map<uint32_t, uint32_t> operators;
  for (size_t group = 0; group < groups.size(); ++group)
    for (size_t step = 0; step < groups[group].plan.steps.size(); ++step)
      ++operators[groups[group].plan.steps[step].logical_operator];
  std::ostringstream text;
  text << "format=open64.ckks2c.facade-manifest.v1\n"
       << "facade_version=1\n"
       << "header=open64_fhe_ckks2c_facade.h\n"
       << "private_c_sha256=" << private_c_sha256 << "\n"
       << "pu_count=" << collected.pu_count << "\n"
       << "group_count=" << emitted.group_count << "\n"
       << "operation_count=" << emitted.operation_count << "\n"
       << "external_asset_count=" << collected.external_asset_count << "\n"
       << "key_use_count=" << collected.key_use_count << "\n"
       << "generated_c_file_io=none\n"
       << "generated_c_embedded_tensor_arrays=none\n";
  for (std::map<uint32_t, uint32_t>::const_iterator operation =
           operators.begin(); operation != operators.end(); ++operation) {
    const char *name = Primitive_Name(operation->first);
    text << "primitive." << (name == NULL ? "unknown" : name) << "="
         << operation->second << "\n";
  }
  return text.str();
}

/* Construct the complete group-to-source/context provenance report. */
std::string Group_Report(
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    const VHO_FHE_CKKS2C_COLLECT_RESULT &collected,
    const VHO_FHE_CKKS2C_EMIT_RESULT &emitted)
{
  std::ostringstream text;
  text << "format=open64.ckks2c.group-report.v1\n"
       << "pu_count=" << collected.pu_count << "\n"
       << "source_count=" << collected.source_count << "\n"
       << "group_count=" << collected.group_count << "\n"
       << "event_count=" << collected.event_count << "\n"
       << "external_asset_count=" << collected.external_asset_count << "\n"
       << "key_use_count=" << collected.key_use_count << "\n"
       << "emitted_group_count=" << emitted.group_count << "\n"
       << "emitted_operation_count=" << emitted.operation_count << "\n"
       << "emitted_source_value_count=" << emitted.source_value_count << "\n"
       << "emitted_bound_value_count=" << emitted.bound_value_count << "\n";
  for (size_t i = 0; i < groups.size(); ++i) {
    const VHO_FHE_CKKS2C_GROUP &group = groups[i];
    text << "group[" << i << "].identity_sha256=" << group.identity_sha256
         << "\n"
         << "group[" << i << "].function=" << group.function_name << "\n"
         << "group[" << i << "].owner_pu=" << group.owner_pu_st << "\n"
         << "group[" << i << "].source_value=" << group.source_value_id
         << "\n"
         << "group[" << i << "].context_identity="
         << group.context_pu_identity_id << "\n"
         << "group[" << i << "].callsite=" << group.context_callsite_id
         << "\n"
         << "group[" << i << "].event_count=" << group.plan.steps.size()
         << "\n";
  }
  return text.str();
}

/* Construct a relocatable command rather than embedding one host build path. */
std::string Compile_Command(const std::string &private_c_final_path)
{
  const std::string source = Basename(private_c_final_path);
  std::string object_path = source;
  if (object_path.size() > 2 &&
      object_path.compare(object_path.size() - 2, 2, ".c") == 0)
    object_path.erase(object_path.size() - 2);
  object_path += ".o";
  std::ostringstream text;
  text << "format=open64.ckks2c.compile-command.v1\n"
       << "language=c11\n"
       << "required_header=open64_fhe_ckks2c_facade.h\n"
       << "source_file=" << source << "\n"
       << "object_file=" << object_path << "\n"
       << "command=${CC:-cc} -std=c11 -Wall -Wextra -Werror "
       << "-I${OPEN64_ROOT}/osprey/include -c \"${CKKS2C_C}\" "
       << "-o \"${CKKS2C_OBJECT}\"\n";
  return text.str();
}

}  // namespace

/* Build, register, and stage one complete private CKKS2C artifact family. */
bool VHO_FHE_CKKS2C_Stage_Private_Artifacts(
    const char *binary_final_path, uint32_t expected_pu_count,
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    const VHO_FHE_CKKS2C_COLLECT_RESULT &collected,
    VHO_FHE_CKKS2C_PUBLISH_RESULT *result, FILE *diagnostic)
{
  if (result == NULL || expected_pu_count == 0 || groups.empty() ||
      collected.pu_count != expected_pu_count ||
      collected.group_count != groups.size() || collected.event_count == 0)
    return Report(diagnostic, "whole-program CKKS2C census is incomplete",
                  NULL);

  const std::string stem = Output_Stem(binary_final_path);
  if (stem.empty())
    return Report(diagnostic, "binary checkpoint path is absent", NULL);

  std::string private_c;
  VHO_FHE_CKKS2C_EMIT_RESULT emitted = {0, 0, 0, 0};
  if (!VHO_FHE_CKKS2C_Emit_Module("open64_fhe_cached_ckks_image", groups,
                                  &private_c, &emitted, diagnostic) ||
      emitted.group_count != collected.group_count ||
      emitted.operation_count != collected.event_count)
    return Report(diagnostic, "emitter census does not match collection",
                  NULL);

  VHO_FHE_CKKS2C_PUBLISH_RESULT candidate;
  candidate.collected = collected;
  candidate.emitted = emitted;
  candidate.private_c_sha256 = VHO_FHE_SHA256(
      reinterpret_cast<const unsigned char *>(private_c.data()),
      private_c.size());
  candidate.private_c_final_path = stem + ".c";
  candidate.facade_manifest_final_path = stem + "-facade-manifest.txt";
  candidate.group_report_final_path = stem + "-report.txt";
  candidate.compile_command_final_path = stem + "-compile-command.txt";
  candidate.private_c_temporary_path =
      candidate.private_c_final_path + ".tmp";
  candidate.facade_manifest_temporary_path =
      candidate.facade_manifest_final_path + ".tmp";
  candidate.group_report_temporary_path =
      candidate.group_report_final_path + ".tmp";
  candidate.compile_command_temporary_path =
      candidate.compile_command_final_path + ".tmp";

  const std::string temporary[kArtifactCount] = {
    candidate.private_c_temporary_path,
    candidate.facade_manifest_temporary_path,
    candidate.group_report_temporary_path,
    candidate.compile_command_temporary_path
  };
  const std::string final[kArtifactCount] = {
    candidate.private_c_final_path,
    candidate.facade_manifest_final_path,
    candidate.group_report_final_path,
    candidate.compile_command_final_path
  };
  for (uint32_t i = 0; i < kArtifactCount; ++i)
    if (!Endpoint_Available(temporary[i], diagnostic) ||
        !Endpoint_Available(final[i], diagnostic))
      return false;
  for (uint32_t i = 0; i < kArtifactCount; ++i)
    if (!VHO_FHE_Checkpoint_Register_Artifact(temporary[i].c_str(),
                                              final[i].c_str()))
      return Report(diagnostic, "could not register checkpoint artifact",
                    final[i].c_str());

  const std::string contents[kArtifactCount] = {
    private_c,
    Facade_Manifest(groups, collected, emitted,
                    candidate.private_c_sha256),
    Group_Report(groups, collected, emitted),
    Compile_Command(candidate.private_c_final_path)
  };
  for (uint32_t i = 0; i < kArtifactCount; ++i)
    if (!Write_Exclusive(temporary[i], contents[i], diagnostic))
      return false;

  *result = candidate;
  return true;
}
