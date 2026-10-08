/*
 * Copyright (C) 2026 Open64 Project
 *
 * Convert the nineteen specialized ResNet common.relu definitions into the
 * pinned ACE bootstrap, normalization, polynomial, and reconstruction CKKS
 * circuit. Scalar bytes remain in a checkpoint-owned side file; WHIRL keeps
 * authenticated tensor references and explicit CKKS operations. See
 * doc/FHE-SYNC6-ACE-RELU-EXECUTION-RECIPE.md.
 */

#include "fhe_ckks_relu_materialize.h"

#include <float.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "config_fhe.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "dsl_tensor_fold.h"
#include "fhe_ckks_conv_expand.h"
#include "fhe_ckks_relu_event_plan.h"
#include "fhe_ckks_relu_recipe.h"
#include "fhe_ckks_source_events.h"
#include "fhe_image.h"
#include "fhe_plan.h"
#include "fhe_sha256.h"
#include "mtypes.h"
#include "strtab.h"
#include "symtab.h"
#include "targ_const.h"

namespace {

struct Scalar_Asset {
  std::string name;
  UINT64 offset;
  UINT64 length;
  std::string sha256;
  TY_IDX ty;
  TCON_IDX tcon;
  DSL_IR_VALUE_ID value_id;
  double value;
};

struct Relu_Job {
  VHO_FHE_CKKS_EVENT_IDENTITY event;
  DSL_FHE_CONTEXT_RANGE_RECORD range;
  DSL_FHE_CONTEXT_CKKS_STATE_RECORD refresh;
  DSL_IR_NODE_ID source_node_id;
  DSL_IR_VALUE_ID source_value_id;
  DSL_IR_VALUE_ID input_value_id;
  double bound;
  Scalar_Asset reciprocal;
  std::vector<Scalar_Asset> scalars;
  std::string identity_sha256;
  BOOL processed;
};

struct Relu_State {
  BOOL initialized;
  BOOL active;
  BOOL asset_registered;
  ST_IDX root_owner;
  VHO_FHE_RELU_RECIPE recipe;
  std::vector<Relu_Job> jobs;
  std::set<ST_IDX> processed_owners;
  std::string asset_final;
  std::string asset_temp;
  std::string asset_leaf;
  std::string report_final;
  std::string report_temp;
  UINT64 asset_bytes;
  UINT32 scalar_count;
  UINT32 operation_count;

  /* Begin each checkpoint with no borrowed image or pathname state. */
  Relu_State()
      : initialized(FALSE), active(FALSE), asset_registered(FALSE),
        root_owner(ST_IDX_ZERO), asset_bytes(0), scalar_count(0),
        operation_count(0) {}
};

Relu_State Relu_state;

/* Emit one stable FHE producer diagnostic. */
BOOL Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-RELU-MATERIALIZE-001: %s\n", message);
  return FALSE;
}

/* Compare jobs in physical owner/node order for deterministic mutation. */
struct Job_Less {
  bool operator()(const Relu_Job &left, const Relu_Job &right) const
  {
    if (left.event.owner_pu_st != right.event.owner_pu_st)
      return left.event.owner_pu_st < right.event.owner_pu_st;
    return left.source_node_id < right.source_node_id;
  }
};

/* Sort the six source events by their stable static ordinal. */
struct Event_Less {
  bool operator()(const VHO_FHE_CKKS_EVENT_IDENTITY &left,
                  const VHO_FHE_CKKS_EVENT_IDENTITY &right) const
  {
    return left.source_static_ordinal < right.source_static_ordinal;
  }
};

/* Return the basename retained in mapped side-file references. */
std::string Leaf_Name(const std::string &path)
{
  const std::string::size_type slash = path.find_last_of("/\\");
  return slash == std::string::npos ? path : path.substr(slash + 1);
}

/* Decode one scalar F4/F8 TCON from the active target tables. */
BOOL Scalar_TCON(TCON_IDX tcon_idx, double *value)
{
  if (value == NULL || tcon_idx == TCON_IDX_ZERO ||
      tcon_idx >= TCON_Table_Size())
    return FALSE;
  const TCON &tcon = Tcon_Table[tcon_idx];
  if (TCON_ty(tcon) != MTYPE_F4 && TCON_ty(tcon) != MTYPE_F8)
    return FALSE;
  const double decoded = Targ_To_Host_Float(tcon);
  if (decoded != decoded || decoded > DBL_MAX || decoded < -DBL_MAX)
    return FALSE;
  *value = decoded;
  return TRUE;
}

/* Decode canonical little-endian IEEE binary64 coefficient bytes. */
BOOL Decode_F64_Vector(TCON_IDX tcon_idx, UINT32 count,
                       std::vector<double> *values)
{
  DSL_TENSOR_TCON_RECORD record;
  const unsigned char *bytes = NULL;
  UINT32 length = 0;
  if (values == NULL || count == 0 ||
      !DSL_Tensor_TCON_Get(tcon_idx, &record) ||
      record.element_mtype != MTYPE_F8 || record.element_size != 8 ||
      record.element_count != count ||
      !DSL_Tensor_TCON_Get_Dense_Bytes(tcon_idx, &bytes, &length) ||
      length != count * 8)
    return FALSE;
  std::vector<double> decoded(count);
  for (UINT32 i = 0; i < count; ++i) {
    UINT64 bits = 0;
    for (UINT32 byte = 0; byte < 8; ++byte)
      bits |= UINT64(bytes[i * 8 + byte]) << (byte * 8);
    memcpy(&decoded[i], &bits, sizeof(bits));
    if (decoded[i] != decoded[i] ||
        decoded[i] > DBL_MAX || decoded[i] < -DBL_MAX)
      return FALSE;
  }
  values->swap(decoded);
  return TRUE;
}

/* Load the exact ordered coefficient vectors from one accepted profile. */
BOOL Load_Profile(DSL_FHE_COMPOSITE_PROFILE_ID profile_id,
                  VHO_FHE_RELU_RECIPE *recipe, FILE *diagnostic)
{
  std::vector<std::vector<double> > coefficients(3);
  static const UINT32 degrees[3] = {7, 15, 13};
  for (UINT32 ordinal = 0; ordinal < 3; ++ordinal) {
    DSL_FHE_APPROX_STAGE_RECORD stage;
    if (!DSL_FHE_Approx_Stage_Find(profile_id, ordinal, &stage) ||
        stage.degree != degrees[ordinal] ||
        !Decode_F64_Vector(stage.coefficient_tensor_tcon,
                           stage.degree + 1, &coefficients[ordinal]))
      return Report(diagnostic, "profile coefficient tensor is unavailable");
  }
  return VHO_FHE_CKKS_Build_Ace_Relu_Recipe(
      coefficients, recipe, diagnostic);
}

/* Read one exact logical operand from a live ReLU node. */
BOOL Operand(const DSL_IR_NODE_RECORD &node, DSL_IR_VALUE_ID *value_id)
{
  DSL_IR_VALUE_REFERENCE_RECORD reference;
  if (value_id == NULL || node.operand_count != 1 ||
      !DSL_IR_Image_Get_Value_Reference(
          node.first_operand_reference_id, &reference) ||
      reference.owner_node_id != node.id || reference.ordinal != 0 ||
      reference.value_id == DSL_IR_VALUE_INVALID_ID)
    return FALSE;
  *value_id = reference.value_id;
  return TRUE;
}

/* Locate one native definition while its owning PU is active. */
WN *Find_Definition(PU_Info *pu_info, WN *tree,
                    DSL_IR_VALUE_ID value_id)
{
  if (tree == NULL)
    return NULL;
  if (WN_operator(tree) == OPR_STID) {
    DSL_IR_VALUE_RECORD value;
    if (DSL_IR_Image_Find_Definition_Value(pu_info, tree, &value) &&
        value.id == value_id)
      return tree;
  }
  if (WN_operator(tree) == OPR_BLOCK) {
    for (WN *statement = WN_first(tree); statement != NULL;
         statement = WN_next(statement)) {
      WN *found = Find_Definition(pu_info, statement, value_id);
      if (found != NULL)
        return found;
    }
    return NULL;
  }
  for (INT kid = 0; kid < WN_kid_count(tree); ++kid) {
    WN *found = Find_Definition(pu_info, WN_kid(tree, kid), value_id);
    if (found != NULL)
      return found;
  }
  return NULL;
}

/* Match one context range to its six consecutive static source events. */
BOOL First_Event(
    const std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> &events,
    const DSL_FHE_CONTEXT_RANGE_RECORD &range,
    VHO_FHE_CKKS_EVENT_IDENTITY *first)
{
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> matches;
  for (size_t i = 0; i < events.size(); ++i)
    if (events[i].owner_pu_st == range.owner_pu_st &&
        events[i].source_value_id == range.source_relu_value_id &&
        events[i].context_pu_identity_id == range.context_pu_identity_id &&
        events[i].context_callsite_id == range.context_callsite_id)
      matches.push_back(events[i]);
  if (first == NULL || matches.size() != 6)
    return FALSE;
  std::sort(matches.begin(), matches.end(), Event_Less());
  for (UINT32 i = 1; i < 6; ++i)
    if (matches[i].source_static_ordinal !=
        matches[0].source_static_ordinal + i)
      return FALSE;
  *first = matches[0];
  return TRUE;
}

/* Build the exact nineteen-job census before source mutation. */
BOOL Initialize(FILE *diagnostic)
{
  if (Relu_state.initialized)
    return TRUE;
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> events;
  if (!VHO_FHE_CKKS_Collect_Source_Events(&events, diagnostic))
    return Report(diagnostic, "source event census cannot be collected");

  DSL_FHE_COMPOSITE_PROFILE_ID profile_id =
      DSL_FHE_COMPOSITE_PROFILE_INVALID_ID;
  std::vector<Relu_Job> jobs;
  std::set<std::pair<ST_IDX, DSL_IR_NODE_ID> > definitions;
  UINT32 roots = 0;
  for (UINT32 id = 1; id <= DSL_FHE_Context_Range_Count(); ++id) {
    DSL_FHE_CONTEXT_RANGE_RECORD range;
    DSL_FHE_CONTEXT_CKKS_STATE_RECORD refresh;
    DSL_IR_VALUE_RECORD result;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    Relu_Job job;
    if (!DSL_FHE_Context_Range_Get(id, &range) ||
        !DSL_FHE_Context_State_Find(
            range.owner_pu_st, range.source_relu_value_id,
            range.context_pu_identity_id, range.context_callsite_id,
            DSL_FHE_CONTEXT_STATE_ROLE_POST_REFRESH, 1, &refresh) ||
        !DSL_IR_Image_Get_Value(range.source_relu_value_id, &result) ||
        !DSL_IR_Image_Get_Node(result.producer_node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor(
            node.opcode_descriptor_id, &opcode) ||
        opcode.logical_operator != OPR_DSLRELU || opcode.version != 2 ||
        node.flags != DSL_IR_NODE_FLAG_NONE || node.operand_count != 1 ||
        node.result_value_id != result.id ||
        !Operand(node, &job.input_value_id) ||
        !First_Event(events, range, &job.event) ||
        !Scalar_TCON(range.positive_bound_tcon, &job.bound) ||
        job.bound <= 0.0 ||
        (refresh.level != 15 && refresh.level != 17 &&
         refresh.level != 18) || refresh.scale_bits != 56 ||
        refresh.component_count != 2 || refresh.precision_bits < 30 ||
        refresh.slot_count != 32768)
      return Report(diagnostic, "ReLU context identity or state is invalid");
    if (profile_id == DSL_FHE_COMPOSITE_PROFILE_INVALID_ID)
      profile_id = range.profile_id;
    else if (profile_id != range.profile_id)
      return Report(diagnostic, "multiple ReLU profiles are unsupported");
    if (range.context_callsite_id == 0) {
      ++roots;
      Relu_state.root_owner = range.owner_pu_st;
    }
    job.range = range;
    job.refresh = refresh;
    job.source_node_id = node.id;
    job.source_value_id = result.id;
    job.processed = FALSE;
    definitions.insert(std::make_pair(range.owner_pu_st, node.id));
    jobs.push_back(job);
  }
  if (jobs.size() != 19 || definitions.size() != 19 || roots != 1 ||
      !Load_Profile(profile_id, &Relu_state.recipe, diagnostic))
    return Report(diagnostic,
                  "ReLU census is not nineteen specialized definitions");
  std::sort(jobs.begin(), jobs.end(), Job_Less());
  Relu_state.jobs.swap(jobs);
  Relu_state.active = TRUE;
  Relu_state.initialized = TRUE;
  return TRUE;
}

/* Create the canonical one-element F64 external side-file tensor type. */
TY_IDX Scalar_Type()
{
  TY_TENSOR_CANONICAL_DESCRIPTOR descriptor;
  memset(&descriptor, 0, sizeof(descriptor));
  descriptor.kind = "tensor";
  descriptor.dtype = "float64";
  descriptor.rank = 1;
  descriptor.logical_shape = "[1]";
  descriptor.traits = "fhe.ckks.scalar";
  descriptor.layout = "dense";
  descriptor.sharding = "replicated";
  descriptor.placement = "side_file";
  descriptor.memory = "external_data";
  descriptor.quantization = "none";
  return TY_Intern_Tensor_Type(
      "fhe_ckks_scalar_f64", MTYPE_To_TY(MTYPE_F8), &descriptor);
}

/* Serialize one host double as canonical little-endian IEEE binary64. */
std::vector<unsigned char> Scalar_Bytes(double value)
{
  UINT64 bits = 0;
  memcpy(&bits, &value, sizeof(bits));
  std::vector<unsigned char> bytes(8);
  for (UINT32 i = 0; i < 8; ++i)
    bytes[i] = static_cast<unsigned char>((bits >> (i * 8)) & 0xff);
  return bytes;
}

/* Append one scalar to the temporary payload and create its side-file TCON. */
BOOL Write_Asset(FILE *file, const std::string &name, double value,
                 TY_IDX ty, UINT64 *offset, Scalar_Asset *asset,
                 FILE *diagnostic)
{
  if (file == NULL || ty == TY_IDX_ZERO || offset == NULL || asset == NULL)
    return Report(diagnostic, "scalar asset request is incomplete");
  const std::vector<unsigned char> bytes = Scalar_Bytes(value);
  const std::string digest = VHO_FHE_SHA256(&bytes[0], bytes.size());
  UINT64 checksum_hi = 0;
  UINT64 checksum_lo = 0;
  for (UINT32 i = 0; i < 16; ++i) {
    char pair[3] = {digest[i * 2], digest[i * 2 + 1], 0};
    const UINT64 byte = strtoul(pair, NULL, 16);
    if (i < 8)
      checksum_hi = (checksum_hi << 8) | byte;
    else
      checksum_lo = (checksum_lo << 8) | byte;
  }
  DSL_TENSOR_TCON_CREATE_INFO info;
  memset(&info, 0, sizeof(info));
  info.descriptor_ty = ty;
  info.element_mtype = MTYPE_F8;
  info.element_count = 1;
  info.logical_bytes = 8;
  info.required_alignment = 8;
  info.element_size = 8;
  info.dense_bytes = &bytes[0];
  info.dense_bytes_length = bytes.size();
  info.side_path = Relu_state.asset_leaf.c_str();
  info.side_path_length = Relu_state.asset_leaf.size();
  info.byte_offset = *offset;
  info.byte_length = bytes.size();
  info.checksum_hi = checksum_hi;
  info.checksum_lo = checksum_lo;
  TCON_IDX tcon = TCON_IDX_ZERO;
  if (!DSL_Tensor_TCON_Create_Side_File_Dense(&info, &tcon, NULL))
    return Report(diagnostic, "scalar side-file TCON was rejected");
  DSL_TENSOR_TCON_RECORD record;
  if (!DSL_Tensor_TCON_Get(tcon, &record) ||
      record.byte_offset != *offset || record.byte_length != bytes.size() ||
      fwrite(&bytes[0], 1, bytes.size(), file) != bytes.size())
    return Report(diagnostic, "scalar side-file write failed");
  asset->name = name;
  asset->offset = *offset;
  asset->length = bytes.size();
  asset->sha256 = digest;
  asset->ty = ty;
  asset->tcon = tcon;
  asset->value_id = DSL_IR_VALUE_INVALID_ID;
  asset->value = value;
  *offset += bytes.size();
  return TRUE;
}

/* Prepare every scalar byte range while the root owner is active. */
BOOL Prepare_Assets(PU_Info *pu_info, FILE *diagnostic)
{
  if (Relu_state.asset_registered)
    return TRUE;
  const char *output = VHO_FHE_Materialization_Checkpoint_Output;
  if (pu_info == NULL || PU_Info_proc_sym(pu_info) != Relu_state.root_owner ||
      output == NULL || output[0] == 0)
    return Report(diagnostic, "root checkpoint output is unavailable");
  Relu_state.asset_final = std::string(output) + ".relu-scalars.f64";
  Relu_state.asset_temp = Relu_state.asset_final + ".tmp";
  Relu_state.asset_leaf = Leaf_Name(Relu_state.asset_final);
  Relu_state.report_final = std::string(output) + ".relu-report.txt";
  Relu_state.report_temp = Relu_state.report_final + ".tmp";
  if (!VHO_FHE_Materialize_Checkpoint_Register_Artifact(
          Relu_state.asset_temp.c_str(), Relu_state.asset_final.c_str()))
    return Report(diagnostic, "ReLU scalar asset cannot join checkpoint");
  Relu_state.asset_registered = TRUE;
  FILE *file = fopen(Relu_state.asset_temp.c_str(), "wb");
  const TY_IDX ty = Scalar_Type();
  if (file == NULL || ty == TY_IDX_ZERO)
    return Report(diagnostic, "ReLU scalar asset cannot be created");
  UINT64 offset = 0;
  BOOL valid = TRUE;
  for (size_t job_index = 0;
       valid && job_index < Relu_state.jobs.size(); ++job_index) {
    Relu_Job &job = Relu_state.jobs[job_index];
    char name[128];
    snprintf(name, sizeof(name), "relu_s%u_reciprocal_bound",
             job.event.source_static_ordinal);
    valid = Write_Asset(file, name, 1.0 / job.bound, ty, &offset,
                        &job.reciprocal, diagnostic);
    job.scalars.resize(Relu_state.recipe.steps.size());
    for (UINT32 step = 0;
         valid && step < Relu_state.recipe.steps.size(); ++step) {
      const VHO_FHE_RELU_RECIPE_STEP &recipe_step =
          Relu_state.recipe.steps[step];
      if (recipe_step.operation != VHO_FHE_RELU_RECIPE_MUL_SCALAR &&
          recipe_step.operation != VHO_FHE_RELU_RECIPE_SCALAR_CONSTANT)
        continue;
      snprintf(name, sizeof(name), "relu_s%u_scalar_%u",
               job.event.source_static_ordinal, step);
      valid = Write_Asset(file, name, recipe_step.scalar, ty, &offset,
                          &job.scalars[step], diagnostic);
      if (valid)
        ++Relu_state.scalar_count;
    }
    char identity[256];
    snprintf(identity, sizeof(identity), "%u:%u:%u:%u:%u:%.17g",
             job.event.owner_pu_st, job.event.source_value_id,
             job.event.source_static_ordinal,
             job.event.context_pu_identity_id,
             job.event.context_callsite_id, job.bound);
    job.identity_sha256 = VHO_FHE_SHA256(
        reinterpret_cast<const unsigned char *>(identity), strlen(identity));
  }
  const BOOL closed = fclose(file) == 0;
  if (!valid || !closed)
    return Report(diagnostic, "ReLU scalar asset construction failed");
  Relu_state.asset_bytes = offset;
  return TRUE;
}

/* Materialize all source-free scalar values immediately before one ReLU. */
BOOL Materialize_Assets(PU_Info *pu_info, WN *definition, Relu_Job *job,
                        FILE *diagnostic)
{
  std::vector<Scalar_Asset *> assets;
  assets.push_back(&job->reciprocal);
  for (UINT32 step = 0; step < job->scalars.size(); ++step)
    if (job->scalars[step].tcon != TCON_IDX_ZERO)
      assets.push_back(&job->scalars[step]);
  std::vector<DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST> requests(
      assets.size());
  std::vector<DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT> results(
      assets.size());
  for (UINT32 i = 0; i < assets.size(); ++i) {
    DSL_IR_Generated_External_Tensor_Request_Init(&requests[i]);
    requests[i].insert_before = definition;
    requests[i].name = assets[i]->name.c_str();
    requests[i].descriptor_ty = assets[i]->ty;
    requests[i].tensor_tcon = assets[i]->tcon;
    requests[i].storage_format = "raw_f64_le";
    requests[i].side_file = Relu_state.asset_leaf.c_str();
    requests[i].tensor_key = assets[i]->name.c_str();
    requests[i].byte_offset = assets[i]->offset;
    requests[i].byte_length = assets[i]->length;
    requests[i].checksum_sha256 = assets[i]->sha256.c_str();
    requests[i].source_position = WN_Get_Linenum(definition);
    requests[i].generation_name = "fhe.ckks.relu.scalar";
    requests[i].generation_version = 1;
    requests[i].geometry_manifest_sha256 = job->identity_sha256.c_str();
    requests[i].stage_ordinal = i;
    requests[i].diagonal_ordinal = i;
    requests[i].variant_signature_sha256 = job->identity_sha256.c_str();
  }
  if (!DSL_IR_Materialize_Generated_External_Tensor_Values(
          pu_info, &requests[0], requests.size(), &results[0]))
    return Report(diagnostic, "generated ReLU scalar transaction failed");
  for (UINT32 i = 0; i < assets.size(); ++i)
    assets[i]->value_id = results[i].value_id;
  return TRUE;
}

/* Find the matching encoded-plaintext descriptor for a ciphertext config. */
BOOL Descriptor_Pair(
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID cipher_id,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *cipher,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *plain)
{
  if (cipher == NULL || plain == NULL ||
      !DSL_FHE_Get_Encryption_Descriptor(cipher_id, cipher) ||
      cipher->scheme != DSL_FHE_SCHEME_CKKS ||
      cipher->value_class != DSL_FHE_VALUE_CLASS_CIPHERTEXT)
    return FALSE;
  for (UINT32 id = 1; id <= DSL_FHE_Encryption_Descriptor_Count(); ++id) {
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD candidate;
    if (!DSL_FHE_Get_Encryption_Descriptor(id, &candidate))
      return FALSE;
    const BOOL empty_key = candidate.key_set_name == STR_IDX_ZERO ||
        (candidate.key_set_name < STR_Table_Size() &&
         Index_To_Str(candidate.key_set_name)[0] == 0);
    if (candidate.scheme == cipher->scheme &&
        candidate.value_class == DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT &&
        candidate.config_id == cipher->config_id && empty_key &&
        candidate.slot_count == cipher->slot_count) {
      *plain = candidate;
      return TRUE;
    }
  }
  return FALSE;
}

/* Add the mandatory pending-refresh state to the exact ReLU operand. */
BOOL Bind_Pending_Refresh(DSL_IR_VALUE_ID value_id, FILE *diagnostic)
{
  DSL_FHE_CKKS_VALUE_STATE_RECORD prior;
  if (!DSL_FHE_Plan_Find_Latest_CKKS_Value_State(value_id, &prior) ||
      prior.scheme != DSL_FHE_SCHEME_CKKS ||
      prior.value_class != DSL_FHE_VALUE_CLASS_CIPHERTEXT ||
      prior.pending_actions != 0)
    return Report(diagnostic, "ReLU operand has no concrete CKKS state");
  DSL_FHE_CKKS_VALUE_STATE_RECORD pending = prior;
  pending.id = DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
  pending.state_version = prior.state_version + 1;
  pending.pending_actions = DSL_FHE_CKKS_PENDING_BOOTSTRAP;
  pending.pending_bootstrap_reason =
      DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH;
  return DSL_FHE_Plan_Add_CKKS_Value_State(&pending) !=
      DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
}

/* Register bootstrap and relinearization requirements for one exact config. */
BOOL Register_Keys(const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD &cipher,
                   FILE *diagnostic)
{
  DSL_FHE_KEY_REQUIREMENT_RECORD key;
  DSL_FHE_Key_Requirement_Record_Init(&key);
  key.config_id = cipher.config_id;
  key.key_set_name = cipher.key_set_name;
  key.key_class = DSL_FHE_KEY_BOOTSTRAP;
  key.bootstrap_profile = Save_Str("pre_relu_refresh_v1");
  if (DSL_FHE_Intern_Key_Requirement(&key) ==
      DSL_FHE_KEY_REQUIREMENT_INVALID_ID)
    return Report(diagnostic, "bootstrap key requirement was rejected");
  DSL_FHE_Key_Requirement_Record_Init(&key);
  key.config_id = cipher.config_id;
  key.key_set_name = cipher.key_set_name;
  key.key_class = DSL_FHE_KEY_RELINEARIZATION;
  if (DSL_FHE_Intern_Key_Requirement(&key) ==
      DSL_FHE_KEY_REQUIREMENT_INVALID_ID)
    return Report(diagnostic, "relinearization key requirement was rejected");
  return TRUE;
}

/* Build and apply one complete six-group ReLU expansion. */
BOOL Expand_Job(PU_Info *pu_info, WN *definition, Relu_Job *job,
                FILE *diagnostic)
{
  DSL_IR_VALUE_RECORD source;
  DSL_IR_NODE_RECORD source_node;
  DSL_FHE_CKKS_VALUE_STATE_RECORD input;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD cipher;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD plain;
  if (!DSL_IR_Image_Get_Value(job->source_value_id, &source) ||
      !DSL_IR_Image_Get_Node(source.producer_node_id, &source_node) ||
      !Operand(source_node, &job->input_value_id) ||
      !DSL_FHE_Plan_Find_Latest_CKKS_Value_State(
          job->input_value_id, &input) ||
      !Descriptor_Pair(input.encryption_descriptor_id, &cipher, &plain) ||
      cipher.key_set_name == STR_IDX_ZERO ||
      !Register_Keys(cipher, diagnostic) ||
      !Bind_Pending_Refresh(job->input_value_id, diagnostic))
    return Report(diagnostic, "ReLU source state or key contract is invalid");

  VHO_FHE_CKKS_RELU_EVENT_POLICY policy;
  policy.source_static_ordinal = job->event.source_static_ordinal;
  policy.source_value_id = job->input_value_id;
  policy.result_ty = source.ty;
  policy.cipher_encryption_descriptor_id = cipher.id;
  policy.plaintext_encryption_descriptor_id = plain.id;
  policy.scheme = DSL_FHE_SCHEME_CKKS;
  policy.ciphertext_value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
  policy.plaintext_value_class =
      DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT;
  policy.post_refresh_level = job->refresh.level;
  policy.scale_bits = job->refresh.scale_bits;
  policy.component_count = job->refresh.component_count;
  policy.precision_bits = job->refresh.precision_bits;
  policy.slot_count = job->refresh.slot_count;
  policy.alignment_group = job->refresh.alignment_group;
  policy.pending_bootstrap_action = DSL_FHE_CKKS_PENDING_BOOTSTRAP;
  policy.pre_relu_bootstrap_reason =
      DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH;
  policy.encrypted_layout = Index_To_Str(job->refresh.encrypted_layout_name);
  policy.bootstrap_key_id = Index_To_Str(cipher.key_set_name);
  policy.relinearization_key_id = Index_To_Str(cipher.key_set_name);
  policy.reciprocal_bound_value_id = job->reciprocal.value_id;
  policy.reciprocal_bound_sha256 = job->reciprocal.sha256;
  policy.scalar_value_ids.resize(Relu_state.recipe.steps.size());
  policy.scalar_sha256.resize(Relu_state.recipe.steps.size());
  for (UINT32 i = 0; i < job->scalars.size(); ++i)
    if (job->scalars[i].tcon != TCON_IDX_ZERO) {
      policy.scalar_value_ids[i] = job->scalars[i].value_id;
      policy.scalar_sha256[i] = job->scalars[i].sha256;
    }

  VHO_FHE_CKKS_EVENT_PLAN plan;
  if (!VHO_FHE_CKKS_Build_Ace_Relu_Event_Plan(
          Relu_state.recipe, policy, &plan, diagnostic))
    return FALSE;
  VHO_FHE_CKKS_EXPANSION_SOURCE expansion;
  expansion.source_definition = definition;
  expansion.source_value_id = job->source_value_id;
  expansion.expected_source_operator = OPR_DSLRELU;
  expansion.expected_source_version = 2;
  expansion.context_pu_identity_id = job->event.context_pu_identity_id;
  expansion.context_callsite_id = job->event.context_callsite_id;
  expansion.origin_owner_pu_st = job->event.owner_pu_st;
  expansion.origin_source_value_id = job->event.source_value_id;
  expansion.encrypted_layout_name = job->refresh.encrypted_layout_name;
  std::vector<DSL_CKKS_EXPANSION_STEP_RESULT> results;
  if (!VHO_FHE_CKKS_Expand_Event(
          pu_info, plan, expansion, diagnostic, &results))
    return Report(diagnostic, "native ReLU expansion was rejected");
  Relu_state.operation_count += results.size();
  job->processed = TRUE;
  return TRUE;
}

/* Process every ReLU definition resident in the active PU. */
BOOL Process_Owner(PU_Info *pu_info, WN *tree, FILE *diagnostic)
{
  const ST_IDX owner = PU_Info_proc_sym(pu_info);
  if (Relu_state.processed_owners.find(owner) !=
      Relu_state.processed_owners.end())
    return Report(diagnostic, "ReLU owner callback was repeated");
  if (owner == Relu_state.root_owner && !Prepare_Assets(pu_info, diagnostic))
    return FALSE;
  if (!Relu_state.asset_registered)
    return Report(diagnostic, "ReLU scalar asset was not prepared first");
  for (size_t i = 0; i < Relu_state.jobs.size(); ++i) {
    Relu_Job &job = Relu_state.jobs[i];
    if (job.event.owner_pu_st != owner)
      continue;
    WN *definition = Find_Definition(pu_info, tree, job.source_value_id);
    if (definition == NULL ||
        !Materialize_Assets(pu_info, definition, &job, diagnostic) ||
        !Expand_Job(pu_info, definition, &job, diagnostic))
      return FALSE;
  }
  Relu_state.processed_owners.insert(owner);
  return TRUE;
}

}  // namespace

/* Validate and retain the exact source census before mutation begins. */
BOOL VHO_FHE_CKKS_Relu_Materialization_Gatekeeper(
    PU_Info *pu_info, WN *tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic)
{
  return pu_info != NULL && tree != NULL && options != NULL &&
         Initialize(diagnostic);
}

/* Expand all ReLUs owned by one active PU. */
BOOL VHO_FHE_CKKS_Relu_Materialization_Pass(
    PU_Info *pu_info, WN **tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic)
{
  if (pu_info == NULL || tree == NULL || *tree == NULL || options == NULL ||
      !Initialize(diagnostic))
    return FALSE;
  return !Relu_state.active || Process_Owner(pu_info, *tree, diagnostic);
}

/* Prove all nineteen sources became six-group explicit CKKS circuits. */
BOOL VHO_FHE_CKKS_Relu_Materialization_Finalize(FILE *diagnostic)
{
  if (!Relu_state.active)
    return TRUE;
  UINT32 processed = 0;
  UINT32 live = 0;
  UINT32 lowered = 0;
  for (size_t i = 0; i < Relu_state.jobs.size(); ++i)
    if (Relu_state.jobs[i].processed)
      ++processed;
  for (UINT32 id = 1; id <= DSL_IR_Image_Node_Count(); ++id) {
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    if (!DSL_IR_Image_Get_Node(id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor(
            node.opcode_descriptor_id, &opcode))
      return Report(diagnostic, "logical node census cannot be read");
    if (opcode.logical_operator != OPR_DSLRELU)
      continue;
    if ((node.flags & DSL_IR_NODE_FLAG_LOWERED) != 0)
      ++lowered;
    else if ((node.flags & (DSL_IR_NODE_FLAG_RETIRED |
                            DSL_IR_NODE_FLAG_DEAD_ELIDED)) == 0)
      ++live;
  }
  if (processed != 19 || Relu_state.scalar_count != 19 * 30 ||
      Relu_state.operation_count != 19 * 223 || live != 0 || lowered != 19 ||
      !DSL_CKKS_Event_Image_Validate(diagnostic) ||
      !DSL_FHE_Plan_Image_Validate(diagnostic))
    return Report(diagnostic,
                  "coverage is not 19 ReLUs, 570 scalars, and 4237 operations");

  FILE *report = fopen(Relu_state.report_temp.c_str(), "w");
  if (report == NULL)
    return Report(diagnostic, "ReLU report temp cannot be created");
  fprintf(report, "FHE SYNC-6 ReLU materialization report\n");
  fprintf(report, "contexts=19\n");
  fprintf(report, "source_groups=114\n");
  fprintf(report, "scalar_values=589\n");
  fprintf(report, "recipe_scalars=570\n");
  fprintf(report, "ckks_operations=4237\n");
  fprintf(report, "source_relu_nodes_live=0\n");
  fprintf(report, "post_refresh_level_consumption=12\n");
  fprintf(report, "side_asset=%s\n", Relu_state.asset_leaf.c_str());
  fprintf(report, "side_asset_bytes=%llu\n",
          static_cast<unsigned long long>(Relu_state.asset_bytes));
  const BOOL closed = fclose(report) == 0;
  if (!closed || !VHO_FHE_Materialize_Checkpoint_Register_Artifact(
          Relu_state.report_temp.c_str(), Relu_state.report_final.c_str()))
    return Report(diagnostic, "ReLU report cannot join checkpoint");
  fprintf(diagnostic,
          "FHE-SYNC6-RELU: contexts=19 groups=114 operations=4237 live=0\n");
  return TRUE;
}

/* Clear process-local producer state after commit or rollback. */
void VHO_FHE_CKKS_Relu_Materialization_Complete(BOOL committed)
{
  (void)committed;
  Relu_state = Relu_State();
}
