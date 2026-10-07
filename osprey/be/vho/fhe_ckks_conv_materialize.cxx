/*
 * Copyright (C) 2026 Open64 Project
 *
 * Complete the fixed O0 column-first Conv conversion after PU specialization.
 * Folded SafeTensors values remain immutable inputs. This module authenticates
 * them, writes derived plaintext rows/bias/masks to a checkpoint-owned side
 * asset, and replaces each source Conv through the generic CKKS expansion
 * transaction.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#include "fhe_ckks_conv_materialize.h"

#include <math.h>
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
#include "fhe_ckks_conv_assets.h"
#include "fhe_ckks_conv_context.h"
#include "fhe_ckks_conv_expand.h"
#include "fhe_ckks_conv_plan.h"
#include "fhe_ckks_source_events.h"
#include "fhe_image.h"
#include "fhe_plan.h"
#include "fhe_sha256.h"
#include "mtypes.h"
#include "strtab.h"
#include "symtab.h"

namespace {

struct Plain_Asset {
  std::string name;
  UINT64 offset;
  UINT64 length;
  std::string sha256;
  TY_IDX ty;
  TCON_IDX tcon;
  DSL_IR_VALUE_ID value_id;
};

struct Conv_Job {
  VHO_FHE_CKKS_CONV_CONTEXT context;
  DSL_IR_VALUE_ID weight_source_value;
  DSL_IR_VALUE_ID bias_source_value;
  DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE weight_source_handle;
  DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE bias_source_handle;
  VHO_FHE_CKKS_CONV_RECIPE recipe;
  std::vector<Plain_Asset> rows;
  Plain_Asset bias;
  std::vector<Plain_Asset> masks;
  std::string geometry_sha256;
  std::string variant_sha256;
  TY_IDX high_resolution_ty;
  BOOL processed;
};

struct Conv_State {
  BOOL initialized;
  BOOL active;
  BOOL input_states_prepared;
  BOOL asset_registered;
  ST_IDX root_owner;
  std::string asset_final;
  std::string asset_temp;
  std::string asset_leaf;
  std::string report_final;
  std::string report_temp;
  UINT64 asset_bytes;
  UINT32 row_count;
  UINT32 bias_count;
  UINT32 mask_count;
  UINT32 operation_count;
  std::vector<Conv_Job> jobs;
  std::set<ST_IDX> processed_owners;
};

static Conv_State Conv_state;

/* Emit one stable Conv producer diagnostic without claiming publication. */
BOOL Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEMAT-CONV-001: %s\n", message);
  return FALSE;
}

/* Order contexts by the source program's deterministic static schedule. */
BOOL Context_Less(const VHO_FHE_CKKS_CONV_CONTEXT &left,
                  const VHO_FHE_CKKS_CONV_CONTEXT &right)
{
  return left.event.source_static_ordinal <
         right.event.source_static_ordinal;
}

/* Return only the final path component used by persisted side-file rows. */
std::string Leaf_Name(const std::string &path)
{
  const std::string::size_type slash = path.find_last_of("/\\");
  return slash == std::string::npos ? path : path.substr(slash + 1);
}

/* Append one little-endian integer to a stable digest preimage. */
void Append_U32(std::vector<unsigned char> *bytes, UINT32 value)
{
  for (UINT32 shift = 0; shift != 32; shift += 8)
    bytes->push_back(static_cast<unsigned char>(value >> shift));
}

/* Hash the exact source ordinal, geometry, stride, and folded tensor digests. */
std::string Context_Digest(
    const VHO_FHE_CKKS_CONV_CONTEXT &context,
    const std::string &weight_sha256, const std::string &bias_sha256)
{
  const VHO_FHE_CKKS_CONV_SHAPE &shape = context.high_resolution_shape;
  std::vector<unsigned char> bytes;
  const unsigned char magic[8] = {'F','H','E','C','T','X','T',1};
  bytes.insert(bytes.end(), magic, magic + sizeof(magic));
  Append_U32(&bytes, context.event.source_static_ordinal);
  Append_U32(&bytes, shape.input_channels);
  Append_U32(&bytes, shape.output_channels);
  Append_U32(&bytes, shape.height);
  Append_U32(&bytes, shape.width);
  Append_U32(&bytes, shape.kernel_height);
  Append_U32(&bytes, shape.kernel_width);
  Append_U32(&bytes, context.source_stride_height);
  Append_U32(&bytes, context.source_stride_width);
  Append_U32(&bytes, shape.pad_top);
  Append_U32(&bytes, shape.pad_left);
  Append_U32(&bytes, shape.slot_count);
  bytes.insert(bytes.end(), weight_sha256.begin(), weight_sha256.end());
  bytes.insert(bytes.end(), bias_sha256.begin(), bias_sha256.end());
  return VHO_FHE_SHA256(&bytes[0], bytes.size());
}

/* Locate a native definition by stable logical value in the active PU tree. */
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

/* Resolve a clone-local formal operand to the exact caller-owned actual. */
BOOL Resolve_External_Source(
    const VHO_FHE_CKKS_CONV_CONTEXT &context,
    DSL_IR_VALUE_ID operand_value, DSL_IR_VALUE_ID *source_value)
{
  if (source_value == NULL || operand_value == DSL_IR_VALUE_INVALID_ID)
    return FALSE;
  if (context.event.context_callsite_id == 0) {
    *source_value = operand_value;
    return TRUE;
  }
  DSL_PU_FORMAL_RECORD formal;
  BOOL found = FALSE;
  for (UINT32 id = 1; id <= DSL_PU_Interface_Image_Formal_Count(); ++id) {
    DSL_PU_FORMAL_RECORD candidate;
    if (!DSL_PU_Interface_Image_Get_Formal(id, &candidate))
      return FALSE;
    if (candidate.owner_pu_st == context.event.owner_pu_st &&
        candidate.formal_value_id == operand_value) {
      if (found)
        return FALSE;
      formal = candidate;
      found = TRUE;
    }
  }
  if (!found ||
      DSL_Call_ABI_Image_Callee_Formal_Count(
          context.event.owner_pu_st, formal.formal_ordinal) != 1)
    return FALSE;
  DSL_CALL_ARGUMENT_RECORD argument;
  if (!DSL_Call_ABI_Image_Get_Callee_Formal_Argument(
          context.event.owner_pu_st, formal.formal_ordinal, 0, &argument) ||
      argument.callsite_id != context.event.context_callsite_id ||
      argument.argument_value_id == DSL_IR_VALUE_INVALID_ID)
    return FALSE;
  *source_value = argument.argument_value_id;
  return TRUE;
}

/* Read and authenticate one exact SafeTensors data range as little-endian F32. */
BOOL Read_F32_Source(
    ST_IDX owner, DSL_IR_VALUE_ID value_id, TCON_IDX expected_tcon,
    std::vector<float> *values, DSL_IR_EXTERNAL_TENSOR_REFERENCE *reference,
    FILE *diagnostic)
{
  if (values == NULL || reference == NULL ||
      !DSL_IR_Image_Get_External_Tensor_Reference(
          owner, value_id, reference) ||
      reference->tensor_tcon != expected_tcon ||
      strcmp(reference->dtype, "float32") != 0 ||
      reference->byte_length == 0 || reference->byte_length % 4 != 0)
    return Report(diagnostic, "folded tensor reference is not exact F32");

  FILE *file = fopen(reference->side_file, "rb");
  unsigned char length_bytes[8];
  UINT64 header_length = 0;
  if (file == NULL ||
      fread(length_bytes, 1, sizeof(length_bytes), file) !=
          sizeof(length_bytes)) {
    if (file != NULL)
      fclose(file);
    return Report(diagnostic, "folded SafeTensors payload cannot be opened");
  }
  for (UINT32 i = 0; i < 8; ++i)
    header_length |= UINT64(length_bytes[i]) << (8 * i);
  if (header_length > 64 * 1024 * 1024ULL ||
      reference->byte_offset > ~UINT64(0) - header_length - 8 ||
      fseek(file, static_cast<long>(
          8 + header_length + reference->byte_offset), SEEK_SET) != 0) {
    fclose(file);
    return Report(diagnostic, "folded SafeTensors range is invalid");
  }
  std::vector<unsigned char> bytes(
      static_cast<size_t>(reference->byte_length));
  const BOOL read = fread(&bytes[0], 1, bytes.size(), file) == bytes.size();
  const BOOL closed = fclose(file) == 0;
  if (!read || !closed || VHO_FHE_SHA256(&bytes[0], bytes.size()) !=
                             reference->checksum)
    return Report(diagnostic, "folded tensor checksum does not match WHIRL");

  std::vector<float> decoded(bytes.size() / 4);
  for (size_t i = 0; i < decoded.size(); ++i) {
    const size_t offset = i * 4;
    UINT32 bits = UINT32(bytes[offset]) |
                  (UINT32(bytes[offset + 1]) << 8) |
                  (UINT32(bytes[offset + 2]) << 16) |
                  (UINT32(bytes[offset + 3]) << 24);
    memcpy(&decoded[i], &bits, sizeof(bits));
    if (!isfinite(decoded[i]))
      return Report(diagnostic, "folded tensor contains NaN or infinity");
  }
  values->swap(decoded);
  return TRUE;
}

/* Intern one canonical rank-one side-file F32 tensor type. */
TY_IDX Asset_Type(UINT32 elements)
{
  char name[80];
  char shape[48];
  snprintf(name, sizeof(name), "fhe_ckks_plain_f32_%u", elements);
  snprintf(shape, sizeof(shape), "[%u]", elements);
  TY_TENSOR_CANONICAL_DESCRIPTOR descriptor;
  memset(&descriptor, 0, sizeof(descriptor));
  descriptor.kind = "tensor";
  descriptor.dtype = "float32";
  descriptor.rank = 1;
  descriptor.logical_shape = shape;
  descriptor.traits = "fhe.ckks.plaintext_asset";
  descriptor.layout = "row_major";
  descriptor.sharding = "replicated";
  descriptor.placement = "side_file";
  descriptor.memory = "external_data";
  descriptor.quantization = "none";
  return TY_Intern_Tensor_Type(
      name, MTYPE_To_TY(MTYPE_F4), &descriptor);
}

/* Intern the high-resolution rank-four type used before stride compaction. */
TY_IDX High_Resolution_Type(
    const VHO_FHE_CKKS_CONV_CONTEXT &context)
{
  const VHO_FHE_CKKS_CONV_SHAPE &shape = context.high_resolution_shape;
  char name[96];
  char dimensions[96];
  snprintf(name, sizeof(name), "fhe_ckks_conv_high_%u_%u_%u",
           shape.output_channels, shape.height, shape.width);
  snprintf(dimensions, sizeof(dimensions), "[1,%u,%u,%u]",
           shape.output_channels, shape.height, shape.width);
  TY_TENSOR_CANONICAL_DESCRIPTOR descriptor;
  memset(&descriptor, 0, sizeof(descriptor));
  descriptor.kind = "tensor";
  descriptor.dtype = "float32";
  descriptor.rank = 4;
  descriptor.logical_shape = dimensions;
  descriptor.traits = "fhe.ckks.activation";
  descriptor.layout = "NCHW";
  descriptor.sharding = "replicated";
  descriptor.placement = "host";
  descriptor.memory = "dense";
  descriptor.quantization = "none";
  return TY_Intern_Tensor_Type(
      name, MTYPE_To_TY(MTYPE_F4), &descriptor);
}

/* Create one canonical side-file TCON and append its exact bytes to the temp. */
BOOL Write_Asset(
    FILE *file, const std::vector<unsigned char> &bytes,
    const std::string &name, TY_IDX ty, UINT64 *offset,
    Plain_Asset *asset, FILE *diagnostic)
{
  if (file == NULL || bytes.empty() || offset == NULL || asset == NULL ||
      ty == TY_IDX_ZERO)
    return Report(diagnostic, "plaintext asset request is incomplete");
  const std::string digest = VHO_FHE_SHA256(&bytes[0], bytes.size());
  UINT64 checksum_hi = 0;
  UINT64 checksum_lo = 0;
  for (UINT32 i = 0; i < 16; ++i) {
    char pair[3] = { digest[i * 2], digest[i * 2 + 1], 0 };
    const UINT64 byte = strtoul(pair, NULL, 16);
    if (i < 8)
      checksum_hi = (checksum_hi << 8) | byte;
    else
      checksum_lo = (checksum_lo << 8) | byte;
  }
  DSL_TENSOR_TCON_CREATE_INFO info;
  memset(&info, 0, sizeof(info));
  info.descriptor_ty = ty;
  info.element_mtype = MTYPE_F4;
  info.element_count = bytes.size() / sizeof(float);
  info.logical_bytes = bytes.size();
  info.required_alignment = TY_align(ty) < 4 ? 4 : TY_align(ty);
  info.element_size = sizeof(float);
  info.dense_bytes = &bytes[0];
  info.dense_bytes_length = bytes.size();
  info.side_path = Conv_state.asset_leaf.c_str();
  info.side_path_length = Conv_state.asset_leaf.size();
  info.byte_offset = *offset;
  info.byte_length = bytes.size();
  info.checksum_hi = checksum_hi;
  info.checksum_lo = checksum_lo;
  TCON_IDX tcon = TCON_IDX_ZERO;
  if (!DSL_Tensor_TCON_Create_Side_File_Dense(&info, &tcon, NULL))
    return Report(diagnostic, "plaintext side-file TCON or write failed");
  DSL_TENSOR_TCON_RECORD record;
  const char *side_path = NULL;
  UINT32 side_path_length = 0;
  if (!DSL_Tensor_TCON_Get(tcon, &record) ||
      !DSL_Tensor_TCON_Get_Side_Path(
          tcon, &side_path, &side_path_length) ||
      side_path_length != Conv_state.asset_leaf.size() ||
      memcmp(side_path, Conv_state.asset_leaf.data(), side_path_length) != 0 ||
      record.byte_length != bytes.size() ||
      record.byte_offset + record.byte_length < record.byte_offset ||
      record.byte_offset > *offset)
    return Report(diagnostic,
                  "canonical plaintext TCON is outside the side asset");
  if (record.byte_offset == *offset) {
    if (fwrite(&bytes[0], 1, bytes.size(), file) != bytes.size())
      return Report(diagnostic, "plaintext side-file write failed");
    *offset += bytes.size();
  } else if (record.byte_offset + record.byte_length > *offset) {
    return Report(diagnostic,
                  "canonical plaintext TCON range is not yet available");
  }
  asset->name = name;
  asset->offset = record.byte_offset;
  asset->length = record.byte_length;
  asset->sha256 = digest;
  asset->ty = ty;
  asset->tcon = tcon;
  asset->value_id = DSL_IR_VALUE_INVALID_ID;
  return TRUE;
}

/* Treat both the null string ID and an interned empty string as absent. */
BOOL String_Empty(STR_IDX value)
{
  return value == STR_IDX_ZERO ||
         (value < STR_Table_Size() && Index_To_Str(value)[0] == 0);
}

/* Find the key-free encoded-plaintext descriptor for one ciphertext config. */
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
    if (candidate.scheme == cipher->scheme &&
        candidate.value_class == DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT &&
        candidate.config_id == cipher->config_id &&
        String_Empty(candidate.key_set_name) &&
        candidate.slot_count == cipher->slot_count) {
      *plain = candidate;
      return TRUE;
    }
  }
  return FALSE;
}

/*
 * Resolve the current CKKS state for a Conv input. The only legal missing
 * state is the encrypted program input, whose initial state is derived from
 * its entry contract, descriptor, and compilation configuration. Intermediate
 * activation states must already have been established by context-sensitive
 * propagation or an earlier materialized CKKS operation.
 */
BOOL Find_Or_Create_Input_State(
    DSL_IR_VALUE_ID value_id, DSL_FHE_CKKS_VALUE_STATE_RECORD *state,
    FILE *diagnostic)
{
  if (state == NULL || value_id == DSL_IR_VALUE_INVALID_ID)
    return Report(diagnostic, "Conv input state request is invalid");
  if (DSL_FHE_Plan_Find_Latest_CKKS_Value_State(value_id, state))
    return TRUE;

  DSL_FHE_ENTRY_VALUE_RECORD entry_value;
  BOOL found = FALSE;
  for (UINT32 id = 1; id <= DSL_FHE_Entry_Value_Count(); ++id) {
    DSL_FHE_ENTRY_VALUE_RECORD candidate;
    if (!DSL_FHE_Get_Entry_Value(id, &candidate))
      return Report(diagnostic, "FHE entry value lookup failed");
    if (candidate.value_id != value_id ||
        candidate.role != DSL_FHE_ENTRY_VALUE_INPUT)
      continue;
    if (found)
      return Report(diagnostic, "Conv input has duplicate FHE entry values");
    entry_value = candidate;
    found = TRUE;
  }
  if (!found)
    return Report(diagnostic, "intermediate Conv input has no CKKS state");

  DSL_FHE_ENTRY_CONTRACT_RECORD entry_contract;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD descriptor;
  DSL_FHE_COMPILATION_CONFIG_RECORD config;
  if (!DSL_FHE_Get_Entry_Contract(
          entry_value.entry_contract_id, &entry_contract) ||
      entry_contract.owner_pu_st != Conv_state.root_owner ||
      !DSL_FHE_Get_Encryption_Descriptor(
          entry_value.encryption_descriptor_id, &descriptor) ||
      !DSL_FHE_Get_Compilation_Config(entry_contract.config_id, &config) ||
      descriptor.config_id != entry_contract.config_id ||
      descriptor.scheme != DSL_FHE_SCHEME_CKKS ||
      descriptor.value_class != DSL_FHE_VALUE_CLASS_CIPHERTEXT ||
      entry_value.value_class != DSL_FHE_VALUE_CLASS_CIPHERTEXT ||
      descriptor.key_set_name == STR_IDX_ZERO ||
      config.scheme != DSL_FHE_SCHEME_CKKS ||
      config.multiplicative_depth_policy != DSL_FHE_POLICY_EXPLICIT ||
      config.slot_count_policy != DSL_FHE_POLICY_EXPLICIT ||
      config.multiplicative_depth == 0 || config.scale_bits == 0 ||
      config.slot_count == 0)
    return Report(diagnostic,
                  "encrypted Conv entry input contract is incomplete");

  DSL_FHE_CKKS_Value_State_Record_Init(state);
  state->value_id = value_id;
  state->encryption_descriptor_id = descriptor.id;
  state->state_version = 1;
  state->scheme = DSL_FHE_SCHEME_CKKS;
  state->value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
  state->level = config.multiplicative_depth;
  state->scale_bits = config.scale_bits;
  state->component_count = 2;
  state->precision_bits = 30;
  state->slot_count = config.slot_count;
  state->alignment_group = 0;
  state->encrypted_layout_name = Save_Str("ckks.packed");
  state->pending_actions = DSL_FHE_CKKS_PENDING_NONE;
  state->pending_bootstrap_reason = DSL_FHE_BOOTSTRAP_REASON_NONE;
  const DSL_FHE_CKKS_VALUE_STATE_ID id =
      DSL_FHE_Plan_Add_CKKS_Value_State(state);
  if (id == DSL_FHE_CKKS_VALUE_STATE_INVALID_ID)
    return Report(diagnostic, "encrypted Conv entry input state was rejected");
  state->id = id;
  return TRUE;
}

/* Validate every Conv input state and descriptor pair before asset creation. */
BOOL Prepare_Input_States(FILE *diagnostic)
{
  if (Conv_state.input_states_prepared)
    return TRUE;
  std::set<DSL_IR_VALUE_ID> visited;
  for (size_t i = 0; i < Conv_state.jobs.size(); ++i) {
    const DSL_IR_VALUE_ID value_id = Conv_state.jobs[i].context.input_value_id;
    if (!visited.insert(value_id).second)
      continue;
    DSL_FHE_CKKS_VALUE_STATE_RECORD state;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD cipher;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD plain;
    if (!Find_Or_Create_Input_State(value_id, &state, diagnostic))
      return FALSE;
    if (!Descriptor_Pair(state.encryption_descriptor_id, &cipher, &plain)) {
      if (diagnostic != NULL)
        fprintf(diagnostic,
                "CFHEMAT-CONV-001: value=%u state=%u descriptor=%u "
                "scheme=%u class=%u\n",
                value_id, state.id, state.encryption_descriptor_id,
                state.scheme, state.value_class);
      return Report(diagnostic,
                    "Conv input has no compatible plaintext descriptor");
    }
  }
  Conv_state.input_states_prepared = TRUE;
  return TRUE;
}

/* Register all exact rotations and optional bootstrap used by one event plan. */
BOOL Register_Keys(
    const VHO_FHE_CKKS_EVENT_PLAN &plan,
    const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD &cipher,
    FILE *diagnostic)
{
  std::set<INT32> rotations;
  BOOL bootstrap = FALSE;
  for (size_t i = 0; i < plan.steps.size(); ++i) {
    rotations.insert(plan.steps[i].signed_rotations.begin(),
                     plan.steps[i].signed_rotations.end());
    if (plan.steps[i].logical_operator == OPR_DSLCKKSBOOTSTRAP)
      bootstrap = TRUE;
  }
  rotations.erase(0);
  for (std::set<INT32>::const_iterator rotation = rotations.begin();
       rotation != rotations.end(); ++rotation) {
    DSL_FHE_KEY_REQUIREMENT_RECORD key;
    DSL_FHE_Key_Requirement_Record_Init(&key);
    key.config_id = cipher.config_id;
    key.key_set_name = cipher.key_set_name;
    key.key_class = DSL_FHE_KEY_ROTATION;
    key.rotation_offset = *rotation;
    if (DSL_FHE_Intern_Key_Requirement(&key) ==
        DSL_FHE_KEY_REQUIREMENT_INVALID_ID)
      return Report(diagnostic, "rotation-key requirement was rejected");
  }
  if (bootstrap) {
    DSL_FHE_KEY_REQUIREMENT_RECORD key;
    DSL_FHE_Key_Requirement_Record_Init(&key);
    key.config_id = cipher.config_id;
    key.key_set_name = cipher.key_set_name;
    key.key_class = DSL_FHE_KEY_BOOTSTRAP;
    key.bootstrap_profile = Save_Str("pre_relu_refresh_v1");
    if (DSL_FHE_Intern_Key_Requirement(&key) ==
        DSL_FHE_KEY_REQUIREMENT_INVALID_ID)
      return Report(diagnostic, "bootstrap-key requirement was rejected");
  }
  return TRUE;
}

/* Mark one stride-two input as requiring the explicit depth refresh in plan. */
BOOL Bind_Depth_Refresh(
    DSL_IR_VALUE_ID value_id, INT32 target_level, FILE *diagnostic)
{
  DSL_FHE_CKKS_VALUE_STATE_RECORD prior;
  if (!DSL_FHE_Plan_Find_Latest_CKKS_Value_State(value_id, &prior) ||
      target_level <= prior.level)
    return Report(diagnostic, "stride-two input state cannot be refreshed");
  DSL_FHE_CKKS_VALUE_STATE_RECORD pending = prior;
  pending.id = DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
  pending.state_version = prior.state_version + 1;
  pending.pending_actions |= DSL_FHE_CKKS_PENDING_BOOTSTRAP;
  pending.pending_bootstrap_reason =
      DSL_FHE_BOOTSTRAP_REASON_DEPTH_EXHAUSTION;
  if (DSL_FHE_Plan_Add_CKKS_Value_State(&pending) ==
      DSL_FHE_CKKS_VALUE_STATE_INVALID_ID)
    return Report(diagnostic, "depth-refresh input state was rejected");
  return TRUE;
}

/* Prepare all authenticated plaintext bytes while the root PU is active. */
BOOL Prepare_Assets(PU_Info *pu_info, FILE *diagnostic)
{
  if (Conv_state.asset_registered)
    return TRUE;
  const char *output = VHO_FHE_Materialization_Checkpoint_Output;
  if (pu_info == NULL || PU_Info_proc_sym(pu_info) != Conv_state.root_owner ||
      output == NULL || output[0] == 0)
    return Report(diagnostic, "root checkpoint output is unavailable");
  Conv_state.asset_final = std::string(output) + ".conv-plaintexts.f32";
  Conv_state.asset_temp = Conv_state.asset_final + ".tmp";
  Conv_state.asset_leaf = Leaf_Name(Conv_state.asset_final);
  Conv_state.report_final = std::string(output) + ".conv-report.txt";
  Conv_state.report_temp = Conv_state.report_final + ".tmp";
  if (!VHO_FHE_Materialize_Checkpoint_Register_Artifact(
          Conv_state.asset_temp.c_str(), Conv_state.asset_final.c_str()))
    return Report(diagnostic, "Conv side asset cannot join checkpoint");
  Conv_state.asset_registered = TRUE;

  FILE *asset_file = fopen(Conv_state.asset_temp.c_str(), "wb");
  if (asset_file == NULL)
    return Report(diagnostic, "Conv side asset temp file cannot be created");
  UINT64 offset = 0;
  std::map<DSL_IR_VALUE_ID, DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE> handles;
  BOOL valid = TRUE;
  for (size_t job_index = 0;
       valid && job_index < Conv_state.jobs.size(); ++job_index) {
    Conv_Job &job = Conv_state.jobs[job_index];
    if (!Resolve_External_Source(
            job.context,
            job.context.source_weight_operand_value_id,
            &job.weight_source_value) ||
        !Resolve_External_Source(
            job.context,
            job.context.source_bias_operand_value_id,
            &job.bias_source_value)) {
      valid = Report(diagnostic, "specialized formal has no exact caller value");
      break;
    }
    DSL_IR_EXTERNAL_TENSOR_REFERENCE weight_reference;
    DSL_IR_EXTERNAL_TENSOR_REFERENCE bias_reference;
    std::vector<float> weights;
    std::vector<float> bias;
    if (!Read_F32_Source(
            Conv_state.root_owner, job.weight_source_value,
            job.context.folded_weight_tcon, &weights,
            &weight_reference, diagnostic) ||
        !Read_F32_Source(
            Conv_state.root_owner, job.bias_source_value,
            job.context.folded_bias_tcon, &bias,
            &bias_reference, diagnostic)) {
      valid = FALSE;
      break;
    }
    if (handles.find(job.weight_source_value) == handles.end()) {
      if (!DSL_IR_Capture_External_Tensor_Source(
              pu_info, job.weight_source_value,
              &handles[job.weight_source_value])) {
        valid = Report(diagnostic, "folded weight source capture failed");
        break;
      }
    }
    if (handles.find(job.bias_source_value) == handles.end()) {
      if (!DSL_IR_Capture_External_Tensor_Source(
              pu_info, job.bias_source_value,
              &handles[job.bias_source_value])) {
        valid = Report(diagnostic, "folded bias source capture failed");
        break;
      }
    }
    job.weight_source_handle = handles[job.weight_source_value];
    job.bias_source_handle = handles[job.bias_source_value];
    if (!VHO_FHE_CKKS_Build_Column_Conv_Recipe(
            job.context.high_resolution_shape,
            weights.empty() ? NULL : &weights[0], weights.size(),
            bias.empty() ? NULL : &bias[0], bias.size(),
            &job.recipe, diagnostic)) {
      valid = FALSE;
      break;
    }
    job.geometry_sha256 = Context_Digest(
        job.context, weight_reference.checksum, bias_reference.checksum);
    job.variant_sha256 = job.geometry_sha256;
    const UINT32 row_elements = job.recipe.active_output_slots;
    const TY_IDX row_ty = Asset_Type(row_elements);
    if (row_ty == TY_IDX_ZERO) {
      valid = Report(diagnostic, "feature-row tensor type cannot be interned");
      break;
    }
    job.rows.resize(size_t(job.recipe.shape.input_channels) *
                    job.recipe.shape.kernel_height *
                    job.recipe.shape.kernel_width);
    for (UINT32 row = 0; valid && row < job.rows.size(); ++row) {
      std::vector<unsigned char> bytes;
      char name[96];
      snprintf(name, sizeof(name), "conv_s%u_row_%u",
               job.context.event.source_static_ordinal, row);
      valid = VHO_FHE_CKKS_Build_Column_Conv_F32_Row_Bytes(
                  job.recipe, row, &bytes, diagnostic) &&
              Write_Asset(asset_file, bytes, name, row_ty, &offset,
                          &job.rows[row], diagnostic);
    }
    std::vector<unsigned char> bias_bytes;
    char bias_name[96];
    snprintf(bias_name, sizeof(bias_name), "conv_s%u_bias",
             job.context.event.source_static_ordinal);
    if (valid)
      valid = VHO_FHE_CKKS_Build_Conv_Expanded_Bias_F32(
                  job.recipe, &bias_bytes, diagnostic) &&
              Write_Asset(asset_file, bias_bytes, bias_name, row_ty, &offset,
                          &job.bias, diagnostic);
    if (valid && job.context.source_stride_height == 2) {
      std::vector<std::vector<unsigned char> > masks;
      valid = VHO_FHE_CKKS_Build_Stride_Compaction_F32_Masks(
          job.recipe.shape.width, job.recipe.shape.output_channels,
          job.recipe.shape.slot_count, &masks, diagnostic);
      const TY_IDX mask_ty = Asset_Type(job.recipe.shape.slot_count);
      if (mask_ty == TY_IDX_ZERO)
        valid = Report(diagnostic, "mask tensor type cannot be interned");
      job.masks.resize(masks.size());
      for (UINT32 mask = 0; valid && mask < masks.size(); ++mask) {
        char name[96];
        snprintf(name, sizeof(name), "conv_s%u_mask_%u",
                 job.context.event.source_static_ordinal, mask);
        valid = Write_Asset(asset_file, masks[mask], name, mask_ty, &offset,
                            &job.masks[mask], diagnostic);
      }
      job.high_resolution_ty = High_Resolution_Type(job.context);
      if (job.high_resolution_ty == TY_IDX_ZERO)
        valid = Report(diagnostic,
                       "stride-two high-resolution type cannot be interned");
    }
  }
  const BOOL closed = fclose(asset_file) == 0;
  if (!valid || !closed)
    return Report(diagnostic, "Conv side asset construction failed");
  Conv_state.asset_bytes = offset;
  return TRUE;
}

/* Initialize the exact post-specialization census without mutating the image. */
BOOL Initialize(PU_Info *pu_info, FILE *diagnostic)
{
  if (Conv_state.initialized)
    return TRUE;
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> events;
  std::vector<VHO_FHE_CKKS_CONV_CONTEXT> contexts;
  if (!VHO_FHE_CKKS_Collect_Source_Events(&events, diagnostic) ||
      !VHO_FHE_CKKS_Collect_Conv_Contexts(
          events, 32768, &contexts, diagnostic))
    return Report(diagnostic, "Conv context census cannot be collected");
  std::set<std::pair<ST_IDX, DSL_IR_NODE_ID> > definitions;
  UINT32 roots = 0;
  for (size_t i = 0; i < contexts.size(); ++i) {
    definitions.insert(std::make_pair(
        contexts[i].event.owner_pu_st, contexts[i].source_node_id));
    if (contexts[i].event.context_callsite_id == 0) {
      ++roots;
      Conv_state.root_owner = contexts[i].event.owner_pu_st;
    }
  }
  if (contexts.size() != 21 || roots != 1 ||
      (definitions.size() != 13 && definitions.size() != 21))
    return Report(diagnostic,
                  "Conv census is not 21 contexts over 13 or 21 definitions");
  Conv_state.initialized = TRUE;
  Conv_state.active = definitions.size() == 21;
  if (!Conv_state.active)
    return TRUE;
  if (pu_info == NULL || PU_Info_proc_sym(pu_info) != Conv_state.root_owner)
    return Report(diagnostic, "specialized Conv entry PU is not first");
  std::sort(contexts.begin(), contexts.end(), Context_Less);
  Conv_state.jobs.resize(contexts.size());
  for (size_t i = 0; i < contexts.size(); ++i) {
    Conv_state.jobs[i].context = contexts[i];
    Conv_state.jobs[i].weight_source_value = DSL_IR_VALUE_INVALID_ID;
    Conv_state.jobs[i].bias_source_value = DSL_IR_VALUE_INVALID_ID;
    Conv_state.jobs[i].weight_source_handle = 0;
    Conv_state.jobs[i].bias_source_handle = 0;
    Conv_state.jobs[i].high_resolution_ty = TY_IDX_ZERO;
    Conv_state.jobs[i].processed = FALSE;
  }
  return TRUE;
}

/* Populate one typed external-value request from a prepared asset. */
void Typed_Request(
    const Plain_Asset &asset, const Conv_Job &job,
    DSL_IR_VALUE_ID source_value,
    DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE source_handle,
    WN *definition, const char *transformation, UINT32 ordinal,
    DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST *request)
{
  DSL_IR_Typed_External_Tensor_Value_Request_Init(request);
  request->name = asset.name.c_str();
  request->source_owner_pu_st = Conv_state.root_owner;
  request->source_value_id = source_value;
  request->source_handle = source_handle;
  request->descriptor_ty = asset.ty;
  request->tensor_tcon = asset.tcon;
  request->insert_before = definition;
  request->source_position = WN_Get_Linenum(definition);
  request->storage_format = "raw_f32_le";
  request->side_file = Conv_state.asset_leaf.c_str();
  request->tensor_key = asset.name.c_str();
  request->byte_offset = asset.offset;
  request->byte_length = asset.length;
  request->checksum = asset.sha256.c_str();
  request->transformation_name = transformation;
  request->transformation_version = 1;
  request->transformation_ordinal = ordinal;
  (void)job;
}

/* Materialize prepared rows/bias/masks immediately before one source Conv. */
BOOL Materialize_Job_Assets(
    PU_Info *pu_info, WN *definition, Conv_Job *job, FILE *diagnostic)
{
  std::vector<DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST> row_requests(
      job->rows.size());
  std::vector<DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT> row_results(
      job->rows.size());
  for (UINT32 row = 0; row < job->rows.size(); ++row)
    Typed_Request(job->rows[row], *job, job->weight_source_value,
                  job->weight_source_handle, definition,
                  "fhe.conv_feature_row", row, &row_requests[row]);
  if (!VHO_FHE_CKKS_Materialize_Conv_Rows(
          pu_info, &job->recipe, &row_requests[0], row_requests.size(),
          &row_results[0], diagnostic))
    return FALSE;
  for (UINT32 row = 0; row < job->rows.size(); ++row)
    job->rows[row].value_id = row_results[row].value_id;

  DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST bias_request;
  DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT bias_result;
  Typed_Request(job->bias, *job, job->bias_source_value,
                job->bias_source_handle, definition,
                "fhe.conv_expanded_bias", 0, &bias_request);
  if (!VHO_FHE_CKKS_Materialize_Conv_Bias(
          pu_info, &job->recipe, &bias_request, &bias_result, diagnostic))
    return FALSE;
  job->bias.value_id = bias_result.value_id;

  if (!job->masks.empty()) {
    std::vector<DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST> requests(
        job->masks.size());
    std::vector<DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT> results(
        job->masks.size());
    for (UINT32 mask = 0; mask < job->masks.size(); ++mask) {
      DSL_IR_Generated_External_Tensor_Request_Init(&requests[mask]);
      requests[mask].insert_before = definition;
      requests[mask].name = job->masks[mask].name.c_str();
      requests[mask].descriptor_ty = job->masks[mask].ty;
      requests[mask].tensor_tcon = job->masks[mask].tcon;
      requests[mask].storage_format = "raw_f32_le";
      requests[mask].side_file = Conv_state.asset_leaf.c_str();
      requests[mask].tensor_key = job->masks[mask].name.c_str();
      requests[mask].byte_offset = job->masks[mask].offset;
      requests[mask].byte_length = job->masks[mask].length;
      requests[mask].checksum_sha256 = job->masks[mask].sha256.c_str();
      requests[mask].source_position = WN_Get_Linenum(definition);
      requests[mask].generation_name =
          "fhe.ckks.stride_compaction.mask";
      requests[mask].generation_version = 1;
      requests[mask].geometry_manifest_sha256 =
          job->geometry_sha256.c_str();
      requests[mask].stage_ordinal = mask;
      requests[mask].diagonal_ordinal = mask;
      requests[mask].variant_signature_sha256 =
          job->variant_sha256.c_str();
    }
    if (!VHO_FHE_CKKS_Materialize_Conv_Masks(
            pu_info, &requests[0], requests.size(),
            job->geometry_sha256.c_str(), job->variant_sha256.c_str(),
            &results[0], diagnostic))
      return FALSE;
    for (UINT32 mask = 0; mask < job->masks.size(); ++mask)
      job->masks[mask].value_id = results[mask].value_id;
  }
  return TRUE;
}

/* Build and execute one complete Conv plan after all plaintext values exist. */
BOOL Expand_Job(
    PU_Info *pu_info, WN *definition, Conv_Job *job, FILE *diagnostic)
{
  DSL_FHE_CKKS_VALUE_STATE_RECORD input;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD cipher;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD plain;
  if (!DSL_FHE_Plan_Find_Latest_CKKS_Value_State(
          job->context.input_value_id, &input))
    return Report(diagnostic, "Conv input CKKS state disappeared");
  if (!Descriptor_Pair(input.encryption_descriptor_id, &cipher, &plain))
    return Report(diagnostic,
                  "Conv input plaintext descriptor disappeared");
  if (input.encrypted_layout_name == STR_IDX_ZERO ||
      input.encrypted_layout_name >= STR_Table_Size() ||
      cipher.key_set_name == STR_IDX_ZERO ||
      cipher.key_set_name >= STR_Table_Size())
    return Report(diagnostic, "Conv input layout or key set is invalid");

  TY_IDX operation_ty = job->context.source_result_ty;
  if (job->context.source_stride_height == 2)
    operation_ty = job->high_resolution_ty;
  if (DSL_FHE_Intern_Tensor_Binding(
          operation_ty, cipher.id, 0) ==
          DSL_FHE_TENSOR_BINDING_INVALID_ID ||
      DSL_FHE_Intern_Tensor_Binding(
          operation_ty, plain.id, 0) ==
          DSL_FHE_TENSOR_BINDING_INVALID_ID ||
      DSL_FHE_Intern_Tensor_Binding(
          job->context.source_result_ty, cipher.id, 0) ==
          DSL_FHE_TENSOR_BINDING_INVALID_ID ||
      DSL_FHE_Intern_Tensor_Binding(
          job->context.source_result_ty, plain.id, 0) ==
          DSL_FHE_TENSOR_BINDING_INVALID_ID)
    return Report(diagnostic, "Conv result tensor bindings were rejected");

  VHO_FHE_CKKS_CONV_PLAN_POLICY policy;
  policy.source_static_ordinal = job->context.event.source_static_ordinal;
  policy.input_value_id = job->context.input_value_id;
  policy.result_ty = job->context.source_result_ty;
  policy.cipher_encryption_descriptor_id = cipher.id;
  policy.plaintext_encryption_descriptor_id = plain.id;
  policy.scheme = DSL_FHE_SCHEME_CKKS;
  policy.ciphertext_value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
  policy.plaintext_value_class = DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT;
  policy.input_level = input.level;
  policy.scale_bits = input.scale_bits;
  policy.component_count = input.component_count;
  policy.precision_bits = input.precision_bits;
  policy.slot_count = input.slot_count;
  policy.alignment_group = input.alignment_group;
  policy.rescale_pending_action = DSL_FHE_CKKS_PENDING_RESCALE;
  policy.encrypted_layout = Index_To_Str(input.encrypted_layout_name);
  policy.rotation_key_id = Index_To_Str(cipher.key_set_name);
  policy.capacity_refresh_target_level = 0;
  policy.capacity_refresh_reason.clear();
  policy.bootstrap_key_id.clear();
  for (size_t row = 0; row < job->rows.size(); ++row) {
    policy.row_value_ids.push_back(job->rows[row].value_id);
    policy.row_sha256.push_back(job->rows[row].sha256);
  }
  policy.bias_value_id = job->bias.value_id;
  policy.bias_sha256 = job->bias.sha256;

  VHO_FHE_CKKS_EVENT_PLAN plan;
  if (job->context.source_stride_height == 2) {
    const UINT32 width = job->recipe.shape.width;
    const UINT32 channels = job->recipe.shape.output_channels;
    UINT32 spatial_bits = 0;
    UINT32 channel_bits = 0;
    for (UINT32 value = width; value > 1; value >>= 1)
      ++spatial_bits;
    for (UINT32 value = channels; value > 1; value >>= 1)
      ++channel_bits;
    const INT32 target_level =
        static_cast<INT32>(2 * (spatial_bits - 1) + channel_bits + 5);
    if (!Bind_Depth_Refresh(
            job->context.input_value_id, target_level, diagnostic))
      return FALSE;
    policy.capacity_refresh_target_level = target_level;
    policy.capacity_refresh_reason = "DEPTH_EXHAUSTION";
    policy.bootstrap_key_id = Index_To_Str(cipher.key_set_name);
    VHO_FHE_CKKS_STRIDE_COMPACTION_POLICY compaction;
    compaction.input_width = width;
    compaction.output_channels = channels;
    compaction.high_resolution_ty = job->high_resolution_ty;
    for (size_t mask = 0; mask < job->masks.size(); ++mask) {
      compaction.mask_value_ids.push_back(job->masks[mask].value_id);
      compaction.mask_sha256.push_back(job->masks[mask].sha256);
    }
    if (!VHO_FHE_CKKS_Build_Stride_Two_Conv_Event_Plan(
            job->recipe, policy, compaction, &plan, diagnostic))
      return FALSE;
  } else if (!VHO_FHE_CKKS_Build_Stride_One_Conv_Event_Plan(
                 job->recipe, policy, &plan, diagnostic)) {
    return FALSE;
  }
  if (!Register_Keys(plan, cipher, diagnostic))
    return FALSE;

  VHO_FHE_CKKS_CONV_EXPANSION_SOURCE source;
  source.source_definition = definition;
  source.source_value_id = job->context.event.source_value_id;
  source.expected_source_operator = OPR_DSLCONV2D;
  source.expected_source_version = 2;
  source.context_pu_identity_id =
      job->context.event.context_pu_identity_id;
  source.context_callsite_id = job->context.event.context_callsite_id;
  source.origin_owner_pu_st = job->context.event.owner_pu_st;
  source.origin_source_value_id = job->context.event.source_value_id;
  source.encrypted_layout_name = input.encrypted_layout_name;
  std::vector<DSL_CKKS_EXPANSION_STEP_RESULT> results;
  if (!VHO_FHE_CKKS_Expand_Conv_Event(
          pu_info, plan, source, diagnostic, &results))
    return FALSE;
  Conv_state.operation_count += results.size();
  job->processed = TRUE;
  return TRUE;
}

/* Replace every specialized Conv definition owned by the active PU. */
BOOL Process_Owner(PU_Info *pu_info, WN *tree, FILE *diagnostic)
{
  const ST_IDX owner = PU_Info_proc_sym(pu_info);
  if (Conv_state.processed_owners.find(owner) !=
      Conv_state.processed_owners.end())
    return Report(diagnostic, "Conv owner callback was repeated");
  for (size_t i = 0; i < Conv_state.jobs.size(); ++i) {
    Conv_Job &job = Conv_state.jobs[i];
    if (job.context.event.owner_pu_st != owner)
      continue;
    WN *definition = Find_Definition(
        pu_info, tree, job.context.event.source_value_id);
    if (definition == NULL ||
        !Materialize_Job_Assets(pu_info, definition, &job, diagnostic) ||
        !Expand_Job(pu_info, definition, &job, diagnostic))
      return FALSE;
    Conv_state.row_count += job.rows.size();
    ++Conv_state.bias_count;
    Conv_state.mask_count += job.masks.size();
  }
  Conv_state.processed_owners.insert(owner);
  return TRUE;
}

}  // namespace

/* Admit only the exact post-specialization Conv census handled by this pass. */
BOOL
VHO_FHE_CKKS_Conv_Materialization_Gatekeeper(
    PU_Info *pu_info, WN *tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic)
{
  if (pu_info == NULL || tree == NULL || options == NULL ||
      !Initialize(pu_info, diagnostic))
    return FALSE;
  return TRUE;
}

/* Prepare shared assets/state and expand every Conv owned by the active PU. */
BOOL
VHO_FHE_CKKS_Conv_Materialization_Pass(
    PU_Info *pu_info, WN **tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic)
{
  if (pu_info == NULL || tree == NULL || *tree == NULL || options == NULL ||
      !Initialize(pu_info, diagnostic))
    return FALSE;
  if (!Conv_state.active)
    return TRUE;
  if (!Prepare_Input_States(diagnostic))
    return FALSE;
  if (!Conv_state.asset_registered &&
      !Prepare_Assets(pu_info, diagnostic))
    return FALSE;
  return Process_Owner(pu_info, *tree, diagnostic);
}

/* Verify whole-program Conv coverage and register the publication report. */
BOOL
VHO_FHE_CKKS_Conv_Materialization_Finalize(FILE *diagnostic)
{
  if (!Conv_state.active)
    return TRUE;
  UINT32 processed = 0;
  for (size_t i = 0; i < Conv_state.jobs.size(); ++i)
    if (Conv_state.jobs[i].processed)
      ++processed;
  if (processed != 21 || Conv_state.row_count != 5691 ||
      Conv_state.bias_count != 21 || Conv_state.mask_count != 104 ||
      !DSL_FHE_Image_Validate(diagnostic) ||
      !DSL_FHE_Plan_Image_Validate(diagnostic) ||
      !DSL_IR_Image_Validate(diagnostic))
    return Report(diagnostic,
                  "Conv coverage is not 21/5691/21/104 or images are invalid");

  FILE *report = fopen(Conv_state.report_temp.c_str(), "w");
  if (report == NULL)
    return Report(diagnostic, "Conv report temp file cannot be created");
  fprintf(report, "FHE SYNC-6 CKKS Conv materialization report\n");
  fprintf(report, "contexts=21\n");
  fprintf(report, "source_definitions=21\n");
  fprintf(report, "feature_rows=%u\n", Conv_state.row_count);
  fprintf(report, "expanded_biases=%u\n", Conv_state.bias_count);
  fprintf(report, "stride_masks=%u\n", Conv_state.mask_count);
  fprintf(report, "ckks_operations=%u\n", Conv_state.operation_count);
  fprintf(report, "side_asset=%s\n", Conv_state.asset_leaf.c_str());
  fprintf(report, "side_asset_bytes=%llu\n",
          static_cast<unsigned long long>(Conv_state.asset_bytes));
  const BOOL closed = fclose(report) == 0;
  if (!closed ||
      !VHO_FHE_Materialize_Checkpoint_Register_Artifact(
          Conv_state.report_temp.c_str(), Conv_state.report_final.c_str()))
    return Report(diagnostic, "Conv report cannot join checkpoint");
  fprintf(diagnostic,
          "FHE-SYNC6-CONV-MATERIALIZATION: contexts=21 rows=5691 "
          "biases=21 masks=104 operations=%u\n",
          Conv_state.operation_count);
  return TRUE;
}

/* Clear process-local producer state after successful commit or rollback. */
void
VHO_FHE_CKKS_Conv_Materialization_Complete(BOOL committed)
{
  (void)committed;
  Conv_state = Conv_State();
}
