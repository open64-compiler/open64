/*
 * Copyright (C) 2026 Open64 Project
 *
 * Convert the SecureResNet20 global-pool/flatten/classifier/output tail into
 * explicit provider-independent CKKS operations. Original SafeTensors bytes
 * remain immutable; derived plaintext masks live in a checkpoint-owned side
 * payload. Persistent IR edits use only reviewed common/com transactions.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md, C2-C8.
 */

#include "fhe_ckks_tail_materialize.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

#include <algorithm>
#include <set>
#include <string>
#include <vector>

#include "config_fhe.h"
#include "dsl_ir_image.h"
#include "dsl_ckks_event.h"
#include "dsl_opcode.h"
#include "dsl_tensor_fold.h"
#include "fhe_ckks_conv_expand.h"
#include "fhe_ckks_source_events.h"
#include "fhe_ckks_tail_plan.h"
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

  /* Keep each prepared asset invalid until bytes and TCON both validate. */
  Plain_Asset()
      : offset(0), length(0), ty(TY_IDX_ZERO), tcon(TCON_IDX_ZERO),
        value_id(DSL_IR_VALUE_INVALID_ID) {}
};

struct Tail_Job {
  ST_IDX owner;
  DSL_IR_NODE_ID pool_node;
  DSL_IR_VALUE_ID pool_input;
  DSL_IR_VALUE_ID pool_result;
  DSL_IR_NODE_ID flatten_node;
  DSL_IR_VALUE_ID flatten_result;
  DSL_IR_NODE_ID linear_node;
  DSL_IR_VALUE_ID linear_weight;
  DSL_IR_VALUE_ID linear_bias;
  DSL_IR_VALUE_ID linear_result;
  DSL_IR_NODE_ID output_node;
  DSL_IR_VALUE_ID output_result;
  VHO_FHE_CKKS_EVENT_IDENTITY pool_event;
  VHO_FHE_CKKS_EVENT_IDENTITY linear_event;
  VHO_FHE_CKKS_EVENT_IDENTITY final_callee_relu_event;
  DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE weight_handle;
  DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE bias_handle;
  Plain_Asset pool_mask;
  std::vector<Plain_Asset> weight_masks;
  Plain_Asset bias;
  BOOL processed;

  /* Start with no borrowed owner, image ID, or source handle. */
  Tail_Job()
      : owner(ST_IDX_ZERO), pool_node(0), pool_input(0), pool_result(0),
        flatten_node(0), flatten_result(0), linear_node(0), linear_weight(0),
        linear_bias(0), linear_result(0), output_node(0), output_result(0),
        weight_handle(0), bias_handle(0), processed(FALSE) {}
};

struct Tail_State {
  BOOL initialized;
  BOOL active;
  BOOL asset_registered;
  BOOL owner_processed;
  Tail_Job job;
  std::string asset_final;
  std::string asset_temp;
  std::string asset_leaf;
  std::string report_final;
  std::string report_temp;
  std::string geometry_sha256;
  UINT64 asset_bytes;
  UINT32 operation_count;

  /* Begin a compiler/checkpoint lifetime without retained mapped state. */
  Tail_State()
      : initialized(FALSE), active(FALSE), asset_registered(FALSE),
        owner_processed(FALSE), asset_bytes(0), operation_count(0) {}
};

Tail_State Tail_state;

/* Emit one stable producer diagnostic without publishing partial artifacts. */
BOOL Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-TAIL-MATERIALIZE-001: %s\n", message);
  return FALSE;
}

/* Return the basename persisted by side-file-backed tensor constants. */
std::string Leaf_Name(const std::string &path)
{
  const std::string::size_type slash = path.find_last_of("/\\");
  return slash == std::string::npos ? path : path.substr(slash + 1);
}

/* Read one exact logical operand from an executable DSL node. */
BOOL Operand(const DSL_IR_NODE_RECORD &node, UINT32 ordinal,
             DSL_IR_VALUE_ID *value_id)
{
  DSL_IR_VALUE_REFERENCE_RECORD reference;
  if (value_id == NULL || ordinal >= node.operand_count ||
      !DSL_IR_Image_Get_Value_Reference(
          node.first_operand_reference_id + ordinal, &reference) ||
      reference.owner_node_id != node.id || reference.ordinal != ordinal ||
      reference.value_id == DSL_IR_VALUE_INVALID_ID)
    return FALSE;
  *value_id = reference.value_id;
  return TRUE;
}

/* Resolve one node's logical operator through the managed descriptor table. */
BOOL Logical_Operator(DSL_IR_NODE_ID node_id, DSL_OPERATOR *logical,
                      UINT16 *version)
{
  DSL_IR_NODE_RECORD node;
  DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
  if (logical == NULL || version == NULL ||
      !DSL_IR_Image_Get_Node(node_id, &node) ||
      !DSL_IR_Image_Get_Opcode_Descriptor(
          node.opcode_descriptor_id, &opcode))
    return FALSE;
  *logical = static_cast<DSL_OPERATOR>(opcode.logical_operator);
  *version = static_cast<UINT16>(opcode.version);
  return TRUE;
}

/* Resolve one unique canonical string attribute from a managed node. */
BOOL Attribute(const DSL_IR_NODE_RECORD &node, const char *name,
               const char **value)
{
  if (name == NULL || value == NULL)
    return FALSE;
  *value = NULL;
  for (UINT32 i = 0; i < node.attribute_count; ++i) {
    DSL_IR_ATTRIBUTE_RECORD attribute;
    if (!DSL_IR_Image_Get_Attribute(node.first_attribute_id + i,
                                    &attribute) ||
        attribute.name == STR_IDX_ZERO)
      return FALSE;
    if (strcmp(Index_To_Str(attribute.name), name) == 0) {
      if (*value != NULL || attribute.value == STR_IDX_ZERO)
        return FALSE;
      *value = Index_To_Str(attribute.value);
    }
  }
  return *value != NULL;
}

/* Find the unique FHE disposition owner for one source node. */
BOOL Disposition_Owner(DSL_IR_NODE_ID node_id, ST_IDX *owner)
{
  BOOL found = FALSE;
  for (UINT32 id = 1;
       id <= DSL_FHE_Plan_Conversion_Disposition_Count(); ++id) {
    DSL_FHE_CONVERSION_DISPOSITION_RECORD record;
    if (!DSL_FHE_Plan_Get_Conversion_Disposition(id, &record))
      return FALSE;
    if (record.source_node_id != node_id)
      continue;
    if (found || record.owner_pu_st == ST_IDX_ZERO)
      return FALSE;
    *owner = record.owner_pu_st;
    found = TRUE;
  }
  return found;
}

/* Locate the source schedule identity for one exact root-owned result. */
BOOL Find_Event(
    const std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> &events,
    ST_IDX owner, DSL_IR_VALUE_ID value_id,
    VHO_FHE_CKKS_EVENT_IDENTITY *event)
{
  BOOL found = FALSE;
  for (size_t i = 0; i < events.size(); ++i) {
    if (events[i].owner_pu_st != owner ||
        events[i].source_value_id != value_id ||
        events[i].context_callsite_id != 0)
      continue;
    if (found)
      return FALSE;
    *event = events[i];
    found = TRUE;
  }
  return found;
}

/* Resolve the called block result to its final source ReLU while the source
 * schedule is still pristine. Later CKKS nodes must never be recensused as
 * source-domain operations. */
BOOL Find_Final_Callee_Relu_Event(
    const Tail_Job &job,
    const std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> &events,
    VHO_FHE_CKKS_EVENT_IDENTITY *event, FILE *diagnostic)
{
  DSL_RUNTIME_CALL_PROJECTION_RECORD projection;
  BOOL found_projection = FALSE;
  for (UINT32 id = 1; id <= DSL_Runtime_Interface_Image_Call_Count(); ++id) {
    DSL_RUNTIME_CALL_PROJECTION_RECORD candidate;
    if (!DSL_Runtime_Interface_Image_Get_Call(id, &candidate))
      return Report(diagnostic, "runtime call projection cannot be read");
    if (candidate.owner_pu_st != job.owner ||
        candidate.source_value_id != job.pool_input ||
        candidate.direction != DSL_RUNTIME_CALL_RESULT)
      continue;
    if (found_projection)
      return Report(diagnostic, "pool input has duplicate call results");
    projection = candidate;
    found_projection = TRUE;
  }
  DSL_CALLSITE_METADATA_RECORD callsite;
  if (!found_projection ||
      !DSL_Call_Image_Get_Callsite(projection.callsite_id, &callsite) ||
      callsite.owner_pu_st != job.owner)
    return Report(diagnostic, "pool input call-result route is missing");

  BOOL found_event = FALSE;
  VHO_FHE_CKKS_EVENT_IDENTITY final_event;
  for (size_t i = 0; i < events.size(); ++i) {
    if (events[i].owner_pu_st != callsite.callee_pu_st ||
        events[i].context_callsite_id != callsite.id)
      continue;
    if (!found_event ||
        events[i].source_static_ordinal > final_event.source_static_ordinal) {
      final_event = events[i];
      found_event = TRUE;
    }
  }
  DSL_IR_VALUE_RECORD final_value;
  DSL_OPERATOR final_operator = OPR_DSLUNKNOWN;
  UINT16 final_version = 0;
  if (!found_event || event == NULL ||
      !DSL_IR_Image_Get_Value(final_event.source_value_id, &final_value) ||
      !Logical_Operator(
          final_value.producer_node_id, &final_operator, &final_version) ||
      final_operator != OPR_DSLRELU || final_version != 2)
    return Report(diagnostic, "callee final scheduled result is not ReLU");
  *event = final_event;
  return TRUE;
}

/* Capture the exact four-node tail before any native expansion mutates it. */
BOOL Initialize(FILE *diagnostic)
{
  if (Tail_state.initialized)
    return TRUE;
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> events;
  if (!VHO_FHE_CKKS_Collect_Source_Events(&events, diagnostic))
    return Report(diagnostic, "source event census cannot be collected");

  Tail_Job job;
  UINT32 pool_count = 0;
  UINT32 flatten_count = 0;
  UINT32 linear_count = 0;
  UINT32 output_count = 0;
  for (UINT32 id = 1; id <= DSL_IR_Image_Node_Count(); ++id) {
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    if (!DSL_IR_Image_Get_Node(id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor(
            node.opcode_descriptor_id, &opcode))
      return Report(diagnostic, "tail node census cannot be read");
    if (node.flags != DSL_IR_NODE_FLAG_NONE)
      continue;
    if (opcode.logical_operator == OPR_DSLGLOBALAVGPOOL2D) {
      if (++pool_count != 1 || opcode.version != 2 ||
          node.operand_count != 1 ||
          !Operand(node, 0, &job.pool_input))
        return Report(diagnostic, "global-average-pool contract is invalid");
      job.pool_node = node.id;
      job.pool_result = node.result_value_id;
    } else if (opcode.logical_operator == OPR_DSLFLATTEN) {
      DSL_IR_VALUE_ID input;
      if (++flatten_count != 1 || opcode.version != 2 ||
          node.operand_count != 1 || !Operand(node, 0, &input))
        return Report(diagnostic, "flatten contract is invalid");
      job.flatten_node = node.id;
      job.flatten_result = node.result_value_id;
      if (job.pool_result != 0 && input != job.pool_result)
        return Report(diagnostic, "flatten does not consume pool result");
    } else if (opcode.logical_operator == OPR_DSLLINEAR) {
      DSL_IR_VALUE_ID input;
      if (++linear_count != 1 || opcode.version != 2 ||
          node.operand_count != 3 || !Operand(node, 0, &input) ||
          !Operand(node, 1, &job.linear_weight) ||
          !Operand(node, 2, &job.linear_bias))
        return Report(diagnostic, "linear contract is invalid");
      job.linear_node = node.id;
      job.linear_result = node.result_value_id;
      if (job.flatten_result != 0 && input != job.flatten_result)
        return Report(diagnostic, "linear does not consume flatten result");
    } else if (opcode.logical_operator == OPR_DSLOUTPUTLOGITS) {
      DSL_IR_VALUE_ID input;
      if (++output_count != 1 || opcode.version != 2 ||
          node.operand_count != 1 || !Operand(node, 0, &input))
        return Report(diagnostic, "output-logits contract is invalid");
      job.output_node = node.id;
      job.output_result = node.result_value_id;
      if (job.linear_result != 0 && input != job.linear_result)
        return Report(diagnostic, "output marker does not consume linear");
    }
  }
  ST_IDX owners[4] = {ST_IDX_ZERO, ST_IDX_ZERO, ST_IDX_ZERO, ST_IDX_ZERO};
  if (pool_count != 1 || flatten_count != 1 || linear_count != 1 ||
      output_count != 1 ||
      !Disposition_Owner(job.pool_node, &owners[0]) ||
      !Disposition_Owner(job.flatten_node, &owners[1]) ||
      !Disposition_Owner(job.linear_node, &owners[2]) ||
      !Disposition_Owner(job.output_node, &owners[3]) ||
      owners[0] != owners[1] || owners[0] != owners[2] ||
      owners[0] != owners[3] ||
      !Find_Event(events, owners[0], job.pool_result, &job.pool_event) ||
      !Find_Event(events, owners[0], job.linear_result, &job.linear_event))
    return Report(diagnostic, "tail ownership or source schedule is incomplete");
  job.owner = owners[0];
  if (!Find_Final_Callee_Relu_Event(
          job, events, &job.final_callee_relu_event, diagnostic))
    return FALSE;
  Tail_state.job = job;
  Tail_state.active = TRUE;
  Tail_state.initialized = TRUE;
  return TRUE;
}

/* Locate one native STID definition by its stable logical value identity. */
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

/* Intern the one canonical packed-slot F32 plaintext asset type. */
TY_IDX Asset_Type()
{
  TY_TENSOR_CANONICAL_DESCRIPTOR descriptor;
  memset(&descriptor, 0, sizeof(descriptor));
  descriptor.kind = "tensor";
  descriptor.dtype = "float32";
  descriptor.rank = 1;
  descriptor.logical_shape = "[32768]";
  descriptor.traits = "fhe.ckks.tail.plaintext";
  descriptor.layout = "packed_slots";
  descriptor.sharding = "replicated";
  descriptor.placement = "side_file";
  descriptor.memory = "external_data";
  descriptor.quantization = "none";
  return TY_Intern_Tensor_Type(
      "fhe_ckks_tail_plain_f32_32768", MTYPE_To_TY(MTYPE_F4),
      &descriptor);
}

/* Encode host F32 values as canonical little-endian bytes. */
std::vector<unsigned char> F32_Bytes(const std::vector<float> &values)
{
  std::vector<unsigned char> bytes(values.size() * 4);
  for (size_t i = 0; i < values.size(); ++i) {
    UINT32 bits = 0;
    memcpy(&bits, &values[i], sizeof(bits));
    bytes[i * 4] = static_cast<unsigned char>(bits & 0xff);
    bytes[i * 4 + 1] = static_cast<unsigned char>((bits >> 8) & 0xff);
    bytes[i * 4 + 2] = static_cast<unsigned char>((bits >> 16) & 0xff);
    bytes[i * 4 + 3] = static_cast<unsigned char>((bits >> 24) & 0xff);
  }
  return bytes;
}

/* Authenticate and decode one exact F32 range from the captured SafeTensors
 * payload. The source file and its WHIRL reference remain unchanged. */
BOOL Read_F32_Source(
    ST_IDX owner, DSL_IR_VALUE_ID value_id, std::vector<float> *values,
    DSL_IR_EXTERNAL_TENSOR_REFERENCE *reference, FILE *diagnostic)
{
  if (values == NULL || reference == NULL ||
      !DSL_IR_Image_Get_External_Tensor_Reference(
          owner, value_id, reference) ||
      reference->tensor_tcon == TCON_IDX_ZERO ||
      reference->side_file == NULL || reference->side_file[0] == 0 ||
      reference->checksum == NULL || reference->checksum[0] == 0 ||
      strcmp(reference->dtype, "float32") != 0 ||
      reference->byte_length == 0 || reference->byte_length % 4 != 0)
    return Report(diagnostic, "classifier source is not exact external F32");

  FILE *file = fopen(reference->side_file, "rb");
  unsigned char length_bytes[8];
  UINT64 header_length = 0;
  if (file == NULL ||
      fread(length_bytes, 1, sizeof(length_bytes), file) !=
          sizeof(length_bytes)) {
    if (file != NULL)
      fclose(file);
    return Report(diagnostic, "classifier SafeTensors payload cannot open");
  }
  for (UINT32 i = 0; i < 8; ++i)
    header_length |= UINT64(length_bytes[i]) << (8 * i);
  if (header_length > 64 * 1024 * 1024ULL ||
      reference->byte_offset > ~UINT64(0) - header_length - 8 ||
      fseek(file, static_cast<long>(
          8 + header_length + reference->byte_offset), SEEK_SET) != 0) {
    fclose(file);
    return Report(diagnostic, "classifier SafeTensors range is invalid");
  }
  std::vector<unsigned char> bytes(
      static_cast<size_t>(reference->byte_length));
  const BOOL read = fread(&bytes[0], 1, bytes.size(), file) == bytes.size();
  const BOOL closed = fclose(file) == 0;
  if (!read || !closed ||
      VHO_FHE_SHA256(&bytes[0], bytes.size()) != reference->checksum)
    return Report(diagnostic, "classifier source checksum mismatch");

  std::vector<float> decoded(bytes.size() / 4);
  for (size_t i = 0; i < decoded.size(); ++i) {
    const size_t offset = i * 4;
    UINT32 bits = UINT32(bytes[offset]) |
                  (UINT32(bytes[offset + 1]) << 8) |
                  (UINT32(bytes[offset + 2]) << 16) |
                  (UINT32(bytes[offset + 3]) << 24);
    memcpy(&decoded[i], &bits, sizeof(bits));
    if (!isfinite(decoded[i]))
      return Report(diagnostic, "classifier source contains nonfinite data");
  }
  values->swap(decoded);
  return TRUE;
}

/* Create one canonical side-file TCON and append exact bytes once. */
BOOL Write_Asset(
    FILE *file, const std::vector<float> &values, const std::string &name,
    TY_IDX ty, UINT64 *offset, Plain_Asset *asset, FILE *diagnostic)
{
  if (file == NULL || values.size() != 32768 || name.empty() ||
      ty == TY_IDX_ZERO || offset == NULL || asset == NULL)
    return Report(diagnostic, "tail plaintext asset request is incomplete");
  const std::vector<unsigned char> bytes = F32_Bytes(values);
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
  info.element_mtype = MTYPE_F4;
  info.element_count = values.size();
  info.logical_bytes = bytes.size();
  info.required_alignment = 4;
  info.element_size = 4;
  info.dense_bytes = &bytes[0];
  info.dense_bytes_length = bytes.size();
  info.side_path = Tail_state.asset_leaf.c_str();
  info.side_path_length = Tail_state.asset_leaf.size();
  info.byte_offset = *offset;
  info.byte_length = bytes.size();
  info.checksum_hi = checksum_hi;
  info.checksum_lo = checksum_lo;
  TCON_IDX tcon = TCON_IDX_ZERO;
  if (!DSL_Tensor_TCON_Create_Side_File_Dense(&info, &tcon, NULL))
    return Report(diagnostic, "tail plaintext TCON was rejected");
  DSL_TENSOR_TCON_RECORD record;
  if (!DSL_Tensor_TCON_Get(tcon, &record) ||
      record.byte_offset != *offset || record.byte_length != bytes.size() ||
      fwrite(&bytes[0], 1, bytes.size(), file) != bytes.size())
    return Report(diagnostic, "tail plaintext side-file write failed");
  asset->name = name;
  asset->offset = *offset;
  asset->length = bytes.size();
  asset->sha256 = digest;
  asset->ty = ty;
  asset->tcon = tcon;
  asset->value_id = DSL_IR_VALUE_INVALID_ID;
  *offset += bytes.size();
  return TRUE;
}

/* Construct and register the immutable derived tail plaintext payload. */
BOOL Prepare_Assets(PU_Info *pu_info, FILE *diagnostic)
{
  if (Tail_state.asset_registered)
    return TRUE;
  const char *output = VHO_FHE_Materialization_Checkpoint_Output;
  if (pu_info == NULL || PU_Info_proc_sym(pu_info) != Tail_state.job.owner ||
      output == NULL || output[0] == 0)
    return Report(diagnostic, "tail checkpoint output is unavailable");
  Tail_state.asset_final = std::string(output) + ".tail-plaintexts.f32";
  Tail_state.asset_temp = Tail_state.asset_final + ".tmp";
  Tail_state.asset_leaf = Leaf_Name(Tail_state.asset_final);
  Tail_state.report_final = std::string(output) + ".tail-report.txt";
  Tail_state.report_temp = Tail_state.report_final + ".tmp";
  if (!VHO_FHE_Materialize_Checkpoint_Register_Artifact(
          Tail_state.asset_temp.c_str(), Tail_state.asset_final.c_str()) ||
      !VHO_FHE_Materialize_Checkpoint_Register_Artifact(
          Tail_state.report_temp.c_str(), Tail_state.report_final.c_str()))
    return Report(diagnostic, "tail artifacts cannot join checkpoint");
  Tail_state.asset_registered = TRUE;

  DSL_IR_EXTERNAL_TENSOR_REFERENCE weight_reference;
  DSL_IR_EXTERNAL_TENSOR_REFERENCE bias_reference;
  std::vector<float> weights;
  std::vector<float> biases;
  if (!Read_F32_Source(Tail_state.job.owner, Tail_state.job.linear_weight,
                       &weights, &weight_reference, diagnostic) ||
      !Read_F32_Source(Tail_state.job.owner, Tail_state.job.linear_bias,
                       &biases, &bias_reference, diagnostic) ||
      weights.size() != 10 * 64 || biases.size() != 10 ||
      !DSL_IR_Capture_External_Tensor_Source(
          pu_info, Tail_state.job.linear_weight,
          &Tail_state.job.weight_handle) ||
      !DSL_IR_Capture_External_Tensor_Source(
          pu_info, Tail_state.job.linear_bias,
          &Tail_state.job.bias_handle))
    return Report(diagnostic, "classifier source tensors are not 10x64/10");

  std::vector<unsigned char> identity;
  identity.insert(identity.end(), weight_reference.checksum,
                  weight_reference.checksum + strlen(weight_reference.checksum));
  identity.insert(identity.end(), bias_reference.checksum,
                  bias_reference.checksum + strlen(bias_reference.checksum));
  static const char geometry[] =
      "resnet20.tail.v1;slots=32768;channels=64;spatial=64;logits=10";
  identity.insert(identity.end(), geometry, geometry + sizeof(geometry) - 1);
  Tail_state.geometry_sha256 = VHO_FHE_SHA256(
      &identity[0], identity.size());

  FILE *file = fopen(Tail_state.asset_temp.c_str(), "wb");
  const TY_IDX ty = Asset_Type();
  UINT64 offset = 0;
  BOOL valid = file != NULL && ty != TY_IDX_ZERO;
  std::vector<float> pool(32768, 0.0f);
  for (UINT32 channel = 0; channel < 64; ++channel)
    pool[channel * 64] = 1.0f / 64.0f;
  if (valid)
    valid = Write_Asset(file, pool, "fhe_tail_pool_scale_mask", ty,
                        &offset, &Tail_state.job.pool_mask, diagnostic);
  Tail_state.job.weight_masks.resize(10);
  for (UINT32 output_index = 0; valid && output_index < 10; ++output_index) {
    std::vector<float> mask(32768, 0.0f);
    for (UINT32 channel = 0; channel < 64; ++channel)
      mask[channel * 64] = weights[output_index * 64 + channel];
    char name[96];
    snprintf(name, sizeof(name), "fhe_tail_linear_weight_%u", output_index);
    valid = Write_Asset(file, mask, name, ty, &offset,
                        &Tail_state.job.weight_masks[output_index],
                        diagnostic);
  }
  std::vector<float> bias(32768, 0.0f);
  for (UINT32 output_index = 0; output_index < 10; ++output_index)
    bias[output_index] = biases[output_index];
  if (valid)
    valid = Write_Asset(file, bias, "fhe_tail_linear_bias", ty,
                        &offset, &Tail_state.job.bias, diagnostic);
  const BOOL closed = file != NULL && fclose(file) == 0;
  if (!valid || !closed)
    return Report(diagnostic, "tail plaintext payload construction failed");
  Tail_state.asset_bytes = offset;
  return TRUE;
}

/* Materialize the generated pool mask and the source-derived classifier
 * weight/bias masks immediately before their owning source operations. */
BOOL Materialize_Assets(PU_Info *pu_info, WN *pool_definition,
                        WN *linear_definition, FILE *diagnostic)
{
  DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST pool_request;
  DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT pool_result;
  DSL_IR_Generated_External_Tensor_Request_Init(&pool_request);
  pool_request.insert_before = pool_definition;
  pool_request.name = Tail_state.job.pool_mask.name.c_str();
  pool_request.descriptor_ty = Tail_state.job.pool_mask.ty;
  pool_request.tensor_tcon = Tail_state.job.pool_mask.tcon;
  pool_request.storage_format = "raw_f32_le";
  pool_request.side_file = Tail_state.asset_leaf.c_str();
  pool_request.tensor_key = Tail_state.job.pool_mask.name.c_str();
  pool_request.byte_offset = Tail_state.job.pool_mask.offset;
  pool_request.byte_length = Tail_state.job.pool_mask.length;
  pool_request.checksum_sha256 = Tail_state.job.pool_mask.sha256.c_str();
  pool_request.source_position = WN_Get_Linenum(pool_definition);
  pool_request.generation_name = "fhe.ckks.global_average_pool.mask";
  pool_request.generation_version = 1;
  pool_request.geometry_manifest_sha256 =
      Tail_state.geometry_sha256.c_str();
  pool_request.stage_ordinal = 0;
  pool_request.diagonal_ordinal = 0;
  pool_request.variant_signature_sha256 =
      Tail_state.geometry_sha256.c_str();
  if (!DSL_IR_Materialize_Generated_External_Tensor_Values(
          pu_info, &pool_request, 1, &pool_result))
    return Report(diagnostic, "pool mask materialization was rejected");
  Tail_state.job.pool_mask.value_id = pool_result.value_id;

  std::vector<DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST> requests(11);
  std::vector<DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT> results(11);
  for (UINT32 i = 0; i < 10; ++i) {
    DSL_IR_Typed_External_Tensor_Value_Request_Init(&requests[i]);
    requests[i].name = Tail_state.job.weight_masks[i].name.c_str();
    requests[i].source_owner_pu_st = Tail_state.job.owner;
    requests[i].source_value_id = Tail_state.job.linear_weight;
    requests[i].source_handle = Tail_state.job.weight_handle;
    requests[i].descriptor_ty = Tail_state.job.weight_masks[i].ty;
    requests[i].tensor_tcon = Tail_state.job.weight_masks[i].tcon;
    requests[i].insert_before = linear_definition;
    requests[i].source_position = WN_Get_Linenum(linear_definition);
    requests[i].storage_format = "raw_f32_le";
    requests[i].side_file = Tail_state.asset_leaf.c_str();
    requests[i].tensor_key = Tail_state.job.weight_masks[i].name.c_str();
    requests[i].byte_offset = Tail_state.job.weight_masks[i].offset;
    requests[i].byte_length = Tail_state.job.weight_masks[i].length;
    requests[i].checksum = Tail_state.job.weight_masks[i].sha256.c_str();
    requests[i].transformation_name = "fhe.linear.channel_mask";
    requests[i].transformation_version = 1;
    requests[i].transformation_ordinal = i;
  }
  DSL_IR_Typed_External_Tensor_Value_Request_Init(&requests[10]);
  requests[10].name = Tail_state.job.bias.name.c_str();
  requests[10].source_owner_pu_st = Tail_state.job.owner;
  requests[10].source_value_id = Tail_state.job.linear_bias;
  requests[10].source_handle = Tail_state.job.bias_handle;
  requests[10].descriptor_ty = Tail_state.job.bias.ty;
  requests[10].tensor_tcon = Tail_state.job.bias.tcon;
  requests[10].insert_before = linear_definition;
  requests[10].source_position = WN_Get_Linenum(linear_definition);
  requests[10].storage_format = "raw_f32_le";
  requests[10].side_file = Tail_state.asset_leaf.c_str();
  requests[10].tensor_key = Tail_state.job.bias.name.c_str();
  requests[10].byte_offset = Tail_state.job.bias.offset;
  requests[10].byte_length = Tail_state.job.bias.length;
  requests[10].checksum = Tail_state.job.bias.sha256.c_str();
  requests[10].transformation_name = "fhe.linear.expanded_bias";
  requests[10].transformation_version = 1;
  requests[10].transformation_ordinal = 0;
  if (!DSL_IR_Materialize_Typed_External_Tensor_Values(
          pu_info, &requests[0], requests.size(), &results[0]))
    return Report(diagnostic, "classifier plaintext transaction failed");
  for (UINT32 i = 0; i < 10; ++i)
    Tail_state.job.weight_masks[i].value_id = results[i].value_id;
  Tail_state.job.bias.value_id = results[10].value_id;
  return TRUE;
}

/* Find the ciphertext and encoded-plaintext descriptors for one config. */
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

/* Join the root call result to the final callee ReLU context result state.
 * This is context-sensitive state propagation: numeric value IDs validate the
 * exact live image, while the call route selects the matching callee result. */
BOOL Bind_Call_Result_State(FILE *diagnostic)
{
  const VHO_FHE_CKKS_EVENT_IDENTITY &final_event =
      Tail_state.job.final_callee_relu_event;
  DSL_FHE_CONTEXT_CKKS_STATE_RECORD context;
  DSL_FHE_CKKS_VALUE_STATE_RECORD prior;
  if (!DSL_FHE_Context_State_Find(
          final_event.owner_pu_st, final_event.source_value_id,
          final_event.context_pu_identity_id,
          final_event.context_callsite_id,
          DSL_FHE_CONTEXT_STATE_ROLE_RESULT, 1, &context) ||
      context.pending_actions != 0 || context.level < 1 ||
      context.scale_bits <= 0 || context.component_count != 2 ||
      context.precision_bits < 30 || context.slot_count != 32768)
    return Report(diagnostic, "callee result state is not CKKS-complete");
  const BOOL has_prior = DSL_FHE_Plan_Find_Latest_CKKS_Value_State(
      Tail_state.job.pool_input, &prior);
  DSL_FHE_CKKS_VALUE_STATE_RECORD state;
  if (has_prior)
    state = prior;
  else
    DSL_FHE_CKKS_Value_State_Record_Init(&state);
  state.id = DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
  state.value_id = Tail_state.job.pool_input;
  state.state_version = has_prior ? prior.state_version + 1 : 1;
  state.encryption_descriptor_id = context.encryption_descriptor_id;
  state.scheme = context.scheme;
  state.value_class = context.value_class;
  state.level = context.level;
  state.scale_bits = context.scale_bits;
  state.component_count = context.component_count;
  state.precision_bits = context.precision_bits;
  state.slot_count = context.slot_count;
  state.alignment_group = context.alignment_group;
  state.encrypted_layout_name = context.encrypted_layout_name;
  state.pending_actions = context.pending_actions;
  state.pending_bootstrap_reason = context.pending_bootstrap_reason;
  if (DSL_FHE_Plan_Add_CKKS_Value_State(&state) ==
      DSL_FHE_CKKS_VALUE_STATE_INVALID_ID)
    return Report(diagnostic, "root call-result state binding was rejected");
  return TRUE;
}

/* Register every signed rotation required by the explicit tail plans. */
BOOL Register_Rotation_Keys(
    const VHO_FHE_CKKS_EVENT_PLAN &pool,
    const VHO_FHE_CKKS_EVENT_PLAN &linear,
    const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD &cipher,
    FILE *diagnostic)
{
  std::set<INT32> rotations;
  for (size_t i = 0; i < pool.steps.size(); ++i)
    rotations.insert(pool.steps[i].signed_rotations.begin(),
                     pool.steps[i].signed_rotations.end());
  for (size_t i = 0; i < linear.steps.size(); ++i)
    rotations.insert(linear.steps[i].signed_rotations.begin(),
                     linear.steps[i].signed_rotations.end());
  rotations.erase(0);
  for (std::set<INT32>::const_iterator it = rotations.begin();
       it != rotations.end(); ++it) {
    DSL_FHE_KEY_REQUIREMENT_RECORD key;
    DSL_FHE_Key_Requirement_Record_Init(&key);
    key.config_id = cipher.config_id;
    key.key_set_name = cipher.key_set_name;
    key.key_class = DSL_FHE_KEY_ROTATION;
    key.rotation_offset = *it;
    if (DSL_FHE_Intern_Key_Requirement(&key) ==
        DSL_FHE_KEY_REQUIREMENT_INVALID_ID)
      return Report(diagnostic, "tail rotation-key requirement rejected");
  }
  return TRUE;
}

/* Translate one concrete input state into the common tail planner policy. */
BOOL State_Policy(
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &input,
    DSL_IR_VALUE_ID input_value, TY_IDX input_ty, TY_IDX result_ty,
    UINT32 source_static_ordinal,
    VHO_FHE_CKKS_TAIL_STATE_POLICY *policy, FILE *diagnostic)
{
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD cipher;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD plain;
  if (policy == NULL || input_value == DSL_IR_VALUE_INVALID_ID ||
      input_ty == TY_IDX_ZERO || result_ty == TY_IDX_ZERO ||
      source_static_ordinal == 0 || input.pending_actions != 0 ||
      input.pending_bootstrap_reason != DSL_FHE_BOOTSTRAP_REASON_NONE ||
      !Descriptor_Pair(input.encryption_descriptor_id, &cipher, &plain) ||
      cipher.key_set_name == STR_IDX_ZERO)
    return Report(diagnostic, "tail input state or descriptor is invalid");
  if (DSL_FHE_Intern_Tensor_Binding(input_ty, cipher.id, 0) ==
          DSL_FHE_TENSOR_BINDING_INVALID_ID ||
      DSL_FHE_Intern_Tensor_Binding(input_ty, plain.id, 0) ==
          DSL_FHE_TENSOR_BINDING_INVALID_ID ||
      DSL_FHE_Intern_Tensor_Binding(result_ty, cipher.id, 0) ==
          DSL_FHE_TENSOR_BINDING_INVALID_ID ||
      DSL_FHE_Intern_Tensor_Binding(result_ty, plain.id, 0) ==
          DSL_FHE_TENSOR_BINDING_INVALID_ID)
    return Report(diagnostic, "tail tensor bindings were rejected");
  policy->source_static_ordinal = source_static_ordinal;
  policy->input_value_id = input_value;
  policy->input_ty = input_ty;
  policy->result_ty = result_ty;
  policy->cipher_encryption_descriptor_id = cipher.id;
  policy->plaintext_encryption_descriptor_id = plain.id;
  policy->scheme = input.scheme;
  policy->ciphertext_value_class = input.value_class;
  policy->plaintext_value_class = plain.value_class;
  policy->input_level = input.level;
  policy->scale_bits = input.scale_bits;
  policy->component_count = input.component_count;
  policy->precision_bits = input.precision_bits;
  policy->slot_count = input.slot_count;
  policy->alignment_group = input.alignment_group;
  policy->rescale_pending_action = DSL_FHE_CKKS_PENDING_RESCALE;
  policy->encrypted_layout = Index_To_Str(input.encrypted_layout_name);
  policy->rotation_key_id = Index_To_Str(cipher.key_set_name);
  return TRUE;
}

/* Retire flatten through the generic zero-motion cross-TY view transaction;
 * both canonical tensor TYs remain immutable inspection evidence. */
BOOL Retire_Flatten_View(
    PU_Info *pu_info, WN *replacement_definition,
    DSL_IR_VALUE_ID replacement_value, WN *flatten_definition,
    FILE *diagnostic)
{
  DSL_IR_NATIVE_VALUE_RETIRE_REQUEST request;
  memset(&request, 0, sizeof(request));
  request.replacement_definition = replacement_definition;
  request.replacement_value_id = replacement_value;
  request.retiring_definition = flatten_definition;
  request.retiring_value_id = Tail_state.job.flatten_result;
  request.expected_retiring_operator = OPR_DSLFLATTEN;
  request.expected_retiring_version = 2;
  request.replacement_operand_ordinal = 0;
  if (!DSL_IR_Redirect_And_Retire_Native_View(pu_info, &request))
    return Report(diagnostic, "flatten view retirement was rejected");
  return TRUE;
}

/* Retire the identity output marker after linear CKKS expansion. */
BOOL Retire_Output_Marker(
    PU_Info *pu_info, WN *replacement_definition,
    DSL_IR_VALUE_ID replacement_value, WN *output_definition,
    FILE *diagnostic)
{
  DSL_IR_NATIVE_VALUE_RETIRE_REQUEST request;
  memset(&request, 0, sizeof(request));
  request.replacement_definition = replacement_definition;
  request.replacement_value_id = replacement_value;
  request.retiring_definition = output_definition;
  request.retiring_value_id = Tail_state.job.output_result;
  request.expected_retiring_operator = OPR_DSLOUTPUTLOGITS;
  request.expected_retiring_version = 2;
  request.replacement_operand_ordinal = 0;
  if (!DSL_IR_Redirect_And_Retire_Native_Value(pu_info, &request))
    return Report(diagnostic, "output-logits retirement was rejected");
  return TRUE;
}

/* Expand the exact root tail after assets and context state are available. */
BOOL Process_Root(PU_Info *pu_info, WN *tree, FILE *diagnostic)
{
  if (Tail_state.owner_processed)
    return Report(diagnostic, "tail owner callback was repeated");
  if (!Prepare_Assets(pu_info, diagnostic) ||
      !Bind_Call_Result_State(diagnostic))
    return FALSE;

  WN *pool_definition = Find_Definition(
      pu_info, tree, Tail_state.job.pool_result);
  WN *linear_definition = Find_Definition(
      pu_info, tree, Tail_state.job.linear_result);
  if (pool_definition == NULL || linear_definition == NULL ||
      !Materialize_Assets(
          pu_info, pool_definition, linear_definition, diagnostic))
    return Report(diagnostic, "tail source definitions are unavailable");

  DSL_IR_VALUE_RECORD pool_input_value;
  DSL_IR_VALUE_RECORD pool_result_value;
  DSL_IR_VALUE_RECORD linear_result_value;
  DSL_FHE_CKKS_VALUE_STATE_RECORD pool_input_state;
  if (!DSL_IR_Image_Get_Value(
          Tail_state.job.pool_input, &pool_input_value) ||
      !DSL_IR_Image_Get_Value(
          Tail_state.job.pool_result, &pool_result_value) ||
      !DSL_IR_Image_Get_Value(
          Tail_state.job.linear_result, &linear_result_value) ||
      !DSL_FHE_Plan_Find_Latest_CKKS_Value_State(
          Tail_state.job.pool_input, &pool_input_state))
    return Report(diagnostic, "pool input/value state cannot be resolved");

  VHO_FHE_CKKS_POOL_PLAN_POLICY pool_policy;
  if (!State_Policy(pool_input_state, Tail_state.job.pool_input,
                    pool_input_value.ty, pool_result_value.ty,
                    Tail_state.job.pool_event.source_static_ordinal,
                    &pool_policy.state, diagnostic))
    return FALSE;
  pool_policy.scale_mask_value_id = Tail_state.job.pool_mask.value_id;
  pool_policy.scale_mask_sha256 = Tail_state.job.pool_mask.sha256;
  VHO_FHE_CKKS_EVENT_PLAN pool_plan;
  if (!VHO_FHE_CKKS_Build_Global_Average_Pool_Event_Plan(
          pool_policy, &pool_plan, diagnostic))
    return FALSE;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD cipher;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD plain;
  if (!Descriptor_Pair(
          pool_input_state.encryption_descriptor_id, &cipher, &plain) ||
      !Register_Rotation_Keys(pool_plan, VHO_FHE_CKKS_EVENT_PLAN(),
                              cipher, diagnostic))
    return FALSE;
  VHO_FHE_CKKS_EXPANSION_SOURCE pool_source;
  pool_source.source_definition = pool_definition;
  pool_source.source_value_id = Tail_state.job.pool_result;
  pool_source.expected_source_operator = OPR_DSLGLOBALAVGPOOL2D;
  pool_source.expected_source_version = 2;
  pool_source.context_pu_identity_id =
      Tail_state.job.pool_event.context_pu_identity_id;
  pool_source.context_callsite_id = 0;
  pool_source.origin_owner_pu_st = Tail_state.job.owner;
  pool_source.origin_source_value_id = Tail_state.job.pool_result;
  pool_source.encrypted_layout_name =
      pool_input_state.encrypted_layout_name;
  std::vector<DSL_CKKS_EXPANSION_STEP_RESULT> pool_results;
  if (!VHO_FHE_CKKS_Expand_Event(
          pu_info, pool_plan, pool_source, diagnostic, &pool_results) ||
      pool_results.size() != 15)
    return Report(diagnostic, "global-average-pool expansion was rejected");
  Tail_state.operation_count += pool_results.size();

  WN *pool_replacement = Find_Definition(
      pu_info, tree, pool_results.back().value_id);
  WN *flatten_definition = Find_Definition(
      pu_info, tree, Tail_state.job.flatten_result);
  if (pool_replacement == NULL || flatten_definition == NULL ||
      !Retire_Flatten_View(
          pu_info, pool_replacement, pool_results.back().value_id,
          flatten_definition, diagnostic))
    return FALSE;

  linear_definition = Find_Definition(
      pu_info, tree, Tail_state.job.linear_result);
  DSL_IR_VALUE_RECORD linear_input_value;
  DSL_FHE_CKKS_VALUE_STATE_RECORD linear_input_state;
  if (linear_definition == NULL ||
      !DSL_IR_Image_Get_Value(
          pool_results.back().value_id, &linear_input_value) ||
      !DSL_FHE_Plan_Find_Latest_CKKS_Value_State(
          pool_results.back().value_id, &linear_input_state))
    return Report(diagnostic, "linear input state is unavailable");

  VHO_FHE_CKKS_LINEAR_PLAN_POLICY linear_policy;
  if (!State_Policy(linear_input_state, pool_results.back().value_id,
                    linear_input_value.ty, linear_result_value.ty,
                    Tail_state.job.linear_event.source_static_ordinal,
                    &linear_policy.state, diagnostic))
    return FALSE;
  for (UINT32 i = 0; i < 10; ++i) {
    linear_policy.weight_mask_value_ids.push_back(
        Tail_state.job.weight_masks[i].value_id);
    linear_policy.weight_mask_sha256.push_back(
        Tail_state.job.weight_masks[i].sha256);
  }
  linear_policy.bias_value_id = Tail_state.job.bias.value_id;
  linear_policy.bias_sha256 = Tail_state.job.bias.sha256;
  VHO_FHE_CKKS_EVENT_PLAN linear_plan;
  if (!VHO_FHE_CKKS_Build_ResNet20_Linear_Event_Plan(
          linear_policy, &linear_plan, diagnostic) ||
      !Register_Rotation_Keys(VHO_FHE_CKKS_EVENT_PLAN(), linear_plan,
                              cipher, diagnostic))
    return FALSE;
  VHO_FHE_CKKS_EXPANSION_SOURCE linear_source;
  linear_source.source_definition = linear_definition;
  linear_source.source_value_id = Tail_state.job.linear_result;
  linear_source.expected_source_operator = OPR_DSLLINEAR;
  linear_source.expected_source_version = 2;
  linear_source.context_pu_identity_id =
      Tail_state.job.linear_event.context_pu_identity_id;
  linear_source.context_callsite_id = 0;
  linear_source.origin_owner_pu_st = Tail_state.job.owner;
  linear_source.origin_source_value_id = Tail_state.job.linear_result;
  linear_source.encrypted_layout_name =
      linear_input_state.encrypted_layout_name;
  std::vector<DSL_CKKS_EXPANSION_STEP_RESULT> linear_results;
  if (!VHO_FHE_CKKS_Expand_Event(
          pu_info, linear_plan, linear_source, diagnostic, &linear_results) ||
      linear_results.size() != 170)
    return Report(diagnostic, "linear expansion was rejected");
  Tail_state.operation_count += linear_results.size();

  WN *linear_replacement = Find_Definition(
      pu_info, tree, linear_results.back().value_id);
  WN *output_definition = Find_Definition(
      pu_info, tree, Tail_state.job.output_result);
  if (linear_replacement == NULL || output_definition == NULL ||
      !Retire_Output_Marker(
          pu_info, linear_replacement, linear_results.back().value_id,
          output_definition, diagnostic))
    return FALSE;
  Tail_state.job.processed = TRUE;
  Tail_state.owner_processed = TRUE;
  return TRUE;
}

}  // namespace

/* Validate the exact tail once before any PU mutation begins. */
BOOL VHO_FHE_CKKS_Tail_Materialization_Gatekeeper(
    PU_Info *pu_info, WN *tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic)
{
  return pu_info != NULL && tree != NULL && options != NULL &&
         Initialize(diagnostic);
}

/* Mutate only the root PU; called block PUs contain no tail definitions. */
BOOL VHO_FHE_CKKS_Tail_Materialization_Pass(
    PU_Info *pu_info, WN **tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic)
{
  if (pu_info == NULL || tree == NULL || *tree == NULL || options == NULL ||
      !Initialize(diagnostic))
    return FALSE;
  if (!Tail_state.active || PU_Info_proc_sym(pu_info) != Tail_state.job.owner)
    return TRUE;
  return Process_Root(pu_info, *tree, diagnostic);
}

/* Prove complete executable tail replacement and register its report. */
BOOL VHO_FHE_CKKS_Tail_Materialization_Finalize(FILE *diagnostic)
{
  if (!Tail_state.active)
    return TRUE;
  UINT32 bootstrap_count = 0;
  UINT32 pre_relu_bootstraps = 0;
  UINT32 depth_bootstraps = 0;
  for (UINT32 id = 1; id <= DSL_CKKS_Event_Image_Count(); ++id) {
    DSL_CKKS_EVENT_RECORD event;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    if (!DSL_CKKS_Event_Image_Get(id, &event) ||
        !DSL_IR_Image_Get_Node(event.result_node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor(
            node.opcode_descriptor_id, &opcode))
      return Report(diagnostic, "whole-model CKKS event census is unreadable");
    if (opcode.logical_operator == OPR_DSLCKKSBOOTSTRAP) {
      const char *reason = NULL;
      if (!Attribute(node, "attr.reason", &reason))
        return Report(diagnostic,
                      "whole-model bootstrap reason is unreadable");
      ++bootstrap_count;
      if (strcmp(reason, "PRE_RELU_REFRESH") == 0)
        ++pre_relu_bootstraps;
      else if (strcmp(reason, "DEPTH_EXHAUSTION") == 0)
        ++depth_bootstraps;
      else
        return Report(diagnostic,
                      "whole-model bootstrap reason is unsupported");
    }
  }
  std::set<INT32> required_rotations;
  static const INT32 tail_rotations[] = {
    -9, -8, -7, -6, -5, -4, -3, -2, -1,
    1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048
  };
  for (UINT32 i = 0;
       i < sizeof(tail_rotations) / sizeof(tail_rotations[0]); ++i)
    required_rotations.insert(tail_rotations[i]);
  for (UINT32 id = 1; id <= DSL_FHE_Key_Requirement_Count(); ++id) {
    DSL_FHE_KEY_REQUIREMENT_RECORD key;
    if (!DSL_FHE_Get_Key_Requirement(id, &key))
      return Report(diagnostic, "whole-model key census is unreadable");
    if (key.key_class == DSL_FHE_KEY_ROTATION)
      required_rotations.erase(key.rotation_offset);
  }
  DSL_IR_NODE_RECORD pool;
  DSL_IR_NODE_RECORD flatten;
  DSL_IR_NODE_RECORD linear;
  DSL_IR_NODE_RECORD output;
  DSL_IR_VALUE_RECORD flatten_value;
  DSL_IR_VALUE_RECORD output_value;
  memset(&pool, 0, sizeof(pool));
  memset(&flatten, 0, sizeof(flatten));
  memset(&linear, 0, sizeof(linear));
  memset(&output, 0, sizeof(output));
  memset(&flatten_value, 0, sizeof(flatten_value));
  memset(&output_value, 0, sizeof(output_value));
  const BOOL pool_read =
      DSL_IR_Image_Get_Node(Tail_state.job.pool_node, &pool);
  const BOOL flatten_read =
      DSL_IR_Image_Get_Node(Tail_state.job.flatten_node, &flatten);
  const BOOL linear_read =
      DSL_IR_Image_Get_Node(Tail_state.job.linear_node, &linear);
  const BOOL output_read =
      DSL_IR_Image_Get_Node(Tail_state.job.output_node, &output);
  const BOOL flatten_value_read = DSL_IR_Image_Get_Value(
      Tail_state.job.flatten_result, &flatten_value);
  const BOOL output_value_read = DSL_IR_Image_Get_Value(
      Tail_state.job.output_result, &output_value);
  const BOOL event_image_valid =
      DSL_CKKS_Event_Image_Validate(diagnostic);
  const BOOL plan_image_valid = DSL_FHE_Plan_Image_Validate(diagnostic);
  /* The O0 graph retains two stride-two Conv paths at each downsampling
   * boundary.  They require four explicit depth repairs in addition to the
   * nineteen semantically mandatory pre-ReLU refreshes. */
  if (!Tail_state.job.processed || !Tail_state.owner_processed ||
      Tail_state.operation_count != 185 ||
      DSL_CKKS_Event_Image_Count() != 33367 ||
      bootstrap_count != 23 || pre_relu_bootstraps != 19 ||
      depth_bootstraps != 4 || !required_rotations.empty() ||
      !pool_read || !flatten_read || !linear_read || !output_read ||
      !flatten_value_read || !output_value_read ||
      (pool.flags & DSL_IR_NODE_FLAG_LOWERED) == 0 ||
      (linear.flags & DSL_IR_NODE_FLAG_LOWERED) == 0 ||
      (flatten.flags & DSL_IR_NODE_FLAG_RETIRED) == 0 ||
      (output.flags & DSL_IR_NODE_FLAG_RETIRED) == 0 ||
      (flatten_value.flags & DSL_IR_VALUE_FLAG_REDIRECTED) == 0 ||
      (output_value.flags & DSL_IR_VALUE_FLAG_REDIRECTED) == 0 ||
      !event_image_valid || !plan_image_valid) {
    if (diagnostic != NULL) {
      fprintf(diagnostic,
              "CFHEIR-TAIL-MATERIALIZE-001: measured processed=%u owner=%u "
              "operations=%u events=%u bootstraps=%u/%u/%u "
              "missing_rotations=%u "
              "reads=%u/%u/%u/%u/%u/%u flags=0x%x/0x%x/0x%x/0x%x/"
              "0x%x/0x%x images=%u/%u\n",
              Tail_state.job.processed, Tail_state.owner_processed,
              Tail_state.operation_count, DSL_CKKS_Event_Image_Count(),
              bootstrap_count, pre_relu_bootstraps, depth_bootstraps,
              static_cast<UINT32>(required_rotations.size()),
              pool_read, flatten_read, linear_read, output_read,
              flatten_value_read, output_value_read,
              pool.flags, flatten.flags, linear.flags, output.flags,
              flatten_value.flags, output_value.flags,
              event_image_valid, plan_image_valid);
      if (!required_rotations.empty()) {
        fprintf(diagnostic,
                "CFHEIR-TAIL-MATERIALIZE-001: missing rotations=");
        for (std::set<INT32>::const_iterator it = required_rotations.begin();
             it != required_rotations.end(); ++it)
          fprintf(diagnostic, "%s%d",
                  it == required_rotations.begin() ? "" : ",", *it);
        fprintf(diagnostic, "\n");
      }
    }
    return Report(diagnostic,
                  "tail coverage is not 15 pool plus 170 linear operations");
  }

  FILE *report = fopen(Tail_state.report_temp.c_str(), "w");
  if (report == NULL)
    return Report(diagnostic, "tail report temp cannot be created");
  fprintf(report, "FHE SYNC-6 ResNet tail materialization report\n");
  fprintf(report, "global_average_pool_operations=15\n");
  fprintf(report, "linear_operations=170\n");
  fprintf(report, "tail_operations=185\n");
  fprintf(report, "whole_model_ckks_operations=33367\n");
  fprintf(report, "whole_model_bootstraps=23\n");
  fprintf(report, "pre_relu_refresh_bootstraps=19\n");
  fprintf(report, "depth_exhaustion_bootstraps=4\n");
  fprintf(report, "pool_nodes_live=0\n");
  fprintf(report, "flatten_nodes_live=0\n");
  fprintf(report, "linear_nodes_live=0\n");
  fprintf(report, "output_logits_nodes_live=0\n");
  fprintf(report, "pool_rotation_keys=1,2,4,8,16,32\n");
  fprintf(report,
          "linear_rotation_keys=-9,-8,-7,-6,-5,-4,-3,-2,-1,64,128,256,512,1024,2048\n");
  fprintf(report, "side_asset=%s\n", Tail_state.asset_leaf.c_str());
  fprintf(report, "side_asset_bytes=%llu\n",
          static_cast<unsigned long long>(Tail_state.asset_bytes));
  fprintf(report, "geometry_sha256=%s\n",
          Tail_state.geometry_sha256.c_str());
  const BOOL closed = fclose(report) == 0;
  if (!closed)
    return Report(diagnostic, "tail report cannot be finalized");
  if (diagnostic != NULL)
    fprintf(diagnostic,
            "FHE-SYNC6-TAIL: pool=15 linear=170 operations=185 live=0\n");
  return TRUE;
}

/* Drop all process-local image IDs, handles, and artifact pathnames. */
void VHO_FHE_CKKS_Tail_Materialization_Complete(BOOL committed)
{
  (void)committed;
  Tail_state = Tail_State();
}
