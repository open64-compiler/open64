/*
 * Copyright (C) 2026 Open64 Project
 *
 * Certify the FHE semantic front door for merged typed-row and generated-mask
 * transactions. The fixture retains mapped WHIRL for separate-process review.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <vector>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "wn_util.h"
#include "stab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "glob.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "dsl_builder.h"
#include "dsl_opcode.h"
#include "dsl_ir_image.h"
#include "dsl_gatekeeper.h"
#include "dsl_tensor_fold.h"
#include "fhe_ckks_conv_assets.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void Signal_Cleanup(INT) {}
const char *Host_Format_Parm(INT, MEM_PTR) { return ""; }

/* Initialize the native symbol, DST, control, and IR tables used here. */
static void
Initialize_Test_Context()
{
  MEM_Initialize();
  Set_Error_Tables(Phases, host_errlist);
  Init_Error_Handler(10);
  Set_Error_File(NULL);
  Set_Error_Line(ERROR_LINE_UNKNOWN);
  Preconfigure();
  Init_Controls_Tbl();
  ABI_Name = "n64";
  Configure();
  IR_reader_init();
  Initialize_Symbol_Tables(TRUE);
  DST_Init(NULL, 0);
}

/* Keep fixture failures concise and attributable to one contract stage. */
static int
Fail(const char *stage)
{
  fprintf(stderr, "CKKS Conv asset fixture failed: %s\n", stage);
  return 1;
}

/* Find the source constant's unique physical definition in the active PU. */
static WN *
Find_Definition(WN *block, ST_IDX result_st)
{
  for (WN *statement = WN_first(block); statement != NULL;
       statement = WN_next(statement)) {
    if (WN_operator(statement) == OPR_STID &&
        WN_st_idx(statement) == result_st)
      return statement;
  }
  return NULL;
}

/* Create one side-file-dense tensor TCON matching an existing canonical TY. */
static TCON_IDX
Create_External_TCON(TY_IDX ty, UINT64 element_count, UINT64 byte_offset,
                     UINT64 byte_length)
{
  DSL_TENSOR_TCON_CREATE_INFO info;
  TCON_IDX tcon = TCON_IDX_ZERO;
  memset(&info, 0, sizeof(info));
  info.descriptor_ty = ty;
  info.element_mtype = MTYPE_F4;
  info.element_count = element_count;
  info.logical_bytes = byte_length;
  info.required_alignment = 16;
  info.element_size = 4;
  info.side_path = "conv-assets.bin";
  info.side_path_length = strlen(info.side_path);
  info.byte_offset = byte_offset;
  info.byte_length = byte_length;
  info.checksum_hi = 0x0123456789abcdefULL + byte_offset;
  info.checksum_lo = 0xfedcba9876543210ULL - byte_offset;
  if (!DSL_Tensor_TCON_Create_Side_File_Dense(&info, &tcon, NULL))
    return TCON_IDX_ZERO;
  return tcon;
}

/* Build one canonical external-data tensor TY for the fixture. */
static TY_IDX
Create_Tensor_Type(const char *name, INT32 rank, const char *shape,
                   const char *traits)
{
  DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
  memset(&descriptor, 0, sizeof(descriptor));
  descriptor.type_core.kind = "tensor";
  descriptor.type_core.dtype = "float32";
  descriptor.type_core.rank = rank;
  descriptor.type_core.logical_shape = shape;
  descriptor.traits.traits = traits;
  descriptor.representation.layout = "row_major";
  descriptor.representation.sharding = "replicated";
  descriptor.representation.placement = "side_file";
  descriptor.representation.memory = "external_data";
  descriptor.representation.quantization = "none";
  return DSL_Builder_Intern_Tensor_Type(
      name, MTYPE_To_TY(MTYPE_F4), &descriptor);
}

/* Exercise FHE admission, common atomic mutation, and mapped publication. */
int
main(int argc, char **argv)
{
  if (argc != 2)
    return Fail("expected one mapped .B output path");
  Initialize_Test_Context();
  if (!DSL_Builder_Begin_Program() ||
      DSL_Opcode_Register_Common_Substrate() == 0)
    return Fail("builder and common operator registry");

  TY_IDX weight_ty = Create_Tensor_Type(
      "ckks_conv_asset_weight_f32_2x1x3x3", 4, "[2,1,3,3]",
      "parameter.folded_weight");
  TY_IDX row_ty = Create_Tensor_Type(
      "ckks_conv_asset_row_f32_32", 1, "[32]", "derived.feature_row");
  DSL_BUILDER_PROGRAM_UNIT pu =
      DSL_Builder_Create_Minimal_PU("ckks_conv_asset_materialization");
  UINT32 file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
  if (weight_ty == TY_IDX_ZERO || row_ty == TY_IDX_ZERO || pu == NULL ||
      file_id == 0)
    return Fail("canonical tensor types and PU");

  const char checksum[] =
      "abcdef0123456789abcdef0123456789"
      "abcdef0123456789abcdef0123456789";
  const char geometry_sha[] =
      "0123456789abcdef0123456789abcdef"
      "0123456789abcdef0123456789abcdef";
  const char variant_sha[] =
      "11111111111111111111111111111111"
      "11111111111111111111111111111111";
  DSL_BUILDER_EXTERNAL_TENSOR_REFERENCE source_reference;
  memset(&source_reference, 0, sizeof(source_reference));
  source_reference.storage_format = "safetensors";
  source_reference.side_file = "folded.safetensors";
  source_reference.tensor_key = "stem.conv.folded_weight";
  source_reference.byte_offset = 0;
  source_reference.byte_length = 18 * sizeof(float);
  source_reference.checksum = checksum;
  DSL_BUILDER_VALUE source = DSL_Builder_Create_External_Tensor_Constant(
      "stem_folded_weight", weight_ty, &source_reference);
  DSL_BUILDER_SOURCE_POSITION position;
  memset(&position, 0, sizeof(position));
  position.file_id = file_id;
  position.line = __LINE__;
  position.column = 1;
  position.statement_begin = 1;
  if (source == NULL ||
      !DSL_Builder_Set_Value_Source_Position(source, &position) ||
      !DSL_Builder_Append_PU_Value(pu, source))
    return Fail("source external folded weight");

  WN *body = WN_func_body(PU_Info_tree_ptr(pu));
  WN *source_definition = Find_Definition(
      body, DSL_Builder_Get_Value_Result_Symbol(source));
  DSL_IR_VALUE_ID source_id = DSL_Builder_Get_Value_Image_Id(source);
  DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE source_handle = 0;
  if (source_definition == NULL || source_id == DSL_IR_VALUE_INVALID_ID ||
      !DSL_IR_Capture_External_Tensor_Source(
          pu, source_id, &source_handle) || source_handle == 0)
    return Fail("typed source capture");

  VHO_FHE_CKKS_CONV_SHAPE shape = {
    1, 1, 2, 4, 4, 3, 3, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 32
  };
  float weights[18];
  float bias[2] = { 0.25f, -0.5f };
  for (UINT32 i = 0; i < 18; ++i)
    weights[i] = float(i + 1) / 32.0f;
  VHO_FHE_CKKS_CONV_RECIPE recipe;
  if (!VHO_FHE_CKKS_Build_Column_Conv_Recipe(
          shape, weights, 18, bias, 2, &recipe, stderr))
    return Fail("bounded Conv recipe");

  std::vector<DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST> row_requests(9);
  std::vector<DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT> row_results(9);
  char row_names[9][32];
  char row_keys[9][32];
  for (UINT32 i = 0; i < row_requests.size(); ++i) {
    snprintf(row_names[i], sizeof(row_names[i]), "stem_conv_row_%u", i);
    snprintf(row_keys[i], sizeof(row_keys[i]), "stem.conv.row.%u", i);
    TCON_IDX tcon = Create_External_TCON(row_ty, 32, i * 128, 128);
    if (tcon == TCON_IDX_ZERO)
      return Fail("row tensor TCON");
    DSL_IR_Typed_External_Tensor_Value_Request_Init(&row_requests[i]);
    row_requests[i].name = row_names[i];
    row_requests[i].source_owner_pu_st = PU_Info_proc_sym(pu);
    row_requests[i].source_value_id = source_id;
    row_requests[i].source_handle = source_handle;
    row_requests[i].descriptor_ty = row_ty;
    row_requests[i].tensor_tcon = tcon;
    row_requests[i].insert_before = source_definition;
    row_requests[i].source_position = WN_Get_Linenum(source_definition);
    row_requests[i].storage_format = "raw_f32_le";
    row_requests[i].side_file = "conv-assets.bin";
    row_requests[i].tensor_key = row_keys[i];
    row_requests[i].byte_offset = i * 128;
    row_requests[i].byte_length = 128;
    row_requests[i].checksum = checksum;
    row_requests[i].transformation_name = "fhe.conv_feature_row";
    row_requests[i].transformation_version = 1;
    row_requests[i].transformation_ordinal = i;
  }

  const UINT32 node_count = DSL_IR_Image_Node_Count();
  const UINT32 value_count = DSL_IR_Image_Value_Count();
  const UINT32 st_count = ST_Table_Size(CURRENT_SYMTAB);
  std::vector<DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST> bad_rows =
      row_requests;
  bad_rows[4].transformation_ordinal = 3;
  for (UINT32 i = 0; i < row_results.size(); ++i) {
    row_results[i].value_id = 99;
    row_results[i].st = ST_IDX(99);
    row_results[i].definition = source_definition;
  }
  if (VHO_FHE_CKKS_Materialize_Conv_Rows(
          pu, &recipe, &bad_rows[0], bad_rows.size(), &row_results[0], NULL) ||
      DSL_IR_Image_Node_Count() != node_count ||
      DSL_IR_Image_Value_Count() != value_count ||
      ST_Table_Size(CURRENT_SYMTAB) != st_count)
    return Fail("row ordering rejection before mutation");
  for (UINT32 i = 0; i < row_results.size(); ++i)
    if (row_results[i].value_id != DSL_IR_VALUE_INVALID_ID ||
        row_results[i].st != ST_IDX_ZERO || row_results[i].definition != NULL)
      return Fail("row rejection result clearing");
  if (!VHO_FHE_CKKS_Materialize_Conv_Rows(
          pu, &recipe, &row_requests[0], row_requests.size(),
          &row_results[0], stderr) ||
      !DSL_IR_Typed_External_Tensor_Validate_PU(pu, stderr))
    return Fail("ordered ACE row materialization");

  DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE lineage;
  if (!DSL_IR_Image_Get_Typed_External_Tensor_Lineage(
          PU_Info_proc_sym(pu), row_results[8].value_id, &lineage) ||
      lineage.source_value_id != source_id ||
      lineage.transformation_ordinal != 8)
    return Fail("typed-row provenance query");

  DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST mask_requests[2];
  DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT mask_results[2];
  const char *mask_names[2] = { "stride_mask_0", "stride_mask_1" };
  const char *mask_keys[2] = { "stride.mask.0", "stride.mask.1" };
  for (UINT32 i = 0; i < 2; ++i) {
    TCON_IDX tcon = Create_External_TCON(
        row_ty, 32, 1152 + i * 128, 128);
    if (tcon == TCON_IDX_ZERO)
      return Fail("mask tensor TCON");
    DSL_IR_Generated_External_Tensor_Request_Init(&mask_requests[i]);
    mask_requests[i].insert_before = source_definition;
    mask_requests[i].name = mask_names[i];
    mask_requests[i].descriptor_ty = row_ty;
    mask_requests[i].tensor_tcon = tcon;
    mask_requests[i].storage_format = "raw_f32_le";
    mask_requests[i].side_file = "conv-assets.bin";
    mask_requests[i].tensor_key = mask_keys[i];
    mask_requests[i].byte_offset = 1152 + i * 128;
    mask_requests[i].byte_length = 128;
    mask_requests[i].checksum_sha256 = checksum;
    mask_requests[i].source_position = WN_Get_Linenum(source_definition);
    mask_requests[i].generation_name =
        "fhe.ckks.stride_compaction.mask";
    mask_requests[i].generation_version = 1;
    mask_requests[i].geometry_manifest_sha256 = geometry_sha;
    mask_requests[i].stage_ordinal = 0;
    mask_requests[i].diagonal_ordinal = i;
    mask_requests[i].variant_signature_sha256 = variant_sha;
  }
  const UINT32 mask_nodes = DSL_IR_Image_Node_Count();
  for (UINT32 i = 0; i < 2; ++i) {
    mask_results[i].value_id = 99;
    mask_results[i].st = ST_IDX(99);
    mask_results[i].definition = source_definition;
  }
  if (VHO_FHE_CKKS_Materialize_Conv_Masks(
          pu, mask_requests, 2, checksum, variant_sha, mask_results, NULL) ||
      DSL_IR_Image_Node_Count() != mask_nodes)
    return Fail("geometry digest rejection before mutation");
  for (UINT32 i = 0; i < 2; ++i)
    if (mask_results[i].value_id != DSL_IR_VALUE_INVALID_ID ||
        mask_results[i].st != ST_IDX_ZERO ||
        mask_results[i].definition != NULL)
      return Fail("mask rejection result clearing");
  if (!VHO_FHE_CKKS_Materialize_Conv_Masks(
          pu, mask_requests, 2, geometry_sha, variant_sha,
          mask_results, stderr) ||
      !DSL_IR_Generated_External_Tensor_Validate_PU(pu, stderr))
    return Fail("generated mask materialization");

  DSL_IR_GENERATED_EXTERNAL_TENSOR_PROVENANCE provenance;
  if (!DSL_IR_Image_Get_Generated_External_Tensor_Provenance(
          PU_Info_proc_sym(pu), mask_results[1].value_id, &provenance) ||
      provenance.stage_ordinal != 0 || provenance.diagonal_ordinal != 1 ||
      strcmp(provenance.geometry_manifest_sha256, geometry_sha) != 0 ||
      strcmp(provenance.variant_signature_sha256, variant_sha) != 0)
    return Fail("generated-mask provenance query");

  DSL_GATEKEEPER_RESULT verification;
  memset(&verification, 0, sizeof(verification));
  if (!DSL_Gatekeeper_Verify_Program_Mode(
          pu, DSL_GATEKEEPER_ADMISSION, stderr, &verification))
    return Fail("post-materialization gatekeeper");
  DSL_BUILDER_MAPPED_IMAGE_REQUEST image;
  image.path = argv[1];
  image.flags = 0;
  if (!DSL_Builder_Finalize_Mapped_Image(&image))
    return Fail("mapped WHIRL output");
  printf("materialized %u typed rows and %u generated masks\n",
         (unsigned)row_results.size(), 2U);
  return 0;
}
