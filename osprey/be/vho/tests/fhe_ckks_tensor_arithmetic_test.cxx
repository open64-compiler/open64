/*
 * Copyright (C) 2026 Open64 Project
 *
 * Certify tensor add and multiply as explicit CKKS WHIRL transformations.
 * Each invocation lowers one source value and retains a mapped binary image.
 * Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md and
 * doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
#include "srcpos.h"
#include "dsl_builder.h"
#include "dsl_opcode.h"
#include "dsl_ir_image.h"
#include "dsl_ckks_event.h"
#include "dsl_gatekeeper.h"
#include "fhe_image.h"
#include "fhe_plan.h"
#include "fhe_ckks_expand.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void Signal_Cleanup(INT) {}
const char *Host_Format_Parm(INT, MEM_PTR) { return ""; }

/* Initialize exactly the IR tables needed by the linked native producer. */
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

/* Keep one diagnostic boundary for malformed fixture or native preflight. */
static int
Fail(const char *stage)
{
  fprintf(stderr, "CKKS tensor arithmetic fixture failed: %s\n", stage);
  return 1;
}

/* Find the source's unique physical definition while its PU is selected. */
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

/* Give an existing tensor value one concrete value-specific CKKS state. */
static BOOL
Bind_Input_State(DSL_IR_VALUE_ID value_id,
                 DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor_id,
                 STR_IDX layout)
{
  DSL_FHE_CKKS_VALUE_STATE_RECORD state;
  DSL_FHE_CKKS_Value_State_Record_Init(&state);
  state.value_id = value_id;
  state.encryption_descriptor_id = descriptor_id;
  state.state_version = 1;
  state.scheme = DSL_FHE_SCHEME_CKKS;
  state.value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
  state.level = 8;
  state.scale_bits = 56;
  state.component_count = 2;
  state.precision_bits = 40;
  state.slot_count = 8;
  state.encrypted_layout_name = layout;
  return DSL_FHE_Plan_Add_CKKS_Value_State(&state) != 0;
}

/* Describe one state without confusing canonical tensor TY with level. */
static void
Set_Result_State(VHO_FHE_CKKS_STEP_STATE *result,
                 DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor_id,
                 UINT32 value_class, INT32 level, INT32 scale,
                 INT32 components, INT32 precision, UINT32 pending,
                 STR_IDX layout)
{
  DSL_FHE_CKKS_VALUE_STATE_RECORD *state = &result->state;
  DSL_FHE_CKKS_Value_State_Record_Init(state);
  state->encryption_descriptor_id = descriptor_id;
  state->state_version = 1;
  state->scheme = DSL_FHE_SCHEME_CKKS;
  state->value_class = value_class;
  state->level = level;
  state->scale_bits = scale;
  state->component_count = components;
  state->precision_bits = precision;
  state->slot_count = 8;
  state->encrypted_layout_name = layout;
  state->pending_actions = pending;
}

/* Each result is a logical CKKS tensor value with real source provenance. */
static void
Set_Step(DSL_CKKS_EXPANSION_STEP *step, DSL_OPERATOR op,
         const char *name, TY_IDX ty, SRCPOS position,
         const DSL_CKKS_EXPANSION_OPERAND *operands, UINT32 operand_count,
         const DSL_CKKS_EXPANSION_ATTRIBUTE *attrs, UINT32 attr_count)
{
  memset(step, 0, sizeof(*step));
  step->dsl_operator = op;
  step->version = 1;
  step->result_name = name;
  step->result_ty = ty;
  step->source_position = position;
  step->operands = operands;
  step->operand_count = operand_count;
  step->attributes = attrs;
  step->attribute_count = attr_count;
}

/* Existing IDs and prior-step indices are distinct native operand kinds. */
static DSL_CKKS_EXPANSION_OPERAND
Existing(DSL_IR_VALUE_ID value_id)
{
  DSL_CKKS_EXPANSION_OPERAND operand;
  memset(&operand, 0, sizeof(operand));
  operand.kind = DSL_CKKS_EXPANSION_EXISTING_VALUE;
  operand.value_id = value_id;
  return operand;
}

static DSL_CKKS_EXPANSION_OPERAND
Prior(UINT32 step_index)
{
  DSL_CKKS_EXPANSION_OPERAND operand;
  memset(&operand, 0, sizeof(operand));
  operand.kind = DSL_CKKS_EXPANSION_PRIOR_STEP;
  operand.step_index = step_index;
  return operand;
}

/* Interpret only this fixture's exact operators on clear two-slot vectors.
 * This is an independent algebra oracle, not CKKS encoding or execution. */
static BOOL
Check_Clear_Oracle(const DSL_CKKS_EXPANSION_STEP *steps,
                   UINT32 step_count, DSL_IR_VALUE_ID left_id,
                   DSL_IR_VALUE_ID right_id, BOOL add, BOOL plain)
{
  const double left[2] = { 1.25, -2.0 };
  const double right_cipher[2] = { 0.5, 3.0 };
  const double right_plain[2] = { 2.0, 2.0 };
  const double *right = plain ? right_plain : right_cipher;
  double results[3][2] = { { 0.0, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0 } };
  for (UINT32 step_index = 0; step_index < step_count; ++step_index) {
    const DSL_CKKS_EXPANSION_STEP &step = steps[step_index];
    if (step.operand_count == 0 || step.operand_count > 2)
      return FALSE;
    for (UINT32 slot = 0; slot < 2; ++slot) {
      double values[2] = { 0.0, 0.0 };
      for (UINT32 kid = 0; kid < step.operand_count; ++kid) {
        const DSL_CKKS_EXPANSION_OPERAND &operand = step.operands[kid];
        if (operand.kind == DSL_CKKS_EXPANSION_EXISTING_VALUE) {
          if (operand.value_id != left_id && operand.value_id != right_id)
            return FALSE;
          values[kid] = operand.value_id == left_id ?
              left[slot] : right[slot];
        } else if (operand.kind == DSL_CKKS_EXPANSION_PRIOR_STEP &&
                   operand.step_index < step_index) {
          values[kid] = results[operand.step_index][slot];
        } else {
          return FALSE;
        }
      }
      switch (step.dsl_operator) {
      case OPR_DSLCKKSENCODE:
      case OPR_DSLCKKSRELIN:
      case OPR_DSLCKKSRESCALE:
        results[step_index][slot] = values[0];
        break;
      case OPR_DSLCKKSADD:
        results[step_index][slot] = values[0] + values[1];
        break;
      case OPR_DSLCKKSMUL:
        results[step_index][slot] = values[0] * values[1];
        break;
      default:
        return FALSE;
      }
    }
  }
  for (UINT32 slot = 0; slot < 2; ++slot) {
    double expected = add ? left[slot] + right[slot] :
                            left[slot] * right[slot];
    if (results[step_count - 1][slot] != expected)
      return FALSE;
  }
  return TRUE;
}

/* Build, preflight, apply, and publish one of the three reviewed O0 recipes. */
int
main(int argc, char **argv)
{
  const BOOL before = argc == 4 && strcmp(argv[3], "--before") == 0;
  if ((argc != 3 && !before) || (strcmp(argv[1], "add") != 0 &&
                    strcmp(argv[1], "add_plain") != 0 &&
                    strcmp(argv[1], "mul_plain") != 0 &&
                    strcmp(argv[1], "mul_cipher") != 0))
    return Fail("expected add|add_plain|mul_plain|mul_cipher, .B path, optional --before");
  const BOOL add = strcmp(argv[1], "add") == 0 ||
                   strcmp(argv[1], "add_plain") == 0;
  const BOOL plain = strcmp(argv[1], "add_plain") == 0 ||
                     strcmp(argv[1], "mul_plain") == 0;
  Initialize_Test_Context();
  if (!DSL_Builder_Begin_Program() ||
      DSL_Opcode_Register_Common_Substrate() == 0 ||
      DSL_Opcode_Register_CKKS_Domain() != 9)
    return Fail("builder and opcode registry");

  DSL_BUILDER_TENSOR_DESCRIPTOR tensor;
  memset(&tensor, 0, sizeof(tensor));
  tensor.type_core.kind = "tensor";
  tensor.type_core.dtype = "float32";
  tensor.type_core.rank = 1;
  tensor.type_core.logical_shape = "[2]";
  TY_IDX ty = DSL_Builder_Intern_Tensor_Type(
      "ckks_arith_tensor", MTYPE_To_TY(MTYPE_F4), &tensor);
  DSL_BUILDER_PROGRAM_UNIT pu =
      DSL_Builder_Create_Minimal_PU("ckks_tensor_arithmetic");
  UINT32 file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
  DSL_BUILDER_PU_SOURCE_IDENTITY source_identity;
  memset(&source_identity, 0, sizeof(source_identity));
  source_identity.canonical_definition_name = "CKKSTensorArithmetic.forward";
  source_identity.defining_module = "fhe_ckks_tensor_arithmetic_test";
  source_identity.defining_file = __FILE__;
  source_identity.defining_line = __LINE__;
  DSL_PU_SOURCE_IDENTITY_RECORD identity;
  if (ty == TY_IDX_ZERO || pu == NULL || file_id == 0 ||
      !DSL_Builder_Set_PU_Source_Identity(pu, &source_identity) ||
      !DSL_Call_Image_Find_PU_Identity(PU_Info_proc_sym(pu), &identity))
    return Fail("canonical tensor and PU identity");

  DSL_BUILDER_VALUE left = DSL_Builder_Create_Model_Input("left", ty, 0);
  DSL_BUILDER_VALUE right = plain ?
      DSL_Builder_Create_Tensor_Constant(
          "right_plain", ty, "float32", 1, "[2]", "splat", "2") :
      DSL_Builder_Create_Model_Input("right", ty, 1);
  DSL_BUILDER_VALUE operands[2] = { left, right };
  DSL_BUILDER_OPERATOR_ATTRIBUTE source_attr = {
      "attr.broadcast_rule", "none"
  };
  DSL_BUILDER_VALUE source = DSL_Builder_Create_Operator_With_Result(
      DSL_Opcode_Find(DSL_Domain_Find("common"),
                      add ? "common.add" : "common.mul", 1),
      1, operands, 2, &source_attr, 1, "source_tensor_arithmetic", ty);
  DSL_BUILDER_SOURCE_POSITION position;
  memset(&position, 0, sizeof(position));
  position.file_id = file_id;
  position.line = __LINE__;
  position.column = 1;
  position.statement_begin = 1;
  DSL_BUILDER_VALUE values[3] = { left, right, source };
  for (UINT32 i = 0; i < 3; ++i) {
    ++position.line;
    if (values[i] == NULL ||
        !DSL_Builder_Set_Value_Source_Position(values[i], &position))
      return Fail("source tensor value position");
  }
  if (!DSL_Builder_Append_PU_Value(pu, source))
    return Fail("physical source tensor definition");
  WN *definition = Find_Definition(
      WN_func_body(PU_Info_tree_ptr(pu)),
      DSL_Builder_Get_Value_Result_Symbol(source));
  if (definition == NULL)
    return Fail("source WHIRL definition");
  const DSL_IR_VALUE_ID left_id = DSL_Builder_Get_Value_Image_Id(left);
  const DSL_IR_VALUE_ID right_id = DSL_Builder_Get_Value_Image_Id(right);
  const DSL_IR_VALUE_ID source_id = DSL_Builder_Get_Value_Image_Id(source);

  DSL_FHE_COMPILATION_CONFIG_RECORD config;
  DSL_FHE_Compilation_Config_Record_Init(&config);
  config.provenance_mask = 1;
  config.scheme = DSL_FHE_SCHEME_CKKS;
  config.security_level = DSL_FHE_SECURITY_128_CLASSIC;
  config.ring_dimension = 65536;
  config.multiplicative_depth_policy = DSL_FHE_POLICY_AUTO;
  config.scale_bits = 56;
  config.first_modulus_bits = 60;
  config.slot_count_policy = DSL_FHE_POLICY_AUTO;
  config.key_switch_policy = 1;
  config.bootstrap_policy = DSL_FHE_BOOTSTRAP_AUTO;
  config.backend_policy = DSL_FHE_BACKEND_AUTO;
  DSL_FHE_CONFIG_ID config_id = DSL_FHE_Intern_Compilation_Config(&config);
  STR_IDX key_name = Save_Str("request_key");
  STR_IDX layout = Save_Str("ckks.packed");
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD descriptor;
  DSL_FHE_Encryption_Descriptor_Record_Init(&descriptor);
  descriptor.value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
  descriptor.scheme = DSL_FHE_SCHEME_CKKS;
  descriptor.config_id = config_id;
  descriptor.key_set_name = key_name;
  descriptor.slot_count_policy = DSL_FHE_POLICY_AUTO;
  descriptor.slot_count = 8;
  descriptor.encoding_policy = DSL_FHE_ENCODING_NONE;
  descriptor.packing_policy = DSL_FHE_PACKING_AUTO;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_ID cipher_id =
      DSL_FHE_Intern_Encryption_Descriptor(&descriptor);
  descriptor.value_class = DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT;
  descriptor.encoding_policy = DSL_FHE_ENCODING_CKKS_PACKED;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_ID plain_id =
      DSL_FHE_Intern_Encryption_Descriptor(&descriptor);
  if (config_id == 0 || cipher_id == 0 || plain_id == 0 ||
      DSL_FHE_Intern_Tensor_Binding(ty, cipher_id, 0) == 0 ||
      DSL_FHE_Intern_Tensor_Binding(ty, plain_id, 0) == 0 ||
      !Bind_Input_State(left_id, cipher_id, layout) ||
      (!plain && !Bind_Input_State(right_id, cipher_id, layout)))
    return Fail("FHE descriptors and input states");
  if (!add && !plain) {
    DSL_FHE_KEY_REQUIREMENT_RECORD key;
    DSL_FHE_Key_Requirement_Record_Init(&key);
    key.config_id = config_id;
    key.key_set_name = key_name;
    key.key_class = DSL_FHE_KEY_RELINEARIZATION;
    if (DSL_FHE_Intern_Key_Requirement(&key) == 0)
      return Fail("relinearization key requirement");
  }
  if (before) {
    DSL_GATEKEEPER_RESULT verification;
    memset(&verification, 0, sizeof(verification));
    if (!DSL_Gatekeeper_Verify_Program_Mode(
            pu, DSL_GATEKEEPER_ADMISSION, stderr, &verification))
      return Fail("source gatekeeper admission");
    DSL_BUILDER_MAPPED_IMAGE_REQUEST image;
    image.path = argv[2];
    image.flags = 0;
    if (!DSL_Builder_Finalize_Mapped_Image(&image))
      return Fail("source mapped WHIRL output");
    printf("%s: source common tensor operator before CKKS expansion\n", argv[1]);
    return 0;
  }

  DSL_CKKS_EXPANSION_STEP steps[3];
  DSL_CKKS_EXPANSION_OPERAND step_operands[3][2];
  VHO_FHE_CKKS_STEP_STATE states[3];
  memset(steps, 0, sizeof(steps));
  memset(step_operands, 0, sizeof(step_operands));
  memset(states, 0, sizeof(states));
  const SRCPOS source_position = WN_Get_Linenum(definition);
  DSL_CKKS_EXPANSION_ATTRIBUTE relin_attr = {
      "attr.key_id", "request_key"
  };
  DSL_CKKS_EXPANSION_ATTRIBUTE rescale_attrs[2] = {
      { "attr.levels", "1" },
      { "attr.target_scale_bits", "56" }
  };
  UINT32 step_count = add ? (plain ? 2 : 1) : 3;
  if (add && plain) {
    step_operands[0][0] = Existing(right_id);
    Set_Step(&steps[0], OPR_DSLCKKSENCODE,
             "tensor_ckks_encoded_plain", ty, source_position,
             step_operands[0], 1, NULL, 0);
    Set_Result_State(&states[0], plain_id,
                     DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT,
                     8, 56, 1, 40, 0, layout);
    step_operands[1][0] = Existing(left_id);
    step_operands[1][1] = Prior(0);
    Set_Step(&steps[1], OPR_DSLCKKSADD,
             "tensor_ckks_plain_add", ty, source_position,
             step_operands[1], 2, NULL, 0);
    Set_Result_State(&states[1], cipher_id,
                     DSL_FHE_VALUE_CLASS_CIPHERTEXT,
                     8, 56, 2, 40, 0, layout);
  } else if (add) {
    step_operands[0][0] = Existing(left_id);
    step_operands[0][1] = Existing(right_id);
    Set_Step(&steps[0], OPR_DSLCKKSADD, "tensor_ckks_add", ty,
             source_position, step_operands[0], 2, NULL, 0);
    Set_Result_State(&states[0], cipher_id,
                     DSL_FHE_VALUE_CLASS_CIPHERTEXT,
                     8, 56, 2, 40, 0, layout);
  } else {
    if (plain) {
      step_operands[0][0] = Existing(right_id);
      Set_Step(&steps[0], OPR_DSLCKKSENCODE,
               "tensor_ckks_encoded_plain", ty, source_position,
               step_operands[0], 1, NULL, 0);
      Set_Result_State(&states[0], plain_id,
                       DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT,
                       8, 56, 1, 40, 0, layout);
      step_operands[1][1] = Prior(0);
    } else {
      step_operands[0][0] = Existing(left_id);
      step_operands[0][1] = Existing(right_id);
      Set_Step(&steps[0], OPR_DSLCKKSMUL,
               "tensor_ckks_cipher_mul", ty, source_position,
               step_operands[0], 2, NULL, 0);
      Set_Result_State(&states[0], cipher_id,
                       DSL_FHE_VALUE_CLASS_CIPHERTEXT,
                       8, 112, 3, 38,
                       DSL_FHE_CKKS_PENDING_RESCALE |
                           DSL_FHE_CKKS_PENDING_RELINEARIZE, layout);
      step_operands[1][0] = Prior(0);
    }
    if (plain) {
      step_operands[1][0] = Existing(left_id);
      Set_Step(&steps[1], OPR_DSLCKKSMUL,
               "tensor_ckks_plain_mul", ty, source_position,
               step_operands[1], 2, NULL, 0);
      Set_Result_State(&states[1], cipher_id,
                       DSL_FHE_VALUE_CLASS_CIPHERTEXT,
                       8, 112, 2, 38, DSL_FHE_CKKS_PENDING_RESCALE,
                       layout);
    } else {
      Set_Step(&steps[1], OPR_DSLCKKSRELIN,
               "tensor_ckks_relin", ty, source_position,
               step_operands[1], 1, &relin_attr, 1);
      Set_Result_State(&states[1], cipher_id,
                       DSL_FHE_VALUE_CLASS_CIPHERTEXT,
                       8, 112, 2, 37, DSL_FHE_CKKS_PENDING_RESCALE,
                       layout);
    }
    step_operands[2][0] = Prior(1);
    Set_Step(&steps[2], OPR_DSLCKKSRESCALE,
             "tensor_ckks_rescale", ty, source_position,
             step_operands[2], 1, rescale_attrs, 2);
    Set_Result_State(&states[2], cipher_id,
                     DSL_FHE_VALUE_CLASS_CIPHERTEXT,
                     7, 56, 2, 36, 0, layout);
  }

  DSL_CKKS_EXPANSION_GROUP group;
  memset(&group, 0, sizeof(group));
  group.source_static_ordinal = 1;
  group.origin_static_ordinal = 1;
  group.first_step = 0;
  group.step_count = step_count;
  DSL_CKKS_EXPANSION_CONTEXT context;
  memset(&context, 0, sizeof(context));
  context.context_pu_identity_id = identity.id;
  context.origin_owner_pu_st = PU_Info_proc_sym(pu);
  context.origin_source_value_id = source_id;
  DSL_CKKS_EXPANSION_REQUEST request;
  memset(&request, 0, sizeof(request));
  request.source_definition = definition;
  request.source_value_id = source_id;
  request.expected_source_operator = add ? OPR_DSLADD : OPR_DSLMUL;
  request.expected_source_version = 1;
  request.groups = &group;
  request.group_count = 1;
  request.steps = steps;
  request.step_count = step_count;
  request.contexts = &context;
  request.context_count = 1;
  request.final_step_index = step_count - 1;
  if (!Check_Clear_Oracle(
          steps, step_count, left_id, right_id, add, plain))
    return Fail("independent two-slot clear tensor oracle");
  fprintf(stderr, "clear two-slot algebra oracle passed\n");

  const UINT32 original_nodes = DSL_IR_Image_Node_Count();
  const UINT32 original_events = DSL_CKKS_Event_Image_Count();
  STR_IDX good_layout = states[0].state.encrypted_layout_name;
  states[0].state.encrypted_layout_name = Save_Str("wrong.layout");
  if (VHO_FHE_CKKS_Can_Expand_And_Bind_States(
          pu, &request, states, step_count, NULL) ||
      DSL_IR_Image_Node_Count() != original_nodes ||
      DSL_CKKS_Event_Image_Count() != original_events)
    return Fail("layout mismatch must reject before native mutation");
  fprintf(stderr, "rejected mismatched CKKS layout without mutation\n");
  states[0].state.encrypted_layout_name = good_layout;
  if (!add) {
    UINT32 final_index = request.final_step_index;
    request.final_step_index = plain ? 1 : 0;
    if (VHO_FHE_CKKS_Can_Expand_And_Bind_States(
            pu, &request, states, step_count, NULL) ||
        DSL_IR_Image_Node_Count() != original_nodes ||
        DSL_CKKS_Event_Image_Count() != original_events)
      return Fail("unrepaired multiplication must not be terminal");
    fprintf(stderr, "rejected unrepaired terminal multiplication\n");
    request.final_step_index = final_index;
  }
  if (!add && !plain) {
    relin_attr.value = "wrong_key";
    if (VHO_FHE_CKKS_Can_Expand_And_Bind_States(
            pu, &request, states, step_count, NULL) ||
        DSL_IR_Image_Node_Count() != original_nodes ||
        DSL_CKKS_Event_Image_Count() != original_events)
      return Fail("wrong relinearization key must not mutate WHIRL");
    fprintf(stderr, "rejected wrong relinearization key without mutation\n");
    relin_attr.value = "request_key";
  }
  DSL_CKKS_EXPANSION_STEP_RESULT results[3];
  memset(results, 0, sizeof(results));
  if (!VHO_FHE_CKKS_Expand_And_Bind_States(
          pu, &request, states, step_count, stderr, results) ||
      results[step_count - 1].value_id == 0 ||
      DSL_IR_Image_Node_Count() != original_nodes + step_count ||
      DSL_CKKS_Event_Image_Count() != original_events + step_count ||
      !DSL_CKKS_Event_Image_Validate(stderr) ||
      !DSL_IR_Image_Validate(stderr))
    return Fail("atomic native expansion and CKKS state binding");
  DSL_FHE_CKKS_VALUE_STATE_RECORD final_state;
  if (!DSL_FHE_Plan_Find_Latest_CKKS_Value_State(
          results[step_count - 1].value_id, &final_state) ||
      final_state.level != (add ? 8 : 7) ||
      final_state.scale_bits != 56 || final_state.pending_actions != 0)
    return Fail("concrete final CKKS state");
  DSL_GATEKEEPER_RESULT verification;
  memset(&verification, 0, sizeof(verification));
  if (!DSL_Gatekeeper_Verify_Program_Mode(
          pu, DSL_GATEKEEPER_ADMISSION, stderr, &verification))
    return Fail("post-transformation gatekeeper admission");
  DSL_BUILDER_MAPPED_IMAGE_REQUEST image;
  image.path = argv[2];
  image.flags = 0;
  if (!DSL_Builder_Finalize_Mapped_Image(&image))
    return Fail("mapped WHIRL output");
  printf("%s: %u native CKKS steps, final value=%u level=%d\n",
         argv[1], step_count, results[step_count - 1].value_id,
         final_state.level);
  return 0;
}
