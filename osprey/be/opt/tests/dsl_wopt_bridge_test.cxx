/*
 * Copyright (C) 2026 Open64 Project
 */

#include <stdio.h>
#include <string.h>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "stab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "glob.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "config_opt.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "dsl_builder.h"
#include "dsl_gatekeeper.h"
#include "dsl_ir_image.h"
#include "opt_dsl.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void
Signal_Cleanup(INT sig)
{
}

const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
  return "";
}

static void
Initialize_Test_Context(void)
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

static TY_IDX
Create_Tensor_Type(void)
{
  DSL_BUILDER_TENSOR_TYPE_CORE core;
  DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
  memset(&core, 0, sizeof(core));
  memset(&descriptor, 0, sizeof(descriptor));
  core.kind = "tensor";
  core.dtype = "int32";
  core.rank = 2;
  core.logical_shape = "[2,2]";
  descriptor.type_core = core;
  descriptor.representation.layout = "row_major";
  return DSL_Builder_Intern_Tensor_Type
             ("wopt_tensor_i32_2x2", MTYPE_To_TY(MTYPE_I4), &descriptor);
}

int
main(int argc, char **argv)
{
  Initialize_Test_Context();
  if (!DSL_Builder_Begin_Program()) {
    fprintf(stderr, "failed to begin DSL builder program\n");
    return 1;
  }
  DSL_BUILDER_PROGRAM_UNIT pu =
      DSL_Builder_Create_Minimal_PU("dsl_wopt_bridge_test");
  if (pu == NULL || !DSL_Builder_Select_PU(pu)) {
    fprintf(stderr, "failed to create/select DSL builder PU\n");
    return 1;
  }
  Current_PU_Info = pu;

  BOOL factor_fixture = argc > 2 && strcmp(argv[2], "factor") == 0;
  BOOL no_factor_fixture =
      argc > 2 && strcmp(argv[2], "no-factor") == 0;
  BOOL divrem_fixture =
      argc > 2 && strcmp(argv[2], "divrem") == 0;
  BOOL algebra_fixture =
      factor_fixture || no_factor_fixture || divrem_fixture;
  TY_IDX ty = Create_Tensor_Type();
  DSL_BUILDER_VALUE x = NULL;
  DSL_BUILDER_VALUE y = NULL;
  DSL_BUILDER_VALUE z = NULL;
  DSL_BUILDER_SOURCE_POSITION position;
  memset(&position, 0, sizeof(position));
  if (argc > 1) {
    position.file_id = DSL_Builder_Register_Source_File
                           (pu, __FILE__);
    position.line = 1;
    position.statement_begin = 1;
    if (algebra_fixture && position.file_id != 0) {
      x = DSL_Builder_Declare_PU_Formal(pu, "wopt_x", 0, ty, &position);
      y = DSL_Builder_Declare_PU_Formal(pu, "wopt_y", 1, ty, &position);
      if (!divrem_fixture)
        z = DSL_Builder_Declare_PU_Formal(pu, "wopt_z", 2, ty, &position);
    }
    if (position.file_id == 0 ||
        (algebra_fixture && (x == NULL || y == NULL ||
                             (!divrem_fixture && z == NULL))) ||
        DSL_Builder_Declare_PU_Result
            (pu, "wopt_result", 0, ty, DSL_PU_RESULT_TENSOR,
             &position) == NULL) {
      fprintf(stderr, "failed to declare DSL WOPT fixture result\n");
      return 1;
    }
    if (divrem_fixture &&
        DSL_Builder_Declare_PU_Result
            (pu, "wopt_remainder", 1, ty, DSL_PU_RESULT_TENSOR,
             &position) == NULL) {
      fprintf(stderr, "failed to declare DSL DIVREM fixture result\n");
      return 1;
    }
  }
  DSL_Opcode_Register_Common_Substrate();
  DSL_DOMAIN_ID common = DSL_Domain_Find("common");
  DSL_OPCODE_ID add_opcode =
      DSL_Opcode_Find(common, DSL_OPCODE_COMMON_ADD, 1);
  DSL_OPCODE_ID mul_opcode =
      DSL_Opcode_Find(common, DSL_OPCODE_COMMON_MUL, 1);
  DSL_OPCODE_ID div_opcode =
      DSL_Opcode_Find(common, DSL_OPCODE_COMMON_DIV, 1);
  DSL_OPCODE_ID rem_opcode =
      DSL_Opcode_Find(common, DSL_OPCODE_COMMON_REM, 1);
  DSL_BUILDER_OPERATOR_ATTRIBUTE attribute = {
      "attr.broadcast_rule", "none"
  };
  Enable_WN_Simp = FALSE;
  DSL_BUILDER_VALUE zero = NULL;
  DSL_BUILDER_VALUE one = NULL;
  DSL_BUILDER_VALUE add = NULL;
  DSL_BUILDER_VALUE second_result = NULL;
  if (divrem_fixture) {
    DSL_BUILDER_VALUE divrem_kids[2] = { x, y };
    position.line = __LINE__ + 1;
    add = DSL_Builder_Create_Operator_With_Result
              (div_opcode, 1, divrem_kids, 2, &attribute, 1,
               "wopt_quotient", ty);
    if (add != NULL)
      DSL_Builder_Set_Value_Source_Position(add, &position);
    position.line = __LINE__ + 1;
    second_result = DSL_Builder_Create_Operator_With_Result
                        (rem_opcode, 1, divrem_kids, 2, &attribute, 1,
                         "wopt_remainder", ty);
    if (second_result != NULL)
      DSL_Builder_Set_Value_Source_Position(second_result, &position);
    if (add == NULL || second_result == NULL ||
        !DSL_Builder_Append_PU_Value(pu, add) ||
        !DSL_Builder_Append_PU_Value(pu, second_result)) {
      fprintf(stderr, "failed to create DSL DIVREM fixture\n");
      return 1;
    }
  } else if (algebra_fixture) {
    DSL_BUILDER_VALUE xy_kids[2] = { x, y };
    DSL_BUILDER_VALUE xz_kids[2] = {
        no_factor_fixture ? z : x, z
    };
    DSL_BUILDER_VALUE xy =
        DSL_Builder_Create_Operator_With_Result
            (mul_opcode, 1, xy_kids, 2, &attribute, 1, "wopt_xy", ty);
    DSL_BUILDER_VALUE xz =
        DSL_Builder_Create_Operator_With_Result
            (mul_opcode, 1, xz_kids, 2, &attribute, 1, "wopt_xz", ty);
    DSL_BUILDER_VALUE factor_kids[2] = { xy, xz };
    add = DSL_Builder_Create_Operator_With_Result
              (add_opcode, 1, factor_kids, 2, &attribute, 1,
               "wopt_factor", ty);
    if (x == NULL || y == NULL || z == NULL || xy == NULL || xz == NULL ||
        add == NULL || !DSL_Builder_Append_PU_Value(pu, xy) ||
        !DSL_Builder_Append_PU_Value(pu, xz) ||
        !DSL_Builder_Append_PU_Value(pu, add)) {
      fprintf(stderr, "failed to create DSL factorization fixture\n");
      return 1;
    }
  } else {
    zero = DSL_Builder_Create_Tensor_Constant
               ("wopt_zero", ty, "int32", 2, "[2,2]", "splat", "0");
    one = DSL_Builder_Create_Tensor_Constant
              ("wopt_one", ty, "int32", 2, "[2,2]", "splat", "1");
    DSL_BUILDER_VALUE kids[2] = { zero, one };
    add = DSL_Builder_Create_Operator_With_Result
              (add_opcode, 1, kids, 2, &attribute, 1, "wopt_add", ty);
    if (zero == NULL || one == NULL || add == NULL ||
        !DSL_Builder_Append_PU_Value(pu, zero) ||
        !DSL_Builder_Append_PU_Value(pu, one) ||
        !DSL_Builder_Append_PU_Value(pu, add)) {
      fprintf(stderr,
              "failed to create DSL fold fixture: zero=%p one=%p add=%p\n",
              zero, one, add);
      return 1;
    }
  }
  if (ty == TY_IDX_ZERO || add_opcode == DSL_OPCODE_INVALID_ID ||
      mul_opcode == DSL_OPCODE_INVALID_ID ||
      div_opcode == DSL_OPCODE_INVALID_ID ||
      rem_opcode == DSL_OPCODE_INVALID_ID) {
    fprintf(stderr, "failed to register DSL WOPT operators\n");
    return 1;
  }

  if (argc > 1) {
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    DSL_BUILDER_VALUE results[2] = { add, second_result };
    UINT32 result_count = divrem_fixture ? 2 : 1;
    if (!DSL_Builder_Return_PU_Values(pu, results, result_count)) {
      fprintf(stderr, "failed to return DSL WOPT fixture result\n");
      return 1;
    }
    request.path = argv[1];
    request.flags = 0;
    if (!DSL_Builder_Finalize_Mapped_Image(&request)) {
      fprintf(stderr, "failed to finalize DSL WOPT input image: %s\n",
              request.path);
      return 1;
    }
    const char *fixture =
        factor_fixture ? "factorization" :
        no_factor_fixture ? "no-factor" :
        divrem_fixture ? "divrem" : "fold";
    printf("wrote DSL WOPT %s input image: %s\n", fixture, request.path);
    return 0;
  }

  const char *owner = ST_name(St_Table[PU_Info_proc_sym(pu)]);
  WOPT_DSL_SEMANTIC_INFO zero_info;
  WOPT_DSL_SEMANTIC_INFO one_info;
  WOPT_DSL_SEMANTIC_INFO add_info;
  if (!WOPT_DSL_Import_Semantic_Info
          (WN_kid0(zero), DSL_Builder_Get_Value_Result_Symbol(zero),
           owner, &zero_info, stderr) ||
      !WOPT_DSL_Import_Semantic_Info
          (WN_kid0(one), DSL_Builder_Get_Value_Result_Symbol(one),
           owner, &one_info, stderr) ||
      !WOPT_DSL_Import_Semantic_Info
          (WN_kid0(add), DSL_Builder_Get_Value_Result_Symbol(add),
           owner, &add_info, stderr))
    return 1;

  TCON_IDX operand_tcon[2] = {
      zero_info.tensor_tcon_idx, one_info.tensor_tcon_idx
  };
  WOPT_DSL_SEMANTIC_INFO folded;
  if (add_info.logical_operator != OPR_DSLADD ||
      !WOPT_DSL_Fold_Compact_Tensors
          (&add_info, operand_tcon, 2, &folded, stderr) ||
      folded.logical_operator != OPR_DSLTENSORCONST ||
      folded.tensor_tcon_idx == TCON_IDX_ZERO) {
    fprintf(stderr, "WOPT DSL fold bridge failed\n");
    return 1;
  }

  WN *emitted = WOPT_DSL_Emit_WN
                    (&folded, WN_kid0(add),
                     DSL_Builder_Get_Value_Result_Symbol(add),
                     NULL, 0, stderr);
  DSL_LOGICAL_OPCODE logical;
  DSL_IR_NODE_RECORD node;
  if (emitted == NULL || WN_operator(emitted) != OPR_DSL ||
      WN_kid_count(emitted) != 0 ||
      !DSL_WN_Get_Logical_Opcode(emitted, &logical, stderr) ||
      logical.dsl_operator != OPR_DSLTENSORCONST ||
      !DSL_IR_Image_Get_Node(add_info.origin_node_id, &node) ||
      node.operand_count != 0) {
    fprintf(stderr,
            "WOPT DSL emission bridge failed: emitted=%p physical=%d "
            "kids=%d logical=%d node=%u operands=%u\n",
            emitted, emitted == NULL ? -1 : (INT)WN_operator(emitted),
            emitted == NULL ? -1 : (INT)WN_kid_count(emitted),
            (INT)logical.dsl_operator, add_info.origin_node_id,
            node.operand_count);
    return 1;
  }

  DSL_GATEKEEPER_RESULT gatekeeper;
  WN_kid0(add) = emitted;
  if (!DSL_Gatekeeper_Verify_PU(pu, stderr, &gatekeeper)) {
    fprintf(stderr, "WOPT DSL emitted PU failed gatekeeper\n");
    return 1;
  }

  printf("import.operator=%s.v%u\n",
         DSL_OPERATOR_name(add_info.logical_operator), add_info.version);
  printf("import.result_ty=%u attributes=%llx operands=%llx effect=%llu\n",
         (UINT32)add_info.result_ty,
         (unsigned long long)add_info.canonical_attribute_hash,
         (unsigned long long)add_info.operand_descriptor_hash,
         (unsigned long long)add_info.effect_identity);
  printf("fold.operands=tcon[%u],tcon[%u]\n",
         (UINT32)operand_tcon[0], (UINT32)operand_tcon[1]);
  printf("fold.result=%s.v%u tcon[%u]\n",
         DSL_OPERATOR_name(folded.logical_operator), folded.version,
         (UINT32)folded.tensor_tcon_idx);
  printf("emit.physical=private logical=%s.v%u kids=%u\n",
         DSL_OPERATOR_name(logical.dsl_operator),
         logical.effective_version, (UINT32)WN_kid_count(emitted));
  printf("image.node=%u operands=%u\n",
         add_info.origin_node_id, node.operand_count);
  printf("gatekeeper.native_nodes=%u results=%u errors=%u\n",
         gatekeeper.native_node_count, gatekeeper.result_symbol_count,
         gatekeeper.error_count);
  printf("WOPT DSL import/fold/emission bridge passed\n");
  return 0;
}
