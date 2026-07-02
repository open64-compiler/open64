/*
 * Smoke test for representing a common.add DSL operation with compatible
 * WHIRL nodes and printing it through the standard fdump_tree interface.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "wn_util.h"
#include "dsl_contract.h"
#include "dsl_domain.h"
#include "dsl_opcode.h"
#include "stab.h"
#include "symtab.h"
#include "symtab_utils.h"
#include "ir_reader.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void
Signal_Cleanup(INT sig) {}

const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
  return "";
}

static char *Read_File(FILE *fp);
static const char *Trace_File_Path(void);
static int Write_Common_Add_Trace(const char *tree_text,
				  const char *annotation_text);
static int Check_DSL_Domain_Registry(void);
static int Check_DSL_Contract_Registry(void);
static int Check_DSL_Opcode_Registry(void);
static int Check_Tensor_Required_Attribute_Diagnostics(void);
static int Check_VHO_Unconsumed_DSL_Scanner(WN *tree);

static void
Initialize_Test_Context(void)
{
  MEM_Initialize();
  Set_Error_Tables(Phases, host_errlist);
  Init_Error_Handler(10);
  Set_Error_File(NULL);
  Set_Error_Line(ERROR_LINE_UNKNOWN);

  Initialize_Symbol_Tables(FALSE);
  New_Scope(GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
}

static WN *
Create_Common_Add_Test_Tree(WN **dsl_marker_out)
{
  WN *block = WN_CreateBlock();
  WN *const_i32_2x2_zero =
    DSL_WN_Create_Tensor_Const ("const_i32_2x2_zero",
			     "int32",
			     2,
			     "[2,2]",
			     "splat",
			     "0");
  WN *const_i32_2x2_one =
    DSL_WN_Create_Tensor_Const ("const_i32_2x2_one",
			     "int32",
			     2,
			     "[2,2]",
			     "splat",
			     "1");
  WN *dsl_marker =
    DSL_WN_Create_Opcode (DSL_OPCODE_COMMON_ADD,
			1,
			"kid0=const_i32_2x2_zero;kid1=const_i32_2x2_one;attr.broadcast_rule=none");

  if (dsl_marker_out != NULL)
    *dsl_marker_out = dsl_marker;

  WN_INSERT_BlockLast(block, const_i32_2x2_zero);
  WN_INSERT_BlockLast(block, const_i32_2x2_one);
  WN_INSERT_BlockLast(block, dsl_marker);

  WN *kid0 = WN_CreateIntconst(OPR_INTCONST, MTYPE_I4, MTYPE_V, 1);
  WN *kid1 = WN_CreateIntconst(OPR_INTCONST, MTYPE_I4, MTYPE_V, 2);
  WN *add = WN_Add(MTYPE_I4, kid0, kid1);
  WN_INSERT_BlockLast(block, WN_CreateEval(add));

  return block;
}

static int
Check_Common_Add_Annotation(WN *dsl_marker)
{
  DSL_OPCODE_ANNOTATION annotation;
  int failed = 0;

  if (!DSL_WN_Get_Opcode_Annotation (dsl_marker, &annotation)) {
    fprintf(stderr, "common.add marker was not decoded as a DSL opcode\n");
    return 1;
  }

  if (annotation.name_len != strlen(DSL_OPCODE_COMMON_ADD) ||
      strncmp(annotation.name, DSL_OPCODE_COMMON_ADD, annotation.name_len) != 0) {
    fprintf(stderr, "decoded DSL opcode name is not common.add\n");
    failed = 1;
  }

  if (annotation.version != 1) {
    fprintf(stderr, "decoded common.add DSL opcode version is not 1\n");
    failed = 1;
  }

  if (strcmp(annotation.payload,
	     "kid0=const_i32_2x2_zero;kid1=const_i32_2x2_one;attr.broadcast_rule=none") != 0) {
    fprintf(stderr, "decoded common.add DSL opcode payload changed\n");
    failed = 1;
  }

  return failed;
}

static int
Check_Tensor_Const_Annotation(WN *tensor_const, const char *name,
			      const char *value)
{
  DSL_OPCODE_ANNOTATION annotation;
  int failed = 0;

  if (!DSL_WN_Get_Opcode_Annotation (tensor_const, &annotation)) {
    fprintf(stderr, "tensor_const marker did not decode\n");
    return 1;
  }

  if (annotation.name_len != strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
      strncmp(annotation.name, DSL_OPCODE_COMMON_TENSOR_CONST,
	      annotation.name_len) != 0) {
    fprintf(stderr, "decoded DSL opcode name is not common.tensor_const\n");
    failed = 1;
  }

  if (annotation.version != 1) {
    fprintf(stderr, "decoded tensor_const DSL opcode version is not 1\n");
    failed = 1;
  }

  if (strstr(annotation.payload, name) == NULL ||
      strstr(annotation.payload, "dtype=int32") == NULL ||
      strstr(annotation.payload, "rank=2") == NULL ||
      strstr(annotation.payload, "shape=[2,2]") == NULL ||
      strstr(annotation.payload, "value_kind=splat") == NULL ||
      strstr(annotation.payload, value) == NULL) {
    fprintf(stderr, "decoded tensor_const payload changed\n");
    failed = 1;
  }

  return failed;
}

static int
Check_Common_Add_On_Tensor_Constants(void)
{
  WN *const_i32_2x2_zero =
    DSL_WN_Create_Tensor_Const ("const_i32_2x2_zero", "int32", 2, "[2,2]", "splat", "0");
  WN *const_i32_2x2_one =
    DSL_WN_Create_Tensor_Const ("const_i32_2x2_one", "int32", 2, "[2,2]", "splat", "1");
  WN *common_add =
    DSL_WN_Create_Opcode (DSL_OPCODE_COMMON_ADD,
			1,
			"kid0=const_i32_2x2_zero;kid1=const_i32_2x2_one;attr.broadcast_rule=none");
  DSL_OPCODE_ANNOTATION annotation;
  FILE *dump = tmpfile();
  char *text;
  int failed = 0;

  failed |= Check_Tensor_Const_Annotation(const_i32_2x2_zero, "name=const_i32_2x2_zero",
					  "value=0");
  failed |= Check_Tensor_Const_Annotation(const_i32_2x2_one, "name=const_i32_2x2_one",
					  "value=1");

  if (!DSL_WN_Get_Opcode_Annotation (common_add, &annotation)) {
    fprintf(stderr, "common.add tensor-constant marker did not decode\n");
    return 1;
  }

  if (annotation.name_len != strlen(DSL_OPCODE_COMMON_ADD) ||
      strncmp(annotation.name, DSL_OPCODE_COMMON_ADD, annotation.name_len) != 0) {
    fprintf(stderr, "tensor-constant DSL opcode name is not common.add\n");
    failed = 1;
  }

  if (annotation.version != 1) {
    fprintf(stderr, "tensor-constant common.add version is not 1\n");
    failed = 1;
  }

  if (strcmp(annotation.payload,
	     "kid0=const_i32_2x2_zero;kid1=const_i32_2x2_one;attr.broadcast_rule=none") != 0) {
    fprintf(stderr, "tensor-constant common.add payload changed\n");
    failed = 1;
  }

  if (dump == NULL) {
    perror("tmpfile");
    return 1;
  }

  fdump_tree(dump, common_add);
  text = Read_File(dump);
  fclose(dump);

  if (text == NULL) {
    fprintf(stderr, "failed to read common.add tensor-constant dump\n");
    return 1;
  }

  fputs(text, stdout);

  if (strstr(text, "__WHIRL_DSL__:opcode:common.add:v1:") == NULL) {
    fprintf(stderr, "common.add tensor-constant marker was not printed\n");
    failed = 1;
  }

  free(text);
  return failed;
}

static int
Check_Zero_Initializer_Operators(void)
{
  WN *zero_init = DSL_WN_Create_Zero_Init ("zero_init", "int32");
  WN *zero_like = DSL_WN_Create_Zero_Like ("zero_like_input",
					"const_i32_2x2_zero");
  DSL_OPCODE_ANNOTATION annotation;
  int failed = 0;

  if (!DSL_WN_Get_Opcode_Annotation (zero_init, &annotation)) {
    fprintf(stderr, "zero_init marker did not decode\n");
    failed = 1;
  } else {
    if (annotation.name_len != strlen(DSL_OPCODE_COMMON_ZERO_INIT) ||
	strncmp(annotation.name, DSL_OPCODE_COMMON_ZERO_INIT,
		annotation.name_len) != 0) {
      fprintf(stderr, "decoded DSL opcode name is not common.zero_init\n");
      failed = 1;
    }
    if (strstr(annotation.payload, "name=zero_init") == NULL ||
	strstr(annotation.payload, "dtype_hint=int32") == NULL ||
	strstr(annotation.payload, "descriptor_state=infer") == NULL) {
      fprintf(stderr, "decoded zero_init payload changed\n");
      failed = 1;
    }
  }

  if (!DSL_WN_Get_Opcode_Annotation (zero_like, &annotation)) {
    fprintf(stderr, "zero_like marker did not decode\n");
    failed = 1;
  } else {
    if (annotation.name_len != strlen(DSL_OPCODE_COMMON_ZERO_LIKE) ||
	strncmp(annotation.name, DSL_OPCODE_COMMON_ZERO_LIKE,
		annotation.name_len) != 0) {
      fprintf(stderr, "decoded DSL opcode name is not common.zero_like\n");
      failed = 1;
    }
    if (strstr(annotation.payload, "name=zero_like_input") == NULL ||
	strstr(annotation.payload, "source=const_i32_2x2_zero") == NULL ||
	strstr(annotation.payload, "descriptor_state=copy_source") == NULL) {
      fprintf(stderr, "decoded zero_like payload changed\n");
      failed = 1;
    }
  }

  return failed;
}

static int
Check_Tensor_Dsl_Symtab_Print(void)
{
  TY_IDX tensor_ty =
    TY_Create_Tensor_Extension_Type("tensor_i32_2x2",
				    MTYPE_To_TY(MTYPE_I4),
				    2);
  ST *tensor_st = New_ST();
  FILE *dump = tmpfile();
  char *text;
  int failed = 0;
  int saw_dtype = 0;
  int saw_shape = 0;
  int saw_layout = 0;
  int saw_lineage_pending = 0;
  int saw_source_layer = 0;
  int saw_lowering_hint_pending = 0;

  if (strcmp(TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_DTYPE),
	     "dtype") != 0 ||
      strcmp(TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_SHAPE),
	     "shape") != 0 ||
      strcmp(TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_LAYOUT),
	     "layout") != 0 ||
      strcmp(TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME),
	     "source_layer_name") != 0) {
    fprintf(stderr, "tensor schema key names changed\n");
    failed = 1;
  }

  TY_tensor_bind_attribute(tensor_ty, TY_TENSOR_SCHEMA_DTYPE, "int32");
  TY_tensor_bind_attribute(tensor_ty, TY_TENSOR_SCHEMA_SHAPE, "[2,2]");
  TY_tensor_bind_attribute(tensor_ty, TY_TENSOR_SCHEMA_LAYOUT, "row_major");
  TY_tensor_declare_attribute(tensor_ty, TY_TENSOR_SCHEMA_LINEAGE);

  if (!TY_tensor_attribute_is_bound(tensor_ty, TY_TENSOR_SCHEMA_DTYPE) ||
      strcmp(TY_tensor_attribute(tensor_ty, TY_TENSOR_SCHEMA_DTYPE),
	     "int32") != 0) {
    fprintf(stderr, "tensor schema enum attribute lookup failed\n");
    failed = 1;
  }

  ST_Init(tensor_st, Save_Str("tensor_tmp"), CLASS_VAR, SCLASS_UGLOBAL,
	  EXPORT_LOCAL, tensor_ty);
  ST_tensor_bind_metadata(ST_st_idx(*tensor_st),
			  TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME,
			  "dsl_common_add_print_test");
  ST_tensor_declare_metadata(ST_st_idx(*tensor_st),
			     TY_TENSOR_SCHEMA_LOWERING_HINT);

  if (!ST_tensor_metadata_is_bound(ST_st_idx(*tensor_st),
				   TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME) ||
      strcmp(ST_tensor_metadata(ST_st_idx(*tensor_st),
				TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME),
	     "dsl_common_add_print_test") != 0) {
    fprintf(stderr, "tensor schema enum metadata lookup failed\n");
    failed = 1;
  }

  if (TY_tensor_attribute_count(tensor_ty) != 4) {
    fprintf(stderr, "tensor attribute iterator count changed\n");
    failed = 1;
  }

  for (UINT32 i = 0; i < TY_tensor_attribute_count(tensor_ty); ++i) {
    const char *key = NULL;
    const char *value = NULL;
    TY_DSL_BIND_STATE state = TY_DSL_BIND_PENDING;

    if (!TY_tensor_attribute_at(tensor_ty, i, &key, &value, &state)) {
      fprintf(stderr, "tensor attribute iterator stopped early\n");
      failed = 1;
      continue;
    }

    if (strcmp(key, "dtype") == 0 && strcmp(value, "int32") == 0 &&
	state == TY_DSL_BIND_BOUND)
      saw_dtype = 1;
    if (strcmp(key, "shape") == 0 && strcmp(value, "[2,2]") == 0 &&
	state == TY_DSL_BIND_BOUND)
      saw_shape = 1;
    if (strcmp(key, "layout") == 0 && strcmp(value, "row_major") == 0 &&
	state == TY_DSL_BIND_BOUND)
      saw_layout = 1;
    if (strcmp(key, "lineage") == 0 && value == NULL &&
	state == TY_DSL_BIND_PENDING)
      saw_lineage_pending = 1;
  }

  if (!saw_dtype || !saw_shape || !saw_layout || !saw_lineage_pending) {
    fprintf(stderr, "tensor attribute iterator missed an expected entry\n");
    failed = 1;
  }

  if (TY_tensor_attribute_at(tensor_ty, TY_tensor_attribute_count(tensor_ty),
			     NULL, NULL, NULL)) {
    fprintf(stderr, "tensor attribute iterator accepted out-of-range ordinal\n");
    failed = 1;
  }

  if (ST_tensor_metadata_count(ST_st_idx(*tensor_st)) != 2) {
    fprintf(stderr, "tensor metadata iterator count changed\n");
    failed = 1;
  }

  for (UINT32 i = 0; i < ST_tensor_metadata_count(ST_st_idx(*tensor_st)); ++i) {
    const char *key = NULL;
    const char *value = NULL;
    TY_DSL_BIND_STATE state = TY_DSL_BIND_PENDING;

    if (!ST_tensor_metadata_at(ST_st_idx(*tensor_st), i, &key, &value,
			       &state)) {
      fprintf(stderr, "tensor metadata iterator stopped early\n");
      failed = 1;
      continue;
    }

    if (strcmp(key, "source_layer_name") == 0 &&
	strcmp(value, "dsl_common_add_print_test") == 0 &&
	state == TY_DSL_BIND_BOUND)
      saw_source_layer = 1;
    if (strcmp(key, "lowering_hint") == 0 && value == NULL &&
	state == TY_DSL_BIND_PENDING)
      saw_lowering_hint_pending = 1;
  }

  if (!saw_source_layer || !saw_lowering_hint_pending) {
    fprintf(stderr, "tensor metadata iterator missed an expected entry\n");
    failed = 1;
  }

  if (ST_tensor_metadata_at(ST_st_idx(*tensor_st),
			    ST_tensor_metadata_count(ST_st_idx(*tensor_st)),
			    NULL, NULL, NULL)) {
    fprintf(stderr, "tensor metadata iterator accepted out-of-range ordinal\n");
    failed = 1;
  }

  if (dump == NULL) {
    perror("tmpfile");
    return 1;
  }

  Print_global_symtab(dump);
  text = Read_File(dump);
  fclose(dump);

  if (text == NULL) {
    fprintf(stderr, "failed to read tensor DSL symtab dump\n");
    return 1;
  }

  if (strstr(text, "DSL Tensor Type Extensions:") == NULL ||
      strstr(text, "rank=2") == NULL ||
      strstr(text, "dtype = int32") == NULL ||
      strstr(text, "shape = [2,2]") == NULL ||
      strstr(text, "layout = row_major") == NULL) {
    fprintf(stderr, "missing tensor descriptor attributes in symtab dump\n");
    failed = 1;
  }

  if (strstr(text, "DSL Tensor Symbol Metadata:") == NULL ||
      strstr(text, "source_layer_name = dsl_common_add_print_test") == NULL) {
    fprintf(stderr, "missing tensor compiler metadata in symtab dump\n");
    failed = 1;
  }

  free(text);
  return failed;
}

static int
Check_DSL_Domain_Registry(void)
{
  DSL_DOMAIN_ID common_id;
  DSL_DOMAIN_ID cnn_id;
  DSL_DOMAIN_ID transformer_id;
  DSL_DOMAIN_INFO info;
  FILE *dump = tmpfile();
  char *text;
  int failed = 0;

  DSL_Domain_Registry_Reset();

  common_id = DSL_Domain_Register("common", DSL_DOMAIN_INVALID_ID, 1, 0);
  cnn_id = DSL_Domain_Register("cnn", common_id, 1, 0);
  transformer_id = DSL_Domain_Register("transformer", common_id, 1, 0);

  if (common_id == DSL_DOMAIN_INVALID_ID ||
      cnn_id == DSL_DOMAIN_INVALID_ID ||
      transformer_id == DSL_DOMAIN_INVALID_ID) {
    fprintf(stderr, "DSL domain registration failed\n");
    failed = 1;
  }

  if (DSL_Domain_Register("common", DSL_DOMAIN_INVALID_ID, 99, 0) !=
      common_id) {
    fprintf(stderr, "DSL domain duplicate registration changed id\n");
    failed = 1;
  }

  if (DSL_Domain_Count() != 3 ||
      !DSL_Domain_Is_Registered("cnn") ||
      DSL_Domain_Find("transformer") != transformer_id ||
      strcmp(DSL_Domain_Name(cnn_id), "cnn") != 0) {
    fprintf(stderr, "DSL domain registry lookup failed\n");
    failed = 1;
  }

  if (!DSL_Domain_Get_Info(transformer_id, &info) ||
      info.parent_id != common_id ||
      strcmp(info.name, "transformer") != 0 ||
      info.version != 1) {
    fprintf(stderr, "DSL domain registry info changed\n");
    failed = 1;
  }

  if (!DSL_Domain_At(1, &info) ||
      info.id != cnn_id ||
      DSL_Domain_At(DSL_Domain_Count(), &info)) {
    fprintf(stderr, "DSL domain registry iteration failed\n");
    failed = 1;
  }

  if (DSL_Domain_Register("bad.child", 9999, 1, 0) !=
      DSL_DOMAIN_INVALID_ID) {
    fprintf(stderr, "DSL domain registry accepted invalid parent\n");
    failed = 1;
  }

  if (DSL_Domain_Register("", DSL_DOMAIN_INVALID_ID, 1, 0) !=
      DSL_DOMAIN_INVALID_ID) {
    fprintf(stderr, "DSL domain registry accepted empty name\n");
    failed = 1;
  }

  if (dump == NULL) {
    perror("tmpfile");
    return 1;
  }

  DSL_Domain_fprint_registry(dump);
  text = Read_File(dump);
  fclose(dump);

  if (text == NULL) {
    fprintf(stderr, "failed to read DSL domain registry dump\n");
    return 1;
  }

  if (strstr(text, "DSL Domain Registry: entries=3") == NULL ||
      strstr(text, "name=common") == NULL ||
      strstr(text, "name=cnn") == NULL ||
      strstr(text, "name=transformer") == NULL) {
    fprintf(stderr, "DSL domain registry dump changed\n");
    failed = 1;
  }

  free(text);
  return failed;
}

static int
Check_DSL_Contract_Registry(void)
{
  static const char *required_checks[] = {
    "tensor_descriptor_complete",
    "layout_compatible",
    "domain_wrapper_visible"
  };
  static const char *diagnostic_codes[] = {
    "DCONTRACT001",
    "DCONTRACT002"
  };
  DSL_DOMAIN_ID common_id;
  DSL_DOMAIN_ID cnn_id;
  DSL_CONTRACT_ID contract_id;
  DSL_CONTRACT_INFO info;
  FILE *dump = tmpfile();
  char *text;
  int failed = 0;

  DSL_Contract_Registry_Reset();
  DSL_Domain_Registry_Reset();

  common_id = DSL_Domain_Register("common", DSL_DOMAIN_INVALID_ID, 1, 0);
  cnn_id = DSL_Domain_Register("cnn", common_id, 1, 0);

  contract_id =
    DSL_Contract_Register("residual_add_common_contract",
			  cnn_id,
			  common_id,
			  1,
			  0,
			  required_checks,
			  sizeof(required_checks) / sizeof(required_checks[0]),
			  diagnostic_codes,
			  sizeof(diagnostic_codes) /
			    sizeof(diagnostic_codes[0]));

  if (contract_id == DSL_CONTRACT_INVALID_ID) {
    fprintf(stderr, "DSL contract registration failed\n");
    failed = 1;
  }

  if (DSL_Contract_Register("residual_add_common_contract",
			    cnn_id,
			    common_id,
			    99,
			    0,
			    NULL,
			    0,
			    NULL,
			    0) != contract_id) {
    fprintf(stderr, "DSL contract duplicate registration changed id\n");
    failed = 1;
  }

  if (DSL_Contract_Count() != 1 ||
      DSL_Contract_Find("residual_add_common_contract", cnn_id, common_id) !=
	contract_id) {
    fprintf(stderr, "DSL contract lookup failed\n");
    failed = 1;
  }

  if (!DSL_Contract_Get_Info(contract_id, &info) ||
      info.source_domain_id != cnn_id ||
      info.target_domain_id != common_id ||
      strcmp(info.name, "residual_add_common_contract") != 0 ||
      info.version != 1 ||
      info.required_check_count != 3 ||
      info.diagnostic_code_count != 2) {
    fprintf(stderr, "DSL contract info changed\n");
    failed = 1;
  }

  if (!DSL_Contract_At(0, &info) ||
      info.id != contract_id ||
      DSL_Contract_At(DSL_Contract_Count(), &info)) {
    fprintf(stderr, "DSL contract iteration failed\n");
    failed = 1;
  }

  if (strcmp(DSL_Contract_Required_Check_At(contract_id, 1),
	     "layout_compatible") != 0 ||
      DSL_Contract_Required_Check_At(contract_id, 3) != NULL ||
      strcmp(DSL_Contract_Diagnostic_Code_At(contract_id, 0),
	     "DCONTRACT001") != 0 ||
      DSL_Contract_Diagnostic_Code_At(contract_id, 2) != NULL) {
    fprintf(stderr, "DSL contract check/diagnostic lookup failed\n");
    failed = 1;
  }

  if (DSL_Contract_Register("bad_contract",
			    9999,
			    common_id,
			    1,
			    0,
			    NULL,
			    0,
			    NULL,
			    0) != DSL_CONTRACT_INVALID_ID ||
      DSL_Contract_Register("", cnn_id, common_id, 1, 0,
			    NULL, 0, NULL, 0) != DSL_CONTRACT_INVALID_ID) {
    fprintf(stderr, "DSL contract registry accepted invalid input\n");
    failed = 1;
  }

  if (dump == NULL) {
    perror("tmpfile");
    return 1;
  }

  DSL_Contract_fprint_registry(dump);
  text = Read_File(dump);
  fclose(dump);

  if (text == NULL) {
    fprintf(stderr, "failed to read DSL contract registry dump\n");
    return 1;
  }

  if (strstr(text, "DSL Contract Registry: entries=1") == NULL ||
      strstr(text, "name=residual_add_common_contract") == NULL ||
      strstr(text, "check[1]=layout_compatible") == NULL ||
      strstr(text, "diagnostic[0]=DCONTRACT001") == NULL) {
    fprintf(stderr, "DSL contract registry dump changed\n");
    failed = 1;
  }

  free(text);
  return failed;
}

static int
Check_DSL_Opcode_Registry(void)
{
  const UINT32 common_seed_count = 61;
  DSL_DOMAIN_ID common_id;
  DSL_OPCODE_ID add_id;
  DSL_OPCODE_ID layout_cast_id;
  DSL_OPCODE_ID residual_shape_check_id;
  DSL_OPCODE_ID dispatch_id;
  DSL_OPCODE_INFO info;
  FILE *dump = tmpfile();
  char *text;
  int failed = 0;

  DSL_Opcode_Registry_Reset();
  DSL_Domain_Registry_Reset();

  if (DSL_Opcode_Register_Common_Substrate() != common_seed_count ||
      DSL_Opcode_Register_Common_Substrate() != common_seed_count) {
    fprintf(stderr, "DSL common opcode substrate seeding failed\n");
    failed = 1;
  }

  common_id = DSL_Domain_Find("common");
  add_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1);

  if (add_id == DSL_OPCODE_INVALID_ID) {
    fprintf(stderr, "DSL common.add seed lookup failed\n");
    failed = 1;
  }

  if (DSL_Opcode_Register(common_id,
			  DSL_OPCODE_COMMON_ADD,
			  1,
			  DSL_OPCODE_CATEGORY_VERIFIER,
			  DSL_OPCODE_LEVEL_1_TENSOR,
			  0,
			  DSL_SHAPE_RULE_OPAQUE,
			  DSL_EFFECT_MODEL_VERIFIER_ONLY,
			  DSL_LOWERING_MODEL_MARKER_ONLY,
			  "DOPC_DUPLICATE",
			  0) != add_id) {
    fprintf(stderr, "DSL opcode duplicate registration changed id\n");
    failed = 1;
  }

  if (DSL_Opcode_Count() != common_seed_count ||
      DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1) != add_id ||
      DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 2) !=
	DSL_OPCODE_INVALID_ID) {
    fprintf(stderr, "DSL opcode lookup failed\n");
    failed = 1;
  }

  if (!DSL_Opcode_Get_Info(add_id, &info) ||
      info.owner_domain_id != common_id ||
      strcmp(info.name, DSL_OPCODE_COMMON_ADD) != 0 ||
      info.version != 1 ||
      info.category != DSL_OPCODE_CATEGORY_EXECUTABLE ||
      info.level != DSL_OPCODE_LEVEL_2_NUMERIC ||
      info.nkids != 2 ||
      info.shape_rule != DSL_SHAPE_RULE_BROADCAST ||
      info.effect_model != DSL_EFFECT_MODEL_PURE ||
      info.lowering_model != DSL_LOWERING_MODEL_MARKER_ONLY ||
      strcmp(info.diagnostic_prefix, "DOPC_COMMON_ADD") != 0) {
    fprintf(stderr, "DSL opcode info changed\n");
    failed = 1;
  }

  layout_cast_id = DSL_Opcode_Find(common_id, "common.layout_cast", 1);
  residual_shape_check_id =
    DSL_Opcode_Find(common_id, "common.residual_shape_check", 1);
  dispatch_id = DSL_Opcode_Find(common_id, "common.dispatch", 1);

  if (!DSL_Opcode_Get_Info(layout_cast_id, &info) ||
      info.category != DSL_OPCODE_CATEGORY_EXECUTABLE ||
      info.level != DSL_OPCODE_LEVEL_1_TENSOR ||
      info.shape_rule != DSL_SHAPE_RULE_LAYOUT ||
      info.effect_model != DSL_EFFECT_MODEL_PURE) {
    fprintf(stderr, "DSL common.layout_cast seed descriptor changed\n");
    failed = 1;
  }

  if (!DSL_Opcode_Get_Info(residual_shape_check_id, &info) ||
      info.category != DSL_OPCODE_CATEGORY_VERIFIER ||
      info.level != DSL_OPCODE_LEVEL_3_NN_COMMON ||
      info.nkids != 2 ||
      info.effect_model != DSL_EFFECT_MODEL_VERIFIER_ONLY) {
    fprintf(stderr, "DSL common.residual_shape_check seed descriptor changed\n");
    failed = 1;
  }

  if (!DSL_Opcode_Get_Info(dispatch_id, &info) ||
      info.category != DSL_OPCODE_CATEGORY_LOWERING_POLICY ||
      info.level != DSL_OPCODE_LEVEL_4_RUNTIME ||
      info.effect_model != DSL_EFFECT_MODEL_LOWERING_POLICY ||
      info.lowering_model != DSL_LOWERING_MODEL_MARKER_ONLY) {
    fprintf(stderr, "DSL common.dispatch seed descriptor changed\n");
    failed = 1;
  }

  if (!DSL_Opcode_At(0, &info) ||
      strcmp(info.name, "common.module") != 0 ||
      DSL_Opcode_At(DSL_Opcode_Count(), &info)) {
    fprintf(stderr, "DSL opcode iteration failed\n");
    failed = 1;
  }

  if (strcmp(DSL_Opcode_Category_Name(DSL_OPCODE_CATEGORY_EXECUTABLE),
	     "executable") != 0 ||
      strcmp(DSL_Opcode_Level_Name(DSL_OPCODE_LEVEL_2_NUMERIC),
	     "level2_numeric") != 0 ||
      strcmp(DSL_Shape_Rule_Name(DSL_SHAPE_RULE_BROADCAST),
	     "broadcast") != 0 ||
      strcmp(DSL_Effect_Model_Name(DSL_EFFECT_MODEL_PURE),
	     "pure") != 0 ||
      strcmp(DSL_Lowering_Model_Name(DSL_LOWERING_MODEL_MARKER_ONLY),
	     "marker_only") != 0) {
    fprintf(stderr, "DSL opcode enum names changed\n");
    failed = 1;
  }

  if (DSL_Opcode_Register(9999,
			  "bad.owner",
			  1,
			  DSL_OPCODE_CATEGORY_EXECUTABLE,
			  DSL_OPCODE_LEVEL_2_NUMERIC,
			  2,
			  DSL_SHAPE_RULE_BROADCAST,
			  DSL_EFFECT_MODEL_PURE,
			  DSL_LOWERING_MODEL_MARKER_ONLY,
			  "DOPC_BAD",
			  0) != DSL_OPCODE_INVALID_ID ||
      DSL_Opcode_Register(common_id,
			  "",
			  1,
			  DSL_OPCODE_CATEGORY_EXECUTABLE,
			  DSL_OPCODE_LEVEL_2_NUMERIC,
			  2,
			  DSL_SHAPE_RULE_BROADCAST,
			  DSL_EFFECT_MODEL_PURE,
			  DSL_LOWERING_MODEL_MARKER_ONLY,
			  "DOPC_EMPTY",
			  0) != DSL_OPCODE_INVALID_ID ||
      DSL_Opcode_Register(common_id,
			  "bad.version",
			  0,
			  DSL_OPCODE_CATEGORY_EXECUTABLE,
			  DSL_OPCODE_LEVEL_2_NUMERIC,
			  2,
			  DSL_SHAPE_RULE_BROADCAST,
			  DSL_EFFECT_MODEL_PURE,
			  DSL_LOWERING_MODEL_MARKER_ONLY,
			  "DOPC_VERSION",
			  0) != DSL_OPCODE_INVALID_ID) {
    fprintf(stderr, "DSL opcode registry accepted invalid input\n");
    failed = 1;
  }

  if (dump == NULL) {
    perror("tmpfile");
    return 1;
  }

  DSL_Opcode_fprint_registry(dump);
  text = Read_File(dump);
  fclose(dump);

  if (text == NULL) {
    fprintf(stderr, "failed to read DSL opcode registry dump\n");
    return 1;
  }

  if (strstr(text, "DSL Opcode Registry: entries=61") == NULL ||
      strstr(text, "name=common.add") == NULL ||
      strstr(text, "category=executable") == NULL ||
      strstr(text, "level=level2_numeric") == NULL ||
      strstr(text, "shape=broadcast") == NULL ||
      strstr(text, "effect=pure") == NULL ||
      strstr(text, "lowering=marker_only") == NULL ||
      strstr(text, "diagnostic_prefix=DOPC_COMMON_ADD") == NULL ||
      strstr(text, "name=common.kernel_variant") == NULL ||
      strstr(text, "level=level4_runtime") == NULL) {
    fprintf(stderr, "DSL opcode registry dump changed\n");
    failed = 1;
  }

  free(text);
  return failed;
}

static int
Check_Tensor_Required_Attribute_Diagnostics(void)
{
  static const TY_TENSOR_SCHEMA_KEY required[] = {
    TY_TENSOR_SCHEMA_DTYPE,
    TY_TENSOR_SCHEMA_SHAPE,
    TY_TENSOR_SCHEMA_LAYOUT,
    TY_TENSOR_SCHEMA_LINEAGE,
    TY_TENSOR_SCHEMA_SHARDING
  };
  TY_IDX tensor_ty =
    TY_Create_Tensor_Extension_Type("tensor_i32_diag",
				    MTYPE_To_TY(MTYPE_I4),
				    2);
  FILE *dump = tmpfile();
  char *text;
  int failed = 0;

  TY_tensor_bind_attribute(tensor_ty, TY_TENSOR_SCHEMA_DTYPE, "int32");
  TY_tensor_bind_attribute(tensor_ty, TY_TENSOR_SCHEMA_SHAPE, "[2,2]");
  TY_tensor_bind_attribute(tensor_ty, TY_TENSOR_SCHEMA_LAYOUT, "row_major");
  TY_tensor_declare_attribute(tensor_ty, TY_TENSOR_SCHEMA_LINEAGE);

  if (TY_tensor_unbound_required_attribute_count
	(tensor_ty, required, sizeof(required) / sizeof(required[0])) != 2) {
    fprintf(stderr, "required tensor attribute diagnostic count changed\n");
    failed = 1;
  }

  if (TY_tensor_has_required_attributes
	(tensor_ty, required, sizeof(required) / sizeof(required[0]))) {
    fprintf(stderr, "required tensor attribute diagnostic missed failures\n");
    failed = 1;
  }

  if (dump == NULL) {
    perror("tmpfile");
    return 1;
  }

  TY_tensor_fprint_unbound_required_attributes
	(dump, tensor_ty, required, sizeof(required) / sizeof(required[0]));
  text = Read_File(dump);
  fclose(dump);

  if (text == NULL) {
    fprintf(stderr, "failed to read required tensor attribute diagnostics\n");
    return 1;
  }

  if (strstr(text, "required_attribute=lineage status=pending") == NULL ||
      strstr(text, "required_attribute=sharding status=missing") == NULL) {
    fprintf(stderr, "required tensor attribute diagnostics changed\n");
    failed = 1;
  }

  free(text);

  TY_tensor_bind_attribute(tensor_ty, TY_TENSOR_SCHEMA_LINEAGE, "created");
  TY_tensor_bind_attribute(tensor_ty, TY_TENSOR_SCHEMA_SHARDING, "replicated");

  if (TY_tensor_unbound_required_attribute_count
	(tensor_ty, required, sizeof(required) / sizeof(required[0])) != 0 ||
      !TY_tensor_has_required_attributes
	(tensor_ty, required, sizeof(required) / sizeof(required[0]))) {
    fprintf(stderr, "required tensor attribute diagnostics stayed active\n");
    failed = 1;
  }

  return failed;
}

static int
Check_VHO_Unconsumed_DSL_Scanner(WN *tree)
{
  VHO_UNCONSUMED_DSL_SCAN scan;
  FILE *dump = tmpfile();
  char *text;
  int failed = 0;

  VHO_Scan_Unconsumed_DSL_Markers(tree, &scan);

  if (scan.dsl_marker_count != 3 || scan.dsl_opcode_count != 3) {
    fprintf(stderr, "VHO scanner marker counts changed\n");
    failed = 1;
  }

  if (!VHO_Has_Unconsumed_DSL_Markers(tree)) {
    fprintf(stderr, "VHO scanner missed unconsumed DSL markers\n");
    failed = 1;
  }

  if (dump == NULL) {
    perror("tmpfile");
    return 1;
  }

  VHO_fprint_unconsumed_DSL_markers(dump, tree);
  text = Read_File(dump);
  fclose(dump);

  if (text == NULL) {
    fprintf(stderr, "failed to read VHO scanner report\n");
    return 1;
  }

  if (strstr(text, "VHO unconsumed DSL marker: opcode=common.tensor_const") == NULL ||
      strstr(text, "VHO unconsumed DSL marker: opcode=common.add") == NULL ||
      strstr(text, "markers=3 opcodes=3") == NULL) {
    fprintf(stderr, "VHO scanner report missed expected DSL markers\n");
    failed = 1;
  }

  free(text);
  return failed;
}

static char *
Read_File(FILE *fp)
{
  long size;
  char *buffer;

  fflush(fp);
  if (fseek(fp, 0, SEEK_END) != 0)
    return NULL;
  size = ftell(fp);
  if (size < 0)
    return NULL;
  if (fseek(fp, 0, SEEK_SET) != 0)
    return NULL;

  buffer = (char *) malloc((size_t) size + 1);
  if (buffer == NULL)
    return NULL;

  if (size != 0 &&
      fread(buffer, 1, (size_t) size, fp) != (size_t) size) {
    free(buffer);
    return NULL;
  }
  buffer[size] = '\0';
  return buffer;
}

static const char *
Trace_File_Path(void)
{
  static char path[4096];
  const char *source = __FILE__;
  const char *slash = strrchr(source, '/');

  if (slash == NULL) {
    snprintf(path, sizeof(path), "dsl_common_add_print_test.trace");
  } else {
    size_t dir_len = (size_t) (slash - source);
    snprintf(path, sizeof(path), "%.*s/dsl_common_add_print_test.trace",
	     (int) dir_len, source);
  }

  return path;
}

static int
Write_Common_Add_Trace(const char *tree_text, const char *annotation_text)
{
  const char *trace_path = Trace_File_Path();
  FILE *trace = fopen(trace_path, "w");

  if (trace == NULL) {
    perror(trace_path);
    return 1;
  }

  fprintf(trace, "dsl_common_add_print_test trace\n");
  fprintf(trace, "created_operator=%s\n", DSL_OPCODE_COMMON_ADD);
  fprintf(trace, "\n[fdump_tree]\n");
  fputs(tree_text, trace);
  fprintf(trace, "\n[dsl_annotation]\n");
  fputs(annotation_text, trace);

  if (fclose(trace) != 0) {
    perror(trace_path);
    return 1;
  }

  return 0;
}

int
main(void)
{
  Initialize_Test_Context();

  WN *dsl_marker = NULL;
  WN *tree = Create_Common_Add_Test_Tree(&dsl_marker);
  FILE *dump = tmpfile();
  FILE *annotation = tmpfile();
  char *text;
  char *annotation_text;
  int failed = 0;

  if (dump == NULL || annotation == NULL) {
    perror("tmpfile");
    return 1;
  }

  if (!DSL_WN_Has_Opcode (dsl_marker)) {
    fprintf(stderr, "common.add marker was not recognized as a DSL opcode\n");
    failed = 1;
  }
  failed |= Check_Common_Add_Annotation(dsl_marker);
  failed |= Check_Common_Add_On_Tensor_Constants();
  failed |= Check_Zero_Initializer_Operators();
  failed |= Check_DSL_Domain_Registry();
  failed |= Check_DSL_Contract_Registry();
  failed |= Check_DSL_Opcode_Registry();
  failed |= Check_Tensor_Dsl_Symtab_Print();
  failed |= Check_Tensor_Required_Attribute_Diagnostics();
  failed |= Check_VHO_Unconsumed_DSL_Scanner(tree);

  fdump_tree(dump, tree);
  text = Read_File(dump);
  fclose(dump);

  DSL_fprint_opcode_annotation (annotation, dsl_marker);
  annotation_text = Read_File(annotation);
  fclose(annotation);

  if (text == NULL || annotation_text == NULL) {
    fprintf(stderr, "failed to read test output\n");
    return 1;
  }

  fputs(text, stdout);
  fputs(annotation_text, stdout);
  failed |= Write_Common_Add_Trace(text, annotation_text);

  if (strstr(text, "__WHIRL_DSL__:opcode:common.add:") == NULL) {
    fprintf(stderr, "missing common.add DSL marker in fdump_tree output\n");
    failed = 1;
  }

  if (strstr(text, "__WHIRL_DSL__:opcode:common.tensor_const:v1:") == NULL ||
      strstr(text, "name=const_i32_2x2_zero") == NULL ||
      strstr(text, "name=const_i32_2x2_one") == NULL) {
    fprintf(stderr, "missing tensor_const DSL markers in fdump_tree output\n");
    failed = 1;
  }

  if (strstr(text, "I4ADD") == NULL && strstr(text, "ADD") == NULL) {
    fprintf(stderr, "missing standard WHIRL ADD in fdump_tree output\n");
    failed = 1;
  }

  if (strstr(annotation_text, "dsl_opcode=common.add.v1") == NULL) {
    fprintf(stderr, "missing formatted common.add DSL opcode annotation\n");
    failed = 1;
  }

  {
    FILE *trace = fopen(Trace_File_Path(), "r");
    char *trace_text = trace == NULL ? NULL : Read_File(trace);

    if (trace != NULL)
      fclose(trace);

    if (trace_text == NULL ||
	strstr(trace_text, "created_operator=common.add") == NULL ||
	strstr(trace_text, "__WHIRL_DSL__:opcode:common.add:") == NULL) {
      fprintf(stderr, "missing common.add creation trace file content\n");
      failed = 1;
    }

    free(trace_text);
  }

  free(text);
  free(annotation_text);
  return failed;
}
