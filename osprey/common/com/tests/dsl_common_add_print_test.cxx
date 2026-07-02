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
