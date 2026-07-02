/*
 * Smoke test for representing a common.matmul DSL operation with tensor-one
 * operands and printing it through the standard fdump_tree interface.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "wn_util.h"
#include "stab.h"
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
static int Write_Common_Matmul_Trace(const char *tree_text,
				     const char *annotation_text);

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
Create_Common_Matmul_Test_Tree(WN **dsl_marker_out)
{
  WN *block = WN_CreateBlock();
  WN *tensor_one_kid0 =
    DSL_WN_Create_Tensor_Const ("tensor_one_kid0",
			     "int32",
			     2,
			     "[2,2]",
			     "splat",
			     "1");
  WN *tensor_one_kid1 =
    DSL_WN_Create_Tensor_Const ("tensor_one_kid1",
			     "int32",
			     2,
			     "[2,2]",
			     "splat",
			     "1");
  WN *dsl_marker =
    DSL_WN_Create_Opcode (DSL_OPCODE_COMMON_MATMUL,
			1,
			"kid0=tensor_one_kid0;kid1=tensor_one_kid1;attr.transpose_kid0=false;attr.transpose_kid1=false");

  if (dsl_marker_out != NULL)
    *dsl_marker_out = dsl_marker;

  WN_INSERT_BlockLast(block, tensor_one_kid0);
  WN_INSERT_BlockLast(block, tensor_one_kid1);
  WN_INSERT_BlockLast(block, dsl_marker);

  return block;
}

static int
Check_Tensor_One_Annotation(WN *tensor_const, const char *name)
{
  DSL_OPCODE_ANNOTATION annotation;
  int failed = 0;

  if (!DSL_WN_Get_Opcode_Annotation (tensor_const, &annotation)) {
    fprintf(stderr, "tensor-one marker did not decode\n");
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
      strstr(annotation.payload, "value=1") == NULL) {
    fprintf(stderr, "decoded tensor-one payload changed\n");
    failed = 1;
  }

  return failed;
}

static int
Check_Common_Matmul_Annotation(WN *dsl_marker)
{
  DSL_OPCODE_ANNOTATION annotation;
  int failed = 0;

  if (!DSL_WN_Get_Opcode_Annotation (dsl_marker, &annotation)) {
    fprintf(stderr, "common.matmul marker was not decoded as a DSL opcode\n");
    return 1;
  }

  if (annotation.name_len != strlen(DSL_OPCODE_COMMON_MATMUL) ||
      strncmp(annotation.name, DSL_OPCODE_COMMON_MATMUL,
	      annotation.name_len) != 0) {
    fprintf(stderr, "decoded DSL opcode name is not common.matmul\n");
    failed = 1;
  }

  if (annotation.version != 1) {
    fprintf(stderr, "decoded common.matmul DSL opcode version is not 1\n");
    failed = 1;
  }

  if (strcmp(annotation.payload,
	     "kid0=tensor_one_kid0;kid1=tensor_one_kid1;attr.transpose_kid0=false;attr.transpose_kid1=false") != 0) {
    fprintf(stderr, "decoded common.matmul DSL opcode payload changed\n");
    failed = 1;
  }

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
    snprintf(path, sizeof(path), "dsl_common_matmul_print_test.trace");
  } else {
    size_t dir_len = (size_t) (slash - source);
    snprintf(path, sizeof(path), "%.*s/dsl_common_matmul_print_test.trace",
	     (int) dir_len, source);
  }

  return path;
}

static int
Write_Common_Matmul_Trace(const char *tree_text, const char *annotation_text)
{
  const char *trace_path = Trace_File_Path();
  FILE *trace = fopen(trace_path, "w");

  if (trace == NULL) {
    perror(trace_path);
    return 1;
  }

  fprintf(trace, "dsl_common_matmul_print_test trace\n");
  fprintf(trace, "created_operator=%s\n", DSL_OPCODE_COMMON_MATMUL);
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
  WN *tree = Create_Common_Matmul_Test_Tree(&dsl_marker);
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
    fprintf(stderr, "common.matmul marker was not recognized as a DSL opcode\n");
    failed = 1;
  }
  failed |= Check_Common_Matmul_Annotation(dsl_marker);

  failed |= Check_Tensor_One_Annotation(WN_first(tree),
					"name=tensor_one_kid0");
  failed |= Check_Tensor_One_Annotation(WN_next(WN_first(tree)),
					"name=tensor_one_kid1");

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
  failed |= Write_Common_Matmul_Trace(text, annotation_text);

  if (strstr(text, "__WHIRL_DSL__:opcode:common.matmul:") == NULL) {
    fprintf(stderr, "missing common.matmul DSL marker in fdump_tree output\n");
    failed = 1;
  }

  if (strstr(text, "__WHIRL_DSL__:opcode:common.tensor_const:v1:") == NULL ||
      strstr(text, "name=tensor_one_kid0") == NULL ||
      strstr(text, "name=tensor_one_kid1") == NULL ||
      strstr(text, "value=1") == NULL) {
    fprintf(stderr, "missing tensor-one DSL markers in fdump_tree output\n");
    failed = 1;
  }

  if (strstr(annotation_text, "dsl_opcode=common.matmul.v1") == NULL) {
    fprintf(stderr, "missing formatted common.matmul DSL opcode annotation\n");
    failed = 1;
  }

  {
    FILE *trace = fopen(Trace_File_Path(), "r");
    char *trace_text = trace == NULL ? NULL : Read_File(trace);

    if (trace != NULL)
      fclose(trace);

    if (trace_text == NULL ||
	strstr(trace_text, "created_operator=common.matmul") == NULL ||
	strstr(trace_text, "__WHIRL_DSL__:opcode:common.matmul:") == NULL) {
      fprintf(stderr, "missing common.matmul creation trace file content\n");
      failed = 1;
    }

    free(trace_text);
  }

  free(text);
  free(annotation_text);
  return failed;
}
