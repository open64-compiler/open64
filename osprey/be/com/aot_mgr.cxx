/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: aot_mgr.cxx
//
// ====================================================================
//
#include "aot_mgr.h"
#include "wn.h"
#include "region_util.h"
#include "targ_sim.h"

// =============================================================================
//
// AOT_COMP_DRIVER::gen_makefile - generate makefile
// Format as below:
//   UWM_BIN =
//   NATIVE_BIN =
//   UWM_LINKER =
//   AOT_TMPDIR =
//   AOT_OUTPUT_FILE =
//   SYMTAB_INPUT =
//   UWM_INPUT_FILES =
//   NATIVE_INPUT_FILES =
//   UWM_OPTIONS =
//   NATIVE_OPTIONS =
//   SYMTAB_OPTIONS =
//   LIBS =
//   UWM_OBJ_FILES = $(UWM_INPUT_FILES:.I=.uwm)
//   NATIVE_OBJ_FILES = $(NATIVE_INPUT_FILES:.I=.o)
//   NATIVE_SO_FILE = native.so
//   SYMTAB_OBJ_FILE = $(SYMTAB_INPUT_FILES:.I=.G)
//
//   .PHONY: default
//
//   default: $(AOT_OUTPUT_FILE)
//
//   $(UWM_OBJ_FILES): $(UWM_INPUT_FILES) $(SYMTAB_OBJ_FILE)
//           $(UWM_BIN) -c $(UWM_OPTIONS) -o $@ $^
//   $(NATIVE_OBJ_FILES): $(NATIVE_INPUT_FILES) $(SYMTAB_OBJ_FILE)
//           $(NATIVE_BIN) -c $(NATIVE_OPTIONS) -o $@ $^
//   $(SYMTAB_OBJ_FILE): $(SYMTAB_INPUT_FILES) $(SYMTAB_OBJ_FILE)
//           $(UWM_BIN) -c $(SYMTAB_OPTIONS) -o $@ $^
//
//   $(NATIVE_SO_FILE) : $(NATIVE_OBJ_FILES)
//            $(NATIVE_BIN) -o $@ $^
//   $(AOT_OUTPUT_FILE) : $(NATIVE_SO_FILE) $(UWM_OBJ_FILES) $(SYMTAB_OBJ_FILE)
//           $(UWM_LINKER) $(UWM_OBJ_FILES) $(NATIVE_SO_FILE)
//
//   clean:
//           rm -rf $(NATIVE_OBJ_FILES) $(NATIVE_SO_FILE) $(UWM_OBJ_FILES) $(AOT_OUTPUT_FILE) $(SYMTAB_OBJ_FILE)
//
//   TODO: Generate makefile according to a description file
// =============================================================================
char *
AOT_COMP_DRIVER::Gen_makefile()
{
  char makefile_name[256];
  sprintf(makefile_name, "makefile.aot%ld", (long) getpid());
  char *full_path = (char *) MEM_POOL_Alloc(
    Mem_pool(), strlen(Get_workdir()) + strlen(makefile_name) + 2);
  sprintf(full_path, "%s/%s", Get_workdir(), makefile_name);
  FILE * fp = fopen(full_path, "w");
  if(fp == NULL) {
    perror(full_path);
    exit(1);
  }

  fprintf(fp, "UWM_BIN = %s\n", Get_uwm_driver());
  fprintf(fp, "NATIVE_BIN = %s\n", Get_nat_driver());
  fprintf(fp, "UWM_LINKER = %s\n", Get_linker());
  fprintf(fp, "AOT_TMPDIR = %s\n", Get_workdir());
  fprintf(fp, "AOT_OUTPUT_FILE = %s\n", Get_output());
  fprintf(fp, "UWM_INPUT_FILES = ");
  STR_ARR::const_iterator it;
  for(it = Get_uwm_files().begin(); it != Get_uwm_files().end(); it++) {
    fprintf(fp, "%s ", *it);
  }
  fprintf(fp, "\n");
  fprintf(fp, "NATIVE_INPUT_FILES = ");
  for(it = Get_nat_files().begin(); it != Get_nat_files().end(); it++) {
    fprintf(fp, "%s ",*it);
  }

  fprintf(fp, "\n");
  fprintf(fp, "SYMTAB_INPUT_FILE = symtab.I\n");

  fprintf(fp, "UWM_OPTIONS = ");
  for(it = Get_uwm_opt().begin(); it != Get_uwm_opt().end(); it++) {
    fprintf(fp, "%s ", *it);
  }

  fprintf(fp, "\n");
  fprintf(fp, "NATIVE_OPTIONS = ");
  for(it = Get_nat_opt().begin(); it != Get_nat_opt().end(); it++) {
    fprintf(fp, "%s ", *it);
  }

  fprintf(fp, "\n");
  fprintf(fp, "SYMTAB_OPTIONS = ");
  for(it = Get_symtab_opt().begin(); it != Get_symtab_opt().end(); it++) {
    fprintf(fp, "%s ", *it);
  }

  fprintf(fp, "\n");
  fprintf(fp, "LIBS = ");
  fprintf(fp, "\n");

  const char *makefile_pattern = "\
UWM_OBJ_FILES = $(UWM_INPUT_FILES:.I=.uwm) \n\n\
NATIVE_OBJ_FILES = $(NATIVE_INPUT_FILES:.I=.o) \n\n\
NATIVE_SO_FILE = native.so \n\n\
SYMTAB_OBJ_FILE = $(SYMTAB_INPUT_FILE:.I=.G) \n\n\
.PHONY: default \n\n\
default: $(AOT_OUTPUT_FILE) \n\n\
$(AOT_TMPDIR)/$(UWM_OBJ_FILES): $(AOT_TMPDIR)/$(UWM_INPUT_FILES) $(AOT_TMPDIR)/$(SYMTAB_OBJ_FILE) \n\
\tcd $(AOT_TMPDIR); $(UWM_BIN) -c $(UWM_OPTIONS) $(UWM_INPUT_FILES) \n\n\
$(AOT_TMPDIR)/$(NATIVE_OBJ_FILES): $(AOT_TMPDIR)/$(NATIVE_INPUT_FILES) $(AOT_TMPDIR)/$(SYMTAB_OBJ_FILE) \n\
\tcd $(AOT_TMPDIR); $(NATIVE_BIN) -c $(NATIVE_OPTIONS) $(NATIVE_INPUT_FILES) \n\n\
$(AOT_TMPDIR)/$(SYMTAB_OBJ_FILE): $(AOT_TMPDIR)/$(SYMTAB_INPUT_FILE) \n\
\tcd $(AOT_TMPDIR); $(UWM_BIN) -c $(SYMTAB_OPTIONS) $(SYMTAB_INPUT_FILE)\n\n\
$(AOT_TMPDIR)/$(NATIVE_SO_FILE) : $(AOT_TMPDIR)/$(NATIVE_OBJ_FILES)\n\
\tcd $(AOT_TMPDIR); $(NATIVE_BIN) $(NATIVE_OPTIONS) -o $(NATIVE_SO_FILE) $(NATIVE_OBJ_FILES) \n\n\
$(AOT_OUTPUT_FILE) : $(AOT_TMPDIR)/$(NATIVE_SO_FILE) $(AOT_TMPDIR)/$(UWM_OBJ_FILES) $(AOT_TMPDIR)/$(SYMTAB_OBJ_FILE)\n\
\t$(UWM_LINKER) $(AOT_TMPDIR)/$(UWM_OBJ_FILES) $(AOT_TMPDIR)/$(NATIVE_SO_FILE) $(LIBS) -o $(AOT_OUTPUT_FILE) \n\n\
clean: \n\
\trm -rf $(AOT_OUTPUT_FILE) \n\
\tcd $(AOT_TMPDIR); rm -rf $(NATIVE_OBJ_FILES) $(NATIVE_SO_FILE) $(UWM_OBJ_FILES) $(SYMTAB_OBJ_FILE) \n\n\
";
  fprintf(fp, "%s", makefile_pattern);
  fclose(fp);
  return full_path;
}

BOOL
AOT_COMP_DRIVER::Verify()
{
  return TRUE;
}

void Adjust_stmt_preg_num(WN *stmt)
{
  OPERATOR opr = WN_operator(stmt);
  switch(opr) {
    case OPR_LDID:
       if (WN_class(stmt) == CLASS_PREG) {
         PREG_NUM preg = WN_load_offset(stmt);
         if (preg > Last_Dedicated_Preg_Offset) {
          preg = preg - Last_Dedicated_Preg_Offset + Last_Dedicated_Preg_Offset_X8664;
          WN_offset(stmt) = preg;
         }
       }
    break;
    case OPR_STID:
       if (WN_class(stmt) == CLASS_PREG) {
         PREG_NUM preg = WN_store_offset(stmt);
         if (preg > Last_Dedicated_Preg_Offset) {
          preg = preg - Last_Dedicated_Preg_Offset + Last_Dedicated_Preg_Offset_X8664;
          WN_offset(stmt) = preg;
         }
       }
    break;
    case OPR_BLOCK:
    {
      WN *item = WN_first(stmt);
      while(item) {
        Adjust_stmt_preg_num(item);
        item = WN_next(item);
      }
    }
    break;
  }

  for (int i = 0; i < WN_kid_count(stmt); i++) {
    Adjust_stmt_preg_num(WN_kid(stmt, i));
  }
}

void Adjust_tree_preg_num(WN *tree)
{
  WN *stmt = tree;
  WN *next_stmt = NULL;
  for (; stmt; stmt = next_stmt)
  {
    next_stmt = WN_next(stmt);
    OPCODE opc = WN_opcode(stmt);
    switch (opc) {
      case OPC_FUNC_ENTRY:
        Adjust_tree_preg_num(WN_entry_first(stmt));
        break;
      case OPC_REGION: {
        RID *rid = REGION_get_rid(tree);
        if (RID_level( rid ) < RL_CG) {
          Adjust_tree_preg_num(WN_first(WN_region_body(stmt)));
        }
        break;
      }
      default:
        Adjust_stmt_preg_num(stmt);
        break;
    }
  }
}
