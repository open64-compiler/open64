/*
 * Copyright (C) 2026 Open64 Project
 *
 * Read-only mapped-image driver for the S6-0d D2 collector and D4 public join.
 * It opens
 * an existing CKKS-semantic WHIRL file, visits each PU while its local symbol
 * table is active, emits deterministic provider-private C, and records the
 * collection and canonical public-selector census. It does not publish
 * checkpoint artifacts or mutate the input image. See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D2 and D4.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <fstream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include "defs.h"
#include "config.h"
#include "config_fhe.h"
#include "config_targ_opt.h"
#include "controls.h"
#include "dsl_builder.h"
#include "dsl_opcode.h"
#include "dwarf_DST_mem.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "fhe_ckks2c_collect.h"
#include "fhe_ckks2c_public_join.h"
#include "glob.h"
#include "ir_bread.h"
#include "ir_bwrite.h"
#include "ir_reader.h"
#include "mempool.h"
#include "pu_info.h"
#include "stab.h"
#include "wn.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

/* The linked IR reader requires the standard driver cleanup hook. */
void Signal_Cleanup(INT signal)
{
  (void)signal;
}

/* The linked error formatter requires the standard host callback. */
const char *Host_Format_Parm(INT kind, MEM_PTR parameter)
{
  (void)kind;
  (void)parameter;
  return "";
}

/* Initialize only the Open64 services required to reopen mapped WHIRL. */
static void Initialize_Reader_Context()
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
  Initialize_Symbol_Tables(FALSE);
  New_Scope(GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
}

/* Replace one local text artifact only after its complete bytes are ready. */
static bool Write_Text(const char *path, const std::string &content)
{
  if (path == NULL || path[0] == 0)
    return false;
  const std::string temporary = std::string(path) + ".tmp";
  std::ofstream output(temporary.c_str(),
                       std::ios::out | std::ios::binary | std::ios::trunc);
  if (!output)
    return false;
  output.write(content.data(), content.size());
  output.close();
  if (!output)
    return false;
  if (rename(temporary.c_str(), path) != 0) {
    remove(temporary.c_str());
    return false;
  }
  return true;
}

/* Traverse the complete PU tree in preorder while each local symbol table is
 * active. Child PUs are not guaranteed to appear in the top-level sibling
 * chain, so a flat PU_Info_next walk would not certify all-PU coverage. */
static bool Collect_PUs(PU_Info *pu_tree,
                        VHO_FHE_CKKS2C_COLLECTION *collection,
                        uint32_t *visited)
{
  if (collection == NULL || visited == NULL)
    return false;
  for (PU_Info *pu = pu_tree; pu != NULL; pu = PU_Info_next(pu)) {
    MEM_POOL_Push(MEM_pu_nz_pool_ptr);
    MEM_POOL_Push(MEM_pu_pool_ptr);
    Read_Local_Info(MEM_pu_nz_pool_ptr, pu);
    Current_PU_Info = pu;
    fprintf(stderr, "CKKS2C collect PU %u owner=%u\n", *visited + 1,
            static_cast<uint32_t>(PU_Info_proc_sym(pu)));
    const bool valid = VHO_FHE_CKKS2C_Collect_Active_PU(
        PU_Info_proc_sym(pu), collection, stderr);
    ++*visited;
    Current_PU_Info = NULL;
    Free_Local_Info(pu);
    MEM_POOL_Pop(MEM_pu_nz_pool_ptr);
    MEM_POOL_Pop(MEM_pu_pool_ptr);
    if (!valid ||
        (PU_Info_child(pu) != NULL &&
         !Collect_PUs(PU_Info_child(pu), collection, visited)))
      return false;
  }
  return true;
}

/* Read every active PU, finish exact event coverage, and emit review files. */
static bool Collect_Image(const char *input_path, const char *c_path,
                          const char *report_path)
{
  void *input = Open_Input_Info(const_cast<char *>(input_path));
  INT32 pu_count = 0;
  PU_Info *pu_tree = input == NULL || input == reinterpret_cast<void *>(-1)
                         ? NULL
                         : Read_Global_Info(&pu_count);
  if (pu_tree == NULL || pu_count <= 0)
    return false;

  VHO_FHE_CKKS2C_COLLECTION collection;
  uint32_t visited = 0;
  bool valid = Collect_PUs(pu_tree, &collection, &visited);

  std::vector<VHO_FHE_CKKS2C_GROUP> groups;
  VHO_FHE_CKKS2C_COLLECT_RESULT collected;
  memset(&collected, 0, sizeof(collected));
  valid = valid && visited == static_cast<uint32_t>(pu_count) &&
          VHO_FHE_CKKS2C_Finish_Collection(
              &collection, &groups, &collected, stderr);

  VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN public_join;
  VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT joined;
  memset(&joined, 0, sizeof(joined));
  valid = valid && VHO_FHE_CKKS2C_Build_Public_Join(
                       groups, &public_join, &joined, stderr);

  std::string generated;
  VHO_FHE_CKKS2C_EMIT_RESULT emitted;
  memset(&emitted, 0, sizeof(emitted));
  valid = valid && VHO_FHE_CKKS2C_Emit_Module(
                       "open64_fhe_cached_ckks_image", groups,
                       &generated, &emitted, stderr);

  std::map<uint32_t, uint32_t> operation_counts;
  uint32_t terminal_groups = 0;
  for (size_t group_index = 0; group_index < groups.size(); ++group_index) {
    if (groups[group_index].plan.source_final_step_index != UINT32_MAX)
      ++terminal_groups;
    for (size_t step = 0; step < groups[group_index].plan.steps.size(); ++step)
      ++operation_counts[groups[group_index].plan.steps[step].logical_operator];
  }

  std::string report;
  if (valid) {
    std::ostringstream report_stream;
    report_stream << "format=open64.ckks2c.collection.v1\n"
                  << "pu_count=" << collected.pu_count << '\n'
                  << "source_count=" << collected.source_count << '\n'
                  << "group_count=" << collected.group_count << '\n'
                  << "terminal_group_count=" << terminal_groups << '\n'
                  << "event_count=" << collected.event_count << '\n'
                  << "external_asset_count="
                  << collected.external_asset_count << '\n'
                  << "key_use_count=" << collected.key_use_count << '\n'
                  << "canonical_pu_count=" << joined.canonical_pu_count << '\n'
                  << "canonical_source_count="
                  << joined.canonical_source_count << '\n'
                  << "public_static_descriptor_count="
                  << joined.static_descriptor_count << '\n'
                  << "public_dynamic_binding_count="
                  << joined.dynamic_binding_count << '\n'
                  << "private_group_binding_count="
                  << joined.private_group_binding_count << '\n'
                  << "representation_binding_count="
                  << joined.representation_binding_count << '\n'
                  << "public_join_sha256="
                  << public_join.identity_sha256 << '\n'
                  << "emitted_operation_count=" << emitted.operation_count
                  << '\n'
                  << "emitted_source_value_count="
                  << emitted.source_value_count << '\n'
                  << "emitted_bound_value_count="
                  << emitted.bound_value_count << '\n';
    for (std::map<uint32_t, uint32_t>::const_iterator operation =
             operation_counts.begin(); operation != operation_counts.end();
         ++operation) {
      report_stream << "operator." << operation->first << '='
                    << operation->second << '\n';
    }
    for (size_t group_index = 0; group_index < groups.size(); ++group_index) {
      report_stream << "group." << groups[group_index].identity_sha256 << '='
                    << groups[group_index].function_name << '\n';
    }
    report = report_stream.str();
  }

  valid = valid && Write_Text(c_path, generated) &&
          Write_Text(report_path, report);
  Free_Input_Info();
  return valid;
}

/* Accept explicit paths so the test can audit a retained artifact family. */
int main(int argc, char **argv)
{
  if (argc != 4) {
    fprintf(stderr, "usage: %s input.B output.c report.txt\n", argv[0]);
    return 2;
  }
  Initialize_Reader_Context();
  if (!Collect_Image(argv[1], argv[2], argv[3])) {
    fprintf(stderr, "CKKS2C mapped-image collection failed\n");
    return 1;
  }
  fprintf(stderr, "CKKS2C mapped-image collection passed\n");
  return 0;
}
