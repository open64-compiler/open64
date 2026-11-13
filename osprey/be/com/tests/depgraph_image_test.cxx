/* Copyright (C) 2026 Open64 Project */
/* Backend PU-local tree/dependence-image contract tests, using the production
 * shared writer and reader. Memory-backed fragments test auxiliary maps;
 * complete ELF/source/numerical evidence belongs to the native review lane.
 * Design: doc/WHIRL-DEPENDENCE-IMAGE-REPAIR.md. No binary layout change. */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>
#include <sys/elf_whirl.h>
#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"
#include "dep_graph.h"
#include "errors.h"
#include "erglob.h"
#include "err_host.tab"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

/* The test owns no driver output requiring signal cleanup. */
void Signal_Cleanup(INT) {}
/* Common error formatting needs no target-specific parameters in this test. */
const char *Host_Format_Parm(INT, MEM_PTR) { return "depgraph_image_test"; }

/* Keep fixture operations outside assertions, including in release tests. */
static void DEPGRAPH_test_require(bool valid, const char *contract)
{
    if (!valid) {
        fprintf(stderr, "FAIL %s\n", contract);
        exit(1);
    }
    printf("PASS %s\n", contract);
}

/* Supply a sufficiently large aligned output region without publishing a
 * partial ELF file. Production save_buf performs the actual image writes. */
static void DEPGRAPH_test_output(Output_File &file, std::vector<UINT64> &storage)
{
    memset(&file, 0, sizeof(file));
    storage.resize(4096, 0);
    file.map_addr = (char *)&storage[0];
    file.mapped_size = storage.size() * sizeof(UINT64);
    file.output_fd = -1;
    file.file_name = (char *)"depgraph-test-memory";
}

/* Prove refusals occur before native map mutation, including an unset offset
 * after an otherwise valid first vertex. Caller retains the input fragment. */
static void DEPGRAPH_test_refusal(char *data, size_t size, char *tree,
                                 const std::vector<WN *> &calls)
{
    for (size_t index = 0; index < calls.size(); ++index)
        WN_MAP_Set(WN_MAP_DEPGRAPH, calls[index], (void *)(INTPTR)1234);
    DEPGRAPH_test_require(Depgraph_Read(data, data + size, tree) == NULL,
                         "malformed_vertex_table_refused");
    for (size_t index = 0; index < calls.size(); ++index)
        DEPGRAPH_test_require(WN_MAP_Get(WN_MAP_DEPGRAPH, calls[index]) == (void *)(INTPTR)1234,
                             "refusal_preserves_native_maps");
}

/* Exercise each call form through the real mapped tree writer, then consume
 * the real graph writer's bytes. Fragment tests need no symbol semantics or
 * native execution; full-program correctness is checked by the review lane.
 * The explicit negative mode injects an internal missing-offset invariant. */
int main(int argc, char **argv)
{
    if (argc > 2 || (argc == 2 && strcmp(argv[1], "--missing-offset") != 0))
        return 2;
    MEM_Initialize();
    Init_Error_Handler(10);
    IR_reader_init();
    Current_Map_Tab = WN_MAP_TAB_Create(&MEM_local_pool);
    WN_MAP offsets = WN_MAP32_Create(&MEM_local_pool);
    Write_BE_Maps = TRUE;
    const OPERATOR operators[] = {
        OPR_CALL, OPR_ICALL, OPR_PICCALL, OPR_INTRINSIC_CALL
    };
    WN *block = WN_CreateBlock();
    std::vector<WN *> calls;
    for (size_t index = 0; index < sizeof(operators) / sizeof(operators[0]); ++index) {
        WN *call = WN_Create(operators[index], MTYPE_V, MTYPE_V, 0);
        DEPGRAPH_test_require(OPCODE_is_call(WN_opcode(call)), "fixture_is_call");
        WN_INSERT_BlockLast(block, call);
        calls.push_back(call);
    }
    Output_File tree_file;
    std::vector<UINT64> tree_storage;
    DEPGRAPH_test_output(tree_file, tree_storage);
    tree_file.file_size = 64;
    const off_t tree_base = tree_file.file_size;
    ir_b_write_tree(block, tree_base, &tree_file, offsets, NULL);
    ARRAY_DIRECTED_GRAPH16 graph(16, 16, WN_MAP_DEPGRAPH, DEP_ARRAY_GRAPH);
    for (size_t index = 0; index < calls.size(); ++index) {
        INT32 offset = WN_MAP32_Get(offsets, calls[index]);
        DEPGRAPH_test_require(offset > 0, "call_offset_recorded");
        WN *mapped = (WN *)(tree_file.map_addr + tree_base + offset);
        DEPGRAPH_test_require(WN_operator(mapped) == operators[index], "offset_names_same_call");
        DEPGRAPH_test_require(graph.Add_Vertex(calls[index]) == index + 1, "vertex_order");
    }
    Output_File graph_file;
    std::vector<UINT64> graph_storage;
    DEPGRAPH_test_output(graph_file, graph_storage);
    if (argc == 2) {
        WN *unmapped = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 0);
        graph.Add_Vertex(unmapped);
        Depgraph_Write(&graph, &graph_file, offsets);
        DEPGRAPH_test_require(false, "writer_must_refuse_unmapped_vertex");
    }
    Depgraph_Write(&graph, &graph_file, offsets);
    char *tree = tree_file.map_addr + tree_base;
    ARRAY_DIRECTED_GRAPH16 *restored = (ARRAY_DIRECTED_GRAPH16 *)
        Depgraph_Read(graph_file.map_addr, graph_file.map_addr + graph_file.file_size, tree);
    DEPGRAPH_test_require(restored != NULL, "graph_image_reopens");
    for (size_t index = 0; index < calls.size(); ++index)
        DEPGRAPH_test_require(WN_operator(restored->Get_Wn(index + 1)) == operators[index],
                             "restored_vertex_keeps_call_kind");
    CXX_DELETE(restored, Malloc_Mem_Pool);
    DEPGRAPH_test_refusal(graph_file.map_addr, 0, tree, calls);
    DEPGRAPH_test_refusal(graph_file.map_addr, 1, tree, calls);
    DEPGRAPH_test_refusal(graph_file.map_addr, 4 + 4 * (calls.size() - 1), tree, calls);
    Elf64_Word zero = 0;
    memcpy(graph_file.map_addr + 4 + sizeof(Elf64_Word), &zero, sizeof(zero));
    DEPGRAPH_test_refusal(graph_file.map_addr, graph_file.file_size, tree, calls);
    printf("image_abi ST=%lu symtab_header=%lu local_header=%lu word=%lu vertex=%lu edge=%lu\n",
           (unsigned long)sizeof(ST), (unsigned long)sizeof(SYMTAB_HEADER),
           (unsigned long)sizeof(LOCAL_SYMTAB_HEADER_TABLE),
           (unsigned long)sizeof(Elf64_Word), (unsigned long)sizeof(VINDEX16),
           (unsigned long)sizeof(EINDEX16));
    puts("depgraph_image_calls=4 malformed_vertex_cases=4 PASS");
    return 0;
}
