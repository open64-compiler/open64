/* Linked native-source ingestion and same-PU serial/SIMT WHIRL producer.
 * Uses the real VHO service and standard mapped-image writer, not CUDA text.
 * Design: doc/AI-COMPILER-MMA3-TYPED-SIMT-BASELINE.md. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#if !defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif
#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "wn_util.h"
#include "wn_tree_util.h"
#include "stab.h"
#include "symtab_utils.h"
#include "strtab.h"
#include "pu_info.h"
#include "glob.h"
#include "ir_reader.h"
#include "ir_bwrite.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "config_opt.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "dsl_builder.h"
#include "dsl_typed_simt_opt.h"
#include "dsl_gatekeeper.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

/* The standalone producer has no interrupted driver publication to clean. */
void Signal_Cleanup(INT signal_number) {}

/* Fixture diagnostics do not use target-specific parameter formatting. */
const char *Host_Format_Parm(INT kind, MEM_PTR parameter) { return ""; }

/* Initialize the same common Open64 tables used by real native ingestion. */
static void VHO_DSL_test_initialize()
{
    MEM_Initialize();
    Set_Error_Tables(Phases, host_errlist);
    Init_Error_Handler(10);
    Set_Error_File(NULL); Set_Error_Line(ERROR_LINE_UNKNOWN);
    Preconfigure(); Init_Controls_Tbl(); ABI_Name = "n64"; Configure();
    IR_reader_init(); Initialize_Symbol_Tables(TRUE); DST_Init(NULL, 0);
}

/* Create only test-owned source symbols; formals use the real pointer ABI. */
static ST *VHO_DSL_test_symbol(const char *name, TY_IDX type, ST_SCLASS storage, SRCPOS source)
{
    ST *symbol = New_ST(CURRENT_SYMTAB);
    ST_Init(symbol, Save_Str(name), CLASS_VAR, storage, EXPORT_LOCAL, type);
    Set_ST_Srcpos(*symbol, source);
    if (storage == SCLASS_FORMAL) Set_ST_is_value_parm(symbol);
    return symbol;
}

/* Bind the existing CUDA dim3 ABI in the source fixture. Production callers
 * already own these builtin symbols; the lowerer only verifies/reads them. */
static void VHO_DSL_test_coordinates(VHO_DSL_TYPED_SIMT_BINDING *binding)
{
    TY_IDX type_index;
    TY &type = New_TY(type_index);
    TY_Init(type, 12, KIND_STRUCT, MTYPE_M, Save_Str("open64_cuda_dim3"));
    Set_TY_align(type_index, 4);
    const char *names[] = {"x", "y", "z"};
    for (UINT32 ordinal = 0; ordinal < 3; ++ordinal) {
        FLD_HANDLE field = New_FLD();
        if (!ordinal) Set_TY_fld(type, field);
        FLD_Init(field, Save_Str(names[ordinal]), MTYPE_To_TY(MTYPE_U4), ordinal * 4);
        if (ordinal == 2) Set_FLD_last_field(field);
    }
    ST **symbols[] = {&binding->thread_idx, &binding->block_idx};
    const char *builtins[] = {"threadIdx", "blockIdx"};
    for (UINT32 ordinal = 0; ordinal < 2; ++ordinal) {
        *symbols[ordinal] = New_ST(GLOBAL_SYMTAB);
        ST_Init(*symbols[ordinal], Save_Str(builtins[ordinal]), CLASS_VAR, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, type_index);
    }
}

/* Fork a same-state checkpoint because the traditional writer finalizes table
 * records in place. Child-only finalization preserves the live parent PU for
 * the next transformation. Publish only a fully closed mapped image. */
static BOOL VHO_DSL_test_image(PU_Info *pu, const char *path)
{
    char temporary[4096];
    if (snprintf(temporary, sizeof(temporary), "%s.tmp", path) >= sizeof(temporary) ||
        access(path, F_OK) == 0 || access(temporary, F_OK) == 0) return FALSE;
    fflush(NULL);
    pid_t child = fork();
    if (child < 0) return FALSE;
    if (!child) {
        Irb_File_Name = temporary;
        if (!Open_Output_Info(Irb_File_Name)) _exit(1);
        Write_PU_Info(pu); Write_Global_Info(pu); Close_Output_Info();
        _exit(rename(temporary, path) == 0 ? 0 : 1);
    }
    int status = 0;
    return waitpid(child, &status, 0) == child && WIFEXITED(status) && WEXITSTATUS(status) == 0;
}

/* All unsupported source contracts must preserve output and inspection. */
static BOOL VHO_DSL_test_negatives(const DSL_TYPED_SIMT_RECORD &record,
    const DSL_NUMERICAL_CONTRACT_RECORD &numerical, const VHO_DSL_TYPED_SIMT_BINDING &binding, WN *body)
{
    for (UINT32 fault = 0; fault < 25; ++fault) {
        DSL_TYPED_SIMT_RECORD bad_record = record;
        DSL_NUMERICAL_CONTRACT_RECORD bad_numerical = numerical;
        VHO_DSL_TYPED_SIMT_BINDING bad_binding = binding;
        switch (fault) {
        case 0: bad_record.owner_pu_st++; break;
        case 1: bad_record.numerical_acceptance_id = 0; break;
        case 2: bad_record.generation = 0; break;
        case 3: bad_record.problem_m = 0; break;
        case 4: bad_record.problem_n = 0; break;
        case 5: bad_record.problem_k = 0; break;
        case 6: bad_record.problem_m = bad_record.problem_n = 0x7ffffffe; break;
        case 7: bad_record.threads_per_cta = 1025; break;
        case 8: bad_record.disjoint_output = 0; break;
        case 9: bad_record.reserved = 1; break;
        case 10: bad_numerical.input_format = DSL_ACC_FORMAT_F32; break;
        case 11: bad_numerical.source_format = DSL_ACC_FORMAT_F32; break;
        case 12: bad_numerical.output_format = numerical.input_format; break;
        case 13: bad_numerical.partial_format = numerical.input_format; break;
        case 14: bad_numerical.product_format = numerical.input_format; break;
        case 15: bad_numerical.maximum_span = 0; break;
        case 16: bad_numerical.permissions = DSL_ACC_PERMISSION_NARROW; break;
        case 17: bad_numerical.evidence_id = 0; break;
        case 18: bad_binding.accumulator = binding.product; break;
        case 19: bad_binding.input_a = binding.output; break;
        case 20: bad_binding.output = binding.input_b; break;
        case 21: bad_binding.product = binding.a_value; break;
        case 22: bad_binding.thread_idx = binding.block_idx; break;
        case 23: bad_binding.source_position = 0; break;
        case 24: bad_record.numerical_contract_id++; break;
        }
        WN *unchanged = body;
        if (VHO_DSL_typed_simt_materialize(&bad_record, &bad_numerical, &bad_binding,
                VHO_DSL_TYPED_SIMT_GRID, &unchanged, NULL) || unchanged != body) return FALSE;
        FILE *file = tmpfile();
        if (!file || VHO_DSL_typed_simt_print(file, &bad_record, &bad_numerical, &bad_binding,
            VHO_DSL_TYPED_SIMT_GRID, body) || ftell(file)) return FALSE;
        fclose(file);
    }
    BOOL saved_fast = OPT_Fast_Math;
    OPT_Fast_Math = TRUE;
    WN *unchanged = body;
    BOOL rejected = !VHO_DSL_typed_simt_materialize(&record, &numerical, &binding,
        VHO_DSL_TYPED_SIMT_GRID, &unchanged, NULL) && unchanged == body;
    OPT_Fast_Math = saved_fast;
    return rejected;
}

/* Locate an owned node for independent postcheck corruption. No simplifier
 * is called, and the valid original body remains untouched. */
static WN *VHO_DSL_test_find(WN *tree, OPERATOR operation, TYPE_ID type = MTYPE_UNKNOWN)
{
    if (WN_operator(tree) == operation && (type == MTYPE_UNKNOWN || WN_rtype(tree) == type)) return tree;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *statement = WN_first(tree); statement; statement = WN_next(statement)) {
            WN *found = VHO_DSL_test_find(statement, operation, type);
            if (found) return found;
        }
    } else {
        for (INT32 index = 0; index < WN_kid_count(tree); ++index) {
            WN *found = VHO_DSL_test_find(WN_kid(tree, index), operation, type);
            if (found) return found;
        }
    }
    return NULL;
}

/* Verify corruption of representation, rounding, K order, address/guard and
 * contract lineage independently from the constructor's preflight checks. */
static BOOL VHO_DSL_test_mutations(const DSL_TYPED_SIMT_RECORD &record,
    const DSL_NUMERICAL_CONTRACT_RECORD &numerical, const VHO_DSL_TYPED_SIMT_BINDING &binding, WN *body)
{
    for (UINT32 fault = 0; fault < 10; ++fault) {
        WN *bad = WN_COPY_Tree(body);
        WN *changed = NULL;
        switch (fault) {
        case 0: changed = VHO_DSL_test_find(bad, OPR_ILOAD); WN_set_opcode(changed, OPC_F4F4ILOAD); break;
        case 1: changed = VHO_DSL_test_find(bad, OPR_TAS); WN_set_opcode(changed, OPC_F4U4CVT); break;
        case 2: changed = VHO_DSL_test_find(bad, OPR_MPY, MTYPE_F4); WN_set_opcode(changed, OPC_F4ADD); break;
        case 3: changed = VHO_DSL_test_find(bad, OPR_ADD, MTYPE_F4); WN_set_opcode(changed, OPC_F4SUB); break;
        case 4: changed = VHO_DSL_test_find(bad, OPR_DO_LOOP); ++WN_const_val(WN_kid1(WN_kid0(WN_step(changed)))); break;
        case 5: changed = VHO_DSL_test_find(bad, OPR_ISTORE); WN_st_idx(WN_kid0(changed)) = ST_st_idx(binding.a_value); break;
        case 6: changed = VHO_DSL_test_find(WN_kid0(VHO_DSL_test_find(bad, OPR_ILOAD)), OPR_INTCONST); ++WN_const_val(changed); break;
        case 7: changed = VHO_DSL_test_find(bad, OPR_DO_LOOP); WN_DELETE_FromBlock(WN_do_body(changed), WN_first(WN_do_body(changed))); break;
        case 8: changed = VHO_DSL_test_find(bad, OPR_LT, MTYPE_I4); if (!changed) changed = VHO_DSL_test_find(bad, OPR_LT); WN_set_opcode(changed, OPCODE_make_op(OPR_LE, WN_rtype(changed), WN_desc(changed))); break;
        case 9: changed = VHO_DSL_test_find(bad, OPR_EXTRACT_BITS); if (changed) WN_st_idx(WN_kid0(changed)) = ST_st_idx(binding.wide); else { changed = VHO_DSL_test_find(bad, OPR_INTCONST); ++WN_const_val(changed); } break;
        }
        UINT32 local_count = ST_Table_Size(CURRENT_SYMTAB), global_count = ST_Table_Size(GLOBAL_SYMTAB);
        BOOL accepted = VHO_DSL_typed_simt_verify(&record, &numerical, &binding, VHO_DSL_TYPED_SIMT_GRID, bad, NULL);
        if (accepted || ST_Table_Size(CURRENT_SYMTAB) != local_count || ST_Table_Size(GLOBAL_SYMTAB) != global_count) return FALSE;
        WN_DELETE_Tree(bad);
    }
    DSL_TYPED_SIMT_RECORD stale = record;
    ++stale.generation;
    if (VHO_DSL_typed_simt_verify(&stale, &numerical, &binding, VHO_DSL_TYPED_SIMT_GRID, body, NULL)) return FALSE;
    DSL_NUMERICAL_CONTRACT_RECORD changed = numerical;
    changed.permissions ^= DSL_ACC_PERMISSION_REASSOCIATE;
    return !VHO_DSL_typed_simt_verify(&record, &changed, &binding, VHO_DSL_TYPED_SIMT_GRID, body, NULL);
}

/* Ingest an explicitly accepted typed source function, publish its serial
 * reference immediately before replacement, then publish verified SIMT. */
int main(int argc, char **argv)
{
    if (argc != 10) return 2;
    VHO_DSL_test_initialize();
    Opt_Level = atoi(argv[8]); Enable_WN_Simp = atoi(argv[9]);
    PU_Info *pu = DSL_Builder_Create_Minimal_PU("typed_simt_case");
    DSL_BUILDER_PU_SOURCE_IDENTITY identity = {};
    identity.canonical_definition_name = "typed_simt_case";
    identity.defining_module = "ai.typed.simt"; identity.defining_file = __FILE__; identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(pu, &identity)) return 3;
    UINT32 file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
    USRCPOS position; USRCPOS_clear(position);
    USRCPOS_filenum(position) = file_id; USRCPOS_linenum(position) = __LINE__;
    VHO_DSL_TYPED_SIMT_BINDING binding = {};
    binding.source_position = USRCPOS_srcpos(position);
    TY_IDX input_type = Make_Pointer_Type(MTYPE_To_TY(MTYPE_U2));
    TY_IDX output_type = Make_Pointer_Type(MTYPE_To_TY(MTYPE_F4));
    binding.input_a = VHO_DSL_test_symbol("input_a", input_type, SCLASS_FORMAL, binding.source_position);
    binding.input_b = VHO_DSL_test_symbol("input_b", input_type, SCLASS_FORMAL, binding.source_position);
    binding.output = VHO_DSL_test_symbol("output", output_type, SCLASS_FORMAL, binding.source_position);
    ST **locals[] = {&binding.row, &binding.column, &binding.index, &binding.raw, &binding.fraction,
        &binding.exponent, &binding.wide, &binding.a_value, &binding.b_value, &binding.product, &binding.accumulator};
    const char *names[] = {"row", "column", "reduction_index", "raw_bits", "fraction", "exponent", "wide_bits",
        "a_value", "b_value", "rounded_product", "accumulator"};
    TY_IDX volatile_type = Copy_TY(MTYPE_To_TY(MTYPE_F4)); Set_TY_is_volatile(volatile_type);
    for (UINT32 ordinal = 0; ordinal < 11; ++ordinal) {
        TY_IDX type = ordinal < 2 ? MTYPE_To_TY(MTYPE_I8) : ordinal == 2 ? MTYPE_To_TY(MTYPE_I4) :
            ordinal < 7 ? MTYPE_To_TY(MTYPE_U4) : ordinal < 9 ? MTYPE_To_TY(MTYPE_F4) : volatile_type;
        *locals[ordinal] = VHO_DSL_test_symbol(names[ordinal], type, SCLASS_AUTO, binding.source_position);
    }
    VHO_DSL_test_coordinates(&binding);
    DSL_TYPED_SIMT_RECORD record = {};
    record.id = record.version = record.numerical_contract_id = record.numerical_acceptance_id = record.generation = 1;
    record.owner_pu_st = PU_Info_proc_sym(pu); record.disjoint_output = 1;
    record.problem_m = strtoul(argv[4], NULL, 10); record.problem_n = strtoul(argv[5], NULL, 10);
    record.problem_k = strtoul(argv[6], NULL, 10); record.threads_per_cta = strtoul(argv[7], NULL, 10);
    DSL_NUMERICAL_CONTRACT_RECORD numerical = {};
    numerical.id = 1; numerical.source_format = numerical.input_format = strtoul(argv[3], NULL, 10);
    numerical.product_format = numerical.partial_format = numerical.outer_format = numerical.output_format = DSL_ACC_FORMAT_F32;
    numerical.maximum_span = record.problem_k; numerical.evidence_kind = DSL_ACC_EVIDENCE_ISA; numerical.evidence_id = 1;
    ST *function = &St_Table[PU_Info_proc_sym(pu)];
    Set_ST_device_symbol_kind(*function, DEVICE_SYMBOL_KERNEL); Set_ST_export(function, EXPORT_PREEMPTIBLE);
    TY_IDX function_type = ST_type(function); Set_TY_has_prototype(function_type);
    TYLIST_IDX list; Set_TYLIST_type(New_TYLIST(list), MTYPE_To_TY(MTYPE_V)); Set_TY_tylist(Ty_Table[function_type], list);
    Set_TYLIST_type(New_TYLIST(list), input_type); Set_TYLIST_type(New_TYLIST(list), input_type);
    Set_TYLIST_type(New_TYLIST(list), output_type); Set_TYLIST_type(New_TYLIST(list), TY_IDX_ZERO);
    WN *before = NULL;
    if (!VHO_DSL_typed_simt_materialize(&record, &numerical, &binding, VHO_DSL_TYPED_SIMT_SERIAL, &before, stderr)) return 4;
    WN *entry = WN_CreateEntry(3, function, before, NULL, NULL);
    ST *formals[] = {binding.input_a, binding.input_b, binding.output};
    for (UINT32 ordinal = 0; ordinal < 3; ++ordinal) WN_kid(entry, ordinal) = WN_CreateIdname(0, formals[ordinal]);
    Set_PU_Info_tree_ptr(pu, entry);
    if (!DSL_Gatekeeper_Verify_PU(pu, stderr, NULL)) return 11;
    if (!VHO_DSL_typed_simt_print(stdout, &record, &numerical, &binding, VHO_DSL_TYPED_SIMT_SERIAL, before)) return 5;
    if (!VHO_DSL_test_image(pu, argv[1])) return 6;
    WN *after = NULL;
    if (!VHO_DSL_typed_simt_materialize(&record, &numerical, &binding, VHO_DSL_TYPED_SIMT_GRID, &after, stderr)) return 7;
    if (!VHO_DSL_test_negatives(record, numerical, binding, after) ||
        !VHO_DSL_test_mutations(record, numerical, binding, after)) return 8;
    WN_func_body(entry) = after;
    if (!DSL_Gatekeeper_Verify_PU(pu, stderr, NULL)) return 12;
    if (!VHO_DSL_typed_simt_print(stdout, &record, &numerical, &binding, VHO_DSL_TYPED_SIMT_GRID, after)) return 9;
    if (!VHO_DSL_test_image(pu, argv[2])) return 10;
    printf("MMA3 typed baseline passed: format=%u M=%u N=%u K=%u negatives=26 mutations=12 strict_fp32=true native_mma=false\n",
        numerical.input_format, record.problem_m, record.problem_n, record.problem_k);
    return 0;
}
