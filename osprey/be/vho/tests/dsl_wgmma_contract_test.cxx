/* MMA4-H linked native source and malformed-body fixture in one active PU.
 * Test-only composition reuses MMA3's table/source/symbol/checkpoint helpers;
 * production code never depends on frontend builder or test construction.
 * Design: doc/AI-COMPILER-MMA4-H-HOPPER-LOWERING.md. */
#define main VHO_DSL_mma3_fixture_main
#include "dsl_typed_simt_contract_test.cxx"
#undef main
#include "dsl_wgmma_opt.h"
#include <string>

/* Exercise disabled/default, precision/target/shape/resource and level gates.
 * Every rejection preserves output and all symbol-table sizes. */
static BOOL VHO_DSL_wgmma_test_negatives(const DSL_TYPED_SIMT_RECORD &record,
    const DSL_NUMERICAL_CONTRACT_RECORD &numerical, const VHO_DSL_WGMMA_CONTROL &control,
    const VHO_DSL_WGMMA_BINDING &binding, WN *source)
{
    for (UINT32 fault = 0; fault < 38; ++fault) {
        DSL_TYPED_SIMT_RECORD bad_record = record;
        DSL_NUMERICAL_CONTRACT_RECORD bad_numerical = numerical;
        VHO_DSL_WGMMA_CONTROL bad_control = control;
        VHO_DSL_WGMMA_BINDING bad_binding = binding;
        INT32 saved_level = Opt_Level;
        TY_IDX shared_type = ST_type(binding.shared_a);
        WN *input_source = source;
        switch (fault) {
        case 0: bad_control.enable = 0; break;
        case 1: bad_control.profile_id = 3; break;
        case 2: bad_control.profile_id = numerical.input_format == 5 ? 2 : 1; break;
        case 3: bad_control.arch_id = DSL_HARDWARE_ARCH_SM_90; break;
        case 4: bad_control.ptx = 799; break;
        case 5: bad_control.toolkit = 11999; break;
        case 6: bad_control.shared_limit_bytes = 4095; break;
        case 7: bad_control.reserved = 1; break;
        case 8: case 9: case 10: case 11: case 12: bad_control.acceptance_flags &= ~(1u << (fault - 8)); break;
        case 13: bad_control.acceptance_flags = 63; break;
        case 14: bad_numerical.permissions = 0; break;
        case 15: bad_numerical.input_format = DSL_ACC_FORMAT_F32; break;
        case 16: bad_numerical.product_format = numerical.input_format; break;
        case 17: bad_numerical.maximum_span = 0; break;
        case 18: bad_numerical.evidence_id = 0; break;
        case 19: bad_numerical.output_format = numerical.input_format; break;
        case 20: bad_record.problem_m = 63; break;
        case 21: bad_record.problem_n = 63; break;
        case 22: bad_record.problem_k = 15; break;
        case 23: bad_record.threads_per_cta = 64; break;
        case 24: bad_record.generation = 0; break;
        case 25: bad_record.owner_pu_st++; break;
        case 26: bad_binding.shared_b = binding.shared_a; break;
        case 27: bad_binding.shared_a = binding.fragment; break;
        case 28: bad_binding.fragment = binding.shared_a; break;
        case 29: bad_binding.tid = binding.simt.index; break;
        case 30: bad_binding.barrier = NULL; break;
        case 31: bad_binding.block_dim = binding.grid_dim; break;
        case 32: bad_record.numerical_acceptance_id = 0; break;
        case 33: case 34: case 35: Opt_Level = fault - 33; break;
        case 36: {
            TY_IDX misaligned = shared_type; Set_TY_align(misaligned, 8);
            Set_ST_type(*binding.shared_a, misaligned); break;
        }
        case 37:
            input_source = WN_COPY_Tree(source);
            WN_set_opcode(VHO_DSL_test_find(input_source, OPR_MPY, MTYPE_F4), OPC_F4ADD); break;
        }
        UINT32 locals = ST_Table_Size(CURRENT_SYMTAB), globals = ST_Table_Size(GLOBAL_SYMTAB);
        WN *output = source;
        BOOL accepted = VHO_DSL_wgmma_materialize(&bad_record, &bad_numerical, &bad_control, &bad_binding, input_source, &output, NULL);
        Opt_Level = saved_level;
        Set_ST_type(*binding.shared_a, shared_type);
        if (input_source != source) WN_DELETE_Tree(input_source);
        if (accepted || output != source || locals != ST_Table_Size(CURRENT_SYMTAB) || globals != ST_Table_Size(GLOBAL_SYMTAB)) return FALSE;
    }
    return VHO_DSL_typed_simt_verify(&record, &numerical, &binding.simt, VHO_DSL_TYPED_SIMT_GRID, source, stderr);
}

/* Corrupt actual operations, not just report labels. A fresh CLASS_NAME owns
 * changed assembly text so no mutation can alter the original valid body. */
static BOOL VHO_DSL_wgmma_test_mutations(const DSL_TYPED_SIMT_RECORD &record,
    const DSL_NUMERICAL_CONTRACT_RECORD &numerical, const VHO_DSL_WGMMA_CONTROL &control,
    const VHO_DSL_WGMMA_BINDING &binding, WN *body)
{
    const char *remove[] = {"wgmma.fence.sync.aligned;", "wgmma.commit_group.sync.aligned;",
        "wgmma.wait_group.sync.aligned 0;", "68720001024", "m64n64k16", "st.f32 [%0+124], acc31;"};
    for (UINT32 fault = 0; fault < 15; ++fault) {
        WN *bad = WN_COPY_Tree(body);
        WN *proxy = VHO_DSL_test_find(bad, OPR_ASM_STMT), *atom = WN_next(WN_next(proxy));
        if (fault < 6) {
            std::string text = WN_asm_string(atom);
            size_t position = text.find(remove[fault]);
            if (position == std::string::npos) return FALSE;
            text.erase(position, strlen(remove[fault]));
            ST *identity = New_ST(CURRENT_SYMTAB);
            ST_Init(identity, Save_Str(text.c_str()), CLASS_NAME, SCLASS_UNKNOWN, EXPORT_LOCAL, TY_IDX_ZERO);
            Set_ST_Srcpos(*identity, binding.simt.source_position);
            WN_st_idx(atom) = ST_st_idx(identity);
        } else switch (fault) {
        case 6: WN_Reset_Asm_Clobbers_Mem(atom); break;
        case 7: WN_Reset_Asm_Volatile(proxy); break;
        case 8: WN_st_idx(WN_kid0(WN_kid(atom, 3))) = ST_st_idx(binding.shared_b); break;
        case 9: ++WN_asm_opnd_num(WN_kid(atom, 2)); break;
        case 10: WN_set_opcode(VHO_DSL_test_find(bad, OPR_ILOAD), OPC_F4F4ILOAD); break;
        case 11: WN_const_val(VHO_DSL_test_find(bad, OPR_INTCONST))++; break;
        case 12: WN_Set_Call_Non_Data_Mod(VHO_DSL_test_find(bad, OPR_CALL)); break;
        case 13: WN_Set_Linenum(atom, 0); break;
        case 14: WN_st_idx(WN_kid0(WN_kid(atom, 2))) = ST_st_idx(binding.shared_a); break;
        }
        UINT32 locals = ST_Table_Size(CURRENT_SYMTAB), globals = ST_Table_Size(GLOBAL_SYMTAB);
        BOOL accepted = VHO_DSL_wgmma_verify(&record, &numerical, &control, &binding, bad, NULL);
        FILE *inspection = tmpfile();
        if (!inspection || accepted || VHO_DSL_wgmma_print(inspection, &record, &numerical, &control, &binding, bad) ||
            ftell(inspection) || locals != ST_Table_Size(CURRENT_SYMTAB) || globals != ST_Table_Size(GLOBAL_SYMTAB)) return FALSE;
        fclose(inspection); WN_DELETE_Tree(bad);
    }
    WN *shared_tree = WN_COPY_Tree(body);
    WN *proxy = VHO_DSL_test_find(shared_tree, OPR_ASM_STMT), *atom = WN_next(WN_next(proxy));
    WN *saved = WN_kid0(WN_kid(atom, 4));
    WN_kid0(WN_kid(atom, 4)) = WN_kid0(WN_kid(atom, 3));
    BOOL shared_accepted = VHO_DSL_wgmma_verify(&record, &numerical, &control, &binding, shared_tree, NULL);
    WN_kid0(WN_kid(atom, 4)) = saved; WN_DELETE_Tree(shared_tree);
    if (shared_accepted) return FALSE;
    DSL_TYPED_SIMT_RECORD stale = record; ++stale.generation;
    DSL_NUMERICAL_CONTRACT_RECORD changed = numerical; ++changed.evidence_id;
    return !VHO_DSL_wgmma_verify(&stale, &numerical, &control, &binding, body, NULL) &&
        !VHO_DSL_wgmma_verify(&record, &changed, &control, &binding, body, NULL);
}

/* Build a canonical typed GRID source, retain it immediately before lowering,
 * then publish only the verified full-wait Hopper body. No default selection. */
int main(int argc, char **argv)
{
    if (argc != 8) return 2;
    VHO_DSL_test_initialize(); Opt_Level = 3; Enable_WN_Simp = atoi(argv[7]);
    PU_Info *pu = DSL_Builder_Create_Minimal_PU("typed_simt_case");
    DSL_BUILDER_PU_SOURCE_IDENTITY identity = {};
    identity.canonical_definition_name = "typed_simt_case"; identity.defining_module = "ai.wgmma.hopper";
    identity.defining_file = __FILE__; identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(pu, &identity)) return 3;
    UINT32 file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
    USRCPOS position; USRCPOS_clear(position); USRCPOS_filenum(position) = file_id; USRCPOS_linenum(position) = __LINE__;
    VHO_DSL_WGMMA_BINDING binding = {};
    binding.simt.source_position = USRCPOS_srcpos(position);
    TY_IDX input_type = Make_Pointer_Type(MTYPE_To_TY(MTYPE_U2)), output_type = Make_Pointer_Type(MTYPE_To_TY(MTYPE_F4));
    binding.simt.input_a = VHO_DSL_test_symbol("input_a", input_type, SCLASS_FORMAL, binding.simt.source_position);
    binding.simt.input_b = VHO_DSL_test_symbol("input_b", input_type, SCLASS_FORMAL, binding.simt.source_position);
    binding.simt.output = VHO_DSL_test_symbol("output", output_type, SCLASS_FORMAL, binding.simt.source_position);
    ST **locals[] = {&binding.simt.row, &binding.simt.column, &binding.simt.index, &binding.simt.raw, &binding.simt.fraction,
        &binding.simt.exponent, &binding.simt.wide, &binding.simt.a_value, &binding.simt.b_value, &binding.simt.product, &binding.simt.accumulator};
    const char *names[] = {"row", "column", "reduction_index", "raw_bits", "fraction", "exponent", "wide_bits", "a_value", "b_value", "rounded_product", "accumulator"};
    TY_IDX volatile_type = Copy_TY(MTYPE_To_TY(MTYPE_F4)); Set_TY_is_volatile(volatile_type);
    for (UINT32 index = 0; index < 11; ++index) {
        TY_IDX type = index < 2 ? MTYPE_To_TY(MTYPE_I8) : index == 2 ? MTYPE_To_TY(MTYPE_I4) : index < 7 ? MTYPE_To_TY(MTYPE_U4) : index < 9 ? MTYPE_To_TY(MTYPE_F4) : volatile_type;
        *locals[index] = VHO_DSL_test_symbol(names[index], type, SCLASS_AUTO, binding.simt.source_position);
    }
    VHO_DSL_test_coordinates(&binding.simt);
    ST **dimensions[] = {&binding.block_dim, &binding.grid_dim}; const char *dimension_names[] = {"blockDim", "gridDim"};
    for (UINT32 index = 0; index < 2; ++index) {
        *dimensions[index] = New_ST(GLOBAL_SYMTAB);
        ST_Init(*dimensions[index], Save_Str(dimension_names[index]), CLASS_VAR, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, ST_type(binding.simt.thread_idx));
        Set_ST_Srcpos(**dimensions[index], binding.simt.source_position);
    }
    ST **private_symbols[] = {&binding.tid, &binding.copy, &binding.base, &binding.row, &binding.column};
    const char *private_names[] = {"mma_tid", "mma_copy", "mma_base", "mma_row", "mma_column"};
    for (UINT32 index = 0; index < 5; ++index)
        *private_symbols[index] = VHO_DSL_test_symbol(private_names[index], MTYPE_To_TY(MTYPE_I8), SCLASS_AUTO, binding.simt.source_position);
    ST **shared[] = {&binding.shared_a, &binding.shared_b}; const char *shared_names[] = {"mma_shared_a", "mma_shared_b"};
    for (UINT32 index = 0; index < 2; ++index) {
        TY_IDX type = Make_Array_Type(MTYPE_U2, 1, 1024); Set_TY_align(type, 16); Set_TY_is_user_align(type);
        *shared[index] = VHO_DSL_test_symbol(shared_names[index], type, SCLASS_AUTO, binding.simt.source_position);
        Set_ST_device_symbol_kind(**shared[index], DEVICE_SYMBOL_SHARED);
    }
    binding.fragment = VHO_DSL_test_symbol("mma_fragment", Make_Array_Type(MTYPE_F4, 1, 32), SCLASS_AUTO, binding.simt.source_position);
    TY_IDX barrier_type; TY &barrier_signature = New_TY(barrier_type);
    TY_Init(barrier_signature, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0); Set_TY_has_prototype(barrier_type);
    TYLIST_IDX barrier_arguments;
    Set_TYLIST_type(New_TYLIST(barrier_arguments), MTYPE_To_TY(MTYPE_V)); Set_TY_tylist(barrier_signature, barrier_arguments);
    Set_TYLIST_type(New_TYLIST(barrier_arguments), TY_IDX_ZERO);
    PU_IDX barrier_pu;
    PU &barrier = New_PU(barrier_pu); PU_Init(barrier, barrier_type, GLOBAL_SYMTAB + 1); Set_PU_c_lang(barrier);
    binding.barrier = New_ST(GLOBAL_SYMTAB);
    ST_Init(binding.barrier, Save_Str("__syncthreads"), CLASS_FUNC, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, barrier_type);
    Set_ST_pu(binding.barrier, barrier_pu); Set_ST_Srcpos(*binding.barrier, binding.simt.source_position);
    DSL_TYPED_SIMT_RECORD record = {};
    record.id = record.version = record.numerical_contract_id = record.numerical_acceptance_id = record.generation = 1;
    record.owner_pu_st = PU_Info_proc_sym(pu); record.disjoint_output = 1; record.threads_per_cta = 128;
    record.problem_m = strtoul(argv[4], NULL, 10); record.problem_n = strtoul(argv[5], NULL, 10); record.problem_k = strtoul(argv[6], NULL, 10);
    DSL_NUMERICAL_CONTRACT_RECORD numerical = {};
    numerical.id = 1; numerical.source_format = numerical.input_format = strtoul(argv[3], NULL, 10);
    numerical.product_format = numerical.partial_format = numerical.outer_format = numerical.output_format = DSL_ACC_FORMAT_F32;
    numerical.permissions = DSL_ACC_PERMISSION_REASSOCIATE; numerical.maximum_span = record.problem_k;
    numerical.evidence_kind = DSL_ACC_EVIDENCE_ISA; numerical.evidence_id = 12081;
    VHO_DSL_WGMMA_CONTROL control = {};
    control.enable = 1; control.profile_id = numerical.input_format == 5 ? 1 : 2;
    control.arch_id = DSL_HARDWARE_ARCH_SM_90A; control.ptx = 807; control.toolkit = 12081;
    control.acceptance_flags = VHO_DSL_WGMMA_ACCEPT_ISA; control.shared_limit_bytes = 4096;
    ST *function = &St_Table[PU_Info_proc_sym(pu)]; Set_ST_device_symbol_kind(*function, DEVICE_SYMBOL_KERNEL); Set_ST_export(function, EXPORT_PREEMPTIBLE);
    TY_IDX function_type = ST_type(function); Set_TY_has_prototype(function_type); TYLIST_IDX list;
    Set_TYLIST_type(New_TYLIST(list), MTYPE_To_TY(MTYPE_V)); Set_TY_tylist(Ty_Table[function_type], list);
    Set_TYLIST_type(New_TYLIST(list), input_type); Set_TYLIST_type(New_TYLIST(list), input_type);
    Set_TYLIST_type(New_TYLIST(list), output_type); Set_TYLIST_type(New_TYLIST(list), TY_IDX_ZERO);
    WN *before = NULL;
    if (!VHO_DSL_typed_simt_materialize(&record, &numerical, &binding.simt, VHO_DSL_TYPED_SIMT_GRID, &before, stderr)) return 4;
    WN *entry = WN_CreateEntry(3, function, before, NULL, NULL);
    ST *formals[] = {binding.simt.input_a, binding.simt.input_b, binding.simt.output};
    for (UINT32 index = 0; index < 3; ++index) WN_kid(entry, index) = WN_CreateIdname(0, formals[index]);
    Set_PU_Info_tree_ptr(pu, entry);
    if (!VHO_DSL_wgmma_test_negatives(record, numerical, control, binding, before) || !DSL_Gatekeeper_Verify_PU(pu, stderr, NULL)) return 5;
    if (!VHO_DSL_test_image(pu, argv[1])) return 6;
    WN *after = NULL;
    if (!VHO_DSL_wgmma_materialize(&record, &numerical, &control, &binding, before, &after, stderr)) return 7;
    if (!VHO_DSL_wgmma_test_mutations(record, numerical, control, binding, after)) return 8;
    WN_func_body(entry) = after;
    if (!DSL_Gatekeeper_Verify_PU(pu, stderr, NULL) || !VHO_DSL_wgmma_print(stdout, &record, &numerical, &control, &binding, after)) return 9;
    if (!VHO_DSL_test_image(pu, argv[2])) return 10;
    printf("MMA4-H passed: negatives=38 mutations=16 stale_records=2 default=SIMT native_gpu=unknown timing=unknown\n");
    return 0;
}
