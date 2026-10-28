/* Linked active-PU MMA5-H fixture reusing the MMA4/MMA3 source construction.
 * Tests explicit pipelines, malformed protocols and original-source rollback.
 * Design: doc/AI-COMPILER-MMA5-H-HOPPER-PIPELINE.md. */
#include "dsl_wgmma_pipeline_opt.h"
#include "pu_info.h"
#include "wn.h"
#include <vector>

/* Run after the shared fixture constructs the canonical SIMT entry. */
static int VHO_DSL_pipeline_fixture(PU_Info *, WN *, DSL_TYPED_SIMT_RECORD &,
    DSL_NUMERICAL_CONTRACT_RECORD &, VHO_DSL_WGMMA_CONTROL &,
    VHO_DSL_WGMMA_BINDING &, WN *, char **);
#define VHO_DSL_WGMMA_PIPELINE_FIXTURE
#include "dsl_wgmma_contract_test.cxx"

/* Collect actual tree edges in traversal order for deterministic corruption. */
static void VHO_DSL_pipeline_nodes(WN *node, std::vector<WN *> &nodes)
{
    nodes.push_back(node);
    if (WN_operator(node) == OPR_BLOCK) {
        for (WN *stmt = WN_first(node); stmt; stmt = WN_next(stmt)) VHO_DSL_pipeline_nodes(stmt, nodes);
    } else for (INT32 kid = 0; kid < WN_kid_count(node); ++kid) VHO_DSL_pipeline_nodes(WN_kid(node, kid), nodes);
}

/* Every changed integer, assembly instruction/effect and call effect must
 * reject. Printing a rejected body is silent and never grows symbol tables. */
static int VHO_DSL_pipeline_mutations(const DSL_TYPED_SIMT_RECORD &record,
    const DSL_NUMERICAL_CONTRACT_RECORD &numerical, const VHO_DSL_WGMMA_PIPELINE_CONTROL &control,
    const VHO_DSL_WGMMA_PIPELINE_BINDING &binding, WN *body)
{
    std::vector<WN *> original; VHO_DSL_pipeline_nodes(body, original);
    UINT32 rejected = 0;
    for (UINT32 index = 0; index < original.size(); ++index) {
        OPERATOR operation = WN_operator(original[index]);
        if (operation != OPR_INTCONST && operation != OPR_ASM_STMT && operation != OPR_CALL) continue;
        WN *bad = WN_COPY_Tree(body); std::vector<WN *> nodes; VHO_DSL_pipeline_nodes(bad, nodes);
        WN *changed = nodes[index];
        if (operation == OPR_INTCONST) ++WN_const_val(changed);
        else if (operation == OPR_CALL) WN_Set_Call_Non_Data_Mod(changed);
        else {
            std::string text = WN_asm_string(changed);
            size_t position = text.find(';');
            if (position == std::string::npos) return -1;
            text.erase(0, position + 1);
            ST *identity = New_ST(CURRENT_SYMTAB);
            ST_Init(identity, Save_Str(text.c_str()), CLASS_NAME, SCLASS_UNKNOWN, EXPORT_LOCAL, TY_IDX_ZERO);
            Set_ST_Srcpos(*identity, binding.compute.simt.source_position); WN_st_idx(changed) = ST_st_idx(identity);
        }
        UINT32 locals = ST_Table_Size(CURRENT_SYMTAB), globals = ST_Table_Size(GLOBAL_SYMTAB);
        FILE *file = tmpfile();
        if (!file || VHO_DSL_wgmma_pipeline_verify(&record, &numerical, &control, &binding, bad, NULL) ||
            VHO_DSL_wgmma_pipeline_print(file, &record, &numerical, &control, &binding, bad) || ftell(file) ||
            locals != ST_Table_Size(CURRENT_SYMTAB) || globals != ST_Table_Size(GLOBAL_SYMTAB)) return -1;
        fclose(file); WN_DELETE_Tree(bad); ++rejected;
    }
    DSL_TYPED_SIMT_RECORD stale = record; ++stale.generation;
    DSL_NUMERICAL_CONTRACT_RECORD changed = numerical; ++changed.evidence_id;
    if (VHO_DSL_wgmma_pipeline_verify(&stale, &numerical, &control, &binding, body, NULL) ||
        VHO_DSL_wgmma_pipeline_verify(&record, &changed, &control, &binding, body, NULL)) return -1;
    const char *tokens[] = {"wgmma.fence.sync.aligned;", "wgmma.commit_group.sync.aligned;",
        "wgmma.wait_group.sync.aligned 0;", "add.u64 desc_a, desc_a, 128;",
        "mbarrier.arrive.expect_tx.shared::cta.b64 _, [ready], 4096;",
        "mbarrier.try_wait.parity.acquire.cta.shared::cta.b64 complete, [ready], 0;",
        "mbarrier.inval.shared::cta.b64 [ready];"};
    for (UINT32 token = 0; token < (control.delivery == 2 ? 7 : 4); ++token) {
        WN *bad = WN_COPY_Tree(body); std::vector<WN *> nodes; VHO_DSL_pipeline_nodes(bad, nodes);
        BOOL touched = FALSE;
        for (UINT32 index = 0; index < nodes.size(); ++index) {
            if (WN_operator(nodes[index]) != OPR_ASM_STMT) continue;
            std::string text = WN_asm_string(nodes[index]); size_t position = text.find(tokens[token]);
            if (position == std::string::npos) continue;
            text.erase(position, strlen(tokens[token]));
            ST *identity = New_ST(CURRENT_SYMTAB);
            ST_Init(identity, Save_Str(text.c_str()), CLASS_NAME, SCLASS_UNKNOWN, EXPORT_LOCAL, TY_IDX_ZERO);
            Set_ST_Srcpos(*identity, binding.compute.simt.source_position); WN_st_idx(nodes[index]) = ST_st_idx(identity);
            touched = TRUE; break;
        }
        if (!touched || VHO_DSL_wgmma_pipeline_verify(&record, &numerical, &control, &binding, bad, NULL)) return -1;
        WN_DELETE_Tree(bad); ++rejected;
    }
    for (UINT32 fault = 0; fault < 4; ++fault) {
        WN *bad = WN_COPY_Tree(body); std::vector<WN *> nodes; VHO_DSL_pipeline_nodes(bad, nodes);
        WN *atom = NULL;
        for (UINT32 index = 0; index < nodes.size(); ++index)
            if (WN_operator(nodes[index]) == OPR_ASM_STMT && strstr(WN_asm_string(nodes[index]), "wgmma.mma_async")) { atom = nodes[index]; break; }
        WN *saved = WN_kid0(WN_kid(atom, 4));
        if (fault == 0) WN_Reset_Asm_Volatile(atom);
        if (fault == 1) WN_Reset_Asm_Clobbers_Mem(atom);
        if (fault == 2) WN_kid0(WN_kid(atom, 4)) = WN_kid0(WN_kid(atom, 3));
        if (fault == 3) ++WN_asm_opnd_num(WN_kid(atom, 2));
        BOOL accepted = VHO_DSL_wgmma_pipeline_verify(&record, &numerical, &control, &binding, bad, NULL);
        if (fault == 2) WN_kid0(WN_kid(atom, 4)) = saved;
        WN_DELETE_Tree(bad);
        if (accepted) return -1;
        ++rejected;
    }
    return rejected;
}

/* Supply fictional scenarios only, then emit complete model inputs and exact
 * ledgers for an independent Python oracle. Unknowns must not become zeros. */
static BOOL VHO_DSL_pipeline_models(const DSL_TYPED_SIMT_RECORD &record,
    const DSL_NUMERICAL_CONTRACT_RECORD &numerical, const VHO_DSL_WGMMA_PIPELINE_CONTROL &control, UINT32 source_line)
{
    for (UINT32 scenario = 0; scenario < 8; ++scenario) {
        DSL_COMPUTE_MODEL_RECORD model;
        if (!DSL_compute_model_unknown(control.compute.profile_id, DSL_COMPUTE_WGMMA, &model)) return FALSE;
        if (scenario) {
            model.provenance = DSL_COMPUTE_MODEL_SYNTHETIC; model.unit = DSL_OPT_COST_UNIT_CYCLES;
            model.scenario_id = scenario; model.configured = 1; model.sms = 2; model.resident_cap = 4; model.queue_capacity = 2;
            DSL_HARDWARE_RESOURCE_RECORD resources = {32, 64, 2048, 1024, 65536, 255, 256, 4, 233472, 65536, 128, 1024};
            model.resources = resources;
            const uint64_t nominal[] = {64, 128, 8192, 128, 64, 128, 64, 100, 4, 400, 8, 12, 16, 8, 20, 40};
            for (UINT32 metric = 0; metric < 16; ++metric) {
                model.metrics[metric].known = 1; model.metrics[metric].nominal = nominal[metric];
                model.metrics[metric].lower = scenario == 2 ? nominal[metric] : nominal[metric] / 2;
                model.metrics[metric].upper = scenario == 2 ? nominal[metric] : nominal[metric] * 2;
            }
            if (scenario == 3) memset(&model.metrics[7], 0, sizeof(model.metrics[7]));
            if (scenario == 4) model.resources.shared_budget = 8192;
            if (scenario == 5) model.queue_capacity = 1;
            if (scenario == 6) model.resources.registers_per_sm = 8192;
            if (scenario == 7) model.resources.shared_budget = 512;
        }
        VHO_DSL_WGMMA_PIPELINE_RESULT result;
        if (!VHO_DSL_wgmma_pipeline_analyze(&record, &numerical, &control, &model, 384, &result, stderr) || result.emitted_family != DSL_COMPUTE_SIMT) return FALSE;
        if (scenario == 0 || scenario == 3)
            for (UINT32 candidate_id = 0; candidate_id < 4; ++candidate_id) if (result.candidates[candidate_id].known) return FALSE;
        printf("AI_MMA_PIPELINE_MODEL 1 INPUT scenario=%u M=%u N=%u K=%u limit=%u sms=%u cap=%u queue=%u smem_budget=%llu registers=%llu register_bytes=384 nominal=%u bounded=%u emitted=1 source_line=%u\n",
            scenario, record.problem_m, record.problem_n, record.problem_k, control.compute.shared_limit_bytes,
            model.sms, model.resident_cap, model.queue_capacity, (unsigned long long)model.resources.shared_budget,
            (unsigned long long)model.resources.registers_per_sm, result.nominal_candidate, result.bounded_candidate, source_line);
        for (UINT32 metric = 0; metric < 16; ++metric)
            printf("AI_MMA_PIPELINE_MODEL 1 METRIC scenario=%u metric=%u known=%u lower=%llu nominal=%llu upper=%llu source_line=%u\n",
                scenario, metric, model.metrics[metric].known, (unsigned long long)model.metrics[metric].lower,
                (unsigned long long)model.metrics[metric].nominal, (unsigned long long)model.metrics[metric].upper, source_line);
        for (UINT32 candidate_id = 0; candidate_id < 4; ++candidate_id) {
            const VHO_DSL_WGMMA_PIPELINE_CANDIDATE &candidate = result.candidates[candidate_id];
            printf("AI_MMA_PIPELINE_MODEL 1 CANDIDATE scenario=%u id=%u family=%u delivery=%u slots=%u groups=%u shared=%u eligible=%u known=%u registers=%llu rounded_shared=%llu resident=%llu limiting=%llu source_line=%u\n",
                scenario, candidate_id, candidate.family, candidate.delivery, candidate.slots, candidate.groups, candidate.shared_bytes,
                candidate.eligible, candidate.known, (unsigned long long)candidate.rounded_registers, (unsigned long long)candidate.rounded_shared,
                (unsigned long long)candidate.resident, (unsigned long long)candidate.limiting, source_line);
            for (UINT32 corner = 0; corner < 3; ++corner) {
                printf("AI_MMA_PIPELINE_MODEL 1 COST scenario=%u id=%u corner=%u kernel=%llu invocation=%llu terms=",
                    scenario, candidate_id, corner, (unsigned long long)candidate.kernel[corner], (unsigned long long)candidate.invocation[corner]);
                for (UINT32 term = 0; term < 8; ++term) printf("%s%llu", term ? "," : "", (unsigned long long)candidate.terms[corner][term]);
                printf(" source_line=%u\n", source_line);
            }
        }
        DSL_COMPUTE_MODEL_RECORD invalid = model; invalid.reserved = 1;
        VHO_DSL_WGMMA_PIPELINE_RESULT saved; memset(&saved, 0x5a, sizeof(saved)); result = saved;
        if (VHO_DSL_wgmma_pipeline_analyze(&record, &numerical, &control, &invalid, 384, &result, NULL) || memcmp(&result, &saved, sizeof(saved))) return FALSE;
        if (scenario == 2) {
            for (UINT32 fault = 0; fault < 7; ++fault) {
                VHO_DSL_WGMMA_PIPELINE_CONTROL rejected = control;
                DSL_TYPED_SIMT_RECORD tail = record; DSL_NUMERICAL_CONTRACT_RECORD strict = numerical;
                if (fault == 0) rejected.compute.enable = 0;
                if (fault == 1) rejected.compute.arch_id = DSL_HARDWARE_ARCH_SM_90;
                if (fault == 2) rejected.compute.acceptance_flags = 30;
                if (fault == 3) strict.permissions = 0;
                if (fault == 4) ++tail.problem_m;
                if (fault == 5) ++tail.problem_n;
                if (fault == 6) { ++tail.problem_k; ++strict.maximum_span; }
                if (!VHO_DSL_wgmma_pipeline_analyze(&tail, &strict, &rejected, &model, 384, &result, stderr) || result.emitted_family != DSL_COMPUTE_SIMT) return FALSE;
                for (UINT32 candidate_id = 1; candidate_id < 4; ++candidate_id)
                    if (result.candidates[candidate_id].eligible || result.candidates[candidate_id].known) return FALSE;
            }
            invalid = model; invalid.metrics[7].lower = invalid.metrics[7].nominal = invalid.metrics[7].upper = ~(uint64_t)0 - 1;
            result = saved;
            if (VHO_DSL_wgmma_pipeline_analyze(&record, &numerical, &control, &invalid, 384, &result, NULL) || memcmp(&result, &saved, sizeof(saved))) return FALSE;
        }
    }
    return TRUE;
}

/* Grow only fixture-owned storage, retain actual before/after images, and
 * exercise opt-in, resource, source and launch-contract preflight failures. */
static int VHO_DSL_pipeline_fixture(PU_Info *pu, WN *entry, DSL_TYPED_SIMT_RECORD &record,
    DSL_NUMERICAL_CONTRACT_RECORD &numerical, VHO_DSL_WGMMA_CONTROL &base_control,
    VHO_DSL_WGMMA_BINDING &base_binding, WN *before, char **argv)
{
    if (!VHO_DSL_wgmma_test_negatives(record, numerical, base_control, base_binding, before)) return 20;
    VHO_DSL_WGMMA_PIPELINE_CONTROL control = {};
    control.compute = base_control; control.delivery = getenv("OPEN64_WGMMA_TMA") ? 2 : 1;
    control.outstanding_groups = 2; control.compute.shared_limit_bytes = control.delivery == 2 ? 16400 : 8192;
    VHO_DSL_WGMMA_PIPELINE_BINDING binding = {}; binding.compute = base_binding;
    ST *shared[] = {binding.compute.shared_a, binding.compute.shared_b};
    for (UINT32 index = 0; index < 2; ++index) {
        TY_IDX type = Make_Array_Type(MTYPE_U2, 1, 2048); Set_TY_align(type, 16); Set_TY_is_user_align(type);
        Set_ST_type(*shared[index], type);
    }
    if (control.delivery == 2) {
        ST **symbols[] = {&binding.raw_a, &binding.raw_b, &binding.ready};
        const char *names[] = {"mma_raw_a", "mma_raw_b", "mma_ready"};
        for (UINT32 index = 0; index < 3; ++index) {
            TY_IDX type = Make_Array_Type(index == 2 ? MTYPE_U8 : MTYPE_U2, 1, index == 2 ? 2 : 2048);
            Set_TY_align(type, 16); Set_TY_is_user_align(type);
            *symbols[index] = VHO_DSL_test_symbol(names[index], type, SCLASS_AUTO, base_binding.simt.source_position);
            Set_ST_device_symbol_kind(**symbols[index], DEVICE_SYMBOL_SHARED);
        }
    }
    for (UINT32 fault = 0; fault < 17; ++fault) {
        VHO_DSL_WGMMA_PIPELINE_CONTROL bad_control = control;
        VHO_DSL_WGMMA_PIPELINE_BINDING bad_binding = binding;
        INT32 level = Opt_Level;
        switch (fault) {
        case 0: bad_control.compute.enable = 0; break;
        case 1: bad_control.outstanding_groups = 1; break;
        case 2: bad_control.outstanding_groups = 9; break;
        case 3: bad_control.delivery = 3; break;
        case 4: bad_control.reserved = 1; break;
        case 5: --bad_control.compute.shared_limit_bytes; break;
        case 6: bad_control.compute.arch_id = DSL_HARDWARE_ARCH_SM_90; break;
        case 7: bad_control.compute.acceptance_flags = 30; break;
        case 8: case 9: case 10: Opt_Level = fault - 8; break;
        case 11: bad_binding.compute.shared_b = binding.compute.shared_a; break;
        case 12:
            if (control.delivery == 2) bad_binding.raw_b = binding.raw_a;
            else bad_binding.ready = binding.compute.shared_a;
            break;
        case 13: bad_control.compute.ptx = 799; break;
        case 14: bad_control.compute.toolkit = 11999; break;
        case 15:
            if (control.delivery == 2) bad_binding.ready = NULL;
            else bad_binding.raw_a = binding.compute.shared_a;
            break;
        case 16:
            if (control.delivery == 2) { bad_control.compute.ptx = 805; bad_control.compute.toolkit = 12070; }
            else bad_control.compute.acceptance_flags = 63;
            break;
        }
        WN *output = before;
        UINT32 locals = ST_Table_Size(CURRENT_SYMTAB), globals = ST_Table_Size(GLOBAL_SYMTAB);
        BOOL accepted = VHO_DSL_wgmma_pipeline_materialize(&record, &numerical, &bad_control, &bad_binding, before, &output, NULL);
        Opt_Level = level;
        if (accepted || output != before || locals != ST_Table_Size(CURRENT_SYMTAB) || globals != ST_Table_Size(GLOBAL_SYMTAB)) return 21;
    }
    if (!DSL_Gatekeeper_Verify_PU(pu, stderr, NULL) || !VHO_DSL_test_image(pu, argv[1])) return 22;
    WN *after = NULL;
    if (!VHO_DSL_wgmma_pipeline_materialize(&record, &numerical, &control, &binding, before, &after, stderr)) return 23;
    WN_func_body(entry) = after;
    if (!DSL_Gatekeeper_Verify_PU(pu, stderr, NULL) ||
        !VHO_DSL_wgmma_pipeline_print(stdout, &record, &numerical, &control, &binding, after) || !VHO_DSL_test_image(pu, argv[2])) return 24;
    int mutations = VHO_DSL_pipeline_mutations(record, numerical, control, binding, after);
    USRCPOS source; USRCPOS_srcpos(source) = binding.compute.simt.source_position;
    if (mutations < 0 || !VHO_DSL_pipeline_models(record, numerical, control, USRCPOS_linenum(source))) return 25;
    if (!VHO_DSL_typed_simt_verify(&record, &numerical, &binding.compute.simt, VHO_DSL_TYPED_SIMT_GRID, before, stderr)) return 26;
    printf("MMA5-H passed: negatives=55 mutations=%d stale=2 model_scenarios=8 default=SIMT native_gpu=unknown timing=unknown\n", mutations);
    return 0;
}
