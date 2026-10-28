/* MMA4-H active-PU typed SIMT-to-Hopper materialization and exact postchecks.
 * All geometry, packing, participation and completion decisions precede CUDA
 * emission. No default selection, image layout or timing policy changes.
 * Design: doc/AI-COMPILER-MMA4-H-HOPPER-LOWERING.md. */
#include "dsl_wgmma_opt.h"
#include "config_opt.h"
#include "pu_info.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"
#include "strtab.h"
#include <set>
#include <string>
#include <vector>
#include <string.h>

/* Report rejection without touching caller outputs or the source body. */
static BOOL VHO_DSL_wgmma_error(FILE *file, const char *reason)
{
    if (file) fprintf(file, "Hopper WGMMA: %s\n", reason);
    return FALSE;
}

/* Validate the caller-owned array storage, including real shared alignment. */
static BOOL VHO_DSL_wgmma_array(ST *symbol, TYPE_ID element, UINT64 count, BOOL shared)
{
    if (!symbol || ST_class(symbol) != CLASS_VAR || ST_level(symbol) != CURRENT_SYMTAB ||
        ST_sclass(symbol) != SCLASS_AUTO) return FALSE;
    TY_IDX type = ST_type(symbol);
    return TY_kind(type) == KIND_ARRAY && TY_size(type) == count * MTYPE_byte_size(element) &&
        TY_kind(TY_etype(type)) == KIND_SCALAR && TY_mtype(TY_etype(type)) == element &&
        !TY_is_volatile(TY_etype(type)) && TY_size(TY_etype(type)) == MTYPE_byte_size(element) &&
        ST_device_symbol_kind(*symbol) == (shared ? DEVICE_SYMBOL_SHARED : DEVICE_SYMBOL_NONE) &&
        (!shared || (TY_align(type) >= 16 && TY_is_user_align(type)));
}

/* Check explicit policy and active-PU bindings before any WN/ST allocation.
 * A nonzero acceptance identity never substitutes for the individual freedoms
 * required by WGMMA's ISA arithmetic contract. */
static BOOL VHO_DSL_wgmma_preflight(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_WGMMA_CONTROL *control,
    const VHO_DSL_WGMMA_BINDING *binding, FILE *diagnostic)
{
    DSL_COMPUTE_PROFILE_RECORD profile;
    if (!record || !numerical || !control || !binding || control->enable != 1 ||
        Opt_Level < 3 || control->reserved || control->acceptance_flags != VHO_DSL_WGMMA_ACCEPT_ISA ||
        !VHO_DSL_typed_simt_binding_verify(record, numerical, &binding->simt, VHO_DSL_TYPED_SIMT_GRID, diagnostic) || !Current_PU_Info ||
        record->owner_pu_st != PU_Info_proc_sym(Current_PU_Info) ||
        numerical->permissions != DSL_ACC_PERMISSION_REASSOCIATE ||
        record->problem_m % 64 || record->problem_n % 64 || record->problem_k % 16 ||
        record->threads_per_cta != 128 || control->shared_limit_bytes < 4096 ||
        !DSL_compute_profile_get(control->profile_id, &profile) ||
        profile.family != DSL_COMPUTE_WGMMA || profile.arch_id != control->arch_id ||
        profile.instruction_m != 64 || profile.instruction_n != 64 || profile.instruction_k != 16 ||
        profile.threads_per_cta != 128 || profile.cta_group != 1 || profile.descriptor_recipe != 1 ||
        profile.fragment_mapping != DSL_COMPUTE_FRAGMENT_WGMMA_64_64 || profile.accumulator_format != DSL_ACC_FORMAT_F32 ||
        profile.input_format != numerical->input_format || control->ptx < profile.minimum_ptx ||
        control->toolkit < profile.minimum_toolkit)
        return VHO_DSL_wgmma_error(diagnostic, "disabled, level, exact target, shape or numerical acceptance");
    if (!VHO_DSL_wgmma_array(binding->shared_a, MTYPE_U2, 1024, TRUE) ||
        !VHO_DSL_wgmma_array(binding->shared_b, MTYPE_U2, 1024, TRUE) ||
        !VHO_DSL_wgmma_array(binding->fragment, MTYPE_F4, 32, FALSE))
        return VHO_DSL_wgmma_error(diagnostic, "shared layout/alignment or fragment storage");
    ST *symbols[] = {binding->shared_a, binding->shared_b, binding->fragment,
        binding->tid, binding->copy, binding->base, binding->row, binding->column};
    ST *source_symbols[] = {binding->simt.input_a, binding->simt.input_b, binding->simt.output,
        binding->simt.row, binding->simt.column, binding->simt.index, binding->simt.raw,
        binding->simt.fraction, binding->simt.exponent, binding->simt.wide,
        binding->simt.a_value, binding->simt.b_value, binding->simt.product, binding->simt.accumulator};
    for (UINT32 index = 0; index < 8; ++index) {
        ST *symbol = symbols[index];
        if (!symbol || ST_class(symbol) != CLASS_VAR || ST_level(symbol) != CURRENT_SYMTAB ||
            ST_sclass(symbol) != SCLASS_AUTO || &St_Table[ST_st_idx(symbol)] != symbol) return VHO_DSL_wgmma_error(diagnostic, "private symbol ownership");
        if (index >= 3 && (TY_kind(ST_type(symbol)) != KIND_SCALAR ||
            TY_mtype(ST_type(symbol)) != MTYPE_I8 || TY_size(ST_type(symbol)) != 8 ||
            TY_is_volatile(ST_type(symbol)))) return VHO_DSL_wgmma_error(diagnostic, "private scalar type");
        for (UINT32 previous = 0; previous < index; ++previous)
            if (symbols[previous] == symbol) return VHO_DSL_wgmma_error(diagnostic, "aliased recipe symbols");
        for (UINT32 source = 0; source < 14; ++source)
            if (source_symbols[source] == symbol) return VHO_DSL_wgmma_error(diagnostic, "aliased source symbols");
    }
    ST *coordinates[] = {binding->block_dim, binding->grid_dim};
    const char *names[] = {"blockDim", "gridDim"};
    for (UINT32 index = 0; index < 2; ++index) {
        ST *symbol = coordinates[index];
        if (!symbol || ST_class(symbol) != CLASS_VAR || ST_level(symbol) != GLOBAL_SYMTAB ||
            ST_sclass(symbol) != SCLASS_EXTERN || strcmp(ST_name(symbol), names[index]) ||
            !binding->simt.thread_idx || ST_type(symbol) != ST_type(binding->simt.thread_idx)) return VHO_DSL_wgmma_error(diagnostic, "launch builtin binding");
    }
    ST *barrier = binding->barrier;
    if (!barrier || ST_class(barrier) != CLASS_FUNC || ST_level(barrier) != GLOBAL_SYMTAB ||
        ST_sclass(barrier) != SCLASS_EXTERN || strcmp(ST_name(barrier), "__syncthreads")) return VHO_DSL_wgmma_error(diagnostic, "CTA barrier binding");
    TYLIST_IDX arguments = TY_tylist(ST_pu_type(barrier));
    if (!arguments || TYLIST_type(Tylist_Table[arguments]) != MTYPE_To_TY(MTYPE_V) ||
        TYLIST_type(Tylist_Table[arguments + 1]) != TY_IDX_ZERO) return VHO_DSL_wgmma_error(diagnostic, "CTA barrier prototype");
    return TRUE;
}

/* Every load is a fresh expression tree owned by the resulting body. */
static WN *VHO_DSL_wgmma_load(ST *symbol)
{
    return WN_Ldid(TY_mtype(ST_type(symbol)), 0, ST_st_idx(symbol), ST_type(symbol));
}

/* Take ownership of one value in a caller-owned private scalar. */
static WN *VHO_DSL_wgmma_store(ST *symbol, WN *value)
{
    return WN_Stid(MTYPE_I8, 0, symbol, ST_type(symbol), value);
}

/* Signed I8 constants keep all valid and hostile coordinate arithmetic safe. */
static WN *VHO_DSL_wgmma_int(INT64 value)
{
    return WN_Intconst(MTYPE_I8, value);
}

/* Read a verified ordinary dim3 field without truncating unsigned launch IDs. */
static WN *VHO_DSL_wgmma_dim(ST *symbol, UINT32 field)
{
    return WN_Cvt(MTYPE_U4, MTYPE_I8, WN_Ldid(MTYPE_U4, field * 4,
        ST_st_idx(symbol), ST_type(symbol), field + 1));
}

/* Array storage uses LDA, pointer formals use LDID; preserve exact byte scaling. */
static WN *VHO_DSL_wgmma_address(ST *symbol, WN *index, UINT32 bytes)
{
    WN *base = TY_kind(ST_type(symbol)) == KIND_ARRAY ? WN_Lda(Pointer_Mtype, 0, symbol, 0) :
        VHO_DSL_wgmma_load(symbol);
    return WN_Add(Pointer_Mtype, base, WN_Cvt(MTYPE_I8, Pointer_Mtype,
        WN_Mpy(MTYPE_I8, index, VHO_DSL_wgmma_int(bytes))));
}

/* Build a conventional owned ascending loop; no independent simplifier. */
static WN *VHO_DSL_wgmma_loop(ST *index, WN *start, UINT32 limit, UINT32 step, WN *body)
{
    return WN_CreateDO(WN_CreateIdname(0, index), VHO_DSL_wgmma_store(index, start),
        WN_LT(MTYPE_I8, VHO_DSL_wgmma_load(index), VHO_DSL_wgmma_int(limit)),
        VHO_DSL_wgmma_store(index, WN_Add(MTYPE_I8, VHO_DSL_wgmma_load(index), VHO_DSL_wgmma_int(step))), body, NULL);
}

/* Exact K-major no-swizzle atom packing; indices range over 64x16 elements. */
static WN *VHO_DSL_wgmma_pack(ST *mn, ST *reduction)
{
    return WN_Add(MTYPE_I8,
        WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, WN_Div(MTYPE_I8, VHO_DSL_wgmma_load(mn), VHO_DSL_wgmma_int(8)), VHO_DSL_wgmma_int(128)),
            WN_Mpy(MTYPE_I8, WN_Div(MTYPE_I8, VHO_DSL_wgmma_load(reduction), VHO_DSL_wgmma_int(8)), VHO_DSL_wgmma_int(64))),
        WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, WN_Binary(OPR_REM, MTYPE_I8, VHO_DSL_wgmma_load(mn), VHO_DSL_wgmma_int(8)), VHO_DSL_wgmma_int(8)),
            WN_Binary(OPR_REM, MTYPE_I8, VHO_DSL_wgmma_load(reduction), VHO_DSL_wgmma_int(8))));
}

/* Literal target mechanism with no selection policy. Fragment accesses stay
 * inside this volatile memory-clobber statement until wait_group 0 completes. */
static std::string VHO_DSL_wgmma_atom(UINT32 format)
{
    std::string text = "{\n.reg .b64 desc_a, desc_b;\n.reg .f32 acc<32>;\n";
    text += "cvta.to.shared.u64 desc_a, %1;\ncvta.to.shared.u64 desc_b, %2;\n";
    text += "shr.u64 desc_a, desc_a, 4;\nshr.u64 desc_b, desc_b, 4;\n";
    text += "and.b64 desc_a, desc_a, 16383;\nand.b64 desc_b, desc_b, 16383;\n";
    text += "or.b64 desc_a, desc_a, 68720001024;\nor.b64 desc_b, desc_b, 68720001024;\n";
    char line[96];
    for (UINT32 index = 0; index < 32; ++index) {
        snprintf(line, sizeof(line), "ld.f32 acc%u, [%%0+%u];\n", index, index * 4); text += line;
    }
    text += "wgmma.fence.sync.aligned;\nwgmma.mma_async.sync.aligned.m64n64k16.f32.";
    text += format == DSL_ACC_FORMAT_F16 ? "f16.f16 {" : "bf16.bf16 {";
    for (UINT32 index = 0; index < 32; ++index) {
        snprintf(line, sizeof(line), "%sacc%u", index ? "," : "", index); text += line;
    }
    text += "}, desc_a, desc_b, 1, 1, 1, 0, 0;\nwgmma.commit_group.sync.aligned;\nwgmma.wait_group.sync.aligned 0;\n";
    for (UINT32 index = 0; index < 32; ++index) {
        snprintf(line, sizeof(line), "st.f32 [%%0+%u], acc%u;\n", index * 4, index); text += line;
    }
    return text + "}";
}

/* Reuse standard ASM_STMT kids/flags. Scratch postchecking borrows only
 * validated string identities, avoiding any new CLASS_NAME symbol records. */
static WN *VHO_DSL_wgmma_assembly(const char *text, WN **arguments, UINT32 count, WN *seed, SRCPOS source)
{
    if (seed && (WN_operator(seed) != OPR_ASM_STMT || WN_kid_count(seed) != count + 2 ||
        strcmp(WN_asm_string(seed), text) || WN_asm_flag(seed) != (WN_ASM_VOLATILE | WN_ASM_CLOBBERS_MEM))) return NULL;
    WN *statement = seed ? WN_Create(OPC_ASM_STMT, count + 2) : WN_CreateAsm_Stmt(count + 2, (char *)text);
    if (seed) WN_st_idx(statement) = WN_st_idx(seed);
    else Set_ST_Srcpos(*WN_st(statement), source);
    WN_asm_flag(statement) = WN_ASM_VOLATILE | WN_ASM_CLOBBERS_MEM;
    WN_kid0(statement) = WN_CreateBlock(); WN_kid1(statement) = WN_CreateBlock();
    for (UINT32 index = 0; index < count; ++index) {
        if (seed && (WN_operator(WN_kid(seed, index + 2)) != OPR_ASM_INPUT ||
            strcmp(WN_asm_input_constraint(WN_kid(seed, index + 2)), "l"))) return NULL;
        WN *input = seed ? WN_Create(OPC_ASM_INPUT, 1) : WN_CreateAsm_Input((char *)"l", index, arguments[index]);
        if (seed) { WN_st_idx(input) = WN_st_idx(WN_kid(seed, index + 2)); WN_kid0(input) = arguments[index]; }
        else Set_ST_Srcpos(*WN_st(input), source);
        WN_asm_opnd_num(input) = index;
        WN_kid(statement, index + 2) = input;
    }
    return statement;
}

/* Source identity records policy and exact ABI; executable work is still
 * independently checked in the real body rather than trusted from this text. */
static std::string VHO_DSL_wgmma_identity(const DSL_TYPED_SIMT_RECORD &record,
    const DSL_NUMERICAL_CONTRACT_RECORD &numerical, const VHO_DSL_WGMMA_CONTROL &control)
{
    char text[512];
    snprintf(text, sizeof(text), "MMA4_H_V1 id=%u owner=%u generation=%u contract=%u acceptance=%u freedoms=%u profile=%u arch=%u ptx=%u toolkit=%u input=%u output=1 permissions=%u max_span=%u evidence_kind=%u evidence_id=%u M=%u N=%u K=%u threads=128 smem=4096 LBO=128 SBO=256 swizzle=0 timing=unknown",
        record.id, record.owner_pu_st, record.generation, numerical.id, record.numerical_acceptance_id,
        control.acceptance_flags, control.profile_id, control.arch_id, control.ptx, control.toolkit,
        numerical.input_format, numerical.permissions, numerical.maximum_span, numerical.evidence_kind,
        numerical.evidence_id, record.problem_m, record.problem_n, record.problem_k);
    return text;
}

/* Propagate the caller's complete source position to all owned statements. */
static void VHO_DSL_wgmma_source(WN *node, SRCPOS source)
{
    if (OPCODE_has_next_prev(WN_opcode(node))) WN_Set_Linenum(node, source);
    if (WN_operator(node) == OPR_BLOCK) {
        for (WN *statement = WN_first(node); statement; statement = WN_next(statement)) VHO_DSL_wgmma_source(statement, source);
    } else for (INT32 index = 0; index < WN_kid_count(node); ++index) VHO_DSL_wgmma_source(WN_kid(node, index), source);
}

/* Build only the reviewed recipe. All rejection is preflight; scratch mode
 * replaces the two validated ASM seeds without interning new symbols. */
static WN *VHO_DSL_wgmma_build(const DSL_TYPED_SIMT_RECORD &record,
    const DSL_NUMERICAL_CONTRACT_RECORD &numerical, const VHO_DSL_WGMMA_CONTROL &control,
    const VHO_DSL_WGMMA_BINDING &binding, WN **seeds)
{
    WN *body = WN_CreateBlock();
    if (!seeds) {
        WN *annotation = WN_CreateComment(VHO_DSL_wgmma_identity(record, numerical, control).c_str());
        Set_ST_Srcpos(*WN_st(annotation), binding.simt.source_position);
        WN_INSERT_BlockLast(body, annotation);
    }
    WN *valid = WN_CAND(WN_EQ(MTYPE_I8, VHO_DSL_wgmma_dim(binding.block_dim, 0), VHO_DSL_wgmma_int(128)),
        WN_CAND(WN_EQ(MTYPE_I8, VHO_DSL_wgmma_dim(binding.block_dim, 1), VHO_DSL_wgmma_int(1)),
        WN_CAND(WN_EQ(MTYPE_I8, VHO_DSL_wgmma_dim(binding.block_dim, 2), VHO_DSL_wgmma_int(1)),
        WN_EQ(MTYPE_I8, VHO_DSL_wgmma_dim(binding.grid_dim, 2), VHO_DSL_wgmma_int(1)))));
    valid = WN_CAND(valid, WN_CAND(
        WN_LT(MTYPE_I8, VHO_DSL_wgmma_dim(binding.simt.block_idx, 0), VHO_DSL_wgmma_int(record.problem_n / 64)),
        WN_LT(MTYPE_I8, VHO_DSL_wgmma_dim(binding.simt.block_idx, 1), VHO_DSL_wgmma_int(record.problem_m / 64))));
    WN *work = WN_CreateBlock();
    WN_INSERT_BlockLast(body, WN_CreateIf(valid, work, WN_CreateBlock()));
    WN_INSERT_BlockLast(work, VHO_DSL_wgmma_store(binding.tid, VHO_DSL_wgmma_dim(binding.simt.thread_idx, 0)));
    WN *initialize = WN_CreateBlock();
    WN_INSERT_BlockLast(initialize, WN_Istore(MTYPE_F4, 0, Make_Pointer_Type(MTYPE_To_TY(MTYPE_F4)),
        VHO_DSL_wgmma_address(binding.fragment, VHO_DSL_wgmma_load(binding.copy), 4), WN_Floatconst(MTYPE_F4, 0.0)));
    WN_INSERT_BlockLast(work, VHO_DSL_wgmma_loop(binding.copy, VHO_DSL_wgmma_int(0), 32, 1, initialize));
    WN *step = WN_CreateBlock(), *copy = WN_CreateBlock();
    WN_INSERT_BlockLast(copy, VHO_DSL_wgmma_store(binding.row, WN_Div(MTYPE_I8, VHO_DSL_wgmma_load(binding.copy), VHO_DSL_wgmma_int(16))));
    WN_INSERT_BlockLast(copy, VHO_DSL_wgmma_store(binding.column, WN_Binary(OPR_REM, MTYPE_I8, VHO_DSL_wgmma_load(binding.copy), VHO_DSL_wgmma_int(16))));
    WN *a_index = WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8,
        WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, VHO_DSL_wgmma_dim(binding.simt.block_idx, 1), VHO_DSL_wgmma_int(64)),
            VHO_DSL_wgmma_load(binding.row)), VHO_DSL_wgmma_int(record.problem_k)),
        WN_Add(MTYPE_I8, VHO_DSL_wgmma_load(binding.base), VHO_DSL_wgmma_load(binding.column)));
    WN *b_index = WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8,
        WN_Add(MTYPE_I8, VHO_DSL_wgmma_load(binding.base), VHO_DSL_wgmma_load(binding.column)), VHO_DSL_wgmma_int(record.problem_n)),
        WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, VHO_DSL_wgmma_dim(binding.simt.block_idx, 0), VHO_DSL_wgmma_int(64)),
            VHO_DSL_wgmma_load(binding.row)));
    ST *inputs[] = {binding.simt.input_a, binding.simt.input_b};
    ST *shared[] = {binding.shared_a, binding.shared_b};
    WN *indices[] = {a_index, b_index};
    for (UINT32 index = 0; index < 2; ++index)
        WN_INSERT_BlockLast(copy, WN_Istore(MTYPE_U2, 0, Make_Pointer_Type(MTYPE_To_TY(MTYPE_U2)),
            VHO_DSL_wgmma_address(shared[index], VHO_DSL_wgmma_pack(binding.row, binding.column), 2),
            WN_Iload(MTYPE_U2, 0, MTYPE_To_TY(MTYPE_U2), VHO_DSL_wgmma_address(inputs[index], indices[index], 2))));
    WN_INSERT_BlockLast(step, VHO_DSL_wgmma_loop(binding.copy, VHO_DSL_wgmma_load(binding.tid), 1024, 128, copy));
    WN_INSERT_BlockLast(step, VHO_DSL_wgmma_assembly("fence.proxy.async.shared::cta;", NULL, 0,
        seeds ? seeds[0] : NULL, binding.simt.source_position));
    WN_INSERT_BlockLast(step, WN_Call(MTYPE_V, MTYPE_V, 0, ST_st_idx(binding.barrier)));
    WN *arguments[] = {WN_Lda(Pointer_Mtype, 0, binding.fragment, 0),
        WN_Lda(Pointer_Mtype, 0, binding.shared_a, 0), WN_Lda(Pointer_Mtype, 0, binding.shared_b, 0)};
    WN_INSERT_BlockLast(step, VHO_DSL_wgmma_assembly(VHO_DSL_wgmma_atom(numerical.input_format).c_str(), arguments, 3,
        seeds ? seeds[1] : NULL, binding.simt.source_position));
    WN_INSERT_BlockLast(step, WN_Call(MTYPE_V, MTYPE_V, 0, ST_st_idx(binding.barrier)));
    WN_INSERT_BlockLast(work, VHO_DSL_wgmma_loop(binding.base, VHO_DSL_wgmma_int(0), record.problem_k, 16, step));
    WN *epilogue = WN_CreateBlock();
    WN *row = WN_Add(MTYPE_I8,
        WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, VHO_DSL_wgmma_dim(binding.simt.block_idx, 1), VHO_DSL_wgmma_int(64)),
            WN_Mpy(MTYPE_I8, WN_Div(MTYPE_I8, VHO_DSL_wgmma_load(binding.tid), VHO_DSL_wgmma_int(32)), VHO_DSL_wgmma_int(16))),
        WN_Add(MTYPE_I8, WN_Div(MTYPE_I8, WN_Binary(OPR_REM, MTYPE_I8, VHO_DSL_wgmma_load(binding.tid), VHO_DSL_wgmma_int(32)), VHO_DSL_wgmma_int(4)),
            WN_Mpy(MTYPE_I8, WN_Div(MTYPE_I8, WN_Binary(OPR_REM, MTYPE_I8, VHO_DSL_wgmma_load(binding.copy), VHO_DSL_wgmma_int(4)), VHO_DSL_wgmma_int(2)), VHO_DSL_wgmma_int(8))));
    WN *column = WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, VHO_DSL_wgmma_dim(binding.simt.block_idx, 0), VHO_DSL_wgmma_int(64)),
        WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, WN_Div(MTYPE_I8, VHO_DSL_wgmma_load(binding.copy), VHO_DSL_wgmma_int(4)), VHO_DSL_wgmma_int(8)),
            WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, WN_Binary(OPR_REM, MTYPE_I8, VHO_DSL_wgmma_load(binding.tid), VHO_DSL_wgmma_int(4)), VHO_DSL_wgmma_int(2)),
                WN_Binary(OPR_REM, MTYPE_I8, VHO_DSL_wgmma_load(binding.copy), VHO_DSL_wgmma_int(2)))));
    WN_INSERT_BlockLast(epilogue, WN_Istore(MTYPE_F4, 0, ST_type(binding.simt.output),
        VHO_DSL_wgmma_address(binding.simt.output, WN_Add(MTYPE_I8,
            WN_Mpy(MTYPE_I8, row, VHO_DSL_wgmma_int(record.problem_n)), column), 4),
        WN_Iload(MTYPE_F4, 0, MTYPE_To_TY(MTYPE_F4), VHO_DSL_wgmma_address(binding.fragment, VHO_DSL_wgmma_load(binding.copy), 4))));
    WN_INSERT_BlockLast(work, VHO_DSL_wgmma_loop(binding.copy, VHO_DSL_wgmma_int(0), 32, 1, epilogue));
    WN_INSERT_BlockLast(body, WN_CreateReturn());
    VHO_DSL_wgmma_source(body, binding.simt.source_position);
    return body;
}

/* Collect the two real assembly statements without trusting inspection rows. */
static BOOL VHO_DSL_wgmma_collect(WN *node, std::vector<WN *> &assembly, std::set<WN *> &seen)
{
    if (!node || !seen.insert(node).second) return FALSE;
    if (WN_operator(node) == OPR_ASM_STMT) assembly.push_back(node);
    if (WN_operator(node) == OPR_BLOCK) {
        for (WN *statement = WN_first(node); statement; statement = WN_next(statement))
            if (!VHO_DSL_wgmma_collect(statement, assembly, seen)) return FALSE;
    } else for (INT32 index = 0; index < WN_kid_count(node); ++index)
        if (!VHO_DSL_wgmma_collect(WN_kid(node, index), assembly, seen)) return FALSE;
    return TRUE;
}

/* Compare complete canonical trees and effects, including list links. */
static BOOL VHO_DSL_wgmma_same(WN *actual, WN *expected, std::set<WN *> &seen)
{
    if (!actual || !expected || !seen.insert(actual).second || WN_opcode(actual) != WN_opcode(expected)) return FALSE;
    OPCODE opcode = WN_opcode(actual); OPERATOR operation = WN_operator(actual);
    if (OPCODE_has_next_prev(opcode) && WN_linenum(actual) != WN_linenum(expected)) return FALSE;
    if ((OPCODE_has_sym(opcode) && WN_st_idx(actual) != WN_st_idx(expected)) ||
        (OPCODE_has_offset(opcode) && WN_offset(actual) != WN_offset(expected)) ||
        (OPCODE_has_1ty(opcode) && WN_ty(actual) != WN_ty(expected)) ||
        (OPCODE_has_2ty(opcode) && WN_load_addr_ty(actual) != WN_load_addr_ty(expected)) ||
        (operation == OPR_INTCONST && WN_const_val(actual) != WN_const_val(expected)) ||
        (operation == OPR_ASM_STMT && WN_asm_flag(actual) != WN_asm_flag(expected)) ||
        (operation == OPR_ASM_INPUT && WN_asm_opnd_num(actual) != WN_asm_opnd_num(expected)) ||
        (operation == OPR_CALL && WN_call_flag(actual) != WN_call_flag(expected)) ||
        ((operation == OPR_LDID || operation == OPR_ILOAD || operation == OPR_ISTORE || operation == OPR_STID) &&
            WN_field_id(actual) != WN_field_id(expected))) return FALSE;
    if (operation == OPR_BLOCK) {
        WN *left = WN_first(actual), *right = WN_first(expected), *previous = NULL;
        while (left && right) {
            if (WN_prev(left) != previous || !VHO_DSL_wgmma_same(left, right, seen)) return FALSE;
            previous = left; left = WN_next(left); right = WN_next(right);
        }
        return !left && !right && WN_last(actual) == previous;
    }
    if (WN_kid_count(actual) != WN_kid_count(expected)) return FALSE;
    for (INT32 index = 0; index < WN_kid_count(actual); ++index)
        if (!VHO_DSL_wgmma_same(WN_kid(actual, index), WN_kid(expected, index), seen)) return FALSE;
    return TRUE;
}

/* A source-compatible SIMT body is also the active-scope/type proof. Preserve
 * it on every rejection; only publish a detached body after the postcheck. */
BOOL VHO_DSL_wgmma_materialize(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_WGMMA_CONTROL *control,
    const VHO_DSL_WGMMA_BINDING *binding, WN *simt_body, WN **body, FILE *diagnostic)
{
    if (!body || !VHO_DSL_wgmma_preflight(record, numerical, control, binding, diagnostic) ||
        !VHO_DSL_typed_simt_verify(record, numerical, &binding->simt, VHO_DSL_TYPED_SIMT_GRID, simt_body, diagnostic)) return FALSE;
    WN *candidate = VHO_DSL_wgmma_build(*record, *numerical, *control, *binding, NULL);
    if (!VHO_DSL_wgmma_verify(record, numerical, control, binding, candidate, diagnostic)) { WN_DELETE_Tree(candidate); return FALSE; }
    *body = candidate;
    return TRUE;
}

/* Rebuild a scratch body using only checked annotation/ASM symbol identities;
 * table sizes and the borrowed actual body remain unchanged. */
BOOL VHO_DSL_wgmma_verify(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_WGMMA_CONTROL *control,
    const VHO_DSL_WGMMA_BINDING *binding, WN *body, FILE *diagnostic)
{
    if (!body || !VHO_DSL_wgmma_preflight(record, numerical, control, binding, diagnostic) ||
        WN_operator(body) != OPR_BLOCK || !WN_first(body) || WN_operator(WN_first(body)) != OPR_COMMENT ||
        strcmp(Index_To_Str(WN_GetComment(WN_first(body))), VHO_DSL_wgmma_identity(*record, *numerical, *control).c_str())) return FALSE;
    std::vector<WN *> assembly; std::set<WN *> visited;
    if (!VHO_DSL_wgmma_collect(body, assembly, visited) || assembly.size() != 2 || strcmp(WN_asm_string(assembly[0]), "fence.proxy.async.shared::cta;") ||
        strcmp(WN_asm_string(assembly[1]), VHO_DSL_wgmma_atom(numerical->input_format).c_str())) return FALSE;
    for (UINT32 index = 0; index < 2; ++index) {
        WN *statement = assembly[index];
        if (WN_asm_flag(statement) != (WN_ASM_VOLATILE | WN_ASM_CLOBBERS_MEM) ||
            WN_kid_count(statement) != (index ? 5 : 2) || WN_operator(WN_kid0(statement)) != OPR_BLOCK ||
            WN_operator(WN_kid1(statement)) != OPR_BLOCK || WN_first(WN_kid0(statement)) || WN_first(WN_kid1(statement))) return FALSE;
        for (INT32 kid = 2; kid < WN_kid_count(statement); ++kid)
            if (WN_operator(WN_kid(statement, kid)) != OPR_ASM_INPUT || strcmp(WN_asm_input_constraint(WN_kid(statement, kid)), "l")) return FALSE;
    }
    WN *expected = VHO_DSL_wgmma_build(*record, *numerical, *control, *binding, &assembly[0]);
    WN_INSERT_BlockFirst(expected, WN_COPY_Tree(WN_first(body)));
    std::set<WN *> seen; BOOL valid = VHO_DSL_wgmma_same(body, expected, seen);
    WN_DELETE_Tree(expected);
    return valid || VHO_DSL_wgmma_error(diagnostic, "changed body, access/protocol/fragment/guard or effects");
}

/* Counts name actual executed operations in the atom and loop, not scalar
 * stand-ins, predicted native instructions, timing or GPU certification. */
BOOL VHO_DSL_wgmma_print(FILE *file, const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_WGMMA_CONTROL *control,
    const VHO_DSL_WGMMA_BINDING *binding, WN *body)
{
    if (!file || !VHO_DSL_wgmma_verify(record, numerical, control, binding, body, NULL)) return FALSE;
    fprintf(file, "# What it summarizes: MMA4-H explicit fully-waited WGMMA lowering; default SIMT, native GPU/timing unknown.\n");
    fprintf(file, "AI_WGMMA 1 CONTRACT %s\n", VHO_DSL_wgmma_identity(*record, *numerical, *control).c_str());
    fprintf(file, "AI_WGMMA 1 ACTION atom=m64n64k16 iterations=%u cta_tiles=%llu proxy=1 barriers=2 register_fence=1 mma=1 commit=1 wait_zero=1 fragment_loads=32 fragment_stores=32 default=SIMT timing=unknown\n",
        record->problem_k / 16, (unsigned long long)(record->problem_m / 64) * (record->problem_n / 64));
    return !ferror(file);
}
