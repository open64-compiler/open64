/* MMA3 active-PU typed scalar matrix lowering and exact-body postchecks.
 * Reuses standard WN construction and caller-owned symbols; no CUDA template,
 * source narrowing, native MMA, runtime call or profitability selector.
 * Design: doc/AI-COMPILER-MMA3-TYPED-SIMT-BASELINE.md. */
#include "dsl_typed_simt_opt.h"
#include "config.h"
#include "config_opt.h"
#include "pu_info.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"
#include "wn_simp.h"
#include "strtab.h"
#include <set>
#include <string.h>

/* An optional diagnostic never changes the caller's symbols or output. */
static BOOL VHO_DSL_typed_error(FILE *file, const char *reason)
{
    if (file) fprintf(file, "typed SIMT: %s\n", reason);
    return FALSE;
}

/* Verify the established CUDA dim3 builtin layout without creating a parallel
 * symbol registry. The owning driver supplies the actual builtin symbols. */
static BOOL VHO_DSL_typed_coordinate(ST *symbol, const char *name)
{
    if (!symbol || ST_class(symbol) != CLASS_VAR || ST_level(symbol) != GLOBAL_SYMTAB ||
        ST_sclass(symbol) != SCLASS_EXTERN || strcmp(ST_name(symbol), name)) return FALSE;
    TY_IDX type = ST_type(symbol);
    if (TY_kind(type) != KIND_STRUCT || TY_size(type) != 12) return FALSE;
    FLD_HANDLE field = TY_fld(type);
    const char *names[] = {"x", "y", "z"};
    for (UINT32 index = 0; index < 3; ++index) {
        if (field.Is_Null() || strcmp(FLD_name(field), names[index]) || FLD_ofst(field) != index * 4 ||
            TY_mtype(FLD_type(field)) != MTYPE_U4 || FLD_last_field(field) != (index == 2)) return FALSE;
        if (index != 2) field = FLD_next(field);
    }
    return TRUE;
}

/* Preflight active-PU ownership, strict arithmetic controls and exact storage.
 * No allocation or mutation occurs on rejection, including aliased scratch. */
static BOOL VHO_DSL_typed_binding(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_TYPED_SIMT_BINDING *binding,
    UINT32 execution, FILE *diagnostic)
{
    if (!DSL_typed_simt_verify(record, numerical, diagnostic) || !binding || !Current_PU_Info ||
        record->owner_pu_st != PU_Info_proc_sym(Current_PU_Info) || !Current_pu ||
        Current_pu != &Pu_Table[ST_pu(St_Table[record->owner_pu_st])] ||
        CURRENT_SYMTAB != GLOBAL_SYMTAB + 1 || !Scope_tab[CURRENT_SYMTAB].st ||
        ST_st_idx(Scope_tab[CURRENT_SYMTAB].st) != record->owner_pu_st ||
        MTYPE_byte_size(Pointer_Mtype) != 8 ||
        (execution != VHO_DSL_TYPED_SIMT_SERIAL && execution != VHO_DSL_TYPED_SIMT_GRID) ||
        OPT_Fast_Math || Roundoff_Level != ROUNDOFF_NONE || IEEE_Arithmetic > IEEE_ACCURATE ||
        !binding->source_position) return VHO_DSL_typed_error(diagnostic, "scope, arithmetic controls or source position");
    ST *symbols[] = {binding->input_a, binding->input_b, binding->output, binding->row, binding->column, binding->index,
        binding->raw, binding->fraction, binding->exponent, binding->wide,
        binding->a_value, binding->b_value, binding->product, binding->accumulator};
    for (UINT32 index = 0; index < 14; ++index) {
        ST *symbol = symbols[index];
        if (!symbol || ST_class(symbol) != CLASS_VAR || ST_level(symbol) != CURRENT_SYMTAB ||
            &St_Table[ST_st_idx(symbol)] != symbol ||
            ST_sclass(symbol) != (index < 3 ? SCLASS_FORMAL : SCLASS_AUTO))
            return VHO_DSL_typed_error(diagnostic, "invalid bound symbol");
        for (UINT32 previous = 0; previous < index; ++previous)
            if (ST_st_idx(symbol) == ST_st_idx(symbols[previous]))
                return VHO_DSL_typed_error(diagnostic, "aliased bound symbols");
        TY_IDX type = ST_type(symbol);
        if (index < 3) {
            if (TY_kind(type) != KIND_POINTER || !ST_is_value_parm(symbol) ||
                TY_kind(TY_pointed(type)) != KIND_SCALAR ||
                TY_mtype(TY_pointed(type)) != (index == 2 ? MTYPE_F4 : MTYPE_U2) ||
                TY_size(TY_pointed(type)) != (index == 2 ? 4 : 2))
                return VHO_DSL_typed_error(diagnostic, "wrong typed pointer formal");
        } else {
            TYPE_ID expected = index < 5 ? MTYPE_I8 : index == 5 ? MTYPE_I4 : index < 10 ? MTYPE_U4 : MTYPE_F4;
            if (TY_kind(type) != KIND_SCALAR || TY_mtype(type) != expected || TY_size(type) != MTYPE_byte_size(expected) ||
                (!!TY_is_volatile(type) != (index >= 12)))
                return VHO_DSL_typed_error(diagnostic, "wrong private scalar or rounding boundary");
        }
    }
    if (execution == VHO_DSL_TYPED_SIMT_GRID &&
        (!VHO_DSL_typed_coordinate(binding->thread_idx, "threadIdx") ||
         !VHO_DSL_typed_coordinate(binding->block_idx, "blockIdx")))
        return VHO_DSL_typed_error(diagnostic, "invalid CUDA coordinate binding");
    return TRUE;
}

/* Each access is fresh and retains the declared volatile/type boundary. */
static WN *VHO_DSL_typed_load(ST *symbol)
{
    return WN_Ldid(TY_mtype(ST_type(symbol)), 0, ST_st_idx(symbol), ST_type(symbol));
}

/* Take an owned expression into a caller-bound scalar assignment. */
static WN *VHO_DSL_typed_store(ST *symbol, WN *value)
{
    return WN_Stid(TY_mtype(ST_type(symbol)), 0, symbol, ST_type(symbol), value);
}

/* Create an unsigned bit-pattern constant, never an arithmetic conversion. */
static WN *VHO_DSL_typed_u4(UINT32 value)
{
    return WN_Intconst(MTYPE_U4, value);
}

/* Mask a fresh bound bit value using traditional WN simplifier controls. */
static WN *VHO_DSL_typed_mask(ST *symbol, UINT32 mask)
{
    return WN_Band(MTYPE_U4, VHO_DSL_typed_load(symbol), VHO_DSL_typed_u4(mask));
}

/* Form a logical left shift with an explicit U4 result and I4 count. */
static WN *VHO_DSL_typed_shift(WN *value, UINT32 count)
{
    return WN_Shl(MTYPE_U4, value, WN_Intconst(MTYPE_I4, count));
}

/* Build a strict-tree typed byte address. Element extents were checked before
 * construction; all matrix indexing/byte scaling uses signed 64-bit values. */
static WN *VHO_DSL_typed_address(ST *array, WN *index, UINT32 element_bytes)
{
    return WN_Add(Pointer_Mtype, VHO_DSL_typed_load(array),
        WN_Mpy(Pointer_Mtype, WN_Cvt(MTYPE_I8, Pointer_Mtype, index),
            WN_Intconst(Pointer_Mtype, element_bytes)));
}

/* Return fresh row-major indexing; source/result width is supplied separately. */
static WN *VHO_DSL_typed_index(ST *row, UINT32 stride, ST *column)
{
    WN *column_value = VHO_DSL_typed_load(column);
    if (TY_mtype(ST_type(column)) == MTYPE_I4) column_value = WN_Cvt(MTYPE_I4, MTYPE_I8, column_value);
    WN *row_value = VHO_DSL_typed_load(row);
    if (TY_mtype(ST_type(row)) == MTYPE_I4) row_value = WN_Cvt(MTYPE_I4, MTYPE_I8, row_value);
    return WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, row_value, WN_Intconst(MTYPE_I8, stride)), column_value);
}

/* Widen one accepted 16-bit load using only integer manipulation and TAS.
 * Both paths preserve zero/subnormal/Inf/NaN encodings without floating work. */
static void VHO_DSL_typed_decode(WN *body, const VHO_DSL_TYPED_SIMT_BINDING &binding,
    UINT32 format, ST *source, WN *index, ST *destination)
{
    WN_INSERT_BlockLast(body, VHO_DSL_typed_store(binding.raw,
        WN_Iload(MTYPE_U2, 0, MTYPE_To_TY(MTYPE_U2), VHO_DSL_typed_address(source, index, 2))));
    if (format == DSL_ACC_FORMAT_BF16) {
        WN_INSERT_BlockLast(body, VHO_DSL_typed_store(binding.wide,
            VHO_DSL_typed_shift(VHO_DSL_typed_load(binding.raw), 16)));
    } else {
        WN_INSERT_BlockLast(body, VHO_DSL_typed_store(binding.fraction, VHO_DSL_typed_mask(binding.raw, 1023)));
        WN_INSERT_BlockLast(body, VHO_DSL_typed_store(binding.exponent,
            WN_Lshr(MTYPE_U4, VHO_DSL_typed_mask(binding.raw, 0x7c00), WN_Intconst(MTYPE_I4, 10))));
        WN *subnormal = WN_CreateBlock(), *normal = WN_CreateBlock();
        WN_INSERT_BlockLast(subnormal, VHO_DSL_typed_store(binding.wide, VHO_DSL_typed_u4(0)));
        WN *nonzero = WN_CreateBlock(), *normalize = WN_CreateBlock();
        WN_INSERT_BlockLast(nonzero, VHO_DSL_typed_store(binding.exponent, VHO_DSL_typed_u4(113)));
        WN_INSERT_BlockLast(normalize, VHO_DSL_typed_store(binding.fraction,
            VHO_DSL_typed_shift(VHO_DSL_typed_load(binding.fraction), 1)));
        WN_INSERT_BlockLast(normalize, VHO_DSL_typed_store(binding.exponent,
            WN_Sub(MTYPE_U4, VHO_DSL_typed_load(binding.exponent), VHO_DSL_typed_u4(1))));
        WN_INSERT_BlockLast(nonzero, WN_CreateWhileDo(
            WN_LT(MTYPE_U4, VHO_DSL_typed_load(binding.fraction), VHO_DSL_typed_u4(1024)), normalize));
        WN_INSERT_BlockLast(nonzero, VHO_DSL_typed_store(binding.wide,
            WN_Bior(MTYPE_U4, VHO_DSL_typed_shift(VHO_DSL_typed_load(binding.exponent), 23),
                VHO_DSL_typed_shift(VHO_DSL_typed_mask(binding.fraction, 1023), 13))));
        WN_INSERT_BlockLast(subnormal, WN_CreateIf(WN_NE(MTYPE_U4,
            VHO_DSL_typed_load(binding.fraction), VHO_DSL_typed_u4(0)), nonzero, WN_CreateBlock()));
        WN *special = WN_CreateBlock(), *finite = WN_CreateBlock();
        WN_INSERT_BlockLast(special, VHO_DSL_typed_store(binding.wide,
            WN_Bior(MTYPE_U4, VHO_DSL_typed_u4(0x7f800000),
                VHO_DSL_typed_shift(VHO_DSL_typed_load(binding.fraction), 13))));
        WN_INSERT_BlockLast(finite, VHO_DSL_typed_store(binding.wide,
            WN_Bior(MTYPE_U4, VHO_DSL_typed_shift(WN_Add(MTYPE_U4,
                VHO_DSL_typed_load(binding.exponent), VHO_DSL_typed_u4(112)), 23),
                VHO_DSL_typed_shift(VHO_DSL_typed_load(binding.fraction), 13))));
        WN_INSERT_BlockLast(normal, WN_CreateIf(WN_EQ(MTYPE_U4,
            VHO_DSL_typed_load(binding.exponent), VHO_DSL_typed_u4(31)), special, finite));
        WN_INSERT_BlockLast(body, WN_CreateIf(WN_EQ(MTYPE_U4,
            VHO_DSL_typed_load(binding.exponent), VHO_DSL_typed_u4(0)), subnormal, normal));
        WN_INSERT_BlockLast(body, VHO_DSL_typed_store(binding.wide,
            WN_Bior(MTYPE_U4, VHO_DSL_typed_load(binding.wide),
                VHO_DSL_typed_shift(VHO_DSL_typed_mask(binding.raw, 0x8000), 16))));
    }
    WN_INSERT_BlockLast(body, VHO_DSL_typed_store(destination,
        WN_Tas(MTYPE_F4, MTYPE_To_TY(MTYPE_F4), VHO_DSL_typed_load(binding.wide))));
}

/* Structured ascending loops keep source K order and own all child nodes. */
static WN *VHO_DSL_typed_loop(ST *index, UINT32 limit, WN *body)
{
    TYPE_ID type = TY_mtype(ST_type(index));
    return WN_CreateDO(WN_CreateIdname(0, index),
        VHO_DSL_typed_store(index, WN_Intconst(type, 0)),
        WN_LT(type, VHO_DSL_typed_load(index), WN_Intconst(type, limit)),
        VHO_DSL_typed_store(index, WN_Add(type, VHO_DSL_typed_load(index), WN_Intconst(type, 1))), body, NULL);
}

/* Read the established unsigned CUDA coordinate as I8 before multiplying;
 * excess launch threads/blocks cannot overflow a signed 32-bit index. */
static WN *VHO_DSL_typed_dim(ST *symbol, UINT32 ordinal)
{
    return WN_Cvt(MTYPE_U4, MTYPE_I8, WN_Ldid(MTYPE_U4, ordinal * 4,
        ST_st_idx(symbol), ST_type(symbol), ordinal + 1));
}

/* Format the complete source identity without allocating symbols or strings;
 * diagnostics are evidence only, while postchecks also prove the actual body. */
static void VHO_DSL_typed_description(const DSL_TYPED_SIMT_RECORD &record,
    const DSL_NUMERICAL_CONTRACT_RECORD &numerical, UINT32 execution, char *description, size_t capacity)
{
    snprintf(description, capacity,
        "MMA3_TYPED_SIMT_V1 id=%u owner=%u generation=%u contract=%u acceptance=%u "
        "source=%u input=%u product=%u partial=%u outer=%u output=%u permissions=%u max_span=%u evidence=%u:%u "
        "M=%u N=%u K=%u threads=%u disjoint=%u execution=%u order=increasing_k contraction=none",
        record.id, record.owner_pu_st, record.generation, numerical.id, record.numerical_acceptance_id,
        numerical.source_format, numerical.input_format, numerical.product_format, numerical.partial_format,
        numerical.outer_format, numerical.output_format, numerical.permissions, numerical.maximum_span,
        numerical.evidence_kind, numerical.evidence_id, record.problem_m, record.problem_n, record.problem_k,
        record.threads_per_cta, record.disjoint_output, execution);
}

/* Attach the caller's source position to generated statements, never to a
 * shared expression view or another PU's tree. */
static void VHO_DSL_typed_source(WN *node, SRCPOS source)
{
    if (OPCODE_has_next_prev(WN_opcode(node))) WN_Set_Linenum(node, source);
    if (WN_operator(node) == OPR_BLOCK) {
        for (WN *statement = WN_first(node); statement; statement = WN_next(statement))
            VHO_DSL_typed_source(statement, source);
    } else {
        for (INT32 index = 0; index < WN_kid_count(node); ++index)
            VHO_DSL_typed_source(WN_kid(node, index), source);
    }
}

/* Construct the same scalar dot under serial or independent-output topology.
 * Scratch regeneration omits annotation creation to keep postchecks read-only. */
static WN *VHO_DSL_typed_build(const DSL_TYPED_SIMT_RECORD &record,
    const DSL_NUMERICAL_CONTRACT_RECORD &numerical, const VHO_DSL_TYPED_SIMT_BINDING &binding,
    UINT32 execution, BOOL annotate)
{
    WN *dot = WN_CreateBlock(), *steps = WN_CreateBlock();
    WN_INSERT_BlockLast(dot, VHO_DSL_typed_store(binding.accumulator, WN_Floatconst(MTYPE_F4, 0.0)));
    VHO_DSL_typed_decode(steps, binding, numerical.input_format, binding.input_a,
        VHO_DSL_typed_index(binding.row, record.problem_k, binding.index), binding.a_value);
    VHO_DSL_typed_decode(steps, binding, numerical.input_format, binding.input_b,
        VHO_DSL_typed_index(binding.index, record.problem_n, binding.column), binding.b_value);
    WN_INSERT_BlockLast(steps, VHO_DSL_typed_store(binding.product,
        WN_Mpy(MTYPE_F4, VHO_DSL_typed_load(binding.a_value), VHO_DSL_typed_load(binding.b_value))));
    WN_INSERT_BlockLast(steps, VHO_DSL_typed_store(binding.accumulator,
        WN_Add(MTYPE_F4, VHO_DSL_typed_load(binding.accumulator), VHO_DSL_typed_load(binding.product))));
    WN_INSERT_BlockLast(dot, VHO_DSL_typed_loop(binding.index, record.problem_k, steps));
    WN_INSERT_BlockLast(dot, WN_Istore(MTYPE_F4, 0, ST_type(binding.output),
        VHO_DSL_typed_address(binding.output,
            VHO_DSL_typed_index(binding.row, record.problem_n, binding.column), 4),
        VHO_DSL_typed_load(binding.accumulator)));
    WN *body = WN_CreateBlock();
    if (annotate) {
        char description[768];
        VHO_DSL_typed_description(record, numerical, execution, description, sizeof(description));
        WN_INSERT_BlockLast(body, WN_CreateComment(description));
    }
    if (execution == VHO_DSL_TYPED_SIMT_SERIAL) {
        WN *columns = WN_CreateBlock();
        WN_INSERT_BlockLast(columns, VHO_DSL_typed_loop(binding.column, record.problem_n, dot));
        WN_INSERT_BlockLast(body, VHO_DSL_typed_loop(binding.row, record.problem_m, columns));
    } else {
        WN_INSERT_BlockLast(body, VHO_DSL_typed_store(binding.row, VHO_DSL_typed_dim(binding.block_idx, 1)));
        WN_INSERT_BlockLast(body, VHO_DSL_typed_store(binding.column, WN_Add(MTYPE_I8,
            WN_Mpy(MTYPE_I8, VHO_DSL_typed_dim(binding.block_idx, 0), WN_Intconst(MTYPE_I8, record.threads_per_cta)),
            VHO_DSL_typed_dim(binding.thread_idx, 0))));
        WN *guard = WN_CAND(WN_LT(MTYPE_I8, VHO_DSL_typed_load(binding.row), WN_Intconst(MTYPE_I8, record.problem_m)),
            WN_LT(MTYPE_I8, VHO_DSL_typed_load(binding.column), WN_Intconst(MTYPE_I8, record.problem_n)));
        WN_INSERT_BlockLast(body, WN_CreateIf(guard, dot, WN_CreateBlock()));
    }
    WN_INSERT_BlockLast(body, WN_CreateReturn());
    VHO_DSL_typed_source(body, binding.source_position);
    return body;
}

/* Compare canonical owned trees, including statement list links and relevant
 * symbol/type/field/value attributes. Shared/cyclic WNs are rejected. */
static BOOL VHO_DSL_typed_same(WN *actual, WN *expected, std::set<WN *> &seen)
{
    if (!actual || !expected || !seen.insert(actual).second || WN_opcode(actual) != WN_opcode(expected)) return FALSE;
    OPCODE opcode = WN_opcode(actual);
    OPERATOR operation = WN_operator(actual);
    if (OPCODE_has_next_prev(opcode) && WN_linenum(actual) != WN_linenum(expected)) return FALSE;
    if (operation == OPR_COMMENT)
        return !strcmp(Index_To_Str(WN_GetComment(actual)), Index_To_Str(WN_GetComment(expected)));
    if ((OPCODE_has_sym(opcode) && WN_st_idx(actual) != WN_st_idx(expected)) ||
        (OPCODE_has_offset(opcode) && WN_offset(actual) != WN_offset(expected)) ||
        (OPCODE_has_1ty(opcode) && WN_ty(actual) != WN_ty(expected)) ||
        (OPCODE_has_2ty(opcode) && WN_load_addr_ty(actual) != WN_load_addr_ty(expected)) ||
        (operation == OPR_INTCONST && WN_const_val(actual) != WN_const_val(expected)) ||
        (operation == OPR_CVTL && WN_cvtl_bits(actual) != WN_cvtl_bits(expected)) ||
        ((operation == OPR_EXTRACT_BITS || operation == OPR_COMPOSE_BITS) &&
            (WN_bit_offset(actual) != WN_bit_offset(expected) || WN_bit_size(actual) != WN_bit_size(expected))) ||
        ((operation == OPR_LDID || operation == OPR_STID || operation == OPR_ILOAD || operation == OPR_ISTORE) &&
            WN_field_id(actual) != WN_field_id(expected))) return FALSE;
    if (operation == OPR_BLOCK) {
        WN *left = WN_first(actual), *right = WN_first(expected), *previous = NULL;
        while (left && right) {
            if (WN_prev(left) != previous || !VHO_DSL_typed_same(left, right, seen)) return FALSE;
            previous = left; left = WN_next(left); right = WN_next(right);
        }
        return !left && !right && WN_last(actual) == previous;
    }
    if (WN_kid_count(actual) != WN_kid_count(expected)) return FALSE;
    for (INT32 index = 0; index < WN_kid_count(actual); ++index)
        if (!VHO_DSL_typed_same(WN_kid(actual, index), WN_kid(expected, index), seen)) return FALSE;
    return TRUE;
}

/* Regenerate only a scratch canonical view for the accepted bindings. Compare
 * and discard it; the borrowed actual body is never rewritten. */
BOOL VHO_DSL_typed_simt_verify(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_TYPED_SIMT_BINDING *binding,
    UINT32 execution, WN *block, FILE *diagnostic)
{
    if (!block || !VHO_DSL_typed_binding(record, numerical, binding, execution, diagnostic)) return FALSE;
    if (WN_operator(block) != OPR_BLOCK || !WN_first(block) || WN_operator(WN_first(block)) != OPR_COMMENT)
        return VHO_DSL_typed_error(diagnostic, "missing source contract identity");
    WN *annotation = WN_first(block);
    ST_IDX annotation_st = WN_st_idx(annotation);
    if (ST_IDX_level(annotation_st) != CURRENT_SYMTAB || ST_IDX_index(annotation_st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        ST_class(annotation_st) != CLASS_NAME) return VHO_DSL_typed_error(diagnostic, "invalid source contract identity");
    char description[768];
    VHO_DSL_typed_description(*record, *numerical, execution, description, sizeof(description));
    if (strcmp(Index_To_Str(WN_GetComment(annotation)), description) || WN_linenum(annotation) != binding->source_position)
        return VHO_DSL_typed_error(diagnostic, "stale source contract identity");
    WN *expected = VHO_DSL_typed_build(*record, *numerical, *binding, execution, FALSE);
    WN_INSERT_BlockFirst(expected, WN_COPY_Tree(annotation));
    std::set<WN *> seen;
    BOOL valid = VHO_DSL_typed_same(block, expected, seen);
    WN_DELETE_Tree(expected);
    return valid || VHO_DSL_typed_error(diagnostic, "body no longer matches typed source contract");
}

/* Preflight, construct and postcheck before assigning the caller's output.
 * No WN/ST/TY supplied by the caller is modified on failure. */
BOOL VHO_DSL_typed_simt_materialize(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_TYPED_SIMT_BINDING *binding,
    UINT32 execution, WN **block, FILE *diagnostic)
{
    if (!block || !VHO_DSL_typed_binding(record, numerical, binding, execution, diagnostic)) return FALSE;
    WN *body = VHO_DSL_typed_build(*record, *numerical, *binding, execution, TRUE);
    WN_Set_Linenum(body, binding->source_position);
    if (!VHO_DSL_typed_simt_verify(record, numerical, binding, execution, body, diagnostic)) {
        WN_DELETE_Tree(body);
        return FALSE;
    }
    *block = body;
    return TRUE;
}

/* Emit source/action evidence only for an independently revalidated body.
 * Counts describe static scalar operations, not symbolic GPU instructions. */
BOOL VHO_DSL_typed_simt_print(FILE *file, const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, const VHO_DSL_TYPED_SIMT_BINDING *binding,
    UINT32 execution, WN *block)
{
    if (!file || !VHO_DSL_typed_simt_verify(record, numerical, binding, execution, block, NULL)) return FALSE;
    fprintf(file, "# What it summarizes: MMA3 typed scalar baseline; no native MMA or timing certification.\n");
    USRCPOS position; USRCPOS_srcpos(position) = binding->source_position;
    fprintf(file, "# AI_SIMT_BASELINE 1 CONTRACT id owner contract source input product partial outer output permissions max_span evidence_kind evidence_id acceptance generation M N K threads disjoint_output source_line\n");
    fprintf(file, "AI_SIMT_BASELINE 1 CONTRACT %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u\n",
        record->id, record->owner_pu_st, numerical->id, numerical->source_format, numerical->input_format, numerical->product_format,
        numerical->partial_format, numerical->outer_format, numerical->output_format, numerical->permissions,
        numerical->maximum_span, numerical->evidence_kind, numerical->evidence_id, record->numerical_acceptance_id, record->generation,
        record->problem_m, record->problem_n, record->problem_k, record->threads_per_cta, record->disjoint_output, USRCPOS_linenum(position));
    fprintf(file, "AI_SIMT_BASELINE 1 ACTION %u %s loads_u2=2 widen_f4=2 product_f4=1 add_f4=1 store_f4=1 order=increasing_k contraction=none timing=unknown %u\n",
        record->id, execution == VHO_DSL_TYPED_SIMT_SERIAL ? "serial_reference" : "direct_simt", USRCPOS_linenum(position));
    return !ferror(file);
}
