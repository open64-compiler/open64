/* VHO-owned bounded PU-local shape solver and refinement discovery.
 * Reuse common local fact/inference contracts, keep compiler tables unchanged
 * before callbacks, and never enlarge the driver's PU/tree scope. Existing
 * diagnostics, iteration limits and callback failure behavior are preserved.
 * Design: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md. */
#include <algorithm>
#include <string.h>
#include <vector>
#include "dsl_shape_analysis.h"
#include "../../common/com/dsl_shape_fact_internal.h"
#include "../../common/ir_image/dsl_image_service.h"
#include "pu_info.h"
#include "symtab.h"
#include "wn.h"

typedef struct {
    DSL_IR_VALUE_ID value_id;
    TY_IDX ty;
    DSL_SHAPE_FACT seed;
    DSL_SHAPE_FACT fact;
    BOOL has_producer;
    SRCPOS source_position;
} DSL_SHAPE_SOLVER_VALUE;

typedef struct {
    DSL_IR_NODE_ID node_id;
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    DSL_IR_NODE_RECORD node;
    UINT32 result_index;
    std::vector<UINT32> operand_indices;
    SRCPOS source_position;
} DSL_SHAPE_SOLVER_CONSTRAINT;

/* Find a value ID in the borrowed PU-local scratch vector. Return its
 * zero-based index or all-ones when absent; never inspect another PU or
 * change compiler state. */
static UINT32
DSL_Shape_Solver_Find_Value
        (const std::vector<DSL_SHAPE_SOLVER_VALUE> &values,
         DSL_IR_VALUE_ID value_id)
{
    for (UINT32 i = 0; i < values.size(); ++i) {
        if (values[i].value_id == value_id)
            return i;
    }
    return ~(UINT32)0;
}

/* Reuse the first scratch value with this ID or append a zero-initialized
 * entry seeded from the resident tensor type. Failed type-fact derivation
 * records contradiction; no type/IR allocation occurs. Existing entries keep
 * their original seed and source position. */
static UINT32
DSL_Shape_Solver_Add_Value
        (std::vector<DSL_SHAPE_SOLVER_VALUE> *values,
         const DSL_IR_VALUE_RECORD &record,
         SRCPOS source_position)
{
    UINT32 found = DSL_Shape_Solver_Find_Value(*values, record.id);
    if (found != ~(UINT32)0)
        return found;
    DSL_SHAPE_SOLVER_VALUE value;
    memset(&value, 0, sizeof(value));
    value.value_id = record.id;
    value.ty = record.ty;
    value.has_producer =
        record.producer_node_id != DSL_IR_NODE_INVALID_ID;
    value.source_position = source_position;
    if (!DSL_Shape_Fact_From_Type(record.ty, &value.seed))
        value.seed.state = DSL_SHAPE_FACT_CONTRADICTION;
    value.fact = value.seed;
    values->push_back(value);
    return values->size() - 1;
}

/* Order borrowed constraints by native node ID for deterministic
 * propagation. Read-only private free-function comparator; no semantic
 * preference or plan choice. */
static BOOL
DSL_Shape_Constraint_Order
        (const DSL_SHAPE_SOLVER_CONSTRAINT &left,
         const DSL_SHAPE_SOLVER_CONSTRAINT &right)
{
    return left.node_id < right.node_id;
}

/* Resolve a native STID's logical opcode and image joins in the caller's
 * resident PU/symbol scope. Return FALSE for structural mismatch; output
 * temporaries may be partly assigned. Do not mutate tables, switch PU or
 * perform optimization. */
static BOOL
DSL_Shape_Find_Definition_Value
        (ST_IDX owner_pu_st,
         WN *definition,
         DSL_IR_VALUE_RECORD *value,
         DSL_IR_NODE_RECORD *node,
         DSL_IR_OPCODE_DESCRIPTOR_RECORD *descriptor,
         DSL_LOGICAL_OPCODE *logical_opcode)
{
    if (definition == NULL || WN_operator(definition) != OPR_STID ||
        WN_kid0(definition) == NULL ||
        !DSL_WN_Get_Logical_Opcode
             (WN_kid0(definition), logical_opcode, NULL))
        return FALSE;
    ST_IDX result_st = WN_st_idx(definition);
    if (ST_IDX_index(owner_pu_st) == 0 || ST_IDX_index(result_st) == 0 ||
        ST_IDX_level(result_st) != CURRENT_SYMTAB ||
        ST_IDX_index(result_st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        !DSL_IR_Image_Find_PU_Value
             (result_st, ST_name(St_Table[result_st]),
              ST_name(St_Table[owner_pu_st]), value) ||
        value->st != result_st || value->ty != WN_ty(definition) ||
        !DSL_IR_Image_Get_Node(value->producer_node_id, node) ||
        node->result_value_id != value->id ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node->opcode_descriptor_id, descriptor) ||
        descriptor->logical_operator !=
            (UINT32)logical_opcode->dsl_operator ||
        descriptor->version != logical_opcode->source_version ||
        node->operand_count != (UINT32)WN_kid_count(WN_kid0(definition)))
        return FALSE;
    return TRUE;
}

/* Traverse the caller-selected tree and collect scratch value/constraint
 * joins, diagnosing malformed native definitions or operands. Model inputs
 * remain producer-free seeds. Rejection may leave partial scratch vectors;
 * no compiler state or WHIRL is changed. */
static BOOL
DSL_Shape_Collect_Constraints
        (ST_IDX owner_pu_st,
         WN *wn,
         std::vector<DSL_SHAPE_SOLVER_VALUE> *values,
         std::vector<DSL_SHAPE_SOLVER_CONSTRAINT> *constraints,
         FILE *diagnostic)
{
    if (wn == NULL)
        return TRUE;
    BOOL valid = TRUE;
    if (WN_operator(wn) == OPR_STID && WN_kid0(wn) != NULL &&
        DSL_WN_Is_Native(WN_kid0(wn))) {
        DSL_IR_VALUE_RECORD result_value;
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_LOGICAL_OPCODE logical_opcode;
        if (!DSL_Shape_Find_Definition_Value
                 (owner_pu_st, wn, &result_value, &node, &descriptor,
                  &logical_opcode)) {
            SRCPOS source_position = WN_Get_Linenum(wn);
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL-SHAPE-001: malformed native definition at "
                        "line %u\n", SRCPOS_linenum(source_position));
            return FALSE;
        }
        DSL_SHAPE_SOLVER_CONSTRAINT constraint;
        constraint.node_id = node.id;
        constraint.dsl_operator = logical_opcode.dsl_operator;
        constraint.version = logical_opcode.effective_version;
        constraint.node = node;
        constraint.source_position = WN_Get_Linenum(wn);
        constraint.result_index = DSL_Shape_Solver_Add_Value
                                      (values, result_value,
                                       constraint.source_position);
        if (logical_opcode.dsl_operator == OPR_DSLMODELINPUT ||
            logical_opcode.dsl_operator == OPR_DSLTENSORCONST) {
            if (logical_opcode.dsl_operator == OPR_DSLMODELINPUT)
                (*values)[constraint.result_index].has_producer = FALSE;
            return TRUE;
        }
        for (UINT32 i = 0; i < node.operand_count; ++i) {
            DSL_IR_VALUE_REFERENCE_RECORD reference;
            DSL_IR_VALUE_RECORD operand;
            if (!DSL_IR_Image_Get_Value_Reference
                    (node.first_operand_reference_id + i, &reference) ||
                reference.owner_node_id != node.id ||
                reference.ordinal != i ||
                !DSL_IR_Image_Get_Value(reference.value_id, &operand)) {
                if (diagnostic != NULL)
                    fprintf(diagnostic,
                            "DSL-SHAPE-001: malformed operand %u for node "
                            "%u at line %u\n", i, node.id,
                            SRCPOS_linenum(constraint.source_position));
                valid = FALSE;
                break;
            }
            constraint.operand_indices.push_back
                (DSL_Shape_Solver_Add_Value
                     (values, operand, constraint.source_position));
        }
        if (valid)
            constraints->push_back(constraint);
    }
    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt)) {
            if (!DSL_Shape_Collect_Constraints
                     (owner_pu_st, stmt, values, constraints, diagnostic))
                valid = FALSE;
        }
        return valid;
    }
    for (INT32 i = 0; i < WN_kid_count(wn); ++i) {
        if (!DSL_Shape_Collect_Constraints
                 (owner_pu_st, WN_kid(wn, i), values, constraints,
                  diagnostic))
            valid = FALSE;
    }
    return valid;
}

/* Merge inferred evidence into a scratch fact and report whether canonical
 * fact identity changed on success. Contradiction returns FALSE with the
 * target marked/partly updated; changed is not assigned on failure. No
 * persistent mutation. */
static BOOL
DSL_Shape_Merge_Inference
        (DSL_SHAPE_FACT *fact,
         const DSL_SHAPE_FACT &inferred,
         BOOL *changed)
{
    DSL_SHAPE_FACT before = *fact;
    if (!DSL_Shape_Merge_Equal_Fact(fact, inferred))
        return FALSE;
    *changed = !DSL_Shape_Facts_Equal(before, *fact);
    return TRUE;
}

/* Extend an FNV-style checksum over borrowed raw bytes, preserving the
 * existing unsigned wraparound. The hash is a mutation sentinel, not
 * canonical identity or artifact authentication. */
static UINT64
DSL_Shape_Fingerprint_Bytes
        (UINT64 fingerprint,
         const void *data,
         size_t size)
{
    const unsigned char *bytes = (const unsigned char *)data;
    for (size_t i = 0; i < size; ++i) {
        fingerprint ^= bytes[i];
        fingerprint *= 1099511628211ULL;
    }
    return fingerprint;
}

/* Hash the current image header and opcode/node/attribute/value/reference
 * rows without changing them. This bounded raw-row sentinel does not prove
 * all compiler tables or side payloads are unchanged. */
static UINT64
DSL_Shape_Image_Fingerprint (void)
{
    UINT64 fingerprint = 1469598103934665603ULL;
    DSL_IR_IMAGE_HEADER header;
    DSL_IR_Image_Get_Header(&header);
    fingerprint = DSL_Shape_Fingerprint_Bytes
                      (fingerprint, &header, sizeof(header));
    for (UINT32 i = 1; i <= header.opcode_descriptor_count; ++i) {
        DSL_IR_OPCODE_DESCRIPTOR_RECORD record;
        DSL_IR_Image_Get_Opcode_Descriptor(i, &record);
        fingerprint = DSL_Shape_Fingerprint_Bytes
                          (fingerprint, &record, sizeof(record));
    }
    for (UINT32 i = 1; i <= header.node_count; ++i) {
        DSL_IR_NODE_RECORD record;
        DSL_IR_Image_Get_Node(i, &record);
        fingerprint = DSL_Shape_Fingerprint_Bytes
                          (fingerprint, &record, sizeof(record));
    }
    for (UINT32 i = 1; i <= header.attribute_count; ++i) {
        DSL_IR_ATTRIBUTE_RECORD record;
        DSL_IR_Image_Get_Attribute(i, &record);
        fingerprint = DSL_Shape_Fingerprint_Bytes
                          (fingerprint, &record, sizeof(record));
    }
    for (UINT32 i = 1; i <= header.value_count; ++i) {
        DSL_IR_VALUE_RECORD record;
        DSL_IR_Image_Get_Value(i, &record);
        fingerprint = DSL_Shape_Fingerprint_Bytes
                          (fingerprint, &record, sizeof(record));
    }
    for (UINT32 i = 1; i <= header.value_reference_count; ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD record;
        DSL_IR_Image_Get_Value_Reference(i, &record);
        fingerprint = DSL_Shape_Fingerprint_Bytes
                          (fingerprint, &record, sizeof(record));
    }
    return fingerprint;
}

/* Analyze only the driver-selected PU/tree using scratch seeds and
 * deterministically sorted constraints, iterating until stable or the
 * existing bounded limit. Report contradictions/counts and compare type
 * count/image sentinels before callbacks; do not allocate types or rewrite
 * IR. Missing inputs return FALSE with counts. Callbacks must honor the
 * caller's intended read-only contract; rejected callbacks and earlier
 * callback effects are not rolled back. Limit exhaustion alone has no
 * separate diagnostic. */
static BOOL
DSL_Shape_Analyze_PU_Internal
        (PU_Info *pu,
         WN *tree,
         FILE *diagnostic,
         DSL_SHAPE_REFINEMENT_VISITOR visitor,
         void *visitor_context,
         DSL_SHAPE_SOLVER_RESULT *result)
{
    DSL_SHAPE_SOLVER_RESULT local_result;
    memset(&local_result, 0, sizeof(local_result));
    DSL_IR_IMAGE_HEADER image_before;
    DSL_IR_IMAGE_HEADER image_after;
    DSL_IR_Image_Get_Header(&image_before);
    UINT64 image_fingerprint_before = DSL_Shape_Image_Fingerprint();
    UINT32 type_count_before = TY_Table_Size();

    if (pu == NULL || tree == NULL) {
        if (diagnostic != NULL)
            fprintf(diagnostic, "DSL-SHAPE-001: missing PU or WHIRL tree\n");
        local_result.contradiction_count = 1;
        local_result.diagnostic_count = 1;
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    std::vector<DSL_SHAPE_SOLVER_VALUE> values;
    std::vector<DSL_SHAPE_SOLVER_CONSTRAINT> constraints;
    BOOL valid = DSL_Shape_Collect_Constraints
                     (PU_Info_proc_sym(pu), tree, &values, &constraints,
                      diagnostic);
    /* Stable native-node order makes propagation independent of tree walk order. */
    std::sort(constraints.begin(), constraints.end(),
              DSL_Shape_Constraint_Order);
    local_result.visited_node_count = constraints.size();
    local_result.value_count = values.size();

    BOOL changed = TRUE;
    UINT32 iteration_limit = values.size() + constraints.size() + 1;
    while (valid && changed && local_result.iteration_count < iteration_limit) {
        changed = FALSE;
        ++local_result.iteration_count;
        for (UINT32 i = 0; i < constraints.size(); ++i) {
            DSL_SHAPE_SOLVER_CONSTRAINT &constraint = constraints[i];
            std::vector<TY_IDX> operand_types;
            std::vector<DSL_SHAPE_FACT> operand_facts;
            for (UINT32 j = 0; j < constraint.operand_indices.size(); ++j) {
                DSL_SHAPE_SOLVER_VALUE &operand =
                    values[constraint.operand_indices[j]];
                operand_types.push_back(operand.ty);
                operand_facts.push_back(operand.fact);
            }
            DSL_SHAPE_INFERENCE_INPUT input;
            input.dsl_operator = constraint.dsl_operator;
            input.version = constraint.version;
            input.node = &constraint.node;
            input.operand_types = operand_types.empty() ?
                                  NULL : &operand_types[0];
            input.operand_facts = operand_facts.empty() ?
                                  NULL : &operand_facts[0];
            input.operand_count = operand_types.size();
            input.result_ty = values[constraint.result_index].ty;
            DSL_SHAPE_FACT inferred;
            DSL_SHAPE_INFERENCE_RESULT inference =
                DSL_Shape_Infer_Operator(&input, &inferred);
            if (inference == DSL_SHAPE_INFERENCE_UNREGISTERED)
                continue;
            BOOL all_seed_facts_complete =
                values[constraint.result_index].seed.state ==
                    DSL_SHAPE_FACT_COMPLETE;
            for (UINT32 j = 0;
                 j < constraint.operand_indices.size() &&
                 all_seed_facts_complete; ++j) {
                all_seed_facts_complete =
                    values[constraint.operand_indices[j]].seed.state ==
                        DSL_SHAPE_FACT_COMPLETE;
            }
            if (all_seed_facts_complete) {
                DSL_SHAPE_OPERATOR_INPUT check;
                check.dsl_operator = constraint.dsl_operator;
                check.version = constraint.version;
                check.node = &constraint.node;
                check.operand_types = input.operand_types;
                check.operand_count = input.operand_count;
                check.result_ty = input.result_ty;
                if (DSL_Shape_Check_Operator(&check) !=
                    DSL_SHAPE_CHECK_VALID)
                    inference = DSL_SHAPE_INFERENCE_CONTRADICTION;
            }
            if (inference == DSL_SHAPE_INFERENCE_CONTRADICTION) {
                values[constraint.result_index].fact.state =
                    DSL_SHAPE_FACT_CONTRADICTION;
                valid = FALSE;
            } else {
                BOOL value_changed = FALSE;
                if (!DSL_Shape_Merge_Inference
                         (&values[constraint.result_index].fact, inferred,
                          &value_changed))
                    valid = FALSE;
                changed = changed || value_changed;
            }
            if (!valid && diagnostic != NULL) {
                fprintf(diagnostic,
                        "DSL-SHAPE-002: %s.v%u shape contradiction for "
                        "value %u at file %u line %u\n",
                        DSL_OPERATOR_name(constraint.dsl_operator),
                        constraint.version,
                        values[constraint.result_index].value_id,
                        SRCPOS_filenum(constraint.source_position),
                        SRCPOS_linenum(constraint.source_position));
                ++local_result.diagnostic_count;
            }
            if (!valid)
                break;
        }
    }

    for (UINT32 i = 0; i < values.size(); ++i) {
        const DSL_SHAPE_SOLVER_VALUE &value = values[i];
        if (TY_kind(value.ty) == KIND_SCALAR)
            continue;
        if (value.fact.state == DSL_SHAPE_FACT_CONTRADICTION) {
            ++local_result.contradiction_count;
            continue;
        }
        if (value.fact.state == DSL_SHAPE_FACT_COMPLETE) {
            BOOL symbolic = FALSE;
            BOOL runtime_dynamic = FALSE;
            for (INT32 j = 0; j < value.fact.rank; ++j) {
                symbolic = symbolic ||
                    value.fact.dimension_kind[j] ==
                        DSL_SHAPE_DIMENSION_SYMBOL ||
                    value.fact.dimension_kind[j] ==
                        DSL_SHAPE_DIMENSION_EXPRESSION;
                runtime_dynamic = runtime_dynamic ||
                    value.fact.dimension_kind[j] ==
                        DSL_SHAPE_DIMENSION_ANONYMOUS_DYNAMIC;
            }
            if (symbolic)
                ++local_result.symbolic_value_count;
            if (runtime_dynamic)
                ++local_result.runtime_dynamic_value_count;
            if (DSL_Shape_Facts_Equal(value.seed, value.fact))
                ++local_result.unchanged_value_count;
            else
                ++local_result.refinable_value_count;
        } else if (value.has_producer) {
            ++local_result.unresolved_value_count;
        } else {
            ++local_result.pending_value_count;
        }
    }

    DSL_IR_Image_Get_Header(&image_after);
    if (TY_Table_Size() != type_count_before ||
        memcmp(&image_before, &image_after, sizeof(image_before)) != 0 ||
        DSL_Shape_Image_Fingerprint() != image_fingerprint_before) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "DSL-SHAPE-003: check-only analysis mutated compiler "
                    "tables\n");
        ++local_result.contradiction_count;
        ++local_result.diagnostic_count;
        valid = FALSE;
    }
    /* Check compiler-state sentinels before handing borrowed refinements to clients. */
    if (valid && visitor != NULL) {
        for (UINT32 i = 0; i < values.size(); ++i) {
            const DSL_SHAPE_SOLVER_VALUE &value = values[i];
            if (value.fact.state != DSL_SHAPE_FACT_COMPLETE ||
                DSL_Shape_Facts_Equal(value.seed, value.fact))
                continue;
            DSL_SHAPE_REFINEMENT refinement;
            refinement.value_id = value.value_id;
            refinement.expected_old_ty = value.ty;
            refinement.refined_fact = value.fact;
            refinement.source_position = value.source_position;
            if (!visitor(&refinement, visitor_context)) {
                if (diagnostic != NULL)
                    fprintf(diagnostic,
                            "DSL-SHAPE-004: refinement consumer rejected "
                            "value %u at file %u line %u\n",
                            refinement.value_id,
                            SRCPOS_filenum(refinement.source_position),
                            SRCPOS_linenum(refinement.source_position));
                ++local_result.diagnostic_count;
                valid = FALSE;
                break;
            }
        }
    }
    if (result != NULL)
        *result = local_result;
    return valid && local_result.contradiction_count == 0;
}

/* Run check-only PU-local propagation with no refinement callback. Return
 * validation status and optional counts/diagnostics; no PU switching,
 * optimization choice, type construction or WHIRL rewrite. */
BOOL
DSL_Shape_Analyze_PU
        (PU_Info *pu,
         WN *tree,
         FILE *diagnostic,
         DSL_SHAPE_SOLVER_RESULT *result)
{
    return DSL_Shape_Analyze_PU_Internal
               (pu, tree, diagnostic, NULL, NULL, result);
}

/* Run the same bounded PU-local solver and visit complete changed facts only
 * after its pre-callback mutation checks pass. Visitor inputs are borrowed
 * for each call. Rejection stops further visits and returns FALSE; earlier
 * callback effects are not rolled back. */
BOOL
DSL_Shape_Analyze_PU_With_Refinements
        (PU_Info *pu,
         WN *tree,
         FILE *diagnostic,
         DSL_SHAPE_REFINEMENT_VISITOR visitor,
         void *visitor_context,
         DSL_SHAPE_SOLVER_RESULT *result)
{
    return DSL_Shape_Analyze_PU_Internal
               (pu, tree, diagnostic, visitor, visitor_context, result);
}
