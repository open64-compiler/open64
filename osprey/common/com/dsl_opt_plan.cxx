/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <vector>

#include "dsl_opt_plan.h"
#include "pu_info.h"

struct dsl_opt_plan_context {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    const DSL_TENSOR_EVOLUTION_GRAPH *graph;
    DSL_OPT_PLAN_BUDGET budget;
    std::vector<DSL_OPT_CANDIDATE_RECORD> candidates;
    std::vector<DSL_OPT_COST_RECORD> costs;
    std::vector<DSL_OPT_PLAN_RECORD> plans;
    std::vector<DSL_OPT_PLAN_MEMBER_RECORD> members;
    BOOL candidate_budget_exhausted;
    BOOL plan_budget_exhausted;
    BOOL selection_complete;
    DSL_OPT_SELECTION_RESULT selection;
};

static const char *DSL_opt_candidate_kind_name[] = {
    "unknown", "baseline", "tile", "fusion", "layout", "placement",
    "sharding", "residency", "pipeline", "kernel", "runtime_variant"
};

static const char *DSL_opt_legality_name[] = {
    "unknown", "proven", "rejected"
};

static const char *DSL_opt_rejection_reason_name[] = {
    "none", "incomplete_analysis", "invalid_reference", "ownership",
    "descriptor", "effect", "resource", "cost_incomplete",
    "target_mismatch", "budget_exhausted", "malformed"
};

static const char *DSL_opt_cost_term_name[] = {
    "compute", "memory_unhidden", "communication_unhidden", "sync",
    "launch", "runtime_selection"
};

static const char *DSL_opt_cost_unit_name[] = {
    "unknown", "relative", "cycles", "nanoseconds"
};

static const char *DSL_opt_cost_confidence_name[] = {
    "unknown", "low", "medium", "high", "exact"
};

static const char *DSL_opt_cost_evidence_name[] = {
    "unknown", "baseline_policy", "static_analysis", "target_model",
    "measured", "telemetry"
};

static BOOL
DSL_Opt_Plan_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL optimization plan error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Opt_Plan_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Opt_Plan_Active (const DSL_OPT_PLAN_CONTEXT *context)
{
    return context != NULL && context->pu != NULL &&
           Current_PU_Info == context->pu &&
           DSL_Opt_Plan_Owner_Valid(context->owner_pu_st) &&
           PU_Info_proc_sym(context->pu) == context->owner_pu_st &&
           Current_pu == &Pu_Table[ST_pu(St_Table[context->owner_pu_st])];
}

static BOOL
DSL_Opt_Legality_Valid (UINT32 legality, UINT32 reason)
{
    if (legality == DSL_OPT_LEGALITY_PROVEN)
        return reason == DSL_OPT_REJECT_NONE;
    if (legality == DSL_OPT_LEGALITY_UNKNOWN)
        return reason == DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
    if (legality == DSL_OPT_LEGALITY_REJECTED)
        return (reason >= DSL_OPT_REJECT_INVALID_REFERENCE &&
                reason <= DSL_OPT_REJECT_RESOURCE) ||
               reason == DSL_OPT_REJECT_MALFORMED;
    return FALSE;
}

static BOOL
DSL_Opt_Cost_Term_Valid
        (const DSL_OPT_COST_TERM &term, BOOL *known)
{
    if (term.reserved != 0)
        return FALSE;
    if (term.unit == DSL_OPT_COST_UNIT_UNKNOWN ||
        term.confidence == DSL_OPT_COST_CONFIDENCE_UNKNOWN ||
        term.evidence == DSL_OPT_COST_EVIDENCE_UNKNOWN) {
        *known = FALSE;
        return term.amount == 0 && term.unit == DSL_OPT_COST_UNIT_UNKNOWN &&
               term.confidence == DSL_OPT_COST_CONFIDENCE_UNKNOWN &&
               term.evidence == DSL_OPT_COST_EVIDENCE_UNKNOWN;
    }
    *known = TRUE;
    return term.unit <= DSL_OPT_COST_UNIT_NANOSECONDS &&
           term.confidence <= DSL_OPT_COST_CONFIDENCE_EXACT &&
           term.evidence <= DSL_OPT_COST_EVIDENCE_TELEMETRY;
}

static BOOL
DSL_Opt_Cost_Calculate
        (const DSL_OPT_COST_TERM *terms,
         UINT64 *total, UINT32 *complete, UINT32 *minimum_confidence)
{
    UINT64 sum = 0;
    UINT32 unit = DSL_OPT_COST_UNIT_UNKNOWN;
    UINT32 minimum = DSL_OPT_COST_CONFIDENCE_EXACT;
    BOOL all_known = TRUE;

    for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i) {
        BOOL known = FALSE;
        if (!DSL_Opt_Cost_Term_Valid(terms[i], &known))
            return FALSE;
        if (!known) {
            all_known = FALSE;
            continue;
        }
        if (unit == DSL_OPT_COST_UNIT_UNKNOWN)
            unit = terms[i].unit;
        else if (unit != terms[i].unit)
            return FALSE;
        if (~(UINT64)0 - sum < terms[i].amount)
            return FALSE;
        sum += terms[i].amount;
        if (terms[i].confidence < minimum)
            minimum = terms[i].confidence;
    }
    *complete = all_known ? 1 : 0;
    *total = all_known ? sum : 0;
    *minimum_confidence = all_known ? minimum : 0;
    return TRUE;
}

static BOOL
DSL_Opt_Candidate_Input_Valid
        (const DSL_OPT_PLAN_CONTEXT *context,
         const DSL_OPT_CANDIDATE_INPUT *input)
{
    DSL_TENSOR_EVOLUTION_NODE_RECORD source;
    DSL_TENSOR_EVOLUTION_NODE_RECORD result;
    DSL_IR_NODE_RECORD semantic_node;
    DSL_IR_VALUE_RECORD semantic_value;

    if (input == NULL || input->kind < DSL_OPT_CANDIDATE_BASELINE ||
        input->kind > DSL_OPT_CANDIDATE_RUNTIME_VARIANT ||
        input->semantic_node_id == DSL_IR_NODE_INVALID_ID ||
        input->source_evolution_node_id ==
            DSL_TENSOR_EVOLUTION_NODE_INVALID_ID ||
        input->result_evolution_node_id ==
            DSL_TENSOR_EVOLUTION_NODE_INVALID_ID ||
        input->ordering_key == 0 ||
        (input->flags & ~(DSL_OPT_CANDIDATE_FLAG_BASELINE |
                          DSL_OPT_CANDIDATE_FLAG_PROVISIONAL)) != 0 ||
        !DSL_Opt_Legality_Valid(input->legality,
                                input->rejection_reason) ||
        !DSL_Tensor_Evolution_Get_Node
             (context->graph, input->source_evolution_node_id, &source) ||
        !DSL_Tensor_Evolution_Get_Node
             (context->graph, input->result_evolution_node_id, &result) ||
        source.owner_pu_st != context->owner_pu_st ||
        result.owner_pu_st != context->owner_pu_st ||
        source.semantic_root_id != result.semantic_root_id ||
        !DSL_IR_Image_Get_Node(input->semantic_node_id, &semantic_node) ||
        (semantic_node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0 ||
        !DSL_IR_Image_Get_Value(semantic_node.result_value_id,
                                &semantic_value) ||
        semantic_value.id != result.semantic_value_id)
        return FALSE;

    if (input->kind == DSL_OPT_CANDIDATE_BASELINE)
        return input->parent_candidate_id == DSL_OPT_CANDIDATE_INVALID_ID &&
               (input->flags & DSL_OPT_CANDIDATE_FLAG_BASELINE) != 0 &&
               (input->flags & DSL_OPT_CANDIDATE_FLAG_PROVISIONAL) == 0;
    if (input->parent_candidate_id == DSL_OPT_CANDIDATE_INVALID_ID ||
        input->parent_candidate_id > context->candidates.size() ||
        (input->flags & DSL_OPT_CANDIDATE_FLAG_BASELINE) != 0)
        return FALSE;
    const DSL_OPT_CANDIDATE_RECORD &parent =
        context->candidates[input->parent_candidate_id - 1];
    return parent.semantic_node_id == input->semantic_node_id &&
           parent.result_evolution_node_id ==
               input->source_evolution_node_id;
}

const char *
DSL_Opt_Candidate_Kind_Name (UINT32 kind)
{
    return kind < sizeof(DSL_opt_candidate_kind_name) /
                      sizeof(DSL_opt_candidate_kind_name[0]) ?
           DSL_opt_candidate_kind_name[kind] : "unknown";
}

const char *
DSL_Opt_Legality_Name (UINT32 legality)
{
    return legality < sizeof(DSL_opt_legality_name) /
                          sizeof(DSL_opt_legality_name[0]) ?
           DSL_opt_legality_name[legality] : "unknown";
}

const char *
DSL_Opt_Rejection_Reason_Name (UINT32 reason)
{
    return reason < sizeof(DSL_opt_rejection_reason_name) /
                      sizeof(DSL_opt_rejection_reason_name[0]) ?
           DSL_opt_rejection_reason_name[reason] : "unknown";
}

const char *
DSL_Opt_Cost_Term_Name (UINT32 term)
{
    return term < DSL_OPT_COST_TERM_COUNT ?
           DSL_opt_cost_term_name[term] : "unknown";
}

const char *
DSL_Opt_Cost_Unit_Name (UINT32 unit)
{
    return unit < sizeof(DSL_opt_cost_unit_name) /
                      sizeof(DSL_opt_cost_unit_name[0]) ?
           DSL_opt_cost_unit_name[unit] : "unknown";
}

const char *
DSL_Opt_Cost_Confidence_Name (UINT32 confidence)
{
    return confidence < sizeof(DSL_opt_cost_confidence_name) /
                            sizeof(DSL_opt_cost_confidence_name[0]) ?
           DSL_opt_cost_confidence_name[confidence] : "unknown";
}

const char *
DSL_Opt_Cost_Evidence_Name (UINT32 evidence)
{
    return evidence < sizeof(DSL_opt_cost_evidence_name) /
                          sizeof(DSL_opt_cost_evidence_name[0]) ?
           DSL_opt_cost_evidence_name[evidence] : "unknown";
}

DSL_OPT_PLAN_CONTEXT *
DSL_Opt_Plan_Create
        (PU_Info *pu, const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_OPT_PLAN_BUDGET *budget, FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || budget == NULL ||
        budget->max_candidates == 0 || budget->max_plans == 0 ||
        Current_PU_Info != pu ||
        DSL_Tensor_Evolution_Owner(graph) != PU_Info_proc_sym(pu) ||
        !DSL_Tensor_Evolution_Verify(graph, diagnostic)) {
        DSL_Opt_Plan_Report(diagnostic, "invalid active program unit", 0);
        return NULL;
    }

    DSL_OPT_PLAN_CONTEXT *context = new DSL_OPT_PLAN_CONTEXT;
    context->pu = pu;
    context->owner_pu_st = PU_Info_proc_sym(pu);
    context->graph = graph;
    context->budget = *budget;
    context->candidate_budget_exhausted = FALSE;
    context->plan_budget_exhausted = FALSE;
    context->selection_complete = FALSE;
    memset(&context->selection, 0, sizeof(context->selection));
    return context;
}

void
DSL_Opt_Plan_Destroy (DSL_OPT_PLAN_CONTEXT *context)
{
    delete context;
}

BOOL
DSL_Opt_Plan_Add_Candidate
        (DSL_OPT_PLAN_CONTEXT *context,
         const DSL_OPT_CANDIDATE_INPUT *input,
         DSL_OPT_CANDIDATE_ID *candidate_id, FILE *diagnostic)
{
    DSL_OPT_CANDIDATE_RECORD candidate;
    if (candidate_id != NULL)
        *candidate_id = DSL_OPT_CANDIDATE_INVALID_ID;
    if (!DSL_Opt_Plan_Active(context) || context->selection_complete)
        return DSL_Opt_Plan_Report
                   (diagnostic, "candidate context is not mutable", 0);
    if (context->candidates.size() >= context->budget.max_candidates) {
        context->candidate_budget_exhausted = TRUE;
        return DSL_Opt_Plan_Report
                   (diagnostic, "candidate budget exhausted",
                    context->candidates.size() + 1);
    }
    if (!DSL_Opt_Candidate_Input_Valid(context, input) ||
        (((input->flags & DSL_OPT_CANDIDATE_FLAG_BASELINE) != 0) !=
         context->candidates.empty()) ||
        (!context->candidates.empty() &&
         input->ordering_key <= context->candidates.back().ordering_key))
        return DSL_Opt_Plan_Report
                   (diagnostic, "invalid candidate", 0);

    memset(&candidate, 0, sizeof(candidate));
    candidate.id = context->candidates.size() + 1;
    candidate.kind = input->kind;
    candidate.owner_pu_st = context->owner_pu_st;
    candidate.semantic_node_id = input->semantic_node_id;
    candidate.source_evolution_node_id =
        input->source_evolution_node_id;
    candidate.result_evolution_node_id =
        input->result_evolution_node_id;
    candidate.parent_candidate_id = input->parent_candidate_id;
    candidate.legality = input->legality;
    candidate.rejection_reason = input->rejection_reason;
    candidate.ordering_key = input->ordering_key;
    candidate.flags = input->flags;
    context->candidates.push_back(candidate);
    if (candidate_id != NULL)
        *candidate_id = candidate.id;
    return TRUE;
}

BOOL
DSL_Opt_Plan_Add_Cost
        (DSL_OPT_PLAN_CONTEXT *context, const DSL_OPT_COST_INPUT *input,
         DSL_OPT_COST_ID *cost_id, FILE *diagnostic)
{
    DSL_OPT_COST_RECORD cost;
    if (cost_id != NULL)
        *cost_id = DSL_OPT_COST_INVALID_ID;
    if (!DSL_Opt_Plan_Active(context) || context->selection_complete)
        return DSL_Opt_Plan_Report
                   (diagnostic, "cost context is not mutable", 0);
    if (context->costs.size() >= context->budget.max_plans)
        return DSL_Opt_Plan_Report
                   (diagnostic, "cost budget exhausted",
                    context->costs.size() + 1);
    if (input == NULL || input->target_profile_id == 0 ||
        input->ordering_key == 0 ||
        (!context->costs.empty() &&
         input->ordering_key <= context->costs.back().ordering_key))
        return DSL_Opt_Plan_Report(diagnostic, "invalid cost", 0);

    memset(&cost, 0, sizeof(cost));
    cost.id = context->costs.size() + 1;
    cost.owner_pu_st = context->owner_pu_st;
    cost.target_profile_id = input->target_profile_id;
    cost.ordering_key = input->ordering_key;
    memcpy(cost.terms, input->terms, sizeof(cost.terms));
    if (!DSL_Opt_Cost_Calculate(cost.terms, &cost.total,
                                &cost.complete,
                                &cost.minimum_confidence))
        return DSL_Opt_Plan_Report(diagnostic, "malformed cost", cost.id);
    context->costs.push_back(cost);
    if (cost_id != NULL)
        *cost_id = cost.id;
    return TRUE;
}

BOOL
DSL_Opt_Plan_Add_Plan
        (DSL_OPT_PLAN_CONTEXT *context, const DSL_OPT_PLAN_INPUT *input,
         DSL_OPT_PLAN_ID *plan_id, FILE *diagnostic)
{
    DSL_OPT_PLAN_RECORD plan;
    if (plan_id != NULL)
        *plan_id = DSL_OPT_PLAN_INVALID_ID;
    if (!DSL_Opt_Plan_Active(context) || context->selection_complete)
        return DSL_Opt_Plan_Report
                   (diagnostic, "plan context is not mutable", 0);
    if (context->plans.size() >= context->budget.max_plans) {
        context->plan_budget_exhausted = TRUE;
        return DSL_Opt_Plan_Report
                   (diagnostic, "plan budget exhausted",
                    context->plans.size() + 1);
    }
    if (input == NULL || input->candidate_ids == NULL ||
        input->candidate_count == 0 || input->cost_id == 0 ||
        input->cost_id > context->costs.size() || input->ordering_key == 0 ||
        (input->flags & ~(DSL_OPT_PLAN_FLAG_BASELINE |
                          DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY)) != 0 ||
        !DSL_Opt_Legality_Valid(input->legality,
                                input->rejection_reason) ||
        (!context->plans.empty() &&
         input->ordering_key <= context->plans.back().ordering_key))
        return DSL_Opt_Plan_Report(diagnostic, "invalid plan", 0);

    BOOL baseline = (input->flags & DSL_OPT_PLAN_FLAG_BASELINE) != 0;
    const DSL_OPT_COST_RECORD &cost = context->costs[input->cost_id - 1];
    if ((baseline && input->fallback_plan_id != 0) ||
        (!baseline && (input->fallback_plan_id == 0 ||
                       input->fallback_plan_id > context->plans.size())))
        return DSL_Opt_Plan_Report(diagnostic, "invalid fallback", 0);
    if (baseline) {
        if (!context->plans.empty() ||
            input->legality != DSL_OPT_LEGALITY_PROVEN || !cost.complete)
            return DSL_Opt_Plan_Report
                       (diagnostic, "invalid baseline plan", 0);
    } else {
        const DSL_OPT_PLAN_RECORD &fallback =
            context->plans[input->fallback_plan_id - 1];
        const DSL_OPT_COST_RECORD &fallback_cost =
            context->costs[fallback.cost_id - 1];
        if (fallback.legality != DSL_OPT_LEGALITY_PROVEN ||
            !fallback_cost.complete ||
            fallback_cost.target_profile_id != cost.target_profile_id)
            return DSL_Opt_Plan_Report
                       (diagnostic, "incompatible fallback", 0);
    }
    for (UINT32 i = 0; i < input->candidate_count; ++i) {
        DSL_OPT_CANDIDATE_ID id = input->candidate_ids[i];
        if (id == 0 || id > context->candidates.size() ||
            (i != 0 && id <= input->candidate_ids[i - 1]))
            return DSL_Opt_Plan_Report
                       (diagnostic, "invalid plan member", id);
        const DSL_OPT_CANDIDATE_RECORD &candidate =
            context->candidates[id - 1];
        if (input->legality == DSL_OPT_LEGALITY_PROVEN &&
            candidate.legality != DSL_OPT_LEGALITY_PROVEN)
            return DSL_Opt_Plan_Report
                       (diagnostic, "unproven plan member", id);
        if (baseline !=
            ((candidate.flags & DSL_OPT_CANDIDATE_FLAG_BASELINE) != 0))
            return DSL_Opt_Plan_Report
                       (diagnostic, "baseline member mismatch", id);
    }

    memset(&plan, 0, sizeof(plan));
    plan.id = context->plans.size() + 1;
    plan.owner_pu_st = context->owner_pu_st;
    plan.first_member = context->members.size();
    plan.member_count = input->candidate_count;
    plan.cost_id = input->cost_id;
    plan.fallback_plan_id = input->fallback_plan_id;
    plan.legality = input->legality;
    plan.rejection_reason = input->rejection_reason;
    plan.ordering_key = input->ordering_key;
    plan.flags = input->flags;
    for (UINT32 i = 0; i < input->candidate_count; ++i) {
        DSL_OPT_PLAN_MEMBER_RECORD member;
        memset(&member, 0, sizeof(member));
        member.plan_id = plan.id;
        member.candidate_id = input->candidate_ids[i];
        member.ordinal = i;
        context->members.push_back(member);
    }
    context->plans.push_back(plan);
    if (plan_id != NULL)
        *plan_id = plan.id;
    return TRUE;
}

BOOL
DSL_Opt_Plan_Verify
        (const DSL_OPT_PLAN_CONTEXT *context, FILE *diagnostic)
{
    if (!DSL_Opt_Plan_Active(context) ||
        !DSL_Tensor_Evolution_Verify(context->graph, diagnostic) ||
        context->candidates.size() > context->budget.max_candidates ||
        context->costs.size() > context->budget.max_plans ||
        context->plans.size() > context->budget.max_plans)
        return DSL_Opt_Plan_Report(diagnostic, "invalid context", 0);

    for (UINT32 i = 0; i < context->candidates.size(); ++i) {
        const DSL_OPT_CANDIDATE_RECORD &candidate = context->candidates[i];
        DSL_OPT_CANDIDATE_INPUT input;
        memset(&input, 0, sizeof(input));
        input.kind = candidate.kind;
        input.semantic_node_id = candidate.semantic_node_id;
        input.source_evolution_node_id =
            candidate.source_evolution_node_id;
        input.result_evolution_node_id =
            candidate.result_evolution_node_id;
        input.parent_candidate_id = candidate.parent_candidate_id;
        input.legality = candidate.legality;
        input.rejection_reason = candidate.rejection_reason;
        input.ordering_key = candidate.ordering_key;
        input.flags = candidate.flags;
        if (candidate.id != i + 1 ||
            candidate.owner_pu_st != context->owner_pu_st ||
            (((candidate.flags & DSL_OPT_CANDIDATE_FLAG_BASELINE) != 0) !=
             (i == 0)) ||
            (i != 0 && candidate.ordering_key <=
                       context->candidates[i - 1].ordering_key) ||
            !DSL_Opt_Candidate_Input_Valid(context, &input))
            return DSL_Opt_Plan_Report
                       (diagnostic, "invalid candidate record", candidate.id);
    }
    for (UINT32 i = 0; i < context->costs.size(); ++i) {
        const DSL_OPT_COST_RECORD &cost = context->costs[i];
        UINT64 total;
        UINT32 complete;
        UINT32 minimum;
        if (cost.id != i + 1 || cost.owner_pu_st != context->owner_pu_st ||
            cost.target_profile_id == 0 || cost.ordering_key == 0 ||
            (i != 0 && cost.ordering_key <=
                       context->costs[i - 1].ordering_key) ||
            !DSL_Opt_Cost_Calculate(cost.terms, &total, &complete, &minimum) ||
            cost.total != total || cost.complete != complete ||
            cost.minimum_confidence != minimum)
            return DSL_Opt_Plan_Report
                       (diagnostic, "invalid cost record", cost.id);
    }
    UINT32 expected_member = 0;
    for (UINT32 i = 0; i < context->plans.size(); ++i) {
        const DSL_OPT_PLAN_RECORD &plan = context->plans[i];
        BOOL baseline = (plan.flags & DSL_OPT_PLAN_FLAG_BASELINE) != 0;
        if (plan.id != i + 1 || plan.owner_pu_st != context->owner_pu_st ||
            plan.first_member != expected_member || plan.member_count == 0 ||
            plan.cost_id == 0 || plan.cost_id > context->costs.size() ||
            plan.first_member > context->members.size() ||
            plan.member_count > context->members.size() - plan.first_member ||
            !DSL_Opt_Legality_Valid(plan.legality,
                                    plan.rejection_reason) ||
            plan.ordering_key == 0 ||
            (plan.flags & ~(DSL_OPT_PLAN_FLAG_BASELINE |
                            DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY)) != 0 ||
            (baseline != (i == 0)) ||
            (i != 0 && plan.ordering_key <=
                       context->plans[i - 1].ordering_key) ||
            (baseline && plan.fallback_plan_id != 0) ||
            (!baseline && (plan.fallback_plan_id == 0 ||
                           plan.fallback_plan_id >= plan.id)))
            return DSL_Opt_Plan_Report
                       (diagnostic, "invalid plan record", plan.id);
        const DSL_OPT_COST_RECORD &cost = context->costs[plan.cost_id - 1];
        if (baseline) {
            if (plan.legality != DSL_OPT_LEGALITY_PROVEN || !cost.complete)
                return DSL_Opt_Plan_Report
                           (diagnostic, "invalid baseline plan", plan.id);
        } else {
            const DSL_OPT_PLAN_RECORD &fallback =
                context->plans[plan.fallback_plan_id - 1];
            const DSL_OPT_COST_RECORD &fallback_cost =
                context->costs[fallback.cost_id - 1];
            if (fallback.legality != DSL_OPT_LEGALITY_PROVEN ||
                !fallback_cost.complete ||
                fallback_cost.target_profile_id != cost.target_profile_id)
                return DSL_Opt_Plan_Report
                           (diagnostic, "invalid fallback plan", plan.id);
        }
        for (UINT32 ordinal = 0; ordinal < plan.member_count; ++ordinal) {
            const DSL_OPT_PLAN_MEMBER_RECORD &member =
                context->members[expected_member + ordinal];
            if (member.plan_id != plan.id || member.ordinal != ordinal ||
                member.reserved != 0 || member.candidate_id == 0 ||
                member.candidate_id > context->candidates.size() ||
                (ordinal != 0 && member.candidate_id <=
                    context->members[expected_member + ordinal - 1].candidate_id))
                return DSL_Opt_Plan_Report
                           (diagnostic, "invalid plan member", plan.id);
            const DSL_OPT_CANDIDATE_RECORD &candidate =
                context->candidates[member.candidate_id - 1];
            if ((plan.legality == DSL_OPT_LEGALITY_PROVEN &&
                 candidate.legality != DSL_OPT_LEGALITY_PROVEN) ||
                (baseline !=
                 ((candidate.flags & DSL_OPT_CANDIDATE_FLAG_BASELINE) != 0)))
                return DSL_Opt_Plan_Report
                           (diagnostic, "incompatible plan member", plan.id);
        }
        expected_member += plan.member_count;
    }
    if (expected_member != context->members.size())
        return DSL_Opt_Plan_Report(diagnostic, "orphan plan member", 0);
    if (context->selection_complete) {
        if (context->selection.selected_plan_id == 0 ||
            context->selection.selected_plan_id > context->plans.size() ||
            context->selection.target_profile_id == 0)
            return DSL_Opt_Plan_Report(diagnostic, "invalid selection", 0);
        const DSL_OPT_PLAN_RECORD &selected =
            context->plans[context->selection.selected_plan_id - 1];
        const DSL_OPT_COST_RECORD &selected_cost =
            context->costs[selected.cost_id - 1];
        if (selected.legality != DSL_OPT_LEGALITY_PROVEN ||
            !selected_cost.complete ||
            selected_cost.target_profile_id !=
                context->selection.target_profile_id)
            return DSL_Opt_Plan_Report
                       (diagnostic, "unselectable selected plan", selected.id);
    }
    return TRUE;
}

BOOL
DSL_Opt_Plan_Select
        (DSL_OPT_PLAN_CONTEXT *context, UINT32 target_profile_id,
         DSL_OPT_SELECTION_RESULT *result, FILE *diagnostic)
{
    DSL_OPT_SELECTION_RESULT selection;
    UINT64 best_cost = ~(UINT64)0;
    memset(&selection, 0, sizeof(selection));
    selection.target_profile_id = target_profile_id;
    if (result != NULL)
        memset(result, 0, sizeof(*result));
    if (target_profile_id == 0 || !DSL_Opt_Plan_Verify(context, diagnostic))
        return FALSE;
    if (context->selection_complete) {
        if (context->selection.target_profile_id != target_profile_id)
            return DSL_Opt_Plan_Report
                       (diagnostic, "selection target changed", 0);
        if (result != NULL)
            *result = context->selection;
        return TRUE;
    }

    /*
     * Selection is deliberately boring: only proven plans with complete cost
     * evidence for this target compete. Candidate generation must preserve a
     * legal baseline instead of asking selection to repair an incomplete plan.
     */
    for (UINT32 i = 0; i < context->plans.size(); ++i) {
        const DSL_OPT_PLAN_RECORD &plan = context->plans[i];
        const DSL_OPT_COST_RECORD &cost = context->costs[plan.cost_id - 1];
        if (plan.legality != DSL_OPT_LEGALITY_PROVEN) {
            ++selection.rejected_plan_count;
            continue;
        }
        ++selection.legal_plan_count;
        if (cost.target_profile_id != target_profile_id) {
            ++selection.target_mismatch_count;
            continue;
        }
        if (!cost.complete) {
            ++selection.incomplete_cost_count;
            continue;
        }
        ++selection.complete_cost_count;
        if (selection.selected_plan_id == 0 || cost.total < best_cost) {
            selection.selected_plan_id = plan.id;
            best_cost = cost.total;
        }
    }
    if (selection.selected_plan_id == 0) {
        if (result != NULL)
            *result = selection;
        return DSL_Opt_Plan_Report(diagnostic, "no selectable plan", 0);
    }
    context->selection = selection;
    context->selection_complete = TRUE;
    if (!DSL_Opt_Plan_Verify(context, diagnostic)) {
        context->selection_complete = FALSE;
        memset(&context->selection, 0, sizeof(context->selection));
        return FALSE;
    }
    if (result != NULL)
        *result = selection;
    return TRUE;
}

static const char *
DSL_Opt_Plan_State_Name
        (const DSL_OPT_PLAN_CONTEXT *context,
         const DSL_OPT_PLAN_RECORD &plan)
{
    const DSL_OPT_COST_RECORD &cost = context->costs[plan.cost_id - 1];
    if (context->selection_complete &&
        context->selection.selected_plan_id == plan.id)
        return "selected";
    if (plan.legality != DSL_OPT_LEGALITY_PROVEN)
        return "legality_rejected";
    if (context->selection_complete &&
        cost.target_profile_id != context->selection.target_profile_id)
        return "target_mismatch";
    if (!cost.complete)
        return "cost_incomplete";
    return context->selection_complete ? "not_selected" : "selectable";
}

void
DSL_Opt_Plan_Print
        (FILE *file, const DSL_OPT_PLAN_CONTEXT *context)
{
    if (file == NULL || context == NULL)
        return;
    fprintf(file,
            "OptimizationPlanIR: owner=<%u,%u,%s> candidates=%u costs=%u "
            "plans=%u candidate_budget=%u plan_budget=%u\n",
            ST_IDX_level(context->owner_pu_st),
            ST_IDX_index(context->owner_pu_st),
            DSL_Opt_Plan_Owner_Valid(context->owner_pu_st) ?
                ST_name(St_Table[context->owner_pu_st]) : "<invalid>",
            (UINT32)context->candidates.size(),
            (UINT32)context->costs.size(),
            (UINT32)context->plans.size(),
            context->budget.max_candidates, context->budget.max_plans);
    for (UINT32 i = 0; i < context->candidates.size(); ++i) {
        const DSL_OPT_CANDIDATE_RECORD &candidate = context->candidates[i];
        fprintf(file,
                "  candidate[%u] kind=%s semantic_node=%u source=%u "
                "result=%u parent=%u legality=%s reason=%s order=%llu "
                "flags=0x%x\n",
                candidate.id, DSL_Opt_Candidate_Kind_Name(candidate.kind),
                candidate.semantic_node_id,
                candidate.source_evolution_node_id,
                candidate.result_evolution_node_id,
                candidate.parent_candidate_id,
                DSL_Opt_Legality_Name(candidate.legality),
                DSL_Opt_Rejection_Reason_Name(candidate.rejection_reason),
                (unsigned long long)candidate.ordering_key, candidate.flags);
    }
    for (UINT32 i = 0; i < context->costs.size(); ++i) {
        const DSL_OPT_COST_RECORD &cost = context->costs[i];
        fprintf(file,
                "  cost[%u] target=%u complete=%s total=",
                cost.id, cost.target_profile_id,
                cost.complete ? "true" : "false");
        if (cost.complete)
            fprintf(file, "%llu", (unsigned long long)cost.total);
        else
            fprintf(file, "<unknown>");
        fprintf(file, " confidence=%s order=%llu\n",
                DSL_Opt_Cost_Confidence_Name(cost.minimum_confidence),
                (unsigned long long)cost.ordering_key);
        for (UINT32 term = 0; term < DSL_OPT_COST_TERM_COUNT; ++term) {
            const DSL_OPT_COST_TERM &value = cost.terms[term];
            fprintf(file, "    %s=", DSL_Opt_Cost_Term_Name(term));
            if (value.unit == DSL_OPT_COST_UNIT_UNKNOWN)
                fprintf(file, "<unknown>\n");
            else
                fprintf(file, "%llu %s confidence=%s evidence=%s\n",
                        (unsigned long long)value.amount,
                        DSL_Opt_Cost_Unit_Name(value.unit),
                        DSL_Opt_Cost_Confidence_Name(value.confidence),
                        DSL_Opt_Cost_Evidence_Name(value.evidence));
        }
    }
    for (UINT32 i = 0; i < context->plans.size(); ++i) {
        const DSL_OPT_PLAN_RECORD &plan = context->plans[i];
        fprintf(file,
                "  plan[%u] members=", plan.id);
        for (UINT32 ordinal = 0; ordinal < plan.member_count; ++ordinal) {
            if (ordinal != 0)
                fputc(',', file);
            fprintf(file, "%u",
                    context->members[plan.first_member + ordinal].candidate_id);
        }
        fprintf(file,
                " cost=%u fallback=%u legality=%s reason=%s order=%llu "
                "flags=0x%x state=%s\n",
                plan.cost_id, plan.fallback_plan_id,
                DSL_Opt_Legality_Name(plan.legality),
                DSL_Opt_Rejection_Reason_Name(plan.rejection_reason),
                (unsigned long long)plan.ordering_key, plan.flags,
                DSL_Opt_Plan_State_Name(context, plan));
    }
    fprintf(file,
            "  selection target=%u selected_plan=%u legal=%u complete=%u "
            "incomplete=%u target_mismatch=%u rejected=%u\n",
            context->selection.target_profile_id,
            context->selection.selected_plan_id,
            context->selection.legal_plan_count,
            context->selection.complete_cost_count,
            context->selection.incomplete_cost_count,
            context->selection.target_mismatch_count,
            context->selection.rejected_plan_count);
}

UINT32
DSL_Opt_Plan_Candidate_Count (const DSL_OPT_PLAN_CONTEXT *context)
{
    return context == NULL ? 0 : context->candidates.size();
}

UINT32
DSL_Opt_Plan_Cost_Count (const DSL_OPT_PLAN_CONTEXT *context)
{
    return context == NULL ? 0 : context->costs.size();
}

UINT32
DSL_Opt_Plan_Plan_Count (const DSL_OPT_PLAN_CONTEXT *context)
{
    return context == NULL ? 0 : context->plans.size();
}

BOOL
DSL_Opt_Plan_Get_Candidate
        (const DSL_OPT_PLAN_CONTEXT *context, DSL_OPT_CANDIDATE_ID id,
         DSL_OPT_CANDIDATE_RECORD *record)
{
    if (context == NULL || record == NULL || id == 0 ||
        id > context->candidates.size())
        return FALSE;
    *record = context->candidates[id - 1];
    return TRUE;
}

BOOL
DSL_Opt_Plan_Get_Cost
        (const DSL_OPT_PLAN_CONTEXT *context, DSL_OPT_COST_ID id,
         DSL_OPT_COST_RECORD *record)
{
    if (context == NULL || record == NULL || id == 0 ||
        id > context->costs.size())
        return FALSE;
    *record = context->costs[id - 1];
    return TRUE;
}

BOOL
DSL_Opt_Plan_Get_Plan
        (const DSL_OPT_PLAN_CONTEXT *context, DSL_OPT_PLAN_ID id,
         DSL_OPT_PLAN_RECORD *record)
{
    if (context == NULL || record == NULL || id == 0 ||
        id > context->plans.size())
        return FALSE;
    *record = context->plans[id - 1];
    return TRUE;
}

BOOL
DSL_Opt_Plan_Get_Member
        (const DSL_OPT_PLAN_CONTEXT *context, DSL_OPT_PLAN_ID plan_id,
         UINT32 ordinal, DSL_OPT_PLAN_MEMBER_RECORD *record)
{
    if (context == NULL || record == NULL || plan_id == 0 ||
        plan_id > context->plans.size())
        return FALSE;
    const DSL_OPT_PLAN_RECORD &plan = context->plans[plan_id - 1];
    if (ordinal >= plan.member_count ||
        plan.first_member + ordinal >= context->members.size())
        return FALSE;
    *record = context->members[plan.first_member + ordinal];
    return TRUE;
}

BOOL
DSL_Opt_Plan_Candidate_Budget_Exhausted
        (const DSL_OPT_PLAN_CONTEXT *context)
{
    return context == NULL ? FALSE : context->candidate_budget_exhausted;
}

BOOL
DSL_Opt_Plan_Plan_Budget_Exhausted
        (const DSL_OPT_PLAN_CONTEXT *context)
{
    return context == NULL ? FALSE : context->plan_budget_exhausted;
}
