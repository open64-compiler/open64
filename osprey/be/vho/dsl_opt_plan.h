/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO-owned AIO-2 runtime candidate, cost, plan and recorded-selection contract
 * for one driver-active PU. Contexts borrow their graph and PU; plan IDs retain
 * the shared identity vocabulary. Recording validates a phase's choice without
 * choosing a winner. These records change no .B rows, sections or layout.
 * Ownership: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 * Plan semantics: doc/AI-COMPILER-OPTIMIZATION-AIO2-PLAN-COST.md.
 */

#ifndef dsl_opt_plan_INCLUDED
#define dsl_opt_plan_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_plan_identity.h"
#include "dsl_ir_image.h"
#include "dsl_tensor_evolution.h"
#include "symtab.h"

struct pu_info;
struct DSL_OPT_PLAN_CONTEXT;

typedef struct DSL_OPT_PLAN_CONTEXT DSL_OPT_PLAN_CONTEXT;
typedef UINT32 DSL_OPT_CANDIDATE_ID;
typedef UINT32 DSL_OPT_COST_ID;

#define DSL_OPT_CANDIDATE_INVALID_ID ((UINT32)0)
#define DSL_OPT_COST_INVALID_ID ((UINT32)0)
#define DSL_OPT_PLAN_INVALID_ID ((UINT32)0)

typedef enum {
    DSL_OPT_CANDIDATE_UNKNOWN = 0,
    DSL_OPT_CANDIDATE_BASELINE = 1,
    DSL_OPT_CANDIDATE_TILE = 2,
    DSL_OPT_CANDIDATE_FUSION = 3,
    DSL_OPT_CANDIDATE_LAYOUT = 4,
    DSL_OPT_CANDIDATE_PLACEMENT = 5,
    DSL_OPT_CANDIDATE_SHARDING = 6,
    DSL_OPT_CANDIDATE_RESIDENCY = 7,
    DSL_OPT_CANDIDATE_PIPELINE = 8,
    DSL_OPT_CANDIDATE_KERNEL = 9,
    DSL_OPT_CANDIDATE_RUNTIME_VARIANT = 10
} DSL_OPT_CANDIDATE_KIND;

typedef enum {
    DSL_OPT_LEGALITY_UNKNOWN = 0,
    DSL_OPT_LEGALITY_PROVEN = 1,
    DSL_OPT_LEGALITY_REJECTED = 2
} DSL_OPT_LEGALITY_STATE;

typedef enum {
    DSL_OPT_REJECT_NONE = 0,
    DSL_OPT_REJECT_INCOMPLETE_ANALYSIS = 1,
    DSL_OPT_REJECT_INVALID_REFERENCE = 2,
    DSL_OPT_REJECT_OWNERSHIP = 3,
    DSL_OPT_REJECT_DESCRIPTOR = 4,
    DSL_OPT_REJECT_EFFECT = 5,
    DSL_OPT_REJECT_RESOURCE = 6,
    DSL_OPT_REJECT_COST_INCOMPLETE = 7,
    DSL_OPT_REJECT_TARGET_MISMATCH = 8,
    DSL_OPT_REJECT_BUDGET_EXHAUSTED = 9,
    DSL_OPT_REJECT_MALFORMED = 10,
    DSL_OPT_REJECT_PROVIDER_MISMATCH = 11,
    DSL_OPT_REJECT_PROVIDER_UNAVAILABLE = 12
} DSL_OPT_REJECTION_REASON;

typedef enum {
    DSL_OPT_COST_COMPUTE = 0,
    DSL_OPT_COST_MEMORY_UNHIDDEN = 1,
    DSL_OPT_COST_COMMUNICATION_UNHIDDEN = 2,
    DSL_OPT_COST_SYNC = 3,
    DSL_OPT_COST_LAUNCH = 4,
    DSL_OPT_COST_RUNTIME_SELECTION = 5,
    DSL_OPT_COST_TERM_COUNT = 6
} DSL_OPT_COST_TERM_KIND;

typedef enum {
    DSL_OPT_COST_UNIT_UNKNOWN = 0,
    DSL_OPT_COST_UNIT_RELATIVE = 1,
    DSL_OPT_COST_UNIT_CYCLES = 2,
    DSL_OPT_COST_UNIT_NANOSECONDS = 3
} DSL_OPT_COST_UNIT;

typedef enum {
    DSL_OPT_COST_CONFIDENCE_UNKNOWN = 0,
    DSL_OPT_COST_CONFIDENCE_LOW = 1,
    DSL_OPT_COST_CONFIDENCE_MEDIUM = 2,
    DSL_OPT_COST_CONFIDENCE_HIGH = 3,
    DSL_OPT_COST_CONFIDENCE_EXACT = 4
} DSL_OPT_COST_CONFIDENCE;

typedef enum {
    DSL_OPT_COST_EVIDENCE_UNKNOWN = 0,
    DSL_OPT_COST_EVIDENCE_BASELINE_POLICY = 1,
    DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS = 2,
    DSL_OPT_COST_EVIDENCE_TARGET_MODEL = 3,
    DSL_OPT_COST_EVIDENCE_MEASURED = 4,
    DSL_OPT_COST_EVIDENCE_TELEMETRY = 5
} DSL_OPT_COST_EVIDENCE;

enum {
    DSL_OPT_CANDIDATE_FLAG_NONE = 0,
    DSL_OPT_CANDIDATE_FLAG_BASELINE = 0x00000001,
    DSL_OPT_CANDIDATE_FLAG_PROVISIONAL = 0x00000002
};

enum {
    DSL_OPT_PLAN_FLAG_NONE = 0,
    DSL_OPT_PLAN_FLAG_BASELINE = 0x00000001,
    DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY = 0x00000002
};

typedef struct {
    UINT32 max_candidates;
    UINT32 max_plans;
} DSL_OPT_PLAN_BUDGET;

typedef struct {
    UINT32 kind;
    DSL_IR_NODE_ID semantic_node_id;
    DSL_TENSOR_EVOLUTION_NODE_ID source_evolution_node_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_evolution_node_id;
    DSL_OPT_CANDIDATE_ID parent_candidate_id;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT64 ordering_key;
    UINT32 flags;
} DSL_OPT_CANDIDATE_INPUT;

typedef struct {
    DSL_OPT_CANDIDATE_ID id;
    UINT32 kind;
    ST_IDX owner_pu_st;
    DSL_IR_NODE_ID semantic_node_id;
    DSL_TENSOR_EVOLUTION_NODE_ID source_evolution_node_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_evolution_node_id;
    DSL_OPT_CANDIDATE_ID parent_candidate_id;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT64 ordering_key;
    UINT32 flags;
} DSL_OPT_CANDIDATE_RECORD;

typedef struct {
    UINT64 amount;
    UINT32 unit;
    UINT32 confidence;
    UINT32 evidence;
    UINT32 reserved;
} DSL_OPT_COST_TERM;

typedef struct {
    UINT32 target_profile_id;
    UINT64 ordering_key;
    DSL_OPT_COST_TERM terms[DSL_OPT_COST_TERM_COUNT];
} DSL_OPT_COST_INPUT;

typedef struct {
    DSL_OPT_COST_ID id;
    ST_IDX owner_pu_st;
    UINT32 target_profile_id;
    UINT64 ordering_key;
    DSL_OPT_COST_TERM terms[DSL_OPT_COST_TERM_COUNT];
    UINT64 total;
    UINT32 complete;
    UINT32 minimum_confidence;
} DSL_OPT_COST_RECORD;

typedef struct {
    const DSL_OPT_CANDIDATE_ID *candidate_ids;
    UINT32 candidate_count;
    DSL_OPT_COST_ID cost_id;
    DSL_OPT_PLAN_ID fallback_plan_id;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT64 ordering_key;
    UINT32 flags;
} DSL_OPT_PLAN_INPUT;

typedef struct {
    DSL_OPT_PLAN_ID id;
    ST_IDX owner_pu_st;
    UINT32 first_member;
    UINT32 member_count;
    DSL_OPT_COST_ID cost_id;
    DSL_OPT_PLAN_ID fallback_plan_id;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT64 ordering_key;
    UINT32 flags;
} DSL_OPT_PLAN_RECORD;

typedef struct {
    DSL_OPT_PLAN_ID plan_id;
    DSL_OPT_CANDIDATE_ID candidate_id;
    UINT32 ordinal;
    UINT32 reserved;
} DSL_OPT_PLAN_MEMBER_RECORD;

typedef struct {
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 target_profile_id;
    UINT32 legal_plan_count;
    UINT32 complete_cost_count;
    UINT32 incomplete_cost_count;
    UINT32 target_mismatch_count;
    UINT32 rejected_plan_count;
} DSL_OPT_SELECTION_RESULT;

/*
 * Create a caller-owned VHO context for an active PU and verified same-owner
 * graph, borrowing both until destruction. Copy nonzero candidate/plan budgets;
 * return NULL on preflight failure without changing PU, graph or WHIRL. The
 * graph and referenced tables/image must outlive the context.
 */
extern DSL_OPT_PLAN_CONTEXT *DSL_opt_plan_create
                                (struct pu_info *pu,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_OPT_PLAN_BUDGET *budget,
                                 FILE *diagnostic);
/*
 * Release only context-owned records and handle; NULL is allowed. Keep the
 * borrowed graph, PU and referenced image/type tables alive for other consumers.
 */
extern void DSL_opt_plan_destroy (DSL_OPT_PLAN_CONTEXT *context);
/*
 * In the active owning PU before selection, append one candidate with the next
 * one-based ID. Require baseline first, increasing ordering keys, valid parent,
 * semantic/evolution joins and legality/reason pair. Reset optional output;
 * validation failure preserves rows, but budget exhaustion sets a sticky flag.
 * No whole-context post-verification or allocation rollback is provided.
 */
extern BOOL DSL_opt_plan_add_candidate
                                (DSL_OPT_PLAN_CONTEXT *context,
                                 const DSL_OPT_CANDIDATE_INPUT *input,
                                 DSL_OPT_CANDIDATE_ID *candidate_id,
                                 FILE *diagnostic);
/*
 * In the active owning PU before selection, append copied cost terms with a
 * nonzero target and increasing key; costs are bounded by max_plans. Reset
 * optional output. Malformed tuples, mixed known units or overflow fail before
 * insertion; unknown terms yield incomplete cost, not a zero estimate. No budget
 * flag or allocation rollback is added by this operation.
 */
extern BOOL DSL_opt_plan_add_cost
                                (DSL_OPT_PLAN_CONTEXT *context,
                                 const DSL_OPT_COST_INPUT *input,
                                 DSL_OPT_COST_ID *cost_id,
                                 FILE *diagnostic);
/*
 * In the active owning PU before selection, copy a nonempty increasing list of
 * candidate IDs into a new plan. Require valid cost/legality, baseline first and
 * an earlier proven complete same-target fallback thereafter. Reset optional
 * output; preflight failures preserve rows except a sticky plan-budget flag.
 * Member and plan appends have no post-verification or allocation rollback.
 */
extern BOOL DSL_opt_plan_add_plan
                                (DSL_OPT_PLAN_CONTEXT *context,
                                 const DSL_OPT_PLAN_INPUT *input,
                                 DSL_OPT_PLAN_ID *plan_id,
                                 FILE *diagnostic);
/*
 * Record a phase-chosen selectable plan and exact plan census after verifying
 * the active context. This does not choose minimum cost or implement policy.
 * An identical existing record is idempotent; a different one fails. Success
 * freezes further appends; post-verification failure clears only the new record.
 */
extern BOOL DSL_opt_plan_record_selection
                                (DSL_OPT_PLAN_CONTEXT *context,
                                 const DSL_OPT_SELECTION_RESULT *result,
                                 FILE *diagnostic);
/*
 * Copy the recorded selection only when context/output are non-NULL and a
 * selection exists. Failure preserves output; no PU or cost-policy validation
 * is performed by this read.
 */
extern BOOL DSL_opt_plan_get_selection
                                (const DSL_OPT_PLAN_CONTEXT *context,
                                 DSL_OPT_SELECTION_RESULT *result);
/*
 * Read-only verification requires the driver-active PU and borrowed graph and
 * image tables to remain live. Check budgets, contiguous rows, ordering, joins,
 * legality, costs, fallback/member structure and any recorded selection census;
 * diagnose failures without repairing rows or applying selection policy.
 */
extern BOOL DSL_opt_plan_verify
                                (const DSL_OPT_PLAN_CONTEXT *context,
                                 FILE *diagnostic);
/*
 * Print runtime plan evidence from structurally valid records and live borrowed
 * tables; NULL file/context is a no-op. This does not verify or activate the PU,
 * choose a plan, change records or write mapped-image state.
 */
extern void DSL_opt_plan_print
                                (FILE *file,
                                 const DSL_OPT_PLAN_CONTEXT *context);
/*
 * Read the stored candidate count without PU validation or mutation; NULL
 * context yields zero.
 */
extern UINT32 DSL_opt_plan_candidate_count
                                (const DSL_OPT_PLAN_CONTEXT *context);
/*
 * Read the stored cost count without recalculation, PU validation or mutation;
 * NULL context yields zero.
 */
extern UINT32 DSL_opt_plan_cost_count
                                (const DSL_OPT_PLAN_CONTEXT *context);
/*
 * Read the stored plan count without selection or PU validation; NULL context
 * yields zero and existing records remain unchanged.
 */
extern UINT32 DSL_opt_plan_plan_count
                                (const DSL_OPT_PLAN_CONTEXT *context);
/*
 * Copy a candidate by one-based context-local ID. Require non-NULL context,
 * output and an in-range ID; failure preserves output, without PU verification.
 */
extern BOOL DSL_opt_plan_get_candidate
                                (const DSL_OPT_PLAN_CONTEXT *context,
                                 DSL_OPT_CANDIDATE_ID id,
                                 DSL_OPT_CANDIDATE_RECORD *record);
/*
 * Copy a stored cost by one-based context-local ID without recalculation.
 * Require non-NULL context/output and an in-range ID; failure preserves output.
 */
extern BOOL DSL_opt_plan_get_cost
                                (const DSL_OPT_PLAN_CONTEXT *context,
                                 DSL_OPT_COST_ID id,
                                 DSL_OPT_COST_RECORD *record);
/*
 * Copy a plan by one-based context-local ID without policy or PU checks.
 * Require non-NULL context/output and an in-range ID; failure preserves output.
 */
extern BOOL DSL_opt_plan_get_plan
                                (const DSL_OPT_PLAN_CONTEXT *context,
                                 DSL_OPT_PLAN_ID id,
                                 DSL_OPT_PLAN_RECORD *record);
/*
 * Copy a plan member using a one-based plan ID and zero-based ordinal; require
 * valid context/output and a stored in-range member span. Failure preserves
 * output; this does not verify the owning PU or candidate membership semantics.
 */
extern BOOL DSL_opt_plan_get_member
                                (const DSL_OPT_PLAN_CONTEXT *context,
                                 DSL_OPT_PLAN_ID plan_id,
                                 UINT32 ordinal,
                                 DSL_OPT_PLAN_MEMBER_RECORD *record);
/*
 * Read the sticky candidate-budget rejection flag, not remaining capacity.
 * NULL context yields FALSE; no ownership validation or mutation occurs.
 */
extern BOOL DSL_opt_plan_candidate_budget_exhausted
                                (const DSL_OPT_PLAN_CONTEXT *context);
/*
 * Read the sticky plan-budget rejection flag; cost-budget rejection does not
 * set it. NULL context yields FALSE, without ownership validation or mutation.
 */
extern BOOL DSL_opt_plan_plan_budget_exhausted
                                (const DSL_OPT_PLAN_CONTEXT *context);
/*
 * Return a borrowed static candidate-kind name or "unknown" for out-of-range
 * input; no context, allocation or mutation is involved.
 */
extern const char *DSL_opt_candidate_kind_name (UINT32 kind);
/*
 * Return a borrowed static legality spelling or "unknown" for out-of-range
 * input; this does not prove the legality of any candidate or plan.
 */
extern const char *DSL_opt_legality_name (UINT32 legality);
/*
 * Return a borrowed static rejection-reason spelling or "unknown" for
 * out-of-range input; no context or mutation is required.
 */
extern const char *DSL_opt_rejection_reason_name (UINT32 reason);
/*
 * Return a borrowed static name for one of the fixed cost terms, otherwise
 * "unknown"; do not inspect or calculate a cost.
 */
extern const char *DSL_opt_cost_term_name (UINT32 term);
/*
 * Return a borrowed static cost-unit spelling or "unknown" for out-of-range
 * input; do not convert units or change cost records.
 */
extern const char *DSL_opt_cost_unit_name (UINT32 unit);
/*
 * Return a borrowed static confidence spelling or "unknown" for out-of-range
 * input; do not infer confidence or change evidence.
 */
extern const char *DSL_opt_cost_confidence_name (UINT32 confidence);
/*
 * Return a borrowed static evidence spelling or "unknown" for out-of-range
 * input; do not query measurements or modify records.
 */
extern const char *DSL_opt_cost_evidence_name (UINT32 evidence);

#endif /* dsl_opt_plan_INCLUDED */
