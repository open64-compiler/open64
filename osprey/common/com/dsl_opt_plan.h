/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-2 PU-local candidate, legality, cost, fallback, and plan-selection
 * service. Its records are runtime-only and do not change WHIRL layout.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO2-PLAN-COST.md.
 */

#ifndef dsl_opt_plan_INCLUDED
#define dsl_opt_plan_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_ir_image.h"
#include "dsl_tensor_evolution.h"
#include "symtab.h"

struct pu_info;
struct DSL_OPT_PLAN_CONTEXT;

typedef struct DSL_OPT_PLAN_CONTEXT DSL_OPT_PLAN_CONTEXT;
typedef UINT32 DSL_OPT_CANDIDATE_ID;
typedef UINT32 DSL_OPT_COST_ID;
typedef UINT32 DSL_OPT_PLAN_ID;

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
    DSL_OPT_REJECT_MALFORMED = 10
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

extern DSL_OPT_PLAN_CONTEXT *DSL_opt_plan_create
                                (struct pu_info *pu,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_OPT_PLAN_BUDGET *budget,
                                 FILE *diagnostic);
extern void DSL_opt_plan_destroy (DSL_OPT_PLAN_CONTEXT *context);
extern BOOL DSL_opt_plan_add_candidate
                                (DSL_OPT_PLAN_CONTEXT *context,
                                 const DSL_OPT_CANDIDATE_INPUT *input,
                                 DSL_OPT_CANDIDATE_ID *candidate_id,
                                 FILE *diagnostic);
extern BOOL DSL_opt_plan_add_cost
                                (DSL_OPT_PLAN_CONTEXT *context,
                                 const DSL_OPT_COST_INPUT *input,
                                 DSL_OPT_COST_ID *cost_id,
                                 FILE *diagnostic);
extern BOOL DSL_opt_plan_add_plan
                                (DSL_OPT_PLAN_CONTEXT *context,
                                 const DSL_OPT_PLAN_INPUT *input,
                                 DSL_OPT_PLAN_ID *plan_id,
                                 FILE *diagnostic);
extern BOOL DSL_opt_plan_select
                                (DSL_OPT_PLAN_CONTEXT *context,
                                 UINT32 target_profile_id,
                                 DSL_OPT_SELECTION_RESULT *result,
                                 FILE *diagnostic);
extern BOOL DSL_opt_plan_verify
                                (const DSL_OPT_PLAN_CONTEXT *context,
                                 FILE *diagnostic);
extern void DSL_opt_plan_print
                                (FILE *file,
                                 const DSL_OPT_PLAN_CONTEXT *context);
extern UINT32 DSL_opt_plan_candidate_count
                                (const DSL_OPT_PLAN_CONTEXT *context);
extern UINT32 DSL_opt_plan_cost_count
                                (const DSL_OPT_PLAN_CONTEXT *context);
extern UINT32 DSL_opt_plan_plan_count
                                (const DSL_OPT_PLAN_CONTEXT *context);
extern BOOL DSL_opt_plan_get_candidate
                                (const DSL_OPT_PLAN_CONTEXT *context,
                                 DSL_OPT_CANDIDATE_ID id,
                                 DSL_OPT_CANDIDATE_RECORD *record);
extern BOOL DSL_opt_plan_get_cost
                                (const DSL_OPT_PLAN_CONTEXT *context,
                                 DSL_OPT_COST_ID id,
                                 DSL_OPT_COST_RECORD *record);
extern BOOL DSL_opt_plan_get_plan
                                (const DSL_OPT_PLAN_CONTEXT *context,
                                 DSL_OPT_PLAN_ID id,
                                 DSL_OPT_PLAN_RECORD *record);
extern BOOL DSL_opt_plan_get_member
                                (const DSL_OPT_PLAN_CONTEXT *context,
                                 DSL_OPT_PLAN_ID plan_id,
                                 UINT32 ordinal,
                                 DSL_OPT_PLAN_MEMBER_RECORD *record);
extern BOOL DSL_opt_plan_candidate_budget_exhausted
                                (const DSL_OPT_PLAN_CONTEXT *context);
extern BOOL DSL_opt_plan_plan_budget_exhausted
                                (const DSL_OPT_PLAN_CONTEXT *context);
extern const char *DSL_opt_candidate_kind_name (UINT32 kind);
extern const char *DSL_opt_legality_name (UINT32 legality);
extern const char *DSL_opt_rejection_reason_name (UINT32 reason);
extern const char *DSL_opt_cost_term_name (UINT32 term);
extern const char *DSL_opt_cost_unit_name (UINT32 unit);
extern const char *DSL_opt_cost_confidence_name (UINT32 confidence);
extern const char *DSL_opt_cost_evidence_name (UINT32 evidence);

#endif /* dsl_opt_plan_INCLUDED */
