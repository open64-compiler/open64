/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO-owned PU-local tensor lifetime/locality records and query contract.
 * The analysis borrows tensor facts and the be/com copied-control snapshot;
 * the driver owns PU scope. Runtime records do not change binary .B layout.
 * Design: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md and
 *  doc/AI-COMPILER-OPTIMIZATION-AIO4-LIFETIME-LOCALITY.md.
 */

#ifndef dsl_tensor_locality_INCLUDED
#define dsl_tensor_locality_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_control_snapshot.h"

struct pu_info;
struct DSL_TENSOR_LOCALITY_ANALYSIS;

typedef struct DSL_TENSOR_LOCALITY_ANALYSIS DSL_TENSOR_LOCALITY_ANALYSIS;
typedef UINT32 DSL_TENSOR_LOCALITY_FACT_ID;
typedef UINT32 DSL_TENSOR_LOCALITY_USE_ID;

#define DSL_TENSOR_LOCALITY_INVALID_ID ((UINT32)0)
#define DSL_TENSOR_LOCALITY_UNKNOWN_U64 (~(UINT64)0)

typedef enum {
    DSL_TENSOR_SIZE_UNKNOWN = 0,
    DSL_TENSOR_SIZE_STATIC = 1,
    DSL_TENSOR_SIZE_SYMBOLIC = 2,
    DSL_TENSOR_SIZE_OVERFLOW = 3
} DSL_TENSOR_SIZE_STATE;

typedef enum {
    DSL_TENSOR_LIFETIME_UNKNOWN = 0,
    DSL_TENSOR_LIFETIME_EXACT_BLOCK = 1,
    DSL_TENSOR_LIFETIME_DOMINATED = 2,
    DSL_TENSOR_LIFETIME_BRANCH = 3,
    DSL_TENSOR_LIFETIME_LOOP = 4,
    DSL_TENSOR_LIFETIME_REGION = 5,
    DSL_TENSOR_LIFETIME_EFFECT = 6,
    DSL_TENSOR_LIFETIME_ALIAS = 7
} DSL_TENSOR_LIFETIME_STATE;

typedef enum {
    DSL_TENSOR_DISTANCE_UNKNOWN = 0,
    DSL_TENSOR_DISTANCE_EXACT = 1,
    DSL_TENSOR_DISTANCE_CONSERVATIVE = 2
} DSL_TENSOR_DISTANCE_STATE;

typedef enum {
    DSL_TENSOR_ACCESS_UNKNOWN = 0,
    DSL_TENSOR_ACCESS_ELEMENTWISE = 1,
    DSL_TENSOR_ACCESS_CONTRACTION = 2,
    DSL_TENSOR_ACCESS_REDUCTION = 3,
    DSL_TENSOR_ACCESS_VIEW = 4,
    DSL_TENSOR_ACCESS_INDEXED = 5,
    DSL_TENSOR_ACCESS_MIXED = 6
} DSL_TENSOR_ACCESS_PATTERN;

typedef enum {
    DSL_TENSOR_RESIDENCY_UNKNOWN = 0,
    DSL_TENSOR_RESIDENCY_NONE = 1,
    DSL_TENSOR_RESIDENCY_LOW = 2,
    DSL_TENSOR_RESIDENCY_MEDIUM = 3,
    DSL_TENSOR_RESIDENCY_HIGH = 4
} DSL_TENSOR_RESIDENCY_BENEFIT;

typedef enum {
    DSL_TENSOR_CRITICAL_PATH_UNKNOWN = 0,
    DSL_TENSOR_CRITICAL_PATH_OFF = 1,
    DSL_TENSOR_CRITICAL_PATH_ON = 2
} DSL_TENSOR_CRITICAL_PATH_STATE;

typedef enum {
    DSL_TENSOR_ALIAS_UNKNOWN = 0,
    DSL_TENSOR_ALIAS_CONSERVATIVE = 1,
    DSL_TENSOR_ALIAS_PROVEN_UNIQUE = 2
} DSL_TENSOR_ALIAS_STATE;

typedef struct {
    DSL_TENSOR_LOCALITY_FACT_ID id;
    DSL_TENSOR_FACT_ID tensor_fact_id;
    DSL_IR_VALUE_ID value_id;
    DSL_IR_NODE_ID producer_node_id;
    DSL_IR_NODE_ID last_consumer_node_id;
    DSL_TENSOR_LOCALITY_USE_ID first_use_id;
    UINT32 use_count;
    UINT32 size_state;
    UINT32 lifetime_state;
    UINT32 reuse_distance_state;
    UINT32 access_pattern;
    UINT32 residency_benefit;
    UINT32 critical_path_state;
    UINT32 alias_state;
    UINT32 producer_path_position;
    UINT32 last_use_path_position;
    UINT64 object_bytes;
    UINT64 reuse_distance_statements;
    UINT64 working_set_bytes;
    UINT64 estimated_read_bytes;
    UINT64 estimated_write_bytes;
    UINT32 flags;
    UINT32 reserved;
} DSL_TENSOR_LOCALITY_FACT_RECORD;

typedef struct {
    DSL_TENSOR_LOCALITY_USE_ID id;
    DSL_TENSOR_LOCALITY_FACT_ID locality_fact_id;
    DSL_TENSOR_USE_FACT_ID tensor_use_fact_id;
    DSL_IR_NODE_ID consumer_node_id;
    UINT32 block_id;
    UINT32 statement_order;
    UINT32 path_position;
    UINT32 loop_depth;
    UINT32 region_id;
    UINT32 control_flags;
    UINT32 reserved;
} DSL_TENSOR_LOCALITY_USE_RECORD;

/*
 * Allocate empty caller-owned locality storage for the active PU, borrowing
 * verified tensor facts and a sealed, verified snapshot for that PU. Both inputs
 * must outlive the result; return NULL with diagnostics on rejected input.
 * This allocation derives no locality facts and changes no WHIRL.
 */
extern DSL_TENSOR_LOCALITY_ANALYSIS *DSL_tensor_locality_create
                                (struct pu_info *pu,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
                                 FILE *diagnostic);
/*
 * Release owned locality vectors; NULL is allowed. Leave the borrowed tensor
 * analysis, control snapshot, driver PU, and WHIRL intact.
 */
extern void DSL_tensor_locality_destroy
                                (DSL_TENSOR_LOCALITY_ANALYSIS *analysis);
/*
 * Read-check active-PU inputs, contiguous fact/use IDs, tensor joins, enum
 * bounds, and unknown sentinels. Return FALSE with optional diagnostics at the
 * first inconsistency; neither repair facts nor establish CFG freshness.
 */
extern BOOL DSL_tensor_locality_verify
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
                                 FILE *diagnostic);
/*
 * Print stored facts and uses, rendering unknown quantities explicitly.
 * Ignore NULL arguments; do not require active-PU validation or mutate inputs.
 * The retained symbol tables must remain readable for the owner label.
 */
extern void DSL_tensor_locality_print
                                (FILE *file,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *analysis);
/*
 * Return the stored fact-vector size, or zero for NULL, without validating
 * the active PU or changing the borrowed analysis.
 */
extern UINT32 DSL_tensor_locality_fact_count
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis);
/*
 * Return the stored use-vector size, or zero for NULL, without validating
 * the active PU or changing the borrowed analysis.
 */
extern UINT32 DSL_tensor_locality_use_count
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis);
/*
 * Copy a one-based stored fact to caller-owned output. Reject NULL inputs
 * or an out-of-range ID without changing output; do not reverify PU activity.
 */
extern BOOL DSL_tensor_locality_get_fact
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
                                 DSL_TENSOR_LOCALITY_FACT_ID id,
                                 DSL_TENSOR_LOCALITY_FACT_RECORD *record);
/*
 * Copy a one-based stored use to caller-owned output. Reject NULL inputs
 * or an out-of-range ID without changing output; do not reverify PU activity.
 */
extern BOOL DSL_tensor_locality_get_use
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
                                 DSL_TENSOR_LOCALITY_USE_ID id,
                                 DSL_TENSOR_LOCALITY_USE_RECORD *record);
/*
 * Find the first stored fact for a nonzero value ID, optionally copying it.
 * Return FALSE for NULL analysis or a missing/invalid value without changing
 * output; allow NULL output for a membership query and do not reverify activity.
 */
extern BOOL DSL_tensor_locality_find_fact
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
                                 DSL_IR_VALUE_ID value_id,
                                 DSL_TENSOR_LOCALITY_FACT_RECORD *record);

/*
 * Return a borrowed static size-state spelling; out-of-range values use
 * "unknown" without changing analysis state.
 */
extern const char *DSL_tensor_size_state_name (UINT32 state);
/*
 * Return a borrowed static lifetime spelling; out-of-range values use
 * "unknown" without changing analysis state.
 */
extern const char *DSL_tensor_lifetime_state_name (UINT32 state);
/*
 * Return a borrowed static distance spelling; out-of-range values use
 * "unknown" without changing analysis state.
 */
extern const char *DSL_tensor_distance_state_name (UINT32 state);
/*
 * Return a borrowed static access-pattern spelling; out-of-range values use
 * "unknown" without changing analysis state.
 */
extern const char *DSL_tensor_access_pattern_name (UINT32 pattern);
/*
 * Return a borrowed static residency-seed spelling; out-of-range values use
 * "unknown" without selecting placement or changing analysis state.
 */
extern const char *DSL_tensor_residency_benefit_name (UINT32 benefit);
/*
 * Return a borrowed static critical-path spelling; out-of-range values use
 * "unknown" without predicting latency or changing analysis state.
 */
extern const char *DSL_tensor_critical_path_state_name (UINT32 state);
/*
 * Return a borrowed static alias-state spelling; out-of-range values use
 * "unknown" without changing ownership or analysis state.
 */
extern const char *DSL_tensor_alias_state_name (UINT32 state);

#endif /* dsl_tensor_locality_INCLUDED */
