/* Driver-PU-local check-only shape propagation and refinement visitation.
 * VHO owns bounded fixed-point analysis; common/com retains ingestion-callable
 * local predicates/inference. No type construction, WHIRL mutation or .B change.
 * Design: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md. */
#ifndef dsl_shape_analysis_INCLUDED
#define dsl_shape_analysis_INCLUDED

#include "dsl_shape.h"

typedef struct pu_info PU_Info;
class WN;

typedef struct {
    UINT32 visited_node_count;
    UINT32 value_count;
    UINT32 unchanged_value_count;
    UINT32 refinable_value_count;
    UINT32 pending_value_count;
    UINT32 unresolved_value_count;
    UINT32 symbolic_value_count;
    UINT32 runtime_dynamic_value_count;
    UINT32 contradiction_count;
    UINT32 iteration_count;
    UINT32 diagnostic_count;
} DSL_SHAPE_SOLVER_RESULT;

typedef struct {
    DSL_IR_VALUE_ID value_id;
    TY_IDX expected_old_ty;
    DSL_SHAPE_FACT refined_fact;
    SRCPOS source_position;
} DSL_SHAPE_REFINEMENT;

/* Borrow each complete refinement for this invocation; return FALSE to stop.
 * The solver does not roll back earlier callback effects. A check-only client
 * must avoid changing compiler tables or WHIRL in the callback. */
typedef BOOL (*DSL_SHAPE_REFINEMENT_VISITOR)
    (const DSL_SHAPE_REFINEMENT *refinement, void *context);

/* Run check-only PU-local propagation with no refinement callback. Return
 * validation status and optional counts/diagnostics; no PU switching,
 * optimization choice, type construction or WHIRL rewrite. */
extern BOOL DSL_Shape_Analyze_PU (PU_Info *pu, WN *tree, FILE *diagnostic,
                                  DSL_SHAPE_SOLVER_RESULT *result);
/* Run the same bounded PU-local solver and visit complete changed facts only
 * after its pre-callback mutation checks pass. Visitor inputs are borrowed
 * for each call. Rejection stops further visits and returns FALSE; earlier
 * callback effects are not rolled back. */
extern BOOL DSL_Shape_Analyze_PU_With_Refinements
                                (PU_Info *pu,
                                 WN *tree,
                                 FILE *diagnostic,
                                 DSL_SHAPE_REFINEMENT_VISITOR visitor,
                                 void *visitor_context,
                                 DSL_SHAPE_SOLVER_RESULT *result);

#endif
