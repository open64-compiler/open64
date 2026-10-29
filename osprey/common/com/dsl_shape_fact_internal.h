/* Private local-fact operations shared by common ingestion-time inference and
 * the VHO PU solver. Internal-linkage helpers have one reviewed source, no
 * shared mutable state and no optimization/WHIRL-image mutation behavior.
 * Facts are caller-owned transient scratch, not new .B records. All helpers
 * require non-NULL pointers, ranks -1 or 0..DSL_SHAPE_MAX_RANK, bounded ordinals
 * and terminated canonical text with matching numeric caches. No allocation,
 * active-PU switching or access to compiler tables is performed.
 * Design: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md. */
#ifndef dsl_shape_fact_internal_INCLUDED
#define dsl_shape_fact_internal_INCLUDED

#include <string.h>
#include "dsl_shape.h"

/* Classify caller-owned fact state from its rank and dimension-kind array.
 * Preserve contradiction; missing rank/dimensions stays pending. The caller
 * supplies bounded arrays. No compiler table, type or IR mutation. */
static void
DSL_Shape_Fact_Classify (DSL_SHAPE_FACT *fact)
{
    if (fact->state == DSL_SHAPE_FACT_CONTRADICTION)
        return;
    if (fact->rank < 0) {
        fact->state = DSL_SHAPE_FACT_PENDING;
        return;
    }
    for (INT32 i = 0; i < fact->rank; ++i) {
        if (fact->dimension_kind[i] == DSL_SHAPE_DIMENSION_PENDING) {
            fact->state = DSL_SHAPE_FACT_PENDING;
            return;
        }
    }
    fact->state = DSL_SHAPE_FACT_COMPLETE;
}

/* Compare borrowed fact state, rank and canonical dimension kind/text only.
 * Negative equal ranks compare equal; numeric caches are not independently
 * compared. Read-only local-fact equality, not tensor type equivalence. */
static BOOL
DSL_Shape_Facts_Equal
        (const DSL_SHAPE_FACT &left,
         const DSL_SHAPE_FACT &right)
{
    if (left.state != right.state || left.rank != right.rank)
        return FALSE;
    if (left.rank < 0)
        return TRUE;
    for (INT32 i = 0; i < left.rank; ++i) {
        if (left.dimension_kind[i] != right.dimension_kind[i] ||
            strcmp(left.dimension_text[i],
                   right.dimension_text[i]) != 0)
            return FALSE;
    }
    return TRUE;
}

/* Merge compatible rank/dimension evidence into the caller's scratch fact.
 * Mark contradiction and return FALSE on incompatible evidence; prior field
 * updates may remain. Reclassify known-rank success; if both ranks stay unknown,
 * return without reclassification. The caller provides consistent pending
 * slots even in an unknown-rank source and distinct source/target storage.
 * Copy only nonpending source axes; no compiler tables or WHIRL are changed. */
static BOOL
DSL_Shape_Merge_Equal_Fact
        (DSL_SHAPE_FACT *target,
         const DSL_SHAPE_FACT &source)
{
    if (target->state == DSL_SHAPE_FACT_CONTRADICTION ||
        source.state == DSL_SHAPE_FACT_CONTRADICTION) {
        target->state = DSL_SHAPE_FACT_CONTRADICTION;
        return FALSE;
    }
    if (target->rank < 0)
        target->rank = source.rank;
    else if (source.rank >= 0 && target->rank != source.rank) {
        target->state = DSL_SHAPE_FACT_CONTRADICTION;
        return FALSE;
    }
    if (target->rank < 0)
        return TRUE;
    for (INT32 i = 0; i < target->rank; ++i) {
        if (source.dimension_kind[i] == DSL_SHAPE_DIMENSION_PENDING)
            continue;
        if (target->dimension_kind[i] != DSL_SHAPE_DIMENSION_PENDING &&
            (target->dimension_kind[i] != source.dimension_kind[i] ||
             strcmp(target->dimension_text[i],
                    source.dimension_text[i]) != 0)) {
            target->state = DSL_SHAPE_FACT_CONTRADICTION;
            return FALSE;
        }
        target->dimension_kind[i] = source.dimension_kind[i];
        target->dimension_known[i] = source.dimension_known[i];
        target->dimension[i] = source.dimension[i];
        strcpy(target->dimension_text[i], source.dimension_text[i]);
    }
    DSL_Shape_Fact_Classify(target);
    return TRUE;
}

#endif
