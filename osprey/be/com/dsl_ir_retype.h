/*
 * Backend-free atomic active-PU shape-only retyping. VHO owns inference
 * and generation invalidation; this mechanism applies reviewed requests.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef DSL_IR_RETYPE_INCLUDED
#define DSL_IR_RETYPE_INCLUDED

#include <stdio.h>
#include "../../common/com/dsl_ir_image.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

/*
 * Runtime-only, active-PU shape refinement request. The complete request
 * array is preflighted and committed atomically; no mapped-image layout is
 * added or changed.
 */
typedef struct {
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID value_id;
    TY_IDX expected_old_ty;
    TY_IDX refined_ty;
} DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST;

typedef struct {
    UINT32 request_count;
    UINT32 updated_st_count;
    UINT32 updated_wn_count;
    UINT32 updated_value_count;
    UINT32 rollback_count;
} DSL_IR_VALUE_TYPE_REFINEMENT_RESULT;

/* Atomically apply a nonempty borrowed array of monotonic shape-only tensor
 * refinements to the exact active Current_PU_Info without editing canonical TY
 * records. Compare element type, kind, dtype, traits, layout, sharding,
 * placement, memory, quantization and alignment, preserving rank and known
 * dimensions; this does not compare every descriptor field, including runtime
 * state and lineage. Preflight distinct value/ST
 * requests, pure unique local definitions/uses and REGION/ABI/auxiliary-image
 * exclusions before changing WN, ST and logical value TYs. Postcheck image,
 * REGION and strict gatekeeper contracts; failure reverses those type journals
 * and asserts restored invariants. Optional result is zeroed on entry; postcheck
 * FALSE reports attempted update/rollback counts, not successful refinement.
 * Caller retains inputs/types; no source conversion, PU activation or general
 * allocation-exception rollback. NULL diagnostic is silent; zero requests reject. */
extern BOOL DSL_IR_Refine_Native_Value_Types
                                (PU_Info *pu_info,
                                 const DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 FILE *diagnostic,
                                 DSL_IR_VALUE_TYPE_REFINEMENT_RESULT *result);

#endif /* DSL_IR_RETYPE_INCLUDED */
