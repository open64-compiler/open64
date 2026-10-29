/*
 * Backend-free, active-PU runtime ABI projection transactions. Callers
 * supply reviewed plans and active locals; this service never chooses policy.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef DSL_RUNTIME_INTERFACE_INCLUDED
#define DSL_RUNTIME_INTERFACE_INCLUDED

#include <stdio.h>
#include "../../common/com/dsl_ir_image.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

typedef struct {
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    ST_IDX expected_source_st;
    TY_IDX expected_source_ty;
    TY_IDX handle_ty;
    UINT32 binding_kind;
    UINT32 formal_ordinal;
} DSL_RUNTIME_VALUE_PROJECTION_REQUEST;

typedef struct {
    ST_IDX owner_pu_st;
    DSL_CALLSITE_METADATA_ID callsite_id;
    DSL_IR_VALUE_ID source_value_id;
    UINT32 actual_ordinal;
    UINT32 callee_formal_ordinal;
    UINT32 direction;
} DSL_RUNTIME_CALL_PROJECTION_REQUEST;

typedef struct {
    const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *values;
    UINT32 value_count;
    const DSL_RUNTIME_CALL_PROJECTION_REQUEST *calls;
    UINT32 call_count;
} DSL_RUNTIME_INTERFACE_PLAN;

typedef struct {
    UINT32 value_projection_count;
    UINT32 call_projection_count;
    UINT32 rebuilt_formal_count;
    UINT32 rewritten_call_count;
    UINT32 rewritten_return_count;
} DSL_RUNTIME_INTERFACE_RESULT;

/* Read a complete borrowed projection plan against canonical shared images:
 * check owner/value/type identities, formal/call direction and exhaustive
 * canonical formal/argument/callee-slot coverage across the supplied plan.
 * Require readable count-sized arrays/strings and initialized shared tables;
 * no inactive local symbols or physical PU trees are loaded/validated here.
 * FALSE reports the first mismatch (NULL diagnostic is silent); no plan, WHIRL,
 * symbol or image writes, PU activation or commit-authorization cache. */
extern BOOL DSL_Runtime_Interface_Plan_Validate
                                (const DSL_RUNTIME_INTERFACE_PLAN *plan,
                                 FILE *diagnostic);

/* Zero caller-owned projection/ABI rewrite counters or ignore NULL; no image
 * reset, projection creation or transaction/savepoint is established. */
extern void DSL_Runtime_Interface_Result_Init
                                (DSL_RUNTIME_INTERFACE_RESULT *result);

/* Apply the complete projection plan only to this already-active PU; caller
 * owns local symtab/map/pool lifetime and coordination of other PUs. Require
 * nonnull result, stable arrays and initialized shared images/types. Clear
 * result first, then global-plan and physical-PU preflight precede ST/WN creation.
 * Rebuild entry/prototype, rewrite call/return ABI and append projection rows,
 * updating counters. Preflight FALSE leaves code/rows intact; late FALSE may
 * retain allocated symbols/trees, and commit/allocation failures have no general
 * rollback. No final validator or projection-ID success check is performed;
 * TRUE is not whole-stage lowering certification. */
extern BOOL DSL_Runtime_Interface_Apply_PU
                                (PU_Info *pu,
                                 const DSL_RUNTIME_INTERFACE_PLAN *plan,
                                 FILE *diagnostic,
                                 DSL_RUNTIME_INTERFACE_RESULT *result);

/* Read-only post-lowering check of this active PU's entry/prototype, projected
 * handle symbols/source positions, formals and associated call PARM ABI, plus
 * absence of executable canonical source-symbol uses. Require valid shared image,
 * type/TYLIST/map state and resident locals; no PU activation, repair or complete
 * core/domain verification. FALSE reports a mismatch; NULL diagnostic is silent.
 * Does not independently prove return-store semantics or all local-handle
 * initialization/CFG dominance properties. */
extern BOOL DSL_Runtime_Interface_Validate_PU
                                (PU_Info *pu, FILE *diagnostic);

#endif /* DSL_RUNTIME_INTERFACE_INCLUDED */
