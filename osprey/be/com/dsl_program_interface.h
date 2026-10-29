/*
 * Backend-free program-interface plan admission and active-PU ABI mutation.
 * The driver/IPA owns traversal and scope; requests are not mapped records.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef DSL_PROGRAM_INTERFACE_INCLUDED
#define DSL_PROGRAM_INTERFACE_INCLUDED

#include <stdio.h>
#include "dsl_runtime_interface.h"
#include "../../common/com/dsl_ir_image.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

/* Absent transient plan-array index, not a persisted row ID. Threaded bindings
 * use it for runtime_input_index; root bindings require an actual input index. */
#define DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX ((UINT32)-1)

typedef struct {
    DSL_PU_FORMAL_ID pu_formal_id;
    const char *semantic_role;
} DSL_RETIRED_FORMAL_REQUEST;

typedef struct {
    DSL_CALL_ARGUMENT_ID call_argument_id;
    const char *semantic_role;
} DSL_RETIRED_CALL_ARGUMENT_REQUEST;

typedef struct {
    UINT32 input_kind;
    ST_IDX source_owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    TY_IDX source_ty;
    TCON_IDX source_tcon;
    const char *stable_role;
    TY_IDX handle_ty;
} DSL_RUNTIME_INPUT_REQUEST;

typedef struct {
    ST_IDX owner_pu_st;
    UINT32 runtime_input_index;
    TY_IDX handle_ty;
    UINT32 binding_kind;
    const char *semantic_role;
    SRCPOS source_position;
} DSL_RUNTIME_INPUT_BINDING_REQUEST;

typedef struct {
    DSL_CALLSITE_METADATA_ID callsite_id;
    UINT32 caller_binding_index;
    UINT32 callee_binding_index;
} DSL_RUNTIME_INPUT_CALL_REQUEST;

typedef struct {
    const DSL_RETIRED_FORMAL_REQUEST *retired_formals;
    UINT32 retired_formal_count;
    const DSL_RETIRED_CALL_ARGUMENT_REQUEST *retired_call_arguments;
    UINT32 retired_call_argument_count;
    const DSL_RUNTIME_INPUT_REQUEST *runtime_inputs;
    UINT32 runtime_input_count;
    const DSL_RUNTIME_INPUT_BINDING_REQUEST *runtime_input_bindings;
    UINT32 runtime_input_binding_count;
    const DSL_RUNTIME_INPUT_CALL_REQUEST *runtime_input_calls;
    UINT32 runtime_input_call_count;
} DSL_PROGRAM_INTERFACE_PLAN;

typedef struct {
    UINT32 retired_formal_count;
    UINT32 retired_call_argument_count;
    UINT32 runtime_input_count;
    UINT32 runtime_binding_count;
    UINT32 runtime_call_count;
    UINT32 canonical_projection_count;
    UINT32 rewritten_call_count;
    UINT32 rewritten_return_count;
} DSL_PROGRAM_INTERFACE_RESULT;

/*
 * Preflight complete borrowed program/runtime plans against canonical shared
 * images before the first PU commit; require count-sized arrays, readable role
 * strings and valid referenced tables. Reject existing program-image rows,
 * inconsistent retirement/projection coverage, input profiles or unrooted/cyclic
 * binding flow. Does not prove physical deadness, activate PUs, install rows or
 * cache a fingerprint; Apply_PU performs physical preflight and records its key.
 * FALSE reports the first mismatch (NULL diagnostic is silent); scratch
 * allocations may throw, but no WHIRL/image/commit evidence is changed here.
 */
extern BOOL DSL_Program_Interface_Plan_Validate
                                (const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
                                 const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan,
                                 FILE *diagnostic);

/* Zero caller-owned combined-interface counters or ignore NULL. Does not clear
 * images, prepared-plan/committed-PU evidence or any physical ABI state. */
extern void DSL_Program_Interface_Result_Init
                                (DSL_PROGRAM_INTERFACE_RESULT *result);

/* Evolve only this already-active PU using stable complete program/runtime
 * plans, readable arrays/strings and initialized canonical images/types. Caller
 * coordinates plan-wide processing and owns local symtab/map/pools. Clear required
 * result first; validate or match the prepared plan, then preflight physical
 * formals/calls/returns, source inputs and REGION pruning. Prepared-plan evidence
 * can change on a rejected attempt. Next install shared inputs (failure may
 * retain an appended prefix), create handles and rebuild entry/prototype/calls/
 * returns, retire ABI slots, prune REGION inputs, append provenance and record
 * this PU committed. No general rollback or final validator; commit assertion/
 * allocation failures are not recoverable FALSE. Counters are authoritative only
 * on TRUE; this grants no authority to switch to or rewrite another PU. */
extern BOOL DSL_Program_Interface_Apply_PU
                                (PU_Info *pu,
                                 const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
                                 const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan,
                                 FILE *diagnostic,
                                 DSL_PROGRAM_INTERFACE_RESULT *result);

/* Read-only active-PU interface check: reconcile surviving/retired formals,
 * threaded bindings, call PARM ABI and result-handle null initialization with
 * canonical/projection/program rows and resident symbols. Require valid shared
 * types/strings/associations; retirement/binding Find helpers inherit their
 * contiguous-storage assumptions. No PU switch, full row/prototype/REGION
 * verification or source-use closure proof. FALSE reports the first mismatch;
 * NULL diagnostic is silent; scratch allocation exceptions propagate. */
extern BOOL DSL_Program_Interface_Validate_PU
                                (PU_Info *pu, FILE *diagnostic);

/* Extend Validate_PU for final lowering: reject executable promoted/canonical
 * and owned dead-elided source uses, check promoted-copy relations and dead-
 * elided projection uses, then check local-handle reads against structured WHIRL
 * definitions. Not a general CFG/SSA dominance or complete domain verifier.
 * Require the same active-PU/shared context and valid lowered relations; no
 * mutation or PU activation. FALSE reports a mismatch (NULL diagnostic is
 * silent); scratch allocation exceptions propagate. */
extern BOOL DSL_Program_Interface_Validate_Lowered_PU
                                (PU_Info *pu, FILE *diagnostic);

#endif /* DSL_PROGRAM_INTERFACE_INCLUDED */
