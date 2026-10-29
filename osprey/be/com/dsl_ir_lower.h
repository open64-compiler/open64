/*
 * Backend-free active-PU lowering and dead-source elision mechanisms.
 * Caller-selected replacements preserve logical provenance; this module
 * does not discover candidates or select optimization policy.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef DSL_IR_LOWER_INCLUDED
#define DSL_IR_LOWER_INCLUDED

#include <stdio.h>
#include "../../common/ir_image/dsl_lowered_relation.h"
#include "../../common/com/dsl_ir_image.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

typedef struct {
    WN *native_definition;
    DSL_IR_VALUE_ID source_value_id;
    DSL_OPERATOR expected_operator;
    UINT16 expected_version;
    UINT16 reserved;
    UINT32 mode;
    DSL_IR_LOWER_RELATION relation;
    WN *standard_block;
} DSL_IR_NATIVE_VALUE_LOWER_REQUEST;

/* Exact identity for one pure tensor constant whose executable definition is
 * dead after its consumers have been lowered or retired. No image row changes. */
typedef struct {
    ST_IDX owner_pu_st;
    WN *native_definition;
    DSL_IR_NODE_ID node_id;
    DSL_IR_VALUE_ID value_id;
    ST_IDX st;
    TY_IDX ty;
    UINT16 expected_version;
    UINT16 reserved;
} DSL_IR_DEAD_NATIVE_CONSTANT_REQUEST;

/* One owner-PU transaction may retire pure private definitions for each
 * checked standard-WHIRL replacement. Requests and detached blocks remain
 * caller-owned on rejection; success consumes the detached blocks. */
typedef struct {
    WN *native_definition;
    DSL_IR_NODE_ID node_id;
    DSL_IR_VALUE_ID value_id;
    DSL_OPERATOR expected_operator;
    UINT16 expected_version;
    UINT16 reserved;
} DSL_IR_NATIVE_GROUP_MEMBER_REQUEST;

typedef struct {
    ST_IDX owner_pu_st;
    const DSL_IR_NATIVE_GROUP_MEMBER_REQUEST *members;
    UINT32 member_count;
    DSL_IR_VALUE_ID group_output_value_id;
    DSL_IR_VALUE_ID semantic_source_value_id;
    WN *standard_block;
    DSL_IR_LOWER_RELATION relation;
} DSL_IR_NATIVE_GROUP_LOWER_REQUEST;

typedef struct {
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID semantic_source_value_id;
    DSL_IR_VALUE_ID retired_group_output_value_id;
    UINT32 retired_member_count;
    UINT32 inserted_statement_count;
    ST_IDX handle_st;
    TY_IDX handle_ty;
} DSL_IR_NATIVE_GROUP_LOWER_RESULT;

/*
 * Preflight and atomically lower a complete request set for one active PU.
 * The API derives PU ownership, the native definitions' containing blocks,
 * and each computed standard block's final result STID from its inputs. It
 * mutates WHIRL and logical lowered flags only after all requests, relations,
 * source uses, effects, and detached block contracts validate; failure before
 * commit leaves the physical tree and image unchanged. Require stable arrays,
 * resident owner locals/shared tables and request_count result slots; clear
 * nonnull results before entry/preflight checks, so FALSE does not preserve
 * old result contents. Detached replacement BLOCKs stay caller-owned on FALSE;
 * success consumes their containers/statements and removes native definitions.
 * Commit marks LOWERED or DEAD_ELIDED, applies REGION redirects and asserts
 * image/relation/REGION postconditions, with no rollback journal or general
 * allocation recovery. Caller retains policy and does not transfer PU scope.
 */
extern BOOL DSL_IR_Lower_Native_Values_To_Standard_Blocks
                                (PU_Info *pu_info,
                                 const DSL_IR_NATIVE_VALUE_LOWER_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 FILE *diagnostic,
                                 DSL_IR_NATIVE_VALUE_LOWER_RESULT *results);

/* Batch preflight then elide dead pure tensor-constant definitions in one PU.
 * Rejection leaves the WHIRL tree/image and required elided_count unchanged;
 * TRUE sets the count after the reused lowering transaction succeeds. Require
 * exact active-PU/shared identity context and a nonempty readable request array.
 * Commit/postcheck failures assert rather than provide recoverable rollback;
 * no payload/constant computation, PU activation or canonical-TY mutation. */
extern BOOL DSL_IR_Elide_Dead_Native_Constants
                                (PU_Info *pu_info,
                                 const DSL_IR_DEAD_NATIVE_CONSTANT_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 FILE *diagnostic,
                                 UINT32 *elided_count);

/* Preflight a nonempty complete group array in the selected active PU: verify
 * unique pure members, contiguous same-BLOCK ordering (comments allowed), exact
 * physical/logical use closure, output projections, retired-handle aliases and
 * REGION transitions. Caller supplies stable arrays, valid shared/local context
 * and request_count result slots; FALSE preserves results, IR and detached blocks.
 * Success consumes blocks, removes native members, rewrites checked aliases,
 * marks provenance LOWERED and fills results. Postchecks assert; there is no
 * commit/exception rollback journal, candidate selection or PU activation. */
extern BOOL DSL_IR_Lower_Native_Value_Groups_To_Standard_Blocks
                                (PU_Info *pu_info,
                                 const DSL_IR_NATIVE_GROUP_LOWER_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 FILE *diagnostic,
                                 DSL_IR_NATIVE_GROUP_LOWER_RESULT *results);

#endif /* DSL_IR_LOWER_INCLUDED */
