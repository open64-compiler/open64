/*
 * Derived DSL lowered-row relations owned by image inspection, not a backend
 * lowering phase. These scalar summaries are not persisted records. Legacy
 * type spellings remain for source and C++ linkage compatibility.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef DSL_LOWERED_RELATION_INCLUDED
#define DSL_LOWERED_RELATION_INCLUDED

#include <stdio.h>
#include "../com/dsl_ir_image.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

/*
 * Nonpersistent modes shared by physical lowering requests and derived image
 * summaries. The owning transaction selects the active PU and derives physical
 * BLOCK/result-STID facts; this header supplies neither a transaction nor PU
 * authority. Source rows remain as provenance with their terminal flags updated
 * after physical removal. No mode/result aggregate here adds a mapped row.
 */
typedef enum {
    DSL_IR_NATIVE_LOWER_UNKNOWN = 0,
    DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK = 1,
    DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION = 2,
    DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION = 3
} DSL_IR_NATIVE_VALUE_LOWER_MODE;

typedef enum {
    DSL_IR_LOWER_RELATION_UNKNOWN = 0,
    DSL_IR_LOWER_RELATION_RUNTIME_VALUE_PROJECTION = 1,
    DSL_IR_LOWER_RELATION_ROOT_PROMOTED_INPUT = 2,
    DSL_IR_LOWER_RELATION_VERIFIED_DEAD_SOURCE = 3
} DSL_IR_LOWER_RELATION_KIND;

typedef struct {
    UINT32 relation_kind;
    DSL_RUNTIME_VALUE_PROJECTION_ID value_projection_id;
    DSL_RUNTIME_INPUT_ID runtime_input_id;
    DSL_RUNTIME_INPUT_BINDING_ID runtime_binding_id;
} DSL_IR_LOWER_RELATION;

typedef struct {
    DSL_IR_NODE_ID source_node_id;
    DSL_IR_VALUE_ID source_value_id;
    UINT32 mode;
    UINT32 relation_kind;
    DSL_RUNTIME_VALUE_PROJECTION_ID value_projection_id;
    DSL_RUNTIME_INPUT_ID runtime_input_id;
    DSL_RUNTIME_INPUT_BINDING_ID runtime_binding_id;
    ST_IDX handle_st;
    TY_IDX handle_ty;
    UINT32 inserted_statement_count;
} DSL_IR_NATIVE_VALUE_LOWER_RESULT;

/* Resolve a LOWERED value's computed projection or primary promoted-input/root
 * relation from current, already-validated shared image tables; a compatible
 * local value projection may accompany the latter. Require nonnull result, not
 * a presence probe. Success fills scalar IDs/handle and zero statement count;
 * no pointers or ownership are transferred. FALSE may leave result cleared
 * with source IDs assigned, so consume output only on TRUE. This is not full
 * owner/type/TCON/physical validation, nor a relation for private lowered, dead
 * or CKKS-only provenance. No live table mutation or PU activation occurs. */
extern BOOL DSL_IR_Image_Resolve_Lowered_Relation
                                (DSL_IR_VALUE_ID value_id,
                                 DSL_IR_NATIVE_VALUE_LOWER_RESULT *result);

/* Validate live lowered/dead relation metadata after core ranges and shared
 * tables are valid: first validate CKKS events, then check runtime relations,
 * dead-argument retirement and CKKS/runtime exclusion. Unresolvable private
 * lowered dependencies may be valid when connected backward to a resolved
 * result/root and free of projection/input/call-ABI associations. Dead-argument
 * lookup inherits Find_Retired_Call's contiguous-storage assumption. No live
 * writes, PU activation, physical ABI/deadness proof or full runtime/program
 * verification. FALSE reports the first error; NULL diagnostic is silent;
 * scratch allocation exceptions propagate. */
extern BOOL DSL_IR_Image_Validate_Lowered_Relations (FILE *diagnostic);

/* Image inspection name; the legacy typedef above preserves C++ linkage. */
typedef DSL_IR_NATIVE_VALUE_LOWER_RESULT DSL_IR_LOWERED_RELATION_INFO;

#endif /* DSL_LOWERED_RELATION_INCLUDED */
