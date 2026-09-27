/*
 * Copyright (C) 2026 Open64 Project
 */

/* VHO-owned AIO-1 active-PU semantic-root capture. */

#include "dsl_tensor_evolution_opt.h"
#include "strtab.h"

BOOL
VHO_DSL_Tensor_Evolution_Build_Semantic_Roots
        (DSL_TENSOR_EVOLUTION_GRAPH *graph, FILE *diagnostic)
{
    if (graph == NULL || !DSL_IR_Image_Validate(diagnostic))
        return FALSE;

    /*
     * Capture only the active PU's live tensor values. Common owns root record
     * construction; VHO owns the image scan that decides which roots exist.
     */
    for (DSL_IR_VALUE_ID id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
        DSL_IR_VALUE_RECORD value;
        DSL_IR_VALUE_RECORD resolved;
        ST_IDX owner = DSL_tensor_evolution_owner(graph);
        if (!DSL_IR_Image_Get_Value(id, &value))
            return FALSE;
        if ((value.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0 ||
            value.name == STR_IDX_ZERO || ST_IDX_index(value.st) == 0 ||
            ST_IDX_level(value.st) != CURRENT_SYMTAB ||
            ST_IDX_index(value.st) >= ST_Table_Size(CURRENT_SYMTAB) ||
            value.ty != ST_type(St_Table[value.st]) ||
            !DSL_IR_Image_Find_PU_Value
                 (value.st, Index_To_Str(value.name),
                  ST_name(St_Table[owner]), &resolved) ||
            resolved.id != value.id)
            continue;
        if (!TY_is_tensor_extension(value.ty))
            continue;
        if (!DSL_tensor_evolution_add_semantic_root
                 (graph, value.id, value.ty, NULL, diagnostic))
            return FALSE;
    }
    return DSL_tensor_evolution_verify(graph, diagnostic);
}
