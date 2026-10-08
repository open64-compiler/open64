/*
 * Copyright (C) 2026 Open64 Project
 */

#include "dsl_ir_image.h"
#include "dsl_ckks_event.h"
#include "fhe_image.h"
#include "fhe_plan.h"
#include "dsl_opcode.h"
#include "strtab.h"
#include "symtab.h"

static const char *
DSL_IR_Attribute_Value_Kind_Name (UINT32 value_kind)
{
    static const char *names[] = {
        "unknown", "string", "signed", "unsigned", "float", "boolean",
        "type", "symbol"
    };
    return value_kind < sizeof(names) / sizeof(names[0]) ?
           names[value_kind] : names[0];
}

static const char *
DSL_IR_Value_Kind_Name (UINT32 value_kind)
{
    static const char *names[] = {
        "unknown", "operator_result", "symbol", "constant"
    };
    return value_kind < sizeof(names) / sizeof(names[0]) ?
           names[value_kind] : names[0];
}

static const char *
DSL_Runtime_Binding_Kind_Name (UINT32 kind)
{
    static const char *names[] = {
        "unknown", "local_value", "input_formal", "result_formal"
    };
    return kind < sizeof(names) / sizeof(names[0]) ?
           names[kind] : names[0];
}

static const char *
DSL_Runtime_Call_Direction_Name (UINT32 direction)
{
    static const char *names[] = { "unknown", "input", "result" };
    return direction < sizeof(names) / sizeof(names[0]) ?
           names[direction] : names[0];
}

static const char *
DSL_Interface_Retirement_Reason_Name (UINT32 reason)
{
    static const char *names[] = { "unknown", "verified_dead_input" };
    return reason < sizeof(names) / sizeof(names[0]) ?
           names[reason] : names[0];
}

static const char *
DSL_Runtime_Input_Kind_Name (UINT32 kind)
{
    static const char *names[] = {
        "unknown", "source_external_tensor", "opaque_resource",
        "tensor_tcon_resource"
    };
    return kind < sizeof(names) / sizeof(names[0]) ?
           names[kind] : names[0];
}

static const char *
DSL_Runtime_Input_Binding_Kind_Name (UINT32 kind)
{
    static const char *names[] = {
        "unknown", "root_promoted_source", "root_resource",
        "threaded_formal"
    };
    return kind < sizeof(names) / sizeof(names[0]) ?
           names[kind] : names[0];
}

static const char *
DSL_IR_String (STR_IDX id)
{
    return id == STR_IDX_ZERO ? "" : Index_To_Str(id);
}

static void
DSL_IR_Print_Tensor_Descriptor (FILE *file, TY_IDX ty)
{
    static const TY_TENSOR_SCHEMA_KEY keys[] = {
        TY_TENSOR_SCHEMA_KIND, TY_TENSOR_SCHEMA_DTYPE,
        TY_TENSOR_SCHEMA_RANK, TY_TENSOR_SCHEMA_SHAPE,
        TY_TENSOR_SCHEMA_TRAITS, TY_TENSOR_SCHEMA_LAYOUT,
        TY_TENSOR_SCHEMA_SHARDING, TY_TENSOR_SCHEMA_PLACEMENT,
        TY_TENSOR_SCHEMA_MEMORY, TY_TENSOR_SCHEMA_QUANTIZATION,
        TY_TENSOR_SCHEMA_RUNTIME_STATE, TY_TENSOR_SCHEMA_LINEAGE
    };

    if (TY_IDX_index(ty) == 0 || !TY_is_tensor_extension(ty))
        return;
    fprintf (file, " tensor_descriptor={");
    BOOL first = TRUE;
    for (UINT32 i = 0; i < sizeof(keys) / sizeof(keys[0]); ++i) {
        const char *value = TY_tensor_attribute(ty, keys[i]);
        if (value == NULL)
            continue;
        fprintf (file, "%s%s=%s", first ? "" : ",",
                 TY_tensor_schema_key_name(keys[i]), value);
        first = FALSE;
    }
    fprintf (file, "}");
}

static void
DSL_IR_Print_Result_Symbol (FILE *file, ST_IDX st, STR_IDX name)
{
    if (ST_IDX_index(st) == 0)
        return;
    fprintf (file, " st=<%u,%u,%s>", ST_IDX_level(st), ST_IDX_index(st),
             DSL_IR_String(name));
    if (ST_IDX_level(st) <= CURRENT_SYMTAB &&
        Scope_tab[ST_IDX_level(st)].st_tab != NULL &&
        ST_IDX_index(st) < ST_Table_Size(ST_IDX_level(st)) &&
        strcmp(ST_name(St_Table[st]), DSL_IR_String(name)) == 0) {
        const char *no_alias = ST_tensor_attribute
                                   (st, TY_tensor_schema_key_name
                                            (TY_TENSOR_SCHEMA_NO_ALIAS));
        if (no_alias != NULL)
            fprintf (file, " no_alias=%s", no_alias);
    }
}

void
DSL_IR_Image_Print (FILE *file)
{
    DSL_IR_IMAGE_HEADER header;
    if (file == NULL)
        return;

    DSL_IR_Image_Get_Header(&header);
    fprintf (file, "\nDSL IR Image: version=%u capabilities=0x%08x "
             "opcode_descriptors=%u nodes=%u attributes=%u values=%u "
             "value_references=%u\n", header.version, header.capabilities,
             header.opcode_descriptor_count, header.node_count,
             header.attribute_count, header.value_count,
             header.value_reference_count);

    fprintf (file, "DSL Opcode Descriptor Table:\n");
    for (UINT32 i = 1; i <= header.opcode_descriptor_count; ++i) {
        DSL_IR_OPCODE_DESCRIPTOR_RECORD record;
        DSL_IR_Image_Get_Opcode_Descriptor(i, &record);
        fprintf (file,
                 "  [%u] operator=%s stable_name=%s version=%u "
                 "operands=%d category=%s level=%s shape_rule=%s "
                 "effect_model=%s lowering_model=%s "
                 "attribute_schema=%s diagnostic_prefix=%s flags=0x%x\n",
                 record.id,
                 DSL_OPERATOR_name((DSL_OPERATOR)record.logical_operator),
                 DSL_IR_String(record.stable_name), record.version,
                 record.operand_count,
                 DSL_Opcode_Category_Name
                     ((DSL_OPCODE_CATEGORY)record.category),
                 DSL_Opcode_Level_Name((DSL_OPCODE_LEVEL)record.level),
                 DSL_Shape_Rule_Name((DSL_SHAPE_RULE)record.shape_rule),
                 DSL_Effect_Model_Name
                     ((DSL_EFFECT_MODEL)record.effect_model),
                 DSL_Lowering_Model_Name
                     ((DSL_LOWERING_MODEL)record.lowering_model),
                 DSL_IR_String(record.attribute_schema),
                 DSL_IR_String(record.diagnostic_prefix), record.flags);
    }

    fprintf (file, "DSL Node Table:\n");
    for (UINT32 i = 1; i <= header.node_count; ++i) {
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_IR_Image_Get_Node(i, &node);
        DSL_IR_Image_Get_Opcode_Descriptor
            (node.opcode_descriptor_id, &descriptor);
        fprintf (file, "  [%u] operator=%s version=%u operands=[", node.id,
                 DSL_OPERATOR_name
                     ((DSL_OPERATOR)descriptor.logical_operator),
                 descriptor.version);
        for (UINT32 j = 0; j < node.operand_count; ++j) {
            DSL_IR_VALUE_REFERENCE_RECORD reference;
            DSL_IR_Image_Get_Value_Reference
                (node.first_operand_reference_id + j, &reference);
            fprintf (file, "%svalue%u", j == 0 ? "" : ",",
                     reference.value_id);
        }
        fprintf (file, "] attributes=[");
        for (UINT32 j = 0; j < node.attribute_count; ++j) {
            DSL_IR_ATTRIBUTE_RECORD attribute;
            DSL_IR_Image_Get_Attribute
                (node.first_attribute_id + j, &attribute);
            fprintf (file, "%s%s=%s", j == 0 ? "" : ",",
                     DSL_IR_String(attribute.name),
                     DSL_IR_String(attribute.value));
        }
        fprintf (file, "] result=value%u", node.result_value_id);
        if ((node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0) {
            DSL_IR_VALUE_ID target = DSL_IR_VALUE_INVALID_ID;
            DSL_IR_Image_Value_Redirect_Target(node.result_value_id, &target);
            fprintf(file, " status=retired redirected_to=value%u", target);
            if ((node.flags & DSL_IR_NODE_FLAG_RETIRED_VIEW) != 0)
                fprintf(file, " relation=representation_view");
        }
        if ((node.flags & DSL_IR_NODE_FLAG_DEAD_ELIDED) != 0)
            fprintf(file, " status=dead_elided relation=none");
        if ((node.flags & DSL_IR_NODE_FLAG_LOWERED) != 0) {
            DSL_IR_NATIVE_VALUE_LOWER_RESULT relation;
            if (DSL_IR_Image_Resolve_Lowered_Relation
                    (node.result_value_id, &relation)) {
                fprintf(file, " status=lowered relation=%s",
                        relation.relation_kind ==
                            DSL_IR_LOWER_RELATION_RUNTIME_VALUE_PROJECTION ?
                        "runtime_value_projection" :
                        "root_promoted_input");
                if (relation.value_projection_id != 0)
                    fprintf(file, " projection=%u",
                            relation.value_projection_id);
                if (relation.runtime_input_id != 0)
                    fprintf(file, " runtime_input=%u runtime_binding=%u",
                            relation.runtime_input_id,
                            relation.runtime_binding_id);
                fprintf(file, " handle=<%u,%u> handle_ty=%u",
                        ST_IDX_level(relation.handle_st),
                        ST_IDX_index(relation.handle_st),
                        (UINT32)relation.handle_ty);
            } else if (DSL_CKKS_Event_Image_Has_Source
                           (node.result_value_id)) {
                fprintf(file, " status=lowered relation=ckks_expansion");
            }
        }
        if (node.payload != STR_IDX_ZERO)
            fprintf (file, " payload=%s", DSL_IR_String(node.payload));
        fprintf (file, " flags=0x%x\n", node.flags);
    }

    fprintf (file, "DSL Attribute Table:\n");
    for (UINT32 i = 1; i <= header.attribute_count; ++i) {
        DSL_IR_ATTRIBUTE_RECORD record;
        DSL_IR_Image_Get_Attribute(i, &record);
        fprintf (file,
                 "  [%u] node=%u name=%s kind=%s value=%s flags=0x%x\n",
                 record.id, record.owner_node_id,
                 DSL_IR_String(record.name),
                 DSL_IR_Attribute_Value_Kind_Name(record.value_kind),
                 DSL_IR_String(record.value), record.flags);
    }

    fprintf (file, "DSL Value Table:\n");
    for (UINT32 i = 1; i <= header.value_count; ++i) {
        DSL_IR_VALUE_RECORD record;
        DSL_IR_Image_Get_Value(i, &record);
        fprintf (file, "  [%u] kind=%s name=%s producer_node=%u ty=%u",
                 record.id, DSL_IR_Value_Kind_Name(record.value_kind),
                 DSL_IR_String(record.name), record.producer_node_id,
                 (UINT32)record.ty);
        if (TY_IDX_index(record.ty) != 0)
            fprintf (file, " type_name=%s", TY_name(record.ty));
        DSL_IR_Print_Tensor_Descriptor(file, record.ty);
        DSL_IR_Print_Result_Symbol(file, record.st, record.name);
        if (record.metadata != STR_IDX_ZERO)
            fprintf (file, " metadata=%s", DSL_IR_String(record.metadata));
        if ((record.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0) {
            DSL_IR_VALUE_ID target = DSL_IR_VALUE_INVALID_ID;
            DSL_IR_Image_Value_Redirect_Target(record.id, &target);
            fprintf(file, " status=redirected redirected_to=value%u", target);
        }
        if ((record.flags & DSL_IR_VALUE_FLAG_DEAD_ELIDED) != 0)
            fprintf(file, " status=dead_elided relation=none");
        if ((record.flags & DSL_IR_VALUE_FLAG_LOWERED) != 0) {
            DSL_IR_NATIVE_VALUE_LOWER_RESULT relation;
            if (DSL_IR_Image_Resolve_Lowered_Relation
                    (record.id, &relation)) {
                fprintf(file, " status=lowered relation=%s",
                        relation.relation_kind ==
                            DSL_IR_LOWER_RELATION_RUNTIME_VALUE_PROJECTION ?
                        "runtime_value_projection" :
                        "root_promoted_input");
                if (relation.value_projection_id != 0)
                    fprintf(file, " projection=%u",
                            relation.value_projection_id);
                if (relation.runtime_input_id != 0)
                    fprintf(file, " runtime_input=%u runtime_binding=%u",
                            relation.runtime_input_id,
                            relation.runtime_binding_id);
                fprintf(file, " handle=<%u,%u> handle_ty=%u",
                        ST_IDX_level(relation.handle_st),
                        ST_IDX_index(relation.handle_st),
                        (UINT32)relation.handle_ty);
            } else if (DSL_CKKS_Event_Image_Has_Source(record.id)) {
                fprintf(file, " status=lowered relation=ckks_expansion");
            }
        }
        fprintf (file, " flags=0x%x\n", record.flags);
    }

    fprintf (file, "DSL Value Reference Table:\n");
    for (UINT32 i = 1; i <= header.value_reference_count; ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD record;
        DSL_IR_Image_Get_Value_Reference(i, &record);
        fprintf (file,
                 "  [%u] node=%u ordinal=kid%u value=value%u flags=0x%x\n",
                 record.id, record.owner_node_id, record.ordinal,
                 record.value_id, record.flags);
    }

    DSL_EFFECT_IMAGE_HEADER effect_header;
    DSL_Effect_Image_Get_Header(&effect_header);
    fprintf (file, "DSL Abstract State Table: version=%u states=%u "
             "effects=%u\n", effect_header.version,
             effect_header.state_object_count,
             effect_header.state_effect_count);
    for (UINT32 i = 1; i <= effect_header.state_object_count; ++i) {
        DSL_STATE_OBJECT_RECORD record;
        DSL_Effect_Image_Get_State_Object(i, &record);
        fprintf (file, "  STATE [%u] name=%s kind=%s owner_pu=<%u,%u> "
                 "st=<%u,%u> flags=0x%x\n", record.id,
                 DSL_IR_String(record.name),
                 DSL_State_Kind_Name((DSL_STATE_KIND)record.kind),
                 ST_IDX_level(record.owner_pu_st),
                 ST_IDX_index(record.owner_pu_st),
                 ST_IDX_level(record.st), ST_IDX_index(record.st),
                 record.flags);
    }
    for (UINT32 i = 1; i <= effect_header.state_effect_count; ++i) {
        DSL_STATE_EFFECT_RECORD record;
        DSL_Effect_Image_Get_State_Effect(i, &record);
        fprintf (file, "  EFFECT [%u] node=%u state=%u ordinal=%u "
                 "kind=%s flags=0x%x\n", record.id,
                 record.owner_node_id, record.state_object_id,
                 record.ordinal,
                 DSL_State_Effect_Kind_Name
                     ((DSL_STATE_EFFECT_KIND)record.effect_kind),
                 record.flags);
    }

    DSL_CALL_IMAGE_HEADER call_header;
    DSL_Call_Image_Get_Header(&call_header);
    fprintf(file, "DSL PU Source Identity Table: version=%u entries=%u\n",
            call_header.version, call_header.pu_identity_count);
    for (UINT32 i = 1; i <= call_header.pu_identity_count; ++i) {
        DSL_PU_SOURCE_IDENTITY_RECORD record;
        DSL_Call_Image_Get_PU_Identity(i, &record);
        fprintf(file, "  [%u] owner_pu=<%u,%u> definition=%s module=%s "
                "file=%s line=%u flags=0x%x\n", record.id,
                ST_IDX_level(record.owner_pu_st),
                ST_IDX_index(record.owner_pu_st),
                DSL_IR_String(record.canonical_definition_name),
                DSL_IR_String(record.defining_module),
                DSL_IR_String(record.defining_file), record.defining_line,
                record.flags);
    }
    fprintf(file, "DSL Callsite Metadata Table: version=%u entries=%u\n",
            call_header.version, call_header.callsite_count);
    for (UINT32 i = 1; i <= call_header.callsite_count; ++i) {
        DSL_CALLSITE_METADATA_RECORD record;
        DSL_Call_Image_Get_Callsite(i, &record);
        fprintf(file, "  [%u] owner_pu=<%u,%u> callee=<%u,%u> "
                "class=%s instance=%s context=%s source_ordinal=%u "
                "flags=0x%x\n", record.id,
                ST_IDX_level(record.owner_pu_st),
                ST_IDX_index(record.owner_pu_st),
                ST_IDX_level(record.callee_pu_st),
                ST_IDX_index(record.callee_pu_st),
                DSL_IR_String(record.canonical_class_name),
                DSL_IR_String(record.instance_path),
                DSL_IR_String(record.context_identity),
                record.source_call_ordinal, record.flags);
    }
    DSL_PU_INTERFACE_IMAGE_HEADER pu_interface_header;
    DSL_PU_Interface_Image_Get_Header(&pu_interface_header);
    fprintf(file, "DSL PU Interface Formal Table: version=%u entries=%u\n",
            pu_interface_header.version, pu_interface_header.formal_count);
    for (UINT32 i = 1; i <= pu_interface_header.formal_count; ++i) {
        DSL_PU_FORMAL_RECORD record;
        DSL_PU_Interface_Image_Get_Formal(i, &record);
        fprintf(file, "  [%u] owner_pu=<%u,%u> formal=%u value=%u "
                "st=<%u,%u> ty=%u flags=0x%x\n", record.id,
                ST_IDX_level(record.owner_pu_st),
                ST_IDX_index(record.owner_pu_st), record.formal_ordinal,
                record.formal_value_id, ST_IDX_level(record.formal_st),
                ST_IDX_index(record.formal_st), (UINT32)record.formal_ty,
                record.flags);
    }
    DSL_CALL_ABI_IMAGE_HEADER call_abi_header;
    DSL_Call_ABI_Image_Get_Header(&call_abi_header);
    fprintf(file, "DSL Call ABI Argument Table: version=%u entries=%u\n",
            call_abi_header.version, call_abi_header.argument_count);
    for (UINT32 i = 1; i <= call_abi_header.argument_count; ++i) {
        DSL_CALL_ARGUMENT_RECORD record;
        DSL_CALLSITE_METADATA_RECORD callsite;
        DSL_Call_ABI_Image_Get_Argument(i, &record);
        DSL_Call_Image_Get_Callsite(record.callsite_id, &callsite);
        fprintf(file, "  [%u] callsite=%u callee=<%u,%u> actual=%u "
                "formal=%u argument_value=%u role=%s flags=0x%x\n",
                record.id, record.callsite_id,
                ST_IDX_level(callsite.callee_pu_st),
                ST_IDX_index(callsite.callee_pu_st), record.actual_ordinal,
                record.callee_formal_ordinal, record.argument_value_id,
                DSL_IR_String(record.semantic_role), record.flags);
    }
    DSL_RUNTIME_INTERFACE_IMAGE_HEADER runtime_header;
    DSL_Runtime_Interface_Image_Get_Header(&runtime_header);
    fprintf(file,
            "DSL Runtime Interface Image: version=%u values=%u calls=%u\n",
            runtime_header.version, runtime_header.value_projection_count,
            runtime_header.call_projection_count);
    fprintf(file, "DSL Runtime Value Projection Table:\n");
    for (UINT32 i = 1; i <= runtime_header.value_projection_count; ++i) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD record;
        DSL_Runtime_Interface_Image_Get_Value(i, &record);
        fprintf(file, "  [%u] owner_pu=<%u,%u> source_value=%u "
                "source_st=<%u,%u> source_ty=%u handle_st=<%u,%u> "
                "handle_ty=%u kind=%s formal=",
                record.id, ST_IDX_level(record.owner_pu_st),
                ST_IDX_index(record.owner_pu_st), record.source_value_id,
                ST_IDX_level(record.source_st), ST_IDX_index(record.source_st),
                (UINT32)record.source_ty, ST_IDX_level(record.handle_st),
                ST_IDX_index(record.handle_st), (UINT32)record.handle_ty,
                DSL_Runtime_Binding_Kind_Name(record.binding_kind));
        if (record.formal_ordinal == DSL_RUNTIME_INTERFACE_INVALID_ORDINAL)
            fprintf(file, "<none>");
        else
            fprintf(file, "%u", record.formal_ordinal);
        fprintf(file, " flags=0x%x", record.flags);
        if ((record.flags &
             DSL_RUNTIME_VALUE_PROJECTION_FLAG_RETIRED_VIEW) != 0)
            fprintf(file, " status=retired_view");
        fprintf(file, "\n");
    }
    fprintf(file, "DSL Runtime Call Projection Table:\n");
    for (UINT32 i = 1; i <= runtime_header.call_projection_count; ++i) {
        DSL_RUNTIME_CALL_PROJECTION_RECORD record;
        DSL_Runtime_Interface_Image_Get_Call(i, &record);
        fprintf(file, "  [%u] owner_pu=<%u,%u> callsite=%u "
                "source_value=%u projection=%u actual=%u formal=%u "
                "direction=%s flags=0x%x\n", record.id,
                ST_IDX_level(record.owner_pu_st),
                ST_IDX_index(record.owner_pu_st), record.callsite_id,
                record.source_value_id, record.value_projection_id,
                record.actual_ordinal, record.callee_formal_ordinal,
                DSL_Runtime_Call_Direction_Name(record.direction),
                record.flags);
    }
    DSL_PROGRAM_INTERFACE_IMAGE_HEADER program_header;
    DSL_Program_Interface_Image_Get_Header(&program_header);
    fprintf(file, "DSL Program Interface Image: version=%u "
            "retired_formals=%u retired_call_arguments=%u "
            "runtime_inputs=%u runtime_bindings=%u runtime_calls=%u\n",
            program_header.version, program_header.retired_formal_count,
            program_header.retired_call_argument_count,
            program_header.runtime_input_count,
            program_header.runtime_input_binding_count,
            program_header.runtime_input_call_count);
    fprintf(file, "DSL Retired Formal Table:\n");
    for (UINT32 i = 1; i <= program_header.retired_formal_count; ++i) {
        DSL_RETIRED_FORMAL_RECORD record;
        DSL_Program_Interface_Image_Get_Retired_Formal(i, &record);
        fprintf(file, "  [%u] owner_pu=<%u,%u> pu_formal=%u "
                "old_formal=%u value=%u st=<%u,%u> ty=%u "
                "reason=%s role=%s flags=0x%x\n", record.id,
                ST_IDX_level(record.owner_pu_st),
                ST_IDX_index(record.owner_pu_st), record.pu_formal_id,
                record.old_formal_ordinal, record.formal_value_id,
                ST_IDX_level(record.formal_st), ST_IDX_index(record.formal_st),
                (UINT32)record.formal_ty,
                DSL_Interface_Retirement_Reason_Name
                    (record.retirement_reason),
                DSL_IR_String(record.semantic_role), record.flags);
    }
    fprintf(file, "DSL Retired Call Argument Table:\n");
    for (UINT32 i = 1;
         i <= program_header.retired_call_argument_count; ++i) {
        DSL_RETIRED_CALL_ARGUMENT_RECORD record;
        DSL_Program_Interface_Image_Get_Retired_Call(i, &record);
        fprintf(file, "  [%u] callsite=%u call_argument=%u old_actual=%u "
                "old_formal=%u value=%u reason=%s role=%s flags=0x%x\n",
                record.id, record.callsite_id, record.call_argument_id,
                record.old_actual_ordinal, record.old_callee_formal_ordinal,
                record.argument_value_id,
                DSL_Interface_Retirement_Reason_Name
                    (record.retirement_reason),
                DSL_IR_String(record.semantic_role), record.flags);
    }
    fprintf(file, "DSL Runtime Input Table:\n");
    for (UINT32 i = 1; i <= program_header.runtime_input_count; ++i) {
        DSL_RUNTIME_INPUT_RECORD record;
        DSL_Program_Interface_Image_Get_Runtime_Input(i, &record);
        fprintf(file, "  [%u] kind=%s role=%s handle_ty=%u "
                "source_owner=<%u,%u> source_value=%u source_st=<%u,%u> "
                "source_ty=%u source_tcon=%u flags=0x%x\n", record.id,
                DSL_Runtime_Input_Kind_Name(record.input_kind),
                DSL_IR_String(record.stable_role), (UINT32)record.handle_ty,
                ST_IDX_level(record.source_owner_pu_st),
                ST_IDX_index(record.source_owner_pu_st),
                record.source_value_id, ST_IDX_level(record.source_st),
                ST_IDX_index(record.source_st), (UINT32)record.source_ty,
                (UINT32)record.source_tcon, record.flags);
    }
    fprintf(file, "DSL Runtime Input Binding Table:\n");
    for (UINT32 i = 1; i <= program_header.runtime_input_binding_count; ++i) {
        DSL_RUNTIME_INPUT_BINDING_RECORD record;
        DSL_Program_Interface_Image_Get_Runtime_Binding(i, &record);
        fprintf(file, "  [%u] owner_pu=<%u,%u> input=%u "
                "handle_st=<%u,%u> handle_ty=%u final_formal=%u "
                "kind=%s role=%s flags=0x%x\n", record.id,
                ST_IDX_level(record.owner_pu_st),
                ST_IDX_index(record.owner_pu_st), record.runtime_input_id,
                ST_IDX_level(record.handle_st), ST_IDX_index(record.handle_st),
                (UINT32)record.handle_ty, record.final_formal_ordinal,
                DSL_Runtime_Input_Binding_Kind_Name(record.binding_kind),
                DSL_IR_String(record.semantic_role), record.flags);
    }
    fprintf(file, "DSL Runtime Input Call Table:\n");
    for (UINT32 i = 1; i <= program_header.runtime_input_call_count; ++i) {
        DSL_RUNTIME_INPUT_CALL_RECORD record;
        DSL_Program_Interface_Image_Get_Runtime_Call(i, &record);
        fprintf(file, "  [%u] callsite=%u caller=<%u,%u> "
                "caller_formal=%u callee=<%u,%u> callee_formal=%u "
                "actual=%u formal=%u handle_ty=%u role=%s flags=0x%x\n",
                record.id, record.callsite_id,
                ST_IDX_level(record.caller_owner_pu_st),
                ST_IDX_index(record.caller_owner_pu_st),
                record.caller_final_formal_ordinal,
                ST_IDX_level(record.callee_owner_pu_st),
                ST_IDX_index(record.callee_owner_pu_st),
                record.callee_final_formal_ordinal,
                record.final_actual_ordinal,
                record.final_callee_formal_ordinal,
                (UINT32)record.handle_ty,
                DSL_IR_String(record.semantic_role), record.flags);
    }
    DSL_FHE_Image_Print(file);
    DSL_FHE_Plan_Image_Print(file);
    DSL_FHE_Approx_Profile_Image_Print(file);
    DSL_FHE_Context_State_Image_Print(file);
    DSL_FHE_Materialization_Image_Print(file);
    DSL_CKKS_Event_Image_Print(file);
}
