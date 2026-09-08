/*
 * Copyright (C) 2026 Open64 Project
 */

#include "dsl_fhe.h"
#include "dsl_tensor_fold.h"
#include "ir_reader.h"
#include "strtab.h"
#include "symtab.h"

static const char *
DSL_FHE_Name (UINT32 value, const char *const *names, UINT32 count)
{
    return value < count ? names[value] : names[0];
}

static const char *
DSL_FHE_Scheme_Name (UINT32 value)
{
    static const char *names[] = { "unknown", "ckks" };
    return DSL_FHE_Name(value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Value_Class_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "ciphertext", "encoded_plaintext", "clear"
    };
    return DSL_FHE_Name(value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Entry_Role_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "input", "output", "parameter"
    };
    return DSL_FHE_Name(value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Bootstrap_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "auto", "on", "manual", "off"
    };
    return DSL_FHE_Name(value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Backend_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "auto", "openfhe", "mock"
    };
    return DSL_FHE_Name(value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Key_Class_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "public", "relinearization", "rotation", "bootstrap"
    };
    return DSL_FHE_Name(value, names, sizeof(names) / sizeof(names[0]));
}

static void
DSL_FHE_Print_Value_Symbol (FILE *file, const DSL_IR_VALUE_RECORD &value)
{
    fprintf(file, " value=value%u", value.id);
    if (value.name != STR_IDX_ZERO)
        fprintf(file, " name=%s", Index_To_Str(value.name));
    if (ST_IDX_index(value.st) == 0 ||
        ST_IDX_level(value.st) > CURRENT_SYMTAB ||
        Scope_tab[ST_IDX_level(value.st)].st_tab == NULL ||
        ST_IDX_index(value.st) >= ST_Table_Size(ST_IDX_level(value.st)))
        return;
    ST &st = St_Table[value.st];
    fprintf(file, " st=<%u,%u,%s>", ST_IDX_level(value.st),
            ST_IDX_index(value.st), ST_name(st));
    SRCPOS source_position = ST_Srcpos(st);
    if (SRCPOS_linenum(source_position) != 0) {
        const char *file_name = NULL;
        const char *directory_name = NULL;
        IR_Srcpos_Filename(source_position, &file_name, &directory_name);
        if (file_name != NULL)
            fprintf(file, " source=%s:%u", file_name,
                    SRCPOS_linenum(source_position));
        else
            fprintf(file, " source_line=%u", SRCPOS_linenum(source_position));
    }
    if (ST_class(st) == CLASS_CONST) {
        fputs(" ", file);
        DSL_Tensor_TCON_Print(file, ST_tcon(st));
    }
}

void
DSL_FHE_Image_Print (FILE *file)
{
    if (file == NULL || !DSL_FHE_Image_Has_Records())
        return;
    DSL_FHE_IMAGE_HEADER header;
    DSL_FHE_Image_Get_Header(&header);
    fprintf(file, "\nFHE Image: version=%u capabilities=0x%08x\n",
            header.version, header.capabilities);

    fprintf(file, "FHE Compilation Configuration Table:\n");
    for (UINT32 i = 1; i <= header.config_count; ++i) {
        DSL_FHE_COMPILATION_CONFIG_RECORD record;
        DSL_FHE_Get_Compilation_Config(i, &record);
        fprintf(file, "  [%u] scheme=%s security=%u ring_dimension=%u "
                "depth=%u scale_bits=%u first_modulus_bits=%u slots=%u "
                "bootstrap=%s backend=%s flags=0x%x\n", record.id,
                DSL_FHE_Scheme_Name(record.scheme), record.security_level,
                record.ring_dimension, record.multiplicative_depth,
                record.scale_bits, record.first_modulus_bits,
                record.slot_count,
                DSL_FHE_Bootstrap_Name(record.bootstrap_policy),
                DSL_FHE_Backend_Name(record.backend_policy), record.flags);
    }

    fprintf(file, "FHE Entry Contract Table:\n");
    for (UINT32 i = 1; i <= header.entry_contract_count; ++i) {
        DSL_FHE_ENTRY_CONTRACT_RECORD record;
        DSL_FHE_Get_Entry_Contract(i, &record);
        const char *pu_name = ST_name(St_Table[record.owner_pu_st]);
        fprintf(file, "  [%u] pu=%s config=%u values=[%u,%u] inputs=%u "
                "outputs=%u parameters=%u parameter_policy=%u flags=0x%x\n",
                record.id, pu_name, record.config_id,
                record.first_entry_value_id, record.entry_value_count,
                record.input_count, record.output_count,
                record.parameter_count, record.parameter_policy, record.flags);
    }

    fprintf(file, "FHE Entry Value Table:\n");
    for (UINT32 i = 1; i <= header.entry_value_count; ++i) {
        DSL_FHE_ENTRY_VALUE_RECORD record;
        DSL_IR_VALUE_RECORD value;
        DSL_FHE_Get_Entry_Value(i, &record);
        DSL_IR_Image_Get_Value(record.value_id, &value);
        fprintf(file, "  [%u] entry=%u role=%s ordinal=%u class=%s "
                "encryption=%u", record.id, record.entry_contract_id,
                DSL_FHE_Entry_Role_Name(record.role), record.ordinal,
                DSL_FHE_Value_Class_Name(record.value_class),
                record.encryption_descriptor_id);
        DSL_FHE_Print_Value_Symbol(file, value);
        fprintf(file, " flags=0x%x\n", record.flags);
    }

    fprintf(file, "FHE Encryption Descriptor Table:\n");
    for (UINT32 i = 1; i <= header.encryption_descriptor_count; ++i) {
        DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD record;
        DSL_FHE_Get_Encryption_Descriptor(i, &record);
        fprintf(file, "  [%u] class=%s scheme=%s config=%u key_set=%s "
                "slots=%u encoding=%u packing=%u flags=0x%x\n", record.id,
                DSL_FHE_Value_Class_Name(record.value_class),
                DSL_FHE_Scheme_Name(record.scheme), record.config_id,
                record.key_set_name == STR_IDX_ZERO ? "" :
                    Index_To_Str(record.key_set_name),
                record.slot_count, record.encoding_policy,
                record.packing_policy, record.flags);
    }

    fprintf(file, "FHE Tensor Binding Table:\n");
    for (UINT32 i = 1; i <= header.tensor_binding_count; ++i) {
        DSL_FHE_TENSOR_BINDING_RECORD record;
        DSL_FHE_Get_Tensor_Binding(i, &record);
        fprintf(file, "  [%u] ty=%u type_name=%s encryption=%u flags=0x%x\n",
                record.id, (UINT32)record.tensor_ty,
                TY_name(record.tensor_ty), record.encryption_descriptor_id,
                record.flags);
    }

    fprintf(file, "FHE Key Requirement Table:\n");
    for (UINT32 i = 1; i <= header.key_requirement_count; ++i) {
        DSL_FHE_KEY_REQUIREMENT_RECORD record;
        DSL_FHE_Get_Key_Requirement(i, &record);
        fprintf(file, "  [%u] config=%u key_set=%s class=%s "
                "rotation=%d bootstrap_profile=%s flags=0x%x\n", record.id,
                record.config_id, Index_To_Str(record.key_set_name),
                DSL_FHE_Key_Class_Name(record.key_class),
                record.rotation_offset,
                record.bootstrap_profile == STR_IDX_ZERO ? "" :
                    Index_To_Str(record.bootstrap_profile), record.flags);
    }
}
