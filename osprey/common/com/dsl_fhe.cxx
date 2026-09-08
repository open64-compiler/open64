/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>

#include "dsl_fhe.h"
#include "pu_info.h"
#include "segmented_array.h"
#include "strtab.h"
#include "symtab.h"

typedef SEGMENTED_ARRAY<DSL_FHE_COMPILATION_CONFIG_RECORD>
    DSL_FHE_CONFIG_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_ENTRY_CONTRACT_RECORD>
    DSL_FHE_ENTRY_CONTRACT_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_ENTRY_VALUE_RECORD>
    DSL_FHE_ENTRY_VALUE_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD>
    DSL_FHE_ENCRYPTION_DESCRIPTOR_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_TENSOR_BINDING_RECORD>
    DSL_FHE_TENSOR_BINDING_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_KEY_REQUIREMENT_RECORD>
    DSL_FHE_KEY_REQUIREMENT_TABLE;

static DSL_FHE_CONFIG_TABLE DSL_fhe_config_table;
static DSL_FHE_ENTRY_CONTRACT_TABLE DSL_fhe_entry_contract_table;
static DSL_FHE_ENTRY_VALUE_TABLE DSL_fhe_entry_value_table;
static DSL_FHE_ENCRYPTION_DESCRIPTOR_TABLE
    DSL_fhe_encryption_descriptor_table;
static DSL_FHE_TENSOR_BINDING_TABLE DSL_fhe_tensor_binding_table;
static DSL_FHE_KEY_REQUIREMENT_TABLE DSL_fhe_key_requirement_table;

typedef struct {
    const DSL_FHE_IMAGE_HEADER *header;
    const DSL_FHE_COMPILATION_CONFIG_RECORD *configs;
    const DSL_FHE_ENTRY_CONTRACT_RECORD *entry_contracts;
    const DSL_FHE_ENTRY_VALUE_RECORD *entry_values;
    const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *encryption_descriptors;
    const DSL_FHE_TENSOR_BINDING_RECORD *tensor_bindings;
    const DSL_FHE_KEY_REQUIREMENT_RECORD *key_requirements;
} DSL_FHE_IMAGE_VIEW;

typedef char DSL_FHE_TY_IDX_Width_Check[sizeof(TY_IDX) == 4 ? 1 : -1];
typedef char DSL_FHE_ST_IDX_Width_Check[sizeof(ST_IDX) == 4 ? 1 : -1];
typedef char DSL_FHE_STR_IDX_Width_Check[sizeof(STR_IDX) == 8 ? 1 : -1];
typedef char DSL_FHE_Value_ID_Width_Check
    [sizeof(DSL_IR_VALUE_ID) == 4 ? 1 : -1];
typedef char DSL_FHE_Image_Header_Size_Check
    [sizeof(DSL_FHE_IMAGE_HEADER) == DSL_FHE_IMAGE_HEADER_SIZE ? 1 : -1];
typedef char DSL_FHE_Config_Size_Check
    [sizeof(DSL_FHE_COMPILATION_CONFIG_RECORD) ==
        DSL_FHE_CONFIG_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Entry_Contract_Size_Check
    [sizeof(DSL_FHE_ENTRY_CONTRACT_RECORD) ==
        DSL_FHE_ENTRY_CONTRACT_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Entry_Value_Size_Check
    [sizeof(DSL_FHE_ENTRY_VALUE_RECORD) ==
        DSL_FHE_ENTRY_VALUE_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Encryption_Descriptor_Size_Check
    [sizeof(DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD) ==
        DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Tensor_Binding_Size_Check
    [sizeof(DSL_FHE_TENSOR_BINDING_RECORD) ==
        DSL_FHE_TENSOR_BINDING_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Key_Requirement_Size_Check
    [sizeof(DSL_FHE_KEY_REQUIREMENT_RECORD) ==
        DSL_FHE_KEY_REQUIREMENT_RECORD_SIZE ? 1 : -1];

template <typename RECORD>
static void
DSL_FHE_Record_Init (RECORD *record)
{
    if (record != NULL)
        memset(record, 0, sizeof(RECORD));
}

template <typename TABLE, typename RECORD>
static BOOL
DSL_FHE_Table_Get (TABLE &table, UINT32 id, RECORD *record)
{
    if (id == 0 || id > table.Size())
        return FALSE;
    if (record != NULL)
        *record = table[id - 1];
    return TRUE;
}

static BOOL
DSL_FHE_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "FHE image error: %s id=%u\n", message, id);
    return FALSE;
}

static BOOL
DSL_FHE_String_Id_Valid (STR_IDX id, BOOL required)
{
    if (id == STR_IDX_ZERO)
        return !required;
    return id < STR_Table_Size();
}

static BOOL
DSL_FHE_PU_ST_Valid (ST_IDX st)
{
    return ST_IDX_level(st) == GLOBAL_SYMTAB && ST_IDX_index(st) != 0 &&
           ST_IDX_index(st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(st) == CLASS_FUNC;
}

static BOOL
DSL_FHE_Config_Valid (const DSL_FHE_COMPILATION_CONFIG_RECORD &record)
{
    return record.scheme == DSL_FHE_SCHEME_CKKS &&
           record.security_level == DSL_FHE_SECURITY_128_CLASSIC &&
           record.multiplicative_depth_policy >= DSL_FHE_POLICY_AUTO &&
           record.multiplicative_depth_policy <= DSL_FHE_POLICY_EXPLICIT &&
           record.slot_count_policy >= DSL_FHE_POLICY_AUTO &&
           record.slot_count_policy <= DSL_FHE_POLICY_EXPLICIT &&
           record.bootstrap_policy >= DSL_FHE_BOOTSTRAP_AUTO &&
           record.bootstrap_policy <= DSL_FHE_BOOTSTRAP_OFF &&
           record.backend_policy >= DSL_FHE_BACKEND_AUTO &&
           record.backend_policy <= DSL_FHE_BACKEND_MOCK &&
           record.reserved == 0;
}

static BOOL
DSL_FHE_Descriptor_Valid
        (const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD &record,
         UINT32 config_count)
{
    if (record.value_class < DSL_FHE_VALUE_CLASS_CIPHERTEXT ||
        record.value_class > DSL_FHE_VALUE_CLASS_CLEAR ||
        record.slot_count_policy < DSL_FHE_POLICY_AUTO ||
        record.slot_count_policy > DSL_FHE_POLICY_INHERIT ||
        record.encoding_policy < DSL_FHE_ENCODING_NONE ||
        record.encoding_policy > DSL_FHE_ENCODING_CKKS_PACKED ||
        record.packing_policy < DSL_FHE_PACKING_AUTO ||
        record.packing_policy > DSL_FHE_PACKING_INHERIT ||
        record.reserved0 != 0 || record.reserved1 != 0 ||
        record.reserved2 != 0)
        return FALSE;

    if (record.value_class == DSL_FHE_VALUE_CLASS_CLEAR)
        return record.scheme == DSL_FHE_SCHEME_UNKNOWN &&
               record.config_id == DSL_FHE_CONFIG_INVALID_ID &&
               record.key_set_name == STR_IDX_ZERO;

    return record.scheme == DSL_FHE_SCHEME_CKKS &&
           record.config_id != DSL_FHE_CONFIG_INVALID_ID &&
           record.config_id <= config_count &&
           DSL_FHE_String_Id_Valid
               (record.key_set_name,
                record.value_class == DSL_FHE_VALUE_CLASS_CIPHERTEXT);
}

static BOOL
DSL_FHE_Range_Valid (UINT32 first, UINT32 count, UINT32 limit)
{
    if (count == 0)
        return first == 0;
    return first != 0 && count <= limit && first <= limit - count + 1;
}

static BOOL
DSL_FHE_View_Validate (const DSL_FHE_IMAGE_VIEW *view, FILE *diagnostic)
{
    const DSL_FHE_IMAGE_HEADER &header = *view->header;
    const UINT32 required_capabilities =
        DSL_FHE_IMAGE_CAP_CONFIG |
        DSL_FHE_IMAGE_CAP_ENTRY_CONTRACT |
        DSL_FHE_IMAGE_CAP_ENTRY_VALUE |
        DSL_FHE_IMAGE_CAP_ENCRYPTION_DESCRIPTOR |
        DSL_FHE_IMAGE_CAP_TENSOR_BINDING |
        DSL_FHE_IMAGE_CAP_KEY_REQUIREMENT;
    UINT32 owned_entry_values = 0;

    if (header.magic != DSL_FHE_IMAGE_MAGIC ||
        header.version != DSL_FHE_IMAGE_VERSION ||
        header.header_size != DSL_FHE_IMAGE_HEADER_SIZE ||
        header.record_kind_count != DSL_FHE_IMAGE_RECORD_KEY_REQUIREMENT ||
        header.capabilities != required_capabilities || header.flags != 0 ||
        header.reserved0 != 0 || header.reserved1 != 0 ||
        header.reserved2 != 0 || header.reserved3 != 0)
        return DSL_FHE_Report(diagnostic, "invalid header", 0);

    for (UINT32 i = 0; i < header.config_count; ++i) {
        const DSL_FHE_COMPILATION_CONFIG_RECORD &record = view->configs[i];
        if (record.id != i + 1 || !DSL_FHE_Config_Valid(record))
            return DSL_FHE_Report(diagnostic, "invalid configuration", i + 1);
    }

    for (UINT32 i = 0; i < header.encryption_descriptor_count; ++i) {
        const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD &record =
            view->encryption_descriptors[i];
        if (record.id != i + 1 ||
            !DSL_FHE_Descriptor_Valid(record, header.config_count))
            return DSL_FHE_Report
                       (diagnostic, "invalid encryption descriptor", i + 1);
    }

    for (UINT32 i = 0; i < header.tensor_binding_count; ++i) {
        const DSL_FHE_TENSOR_BINDING_RECORD &record =
            view->tensor_bindings[i];
        UINT32 ty_index = TY_IDX_index(record.tensor_ty);
        if (record.id != i + 1 || ty_index == 0 || ty_index >= Ty_tab.Size() ||
            !TY_is_tensor_extension(record.tensor_ty) ||
            record.encryption_descriptor_id == 0 ||
            record.encryption_descriptor_id >
                header.encryption_descriptor_count ||
            record.reserved0 != 0 || record.reserved1 != 0)
            return DSL_FHE_Report(diagnostic, "invalid tensor binding", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (view->tensor_bindings[j].tensor_ty == record.tensor_ty &&
                view->tensor_bindings[j].encryption_descriptor_id ==
                    record.encryption_descriptor_id)
                return DSL_FHE_Report
                           (diagnostic, "duplicate tensor binding", i + 1);
        }
    }

    for (UINT32 i = 0; i < header.entry_value_count; ++i) {
        const DSL_FHE_ENTRY_VALUE_RECORD &record = view->entry_values[i];
        DSL_IR_VALUE_RECORD value;
        if (record.id != i + 1 || record.entry_contract_id == 0 ||
            record.entry_contract_id > header.entry_contract_count ||
            record.value_id == 0 ||
            !DSL_IR_Image_Get_Value(record.value_id, &value) ||
            record.role < DSL_FHE_ENTRY_VALUE_INPUT ||
            record.role > DSL_FHE_ENTRY_VALUE_PARAMETER ||
            record.value_class < DSL_FHE_VALUE_CLASS_CIPHERTEXT ||
            record.value_class > DSL_FHE_VALUE_CLASS_CLEAR ||
            record.encryption_descriptor_id == 0 ||
            record.encryption_descriptor_id >
                header.encryption_descriptor_count ||
            view->encryption_descriptors
                [record.encryption_descriptor_id - 1].value_class !=
                    record.value_class)
            return DSL_FHE_Report(diagnostic, "invalid entry value", i + 1);
    }

    for (UINT32 i = 0; i < header.entry_contract_count; ++i) {
        const DSL_FHE_ENTRY_CONTRACT_RECORD &record =
            view->entry_contracts[i];
        UINT32 inputs = 0;
        UINT32 outputs = 0;
        UINT32 parameters = 0;
        if (record.id != i + 1 ||
            !DSL_FHE_PU_ST_Valid(record.owner_pu_st) ||
            record.config_id == 0 || record.config_id > header.config_count ||
            !DSL_FHE_Range_Valid(record.first_entry_value_id,
                                 record.entry_value_count,
                                 header.entry_value_count) ||
            (record.entry_value_count != 0 &&
             record.first_entry_value_id != owned_entry_values + 1) ||
            record.parameter_policy < DSL_FHE_PARAMETER_POLICY_PLAINTEXT ||
            record.parameter_policy >
                DSL_FHE_PARAMETER_POLICY_ENCODED_PLAINTEXT ||
            record.reserved != 0)
            return DSL_FHE_Report(diagnostic, "invalid entry contract", i + 1);
        for (UINT32 j = 0; j < record.entry_value_count; ++j) {
            const DSL_FHE_ENTRY_VALUE_RECORD &value =
                view->entry_values[record.first_entry_value_id - 1 + j];
            if (value.entry_contract_id != record.id)
                return DSL_FHE_Report
                           (diagnostic, "entry value owner mismatch", value.id);
            if (value.role == DSL_FHE_ENTRY_VALUE_INPUT)
                ++inputs;
            else if (value.role == DSL_FHE_ENTRY_VALUE_OUTPUT)
                ++outputs;
            else if (value.role == DSL_FHE_ENTRY_VALUE_PARAMETER)
                ++parameters;
            UINT32 role_limit = value.role == DSL_FHE_ENTRY_VALUE_INPUT ?
                                record.input_count :
                                value.role == DSL_FHE_ENTRY_VALUE_OUTPUT ?
                                record.output_count : record.parameter_count;
            if (value.ordinal >= role_limit)
                return DSL_FHE_Report
                           (diagnostic, "entry ordinal out of range", value.id);
            for (UINT32 k = 0; k < j; ++k) {
                const DSL_FHE_ENTRY_VALUE_RECORD &prior =
                    view->entry_values[record.first_entry_value_id - 1 + k];
                if (prior.role == value.role && prior.ordinal == value.ordinal)
                    return DSL_FHE_Report
                               (diagnostic, "duplicate entry ordinal", value.id);
            }
        }
        if (inputs != record.input_count || outputs != record.output_count ||
            parameters != record.parameter_count)
            return DSL_FHE_Report
                       (diagnostic, "entry role count mismatch", record.id);
        owned_entry_values += record.entry_value_count;
    }
    if (owned_entry_values != header.entry_value_count)
        return DSL_FHE_Report(diagnostic, "unowned entry value", 0);

    for (UINT32 i = 0; i < header.key_requirement_count; ++i) {
        const DSL_FHE_KEY_REQUIREMENT_RECORD &record =
            view->key_requirements[i];
        if (record.id != i + 1 || record.config_id == 0 ||
            record.config_id > header.config_count ||
            !DSL_FHE_String_Id_Valid(record.key_set_name, TRUE) ||
            record.key_class < DSL_FHE_KEY_PUBLIC ||
            record.key_class > DSL_FHE_KEY_BOOTSTRAP ||
            (record.key_class != DSL_FHE_KEY_ROTATION &&
             record.rotation_offset != 0) ||
            (record.key_class != DSL_FHE_KEY_BOOTSTRAP &&
             record.bootstrap_profile != STR_IDX_ZERO) ||
            !DSL_FHE_String_Id_Valid(record.bootstrap_profile, FALSE) ||
            record.reserved0 != 0 || record.reserved1 != 0 ||
            record.reserved2 != 0)
            return DSL_FHE_Report(diagnostic, "invalid key requirement", i + 1);
    }
    return TRUE;
}

void
DSL_FHE_Image_Reset (void)
{
    DSL_fhe_config_table.Delete_down_to(0);
    DSL_fhe_entry_contract_table.Delete_down_to(0);
    DSL_fhe_entry_value_table.Delete_down_to(0);
    DSL_fhe_encryption_descriptor_table.Delete_down_to(0);
    DSL_fhe_tensor_binding_table.Delete_down_to(0);
    DSL_fhe_key_requirement_table.Delete_down_to(0);
}

void
DSL_FHE_Image_Get_Header (DSL_FHE_IMAGE_HEADER *header)
{
    if (header == NULL)
        return;
    memset(header, 0, sizeof(*header));
    header->magic = DSL_FHE_IMAGE_MAGIC;
    header->version = DSL_FHE_IMAGE_VERSION;
    header->header_size = DSL_FHE_IMAGE_HEADER_SIZE;
    header->record_kind_count = DSL_FHE_IMAGE_RECORD_KEY_REQUIREMENT;
    header->capabilities = DSL_FHE_IMAGE_CAP_CONFIG |
                           DSL_FHE_IMAGE_CAP_ENTRY_CONTRACT |
                           DSL_FHE_IMAGE_CAP_ENTRY_VALUE |
                           DSL_FHE_IMAGE_CAP_ENCRYPTION_DESCRIPTOR |
                           DSL_FHE_IMAGE_CAP_TENSOR_BINDING |
                           DSL_FHE_IMAGE_CAP_KEY_REQUIREMENT;
    header->config_count = DSL_fhe_config_table.Size();
    header->entry_contract_count = DSL_fhe_entry_contract_table.Size();
    header->entry_value_count = DSL_fhe_entry_value_table.Size();
    header->encryption_descriptor_count =
        DSL_fhe_encryption_descriptor_table.Size();
    header->tensor_binding_count = DSL_fhe_tensor_binding_table.Size();
    header->key_requirement_count = DSL_fhe_key_requirement_table.Size();
}

BOOL
DSL_FHE_Image_Has_Records (void)
{
    return DSL_fhe_config_table.Size() != 0 ||
           DSL_fhe_entry_contract_table.Size() != 0 ||
           DSL_fhe_entry_value_table.Size() != 0 ||
           DSL_fhe_encryption_descriptor_table.Size() != 0 ||
           DSL_fhe_tensor_binding_table.Size() != 0 ||
           DSL_fhe_key_requirement_table.Size() != 0;
}

BOOL
DSL_FHE_Image_Validate (FILE *diagnostic)
{
    DSL_FHE_IMAGE_HEADER header;
    DSL_FHE_Image_Get_Header(&header);
    DSL_FHE_COMPILATION_CONFIG_RECORD *configs =
        header.config_count == 0 ? NULL :
        new DSL_FHE_COMPILATION_CONFIG_RECORD[header.config_count];
    DSL_FHE_ENTRY_CONTRACT_RECORD *entry_contracts =
        header.entry_contract_count == 0 ? NULL :
        new DSL_FHE_ENTRY_CONTRACT_RECORD[header.entry_contract_count];
    DSL_FHE_ENTRY_VALUE_RECORD *entry_values =
        header.entry_value_count == 0 ? NULL :
        new DSL_FHE_ENTRY_VALUE_RECORD[header.entry_value_count];
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *encryption_descriptors =
        header.encryption_descriptor_count == 0 ? NULL :
        new DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD
            [header.encryption_descriptor_count];
    DSL_FHE_TENSOR_BINDING_RECORD *tensor_bindings =
        header.tensor_binding_count == 0 ? NULL :
        new DSL_FHE_TENSOR_BINDING_RECORD[header.tensor_binding_count];
    DSL_FHE_KEY_REQUIREMENT_RECORD *key_requirements =
        header.key_requirement_count == 0 ? NULL :
        new DSL_FHE_KEY_REQUIREMENT_RECORD[header.key_requirement_count];
    for (UINT32 i = 0; i < header.config_count; ++i)
        configs[i] = DSL_fhe_config_table[i];
    for (UINT32 i = 0; i < header.entry_contract_count; ++i)
        entry_contracts[i] = DSL_fhe_entry_contract_table[i];
    for (UINT32 i = 0; i < header.entry_value_count; ++i)
        entry_values[i] = DSL_fhe_entry_value_table[i];
    for (UINT32 i = 0; i < header.encryption_descriptor_count; ++i)
        encryption_descriptors[i] =
            DSL_fhe_encryption_descriptor_table[i];
    for (UINT32 i = 0; i < header.tensor_binding_count; ++i)
        tensor_bindings[i] = DSL_fhe_tensor_binding_table[i];
    for (UINT32 i = 0; i < header.key_requirement_count; ++i)
        key_requirements[i] = DSL_fhe_key_requirement_table[i];

    DSL_FHE_IMAGE_VIEW view;
    view.header = &header;
    view.configs = configs;
    view.entry_contracts = entry_contracts;
    view.entry_values = entry_values;
    view.encryption_descriptors = encryption_descriptors;
    view.tensor_bindings = tensor_bindings;
    view.key_requirements = key_requirements;
    BOOL valid = DSL_FHE_View_Validate(&view, diagnostic);

    delete [] key_requirements;
    delete [] tensor_bindings;
    delete [] encryption_descriptors;
    delete [] entry_values;
    delete [] entry_contracts;
    delete [] configs;
    return valid;
}

static BOOL
DSL_FHE_Add_Section_Size (UINT64 *size, UINT32 count, UINT32 record_size)
{
    const UINT64 max_size = (UINT64)-1;
    if (count != 0 && count > (max_size - *size) / record_size)
        return FALSE;
    *size += (UINT64)count * record_size;
    return TRUE;
}

BOOL
DSL_FHE_Image_Load_Mapped
        (const void *section_base, UINT64 section_size, FILE *diagnostic)
{
    if (section_base == NULL || section_size < DSL_FHE_IMAGE_HEADER_SIZE)
        return DSL_FHE_Report(diagnostic, "section is truncated", 0);
    const char *cursor = (const char *)section_base;
    const DSL_FHE_IMAGE_HEADER *header =
        (const DSL_FHE_IMAGE_HEADER *)cursor;
    UINT64 expected_size = DSL_FHE_IMAGE_HEADER_SIZE;
    if (!DSL_FHE_Add_Section_Size
             (&expected_size, header->config_count,
              DSL_FHE_CONFIG_RECORD_SIZE) ||
        !DSL_FHE_Add_Section_Size
             (&expected_size, header->entry_contract_count,
              DSL_FHE_ENTRY_CONTRACT_RECORD_SIZE) ||
        !DSL_FHE_Add_Section_Size
             (&expected_size, header->entry_value_count,
              DSL_FHE_ENTRY_VALUE_RECORD_SIZE) ||
        !DSL_FHE_Add_Section_Size
             (&expected_size, header->encryption_descriptor_count,
              DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD_SIZE) ||
        !DSL_FHE_Add_Section_Size
             (&expected_size, header->tensor_binding_count,
              DSL_FHE_TENSOR_BINDING_RECORD_SIZE) ||
        !DSL_FHE_Add_Section_Size
             (&expected_size, header->key_requirement_count,
              DSL_FHE_KEY_REQUIREMENT_RECORD_SIZE) ||
        expected_size != section_size)
        return DSL_FHE_Report(diagnostic, "section size mismatch", 0);

    DSL_FHE_IMAGE_VIEW view;
    view.header = header;
    cursor += DSL_FHE_IMAGE_HEADER_SIZE;
    view.configs = (const DSL_FHE_COMPILATION_CONFIG_RECORD *)cursor;
    cursor += (UINT64)header->config_count * DSL_FHE_CONFIG_RECORD_SIZE;
    view.entry_contracts =
        (const DSL_FHE_ENTRY_CONTRACT_RECORD *)cursor;
    cursor += (UINT64)header->entry_contract_count *
              DSL_FHE_ENTRY_CONTRACT_RECORD_SIZE;
    view.entry_values = (const DSL_FHE_ENTRY_VALUE_RECORD *)cursor;
    cursor += (UINT64)header->entry_value_count *
              DSL_FHE_ENTRY_VALUE_RECORD_SIZE;
    view.encryption_descriptors =
        (const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *)cursor;
    cursor += (UINT64)header->encryption_descriptor_count *
              DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD_SIZE;
    view.tensor_bindings = (const DSL_FHE_TENSOR_BINDING_RECORD *)cursor;
    cursor += (UINT64)header->tensor_binding_count *
              DSL_FHE_TENSOR_BINDING_RECORD_SIZE;
    view.key_requirements =
        (const DSL_FHE_KEY_REQUIREMENT_RECORD *)cursor;

    if (!DSL_FHE_View_Validate(&view, diagnostic))
        return FALSE;

    DSL_FHE_Image_Reset();
    if (header->config_count != 0)
        DSL_fhe_config_table.Insert(view.configs, header->config_count);
    if (header->entry_contract_count != 0)
        DSL_fhe_entry_contract_table.Insert
            (view.entry_contracts, header->entry_contract_count);
    if (header->entry_value_count != 0)
        DSL_fhe_entry_value_table.Insert
            (view.entry_values, header->entry_value_count);
    if (header->encryption_descriptor_count != 0)
        DSL_fhe_encryption_descriptor_table.Insert
            (view.encryption_descriptors,
             header->encryption_descriptor_count);
    if (header->tensor_binding_count != 0)
        DSL_fhe_tensor_binding_table.Insert
            (view.tensor_bindings, header->tensor_binding_count);
    if (header->key_requirement_count != 0)
        DSL_fhe_key_requirement_table.Insert
            (view.key_requirements, header->key_requirement_count);
    return TRUE;
}

void DSL_FHE_Compilation_Config_Record_Init
        (DSL_FHE_COMPILATION_CONFIG_RECORD *record)
{ DSL_FHE_Record_Init(record); }
void DSL_FHE_Entry_Contract_Record_Init
        (DSL_FHE_ENTRY_CONTRACT_RECORD *record)
{ DSL_FHE_Record_Init(record); }
void DSL_FHE_Entry_Value_Record_Init
        (DSL_FHE_ENTRY_VALUE_RECORD *record)
{ DSL_FHE_Record_Init(record); }
void DSL_FHE_Encryption_Descriptor_Record_Init
        (DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record)
{ DSL_FHE_Record_Init(record); }
void DSL_FHE_Tensor_Binding_Record_Init
        (DSL_FHE_TENSOR_BINDING_RECORD *record)
{ DSL_FHE_Record_Init(record); }
void DSL_FHE_Key_Requirement_Record_Init
        (DSL_FHE_KEY_REQUIREMENT_RECORD *record)
{ DSL_FHE_Record_Init(record); }

template <typename RECORD>
static BOOL
DSL_FHE_Equivalent_Record (const RECORD &left, const RECORD &right)
{
    RECORD left_copy = left;
    RECORD right_copy = right;
    left_copy.id = 0;
    right_copy.id = 0;
    return memcmp(&left_copy, &right_copy, sizeof(RECORD)) == 0;
}

DSL_FHE_CONFIG_ID
DSL_FHE_Intern_Compilation_Config
        (const DSL_FHE_COMPILATION_CONFIG_RECORD *record)
{
    if (record == NULL || !DSL_FHE_Config_Valid(*record))
        return DSL_FHE_CONFIG_INVALID_ID;
    for (UINT32 i = 0; i < DSL_fhe_config_table.Size(); ++i) {
        if (DSL_FHE_Equivalent_Record(DSL_fhe_config_table[i], *record))
            return i + 1;
    }
    DSL_FHE_COMPILATION_CONFIG_RECORD copy = *record;
    UINT32 index = DSL_fhe_config_table.Insert(copy);
    DSL_fhe_config_table[index].id = index + 1;
    return index + 1;
}

DSL_FHE_ENCRYPTION_DESCRIPTOR_ID
DSL_FHE_Intern_Encryption_Descriptor
        (const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record)
{
    if (record == NULL ||
        !DSL_FHE_Descriptor_Valid(*record, DSL_fhe_config_table.Size()))
        return DSL_FHE_ENCRYPTION_DESCRIPTOR_INVALID_ID;
    for (UINT32 i = 0;
         i < DSL_fhe_encryption_descriptor_table.Size(); ++i) {
        if (DSL_FHE_Equivalent_Record
                (DSL_fhe_encryption_descriptor_table[i], *record))
            return i + 1;
    }
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD copy = *record;
    UINT32 index = DSL_fhe_encryption_descriptor_table.Insert(copy);
    DSL_fhe_encryption_descriptor_table[index].id = index + 1;
    return index + 1;
}

DSL_FHE_TENSOR_BINDING_ID
DSL_FHE_Intern_Tensor_Binding
        (TY_IDX tensor_ty,
         DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor_id,
         UINT32 flags)
{
    if (TY_IDX_index(tensor_ty) == 0 ||
        TY_IDX_index(tensor_ty) >= Ty_tab.Size() ||
        !TY_is_tensor_extension(tensor_ty) || descriptor_id == 0 ||
        descriptor_id > DSL_fhe_encryption_descriptor_table.Size())
        return DSL_FHE_TENSOR_BINDING_INVALID_ID;
    for (UINT32 i = 0; i < DSL_fhe_tensor_binding_table.Size(); ++i) {
        const DSL_FHE_TENSOR_BINDING_RECORD &record =
            DSL_fhe_tensor_binding_table[i];
        if (record.tensor_ty == tensor_ty &&
            record.encryption_descriptor_id == descriptor_id &&
            record.flags == flags)
            return i + 1;
    }
    DSL_FHE_TENSOR_BINDING_RECORD record;
    DSL_FHE_Tensor_Binding_Record_Init(&record);
    record.tensor_ty = tensor_ty;
    record.encryption_descriptor_id = descriptor_id;
    record.flags = flags;
    UINT32 index = DSL_fhe_tensor_binding_table.Insert(record);
    DSL_fhe_tensor_binding_table[index].id = index + 1;
    return index + 1;
}

DSL_FHE_ENTRY_CONTRACT_ID
DSL_FHE_Add_Entry_Contract (const DSL_FHE_ENTRY_CONTRACT_RECORD *record)
{
    if (record == NULL || !DSL_FHE_PU_ST_Valid(record->owner_pu_st) ||
        record->config_id == 0 ||
        record->config_id > DSL_fhe_config_table.Size())
        return DSL_FHE_ENTRY_CONTRACT_INVALID_ID;
    DSL_FHE_ENTRY_CONTRACT_RECORD copy = *record;
    UINT32 index = DSL_fhe_entry_contract_table.Insert(copy);
    DSL_fhe_entry_contract_table[index].id = index + 1;
    return index + 1;
}

DSL_FHE_ENTRY_VALUE_ID
DSL_FHE_Add_Entry_Value (const DSL_FHE_ENTRY_VALUE_RECORD *record)
{
    DSL_IR_VALUE_RECORD value;
    if (record == NULL || record->entry_contract_id == 0 ||
        record->entry_contract_id > DSL_fhe_entry_contract_table.Size() ||
        record->value_id == 0 ||
        !DSL_IR_Image_Get_Value(record->value_id, &value) ||
        record->role < DSL_FHE_ENTRY_VALUE_INPUT ||
        record->role > DSL_FHE_ENTRY_VALUE_PARAMETER ||
        record->value_class < DSL_FHE_VALUE_CLASS_CIPHERTEXT ||
        record->value_class > DSL_FHE_VALUE_CLASS_CLEAR ||
        record->encryption_descriptor_id == 0 ||
        record->encryption_descriptor_id >
            DSL_fhe_encryption_descriptor_table.Size() ||
        DSL_fhe_encryption_descriptor_table
            [record->encryption_descriptor_id - 1].value_class !=
                record->value_class)
        return DSL_FHE_ENTRY_VALUE_INVALID_ID;
    DSL_FHE_ENTRY_VALUE_RECORD copy = *record;
    UINT32 index = DSL_fhe_entry_value_table.Insert(copy);
    DSL_fhe_entry_value_table[index].id = index + 1;
    return index + 1;
}

BOOL
DSL_FHE_Set_Entry_Value_Range
        (DSL_FHE_ENTRY_CONTRACT_ID entry_contract_id,
         DSL_FHE_ENTRY_VALUE_ID first_entry_value_id,
         UINT32 entry_value_count)
{
    if (entry_contract_id == 0 ||
        entry_contract_id > DSL_fhe_entry_contract_table.Size() ||
        !DSL_FHE_Range_Valid(first_entry_value_id, entry_value_count,
                             DSL_fhe_entry_value_table.Size()))
        return FALSE;
    DSL_FHE_ENTRY_CONTRACT_RECORD &entry =
        DSL_fhe_entry_contract_table[entry_contract_id - 1];
    entry.first_entry_value_id = first_entry_value_id;
    entry.entry_value_count = entry_value_count;
    return TRUE;
}

DSL_FHE_KEY_REQUIREMENT_ID
DSL_FHE_Intern_Key_Requirement
        (const DSL_FHE_KEY_REQUIREMENT_RECORD *record)
{
    if (record == NULL || record->config_id == 0 ||
        record->config_id > DSL_fhe_config_table.Size() ||
        !DSL_FHE_String_Id_Valid(record->key_set_name, TRUE) ||
        record->key_class < DSL_FHE_KEY_PUBLIC ||
        record->key_class > DSL_FHE_KEY_BOOTSTRAP ||
        (record->key_class != DSL_FHE_KEY_ROTATION &&
         record->rotation_offset != 0) ||
        (record->key_class != DSL_FHE_KEY_BOOTSTRAP &&
         record->bootstrap_profile != STR_IDX_ZERO) ||
        !DSL_FHE_String_Id_Valid(record->bootstrap_profile, FALSE) ||
        record->reserved0 != 0 || record->reserved1 != 0 ||
        record->reserved2 != 0)
        return DSL_FHE_KEY_REQUIREMENT_INVALID_ID;
    for (UINT32 i = 0; i < DSL_fhe_key_requirement_table.Size(); ++i) {
        if (DSL_FHE_Equivalent_Record
                (DSL_fhe_key_requirement_table[i], *record))
            return i + 1;
    }
    DSL_FHE_KEY_REQUIREMENT_RECORD copy = *record;
    UINT32 index = DSL_fhe_key_requirement_table.Insert(copy);
    DSL_fhe_key_requirement_table[index].id = index + 1;
    return index + 1;
}

UINT32 DSL_FHE_Config_Count (void)
{ return DSL_fhe_config_table.Size(); }
UINT32 DSL_FHE_Entry_Contract_Count (void)
{ return DSL_fhe_entry_contract_table.Size(); }
UINT32 DSL_FHE_Entry_Value_Count (void)
{ return DSL_fhe_entry_value_table.Size(); }
UINT32 DSL_FHE_Encryption_Descriptor_Count (void)
{ return DSL_fhe_encryption_descriptor_table.Size(); }
UINT32 DSL_FHE_Tensor_Binding_Count (void)
{ return DSL_fhe_tensor_binding_table.Size(); }
UINT32 DSL_FHE_Key_Requirement_Count (void)
{ return DSL_fhe_key_requirement_table.Size(); }

BOOL DSL_FHE_Get_Compilation_Config
        (DSL_FHE_CONFIG_ID id, DSL_FHE_COMPILATION_CONFIG_RECORD *record)
{ return DSL_FHE_Table_Get(DSL_fhe_config_table, id, record); }
BOOL DSL_FHE_Get_Entry_Contract
        (DSL_FHE_ENTRY_CONTRACT_ID id, DSL_FHE_ENTRY_CONTRACT_RECORD *record)
{ return DSL_FHE_Table_Get(DSL_fhe_entry_contract_table, id, record); }
BOOL DSL_FHE_Get_Entry_Value
        (DSL_FHE_ENTRY_VALUE_ID id, DSL_FHE_ENTRY_VALUE_RECORD *record)
{ return DSL_FHE_Table_Get(DSL_fhe_entry_value_table, id, record); }
BOOL DSL_FHE_Get_Encryption_Descriptor
        (DSL_FHE_ENCRYPTION_DESCRIPTOR_ID id,
         DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record)
{ return DSL_FHE_Table_Get(DSL_fhe_encryption_descriptor_table, id, record); }
BOOL DSL_FHE_Get_Tensor_Binding
        (DSL_FHE_TENSOR_BINDING_ID id, DSL_FHE_TENSOR_BINDING_RECORD *record)
{ return DSL_FHE_Table_Get(DSL_fhe_tensor_binding_table, id, record); }

BOOL
DSL_FHE_Find_Tensor_Binding
        (TY_IDX tensor_ty,
         DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor_id,
         DSL_FHE_TENSOR_BINDING_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_fhe_tensor_binding_table.Size(); ++i) {
        if (DSL_fhe_tensor_binding_table[i].tensor_ty == tensor_ty &&
            DSL_fhe_tensor_binding_table[i].encryption_descriptor_id ==
                descriptor_id)
            return DSL_FHE_Table_Get
                       (DSL_fhe_tensor_binding_table, i + 1, record);
    }
    return FALSE;
}

BOOL DSL_FHE_Get_Key_Requirement
        (DSL_FHE_KEY_REQUIREMENT_ID id,
         DSL_FHE_KEY_REQUIREMENT_RECORD *record)
{ return DSL_FHE_Table_Get(DSL_fhe_key_requirement_table, id, record); }

DSL_FHE_TENSOR_BINDING_ID
DSL_Builder_Bind_FHE_Tensor_Descriptor
        (TY_IDX tensor_ty,
         DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor_id,
         UINT32 flags)
{
    return DSL_FHE_Intern_Tensor_Binding(tensor_ty, descriptor_id, flags);
}

DSL_FHE_ENTRY_CONTRACT_ID
DSL_Builder_Attach_FHE_Entry_Contract
        (DSL_BUILDER_PROGRAM_UNIT pu,
         const DSL_FHE_ENTRY_CONTRACT_INFO *info)
{
    if (pu == NULL || info == NULL)
        return DSL_FHE_ENTRY_CONTRACT_INVALID_ID;
    DSL_FHE_ENTRY_CONTRACT_RECORD record;
    DSL_FHE_Entry_Contract_Record_Init(&record);
    record.owner_pu_st = PU_Info_proc_sym(pu);
    record.config_id = info->config_id;
    record.input_count = info->input_count;
    record.output_count = info->output_count;
    record.parameter_count = info->parameter_count;
    record.encrypted_io_policy = info->encrypted_io_policy;
    record.parameter_policy = info->parameter_policy;
    record.flags = info->flags;
    return DSL_FHE_Add_Entry_Contract(&record);
}

DSL_FHE_ENTRY_VALUE_ID
DSL_Builder_Declare_FHE_Entry_Value
        (DSL_FHE_ENTRY_CONTRACT_ID entry_contract_id,
         DSL_BUILDER_VALUE value,
         UINT32 ordinal,
         DSL_FHE_ENTRY_VALUE_ROLE role,
         const DSL_FHE_ENTRY_VALUE_INFO *info)
{
    if (entry_contract_id == 0 ||
        entry_contract_id > DSL_fhe_entry_contract_table.Size() ||
        value == NULL || info == NULL)
        return DSL_FHE_ENTRY_VALUE_INVALID_ID;
    DSL_IR_VALUE_ID value_id = DSL_Builder_Get_Value_Image_Id(value);
    TY_IDX tensor_ty = DSL_Builder_Get_Value_Type(value);
    DSL_FHE_TENSOR_BINDING_RECORD binding;
    if (value_id == DSL_IR_VALUE_INVALID_ID ||
        !DSL_FHE_Find_Tensor_Binding
             (tensor_ty, info->encryption_descriptor_id, &binding))
        return DSL_FHE_ENTRY_VALUE_INVALID_ID;

    DSL_FHE_ENTRY_CONTRACT_RECORD &entry =
        DSL_fhe_entry_contract_table[entry_contract_id - 1];
    UINT32 role_count = 0;
    for (UINT32 i = 0; i < entry.entry_value_count; ++i) {
        if (DSL_fhe_entry_value_table
                [entry.first_entry_value_id - 1 + i].role == (UINT32)role)
            ++role_count;
    }
    UINT32 expected_role_count = role == DSL_FHE_ENTRY_VALUE_INPUT ?
                                 entry.input_count :
                                 role == DSL_FHE_ENTRY_VALUE_OUTPUT ?
                                 entry.output_count :
                                 role == DSL_FHE_ENTRY_VALUE_PARAMETER ?
                                 entry.parameter_count : 0;
    if (expected_role_count == 0 || role_count >= expected_role_count)
        return DSL_FHE_ENTRY_VALUE_INVALID_ID;
    UINT32 next_id = DSL_fhe_entry_value_table.Size() + 1;
    if (entry.entry_value_count != 0 &&
        next_id != entry.first_entry_value_id + entry.entry_value_count)
        return DSL_FHE_ENTRY_VALUE_INVALID_ID;
    if (entry.entry_value_count == 0)
        entry.first_entry_value_id = next_id;

    DSL_FHE_ENTRY_VALUE_RECORD record;
    DSL_FHE_Entry_Value_Record_Init(&record);
    record.entry_contract_id = entry_contract_id;
    record.value_id = value_id;
    record.ordinal = ordinal;
    record.role = role;
    record.value_class = info->value_class;
    record.encryption_descriptor_id = info->encryption_descriptor_id;
    record.flags = info->flags;
    DSL_FHE_ENTRY_VALUE_ID id = DSL_FHE_Add_Entry_Value(&record);
    if (id == DSL_FHE_ENTRY_VALUE_INVALID_ID)
        return id;
    ++entry.entry_value_count;
    return id;
}

BOOL
DSL_Builder_Get_FHE_Value_Encryption_Descriptor
        (DSL_BUILDER_VALUE value,
         DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record)
{
    DSL_IR_VALUE_ID value_id = DSL_Builder_Get_Value_Image_Id(value);
    if (value_id == DSL_IR_VALUE_INVALID_ID)
        return FALSE;
    for (UINT32 i = 0; i < DSL_fhe_entry_value_table.Size(); ++i) {
        const DSL_FHE_ENTRY_VALUE_RECORD &entry_value =
            DSL_fhe_entry_value_table[i];
        if (entry_value.value_id == value_id)
            return DSL_FHE_Get_Encryption_Descriptor
                       (entry_value.encryption_descriptor_id, record);
    }
    return FALSE;
}
