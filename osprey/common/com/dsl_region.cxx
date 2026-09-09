/*
 * Copyright (C) 2026 Open64 Project
 */

#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <vector>

#include "dsl_region.h"
#include "dsl_ir_image.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"
#include "strtab.h"
#include "symtab.h"

typedef char DSL_Region_Header_Size_Check
    [sizeof(DSL_REGION_IMAGE_HEADER) == 24 ? 1 : -1];
typedef char DSL_Region_Record_Size_Check
    [sizeof(DSL_REGION_IMAGE_RECORD) == 40 ? 1 : -1];
typedef char DSL_Region_Interface_Size_Check
    [sizeof(DSL_REGION_INTERFACE_RECORD) == 24 ? 1 : -1];

struct dsl_region_runtime {
    DSL_REGION_IMAGE_RECORD image;
    WN *wn;
};

typedef struct {
    PU_Info *pu;
    std::vector<dsl_region_runtime *> regions;
    std::vector<DSL_REGION_INTERFACE_RECORD> interfaces;
} DSL_REGION_STORE;

static std::vector<DSL_REGION_STORE *> DSL_region_stores;

static DSL_REGION_STORE *
DSL_Region_Find_Store (PU_Info *pu)
{
    for (UINT32 i = 0; i < DSL_region_stores.size(); ++i) {
        if (DSL_region_stores[i]->pu == pu)
            return DSL_region_stores[i];
    }
    return NULL;
}

static DSL_REGION_STORE *
DSL_Region_Get_Store (PU_Info *pu, BOOL create)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (store != NULL || !create)
        return store;

    store = new DSL_REGION_STORE;
    store->pu = pu;
    DSL_region_stores.push_back(store);
    Set_PU_Info_regions_ptr(pu, NULL);
    Set_PU_Info_state(pu, WT_REGIONS, Subsect_InMem);
    return store;
}

static DSL_REGION_STORE *
DSL_Region_Find_Owning_Store (DSL_REGION region)
{
    if (region == NULL)
        return NULL;
    for (UINT32 i = 0; i < DSL_region_stores.size(); ++i) {
        for (UINT32 j = 0; j < DSL_region_stores[i]->regions.size(); ++j) {
            if (DSL_region_stores[i]->regions[j] == region)
                return DSL_region_stores[i];
        }
    }
    return NULL;
}

static const char *
DSL_Region_Metadata_Value (const dsl_region_runtime &region,
                           const char *key)
{
    const char *prefix = WN_DSL_Comment_Prefix();
    const char *domain = "region_metadata:";
    const size_t prefix_length = strlen(prefix);
    const size_t domain_length = strlen(domain);
    const size_t key_length = key == NULL ? 0 : strlen(key);

    for (WN *wn = WN_first(WN_region_pragmas(region.wn)); wn != NULL;
         wn = WN_next(wn)) {
        if (!WN_Is_DSL_Comment(wn))
            continue;
        const char *comment = Index_To_Str(WN_GetComment(wn));
        const char *cursor = comment + prefix_length;
        if (strncmp(cursor, domain, domain_length) != 0)
            continue;
        cursor += domain_length;
        if (strncmp(cursor, key, key_length) == 0 &&
            cursor[key_length] == ':')
            return cursor + key_length + 1;
    }
    return NULL;
}

void
DSL_Region_Reset (void)
{
    for (UINT32 i = 0; i < DSL_region_stores.size(); ++i) {
        for (UINT32 j = 0; j < DSL_region_stores[i]->regions.size(); ++j)
            delete DSL_region_stores[i]->regions[j];
        delete DSL_region_stores[i];
    }
    DSL_region_stores.clear();
}

DSL_REGION
DSL_Region_Create (PU_Info *pu, DSL_REGION parent,
                   const char *contract_name, UINT32 contract_version)
{
    if (pu == NULL || contract_name == NULL || contract_name[0] == '\0' ||
        contract_version == 0 || PU_Info_state(pu, WT_TREE) != Subsect_InMem)
        return NULL;

    DSL_REGION_STORE *store = DSL_Region_Get_Store(pu, TRUE);
    WN *body = WN_CreateBlock();
    WN *pragmas = WN_CreateBlock();
    WN *exits = WN_CreateBlock();
    WN *classifier = WN_CreatePragma
                         (WN_PRAGMA_OPAQUE, ST_IDX_ZERO,
                          (INT32)contract_version, 0);
    WN_INSERT_BlockLast(pragmas, classifier);
    WN *region_wn = WN_CreateRegion
                        (REGION_KIND_PRAGMA, body, pragmas, exits, -1,
                         INITO_IDX_ZERO);

    dsl_region_runtime *runtime = new dsl_region_runtime;
    memset (&runtime->image, 0, sizeof(runtime->image));
    runtime->image.region_id = WN_region_id(region_wn);
    runtime->image.parent_region_id = parent == NULL ? 0 :
                                      parent->image.region_id;
    runtime->image.kind = REGION_KIND_PRAGMA;
    runtime->image.depth = parent == NULL ? 1 : parent->image.depth + 1;
    runtime->image.contract_version = contract_version;
    runtime->image.contract_name = Save_Str(contract_name);
    runtime->wn = region_wn;
    store->regions.push_back(runtime);
    return runtime;
}

BOOL
DSL_Region_Append_Statement (DSL_REGION region, WN *statement)
{
    if (region == NULL || statement == NULL ||
        WN_operator(statement) == OPR_BLOCK)
        return FALSE;
    WN_INSERT_BlockLast(WN_region_body(region->wn), statement);
    return TRUE;
}

BOOL
DSL_Region_Append_Child (DSL_REGION parent, DSL_REGION child)
{
    DSL_REGION_STORE *parent_store = DSL_Region_Find_Owning_Store(parent);
    DSL_REGION_STORE *child_store = DSL_Region_Find_Owning_Store(child);
    if (parent_store == NULL || parent_store != child_store ||
        child->image.parent_region_id != parent->image.region_id)
        return FALSE;

    for (WN *wn = WN_first(WN_region_body(parent->wn)); wn != NULL;
         wn = WN_next(wn)) {
        if (wn == child->wn)
            return FALSE;
    }
    WN_INSERT_BlockLast(WN_region_body(parent->wn), child->wn);
    return TRUE;
}

BOOL
DSL_Region_Append_To_PU (DSL_REGION region)
{
    if (region == NULL)
        return FALSE;
    DSL_REGION_STORE *store = DSL_Region_Find_Owning_Store(region);
    if (store == NULL)
        return FALSE;
    WN *entry = PU_Info_tree_ptr(store->pu);
    WN_INSERT_BlockLast(WN_func_body(entry), region->wn);
    return TRUE;
}

BOOL
DSL_Region_Set_Metadata (DSL_REGION region, const char *key,
                         const char *value)
{
    if (region == NULL || key == NULL || key[0] == '\0' || value == NULL ||
        DSL_Region_Metadata_Value(*region, key) != NULL)
        return FALSE;
    for (const char *cursor = key; *cursor != '\0'; ++cursor) {
        if (!isalnum((unsigned char)*cursor) && *cursor != '_' &&
            *cursor != '.' && *cursor != '-')
            return FALSE;
    }

    WN *metadata = WN_Create_DSL_Comment("region_metadata", key, value);
    WN_Set_Linenum(metadata, WN_Get_Linenum(region->wn));
    WN_INSERT_BlockLast(WN_region_pragmas(region->wn), metadata);
    return TRUE;
}

BOOL
DSL_Region_Declare_Symbol (DSL_REGION region, ST_IDX st, UINT32 roles,
                           UINT32 ordinal, UINT32 flags)
{
    if (region == NULL || ST_IDX_index(st) == 0 || roles == 0 ||
        (roles & ~(DSL_REGION_VALUE_INPUT | DSL_REGION_VALUE_OUTPUT |
                   DSL_REGION_VALUE_INOUT | DSL_REGION_VALUE_RESULT)) != 0)
        return FALSE;

    DSL_REGION_STORE *store = DSL_Region_Find_Owning_Store(region);
    if (store == NULL)
        return FALSE;

    DSL_REGION_INTERFACE_RECORD record;
    memset (&record, 0, sizeof(record));
    record.region_id = region->image.region_id;
    record.ordinal = ordinal;
    record.st = st;
    record.roles = roles;
    record.flags = flags;
    store->interfaces.push_back(record);
    return TRUE;
}

BOOL
DSL_Region_Set_Source_Position (DSL_REGION region, SRCPOS spos)
{
    if (region == NULL)
        return FALSE;
    WN_Set_Linenum(region->wn, spos);
    WN_Set_Linenum(WN_region_body(region->wn), spos);
    WN_Set_Linenum(WN_region_pragmas(region->wn), spos);
    WN_Set_Linenum(WN_region_exits(region->wn), spos);
    for (WN *wn = WN_first(WN_region_pragmas(region->wn)); wn != NULL;
         wn = WN_next(wn))
        WN_Set_Linenum(wn, spos);
    return TRUE;
}

WN *
DSL_Region_WN (DSL_REGION region)
{
    return region == NULL ? NULL : region->wn;
}

BOOL
DSL_Region_Is_Managed_WN (PU_Info *pu, const WN *wn)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (store == NULL || wn == NULL)
        return FALSE;
    for (UINT32 i = 0; i < store->regions.size(); ++i) {
        if (store->regions[i]->wn == wn)
            return TRUE;
    }
    return FALSE;
}

BOOL
DSL_Region_Consume_WN (PU_Info *pu, const WN *wn)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (store == NULL || wn == NULL)
        return FALSE;

    UINT32 region_index = store->regions.size();
    UINT32 region_id = 0;
    for (UINT32 i = 0; i < store->regions.size(); ++i) {
        if (store->regions[i]->wn == wn) {
            region_index = i;
            region_id = store->regions[i]->image.region_id;
            break;
        }
    }
    if (region_index == store->regions.size())
        return FALSE;

    delete store->regions[region_index];
    store->regions.erase(store->regions.begin() + region_index);
    for (UINT32 i = 0; i < store->interfaces.size(); ) {
        if (store->interfaces[i].region_id == region_id)
            store->interfaces.erase(store->interfaces.begin() + i);
        else
            ++i;
    }
    if (!store->regions.empty())
        return TRUE;

    for (UINT32 i = 0; i < DSL_region_stores.size(); ++i) {
        if (DSL_region_stores[i] == store) {
            DSL_region_stores.erase(DSL_region_stores.begin() + i);
            break;
        }
    }
    delete store;
    Set_PU_Info_regions_ptr(pu, NULL);
    Set_PU_Info_state(pu, WT_REGIONS, Subsect_Missing);
    return TRUE;
}

static BOOL
DSL_Region_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf (diagnostic, "DSL region error: %s id=%u\n", message, id);
    return FALSE;
}

static BOOL
DSL_Region_Vector_Has_ST (const std::vector<ST_IDX> &values, ST_IDX st)
{
    for (UINT32 i = 0; i < values.size(); ++i) {
        if (values[i] == st)
            return TRUE;
    }
    return FALSE;
}

static void
DSL_Region_Collect_Definitions (WN *block, std::vector<ST_IDX> *definitions)
{
    for (WN *stmt = WN_first(block); stmt != NULL; stmt = WN_next(stmt)) {
        if (WN_operator(stmt) == OPR_STID)
            definitions->push_back(WN_st_idx(stmt));
        else if (WN_operator(stmt) == OPR_REGION)
            DSL_Region_Collect_Definitions(WN_region_body(stmt), definitions);
    }
}

static void
DSL_Region_Collect_External_First_Use
        (WN *block,
         const std::vector<ST_IDX> &definitions,
         std::vector<ST_IDX> *external)
{
    for (WN *stmt = WN_first(block); stmt != NULL; stmt = WN_next(stmt)) {
        if (WN_operator(stmt) == OPR_REGION) {
            DSL_Region_Collect_External_First_Use
                (WN_region_body(stmt), definitions, external);
            continue;
        }
        if (WN_operator(stmt) != OPR_STID || WN_kid_count(stmt) != 1 ||
            !DSL_WN_Is_Native(WN_kid0(stmt)))
            continue;
        WN *expression = WN_kid0(stmt);
        for (UINT32 kid = 0; kid < WN_kid_count(expression); ++kid) {
            WN *operand = WN_kid(expression, kid);
            if (operand == NULL || WN_operator(operand) != OPR_LDID)
                continue;
            ST_IDX st = WN_st_idx(operand);
            if (!DSL_Region_Vector_Has_ST(definitions, st) &&
                !DSL_Region_Vector_Has_ST(*external, st))
                external->push_back(st);
        }
    }
}

static ST_IDX
DSL_Region_Last_Result (WN *block)
{
    ST_IDX result = ST_IDX_ZERO;
    for (WN *stmt = WN_first(block); stmt != NULL; stmt = WN_next(stmt)) {
        if (WN_operator(stmt) == OPR_STID && WN_kid_count(stmt) == 1 &&
            DSL_WN_Is_Native(WN_kid0(stmt)))
            result = WN_st_idx(stmt);
        else if (WN_operator(stmt) == OPR_REGION) {
            ST_IDX nested = DSL_Region_Last_Result(WN_region_body(stmt));
            if (ST_IDX_index(nested) != 0)
                result = nested;
        }
    }
    return result;
}

static BOOL
DSL_Region_Source_Positions_Valid (WN *block)
{
    for (WN *stmt = WN_first(block); stmt != NULL; stmt = WN_next(stmt)) {
        if (WN_Get_Linenum(stmt) == 0)
            return FALSE;
        if (WN_operator(stmt) == OPR_REGION &&
            !DSL_Region_Source_Positions_Valid(WN_region_body(stmt)))
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Region_Verify_Value_Interface
        (const DSL_REGION_STORE &store,
         const dsl_region_runtime &region,
         FILE *diagnostic)
{
    std::vector<ST_IDX> definitions;
    std::vector<ST_IDX> external;
    std::vector<const DSL_REGION_INTERFACE_RECORD *> inputs;
    const DSL_REGION_INTERFACE_RECORD *output = NULL;
    DSL_Region_Collect_Definitions
        (WN_region_body(region.wn), &definitions);
    DSL_Region_Collect_External_First_Use
        (WN_region_body(region.wn), definitions, &external);

    for (UINT32 i = 0; i < store.interfaces.size(); ++i) {
        const DSL_REGION_INTERFACE_RECORD &binding = store.interfaces[i];
        if (binding.region_id != region.image.region_id)
            continue;
        if (binding.roles == DSL_REGION_VALUE_INPUT)
            inputs.push_back(&binding);
        else if (binding.roles ==
                 (DSL_REGION_VALUE_OUTPUT | DSL_REGION_VALUE_RESULT)) {
            if (output != NULL)
                return DSL_Region_Report
                           (diagnostic, "DOPC_LLAMA_TOPOLOGY duplicate "
                            "result interface", region.image.region_id);
            output = &binding;
        } else {
            return DSL_Region_Report
                       (diagnostic, "DOPC_LLAMA_TOPOLOGY unsupported "
                        "transformer interface role",
                        region.image.region_id);
        }
    }
    if (inputs.size() != external.size())
        return DSL_Region_Report
                   (diagnostic, "DOPC_LLAMA_TOPOLOGY external input count "
                    "does not match first-use interface",
                    region.image.region_id);
    for (UINT32 ordinal = 0; ordinal < external.size(); ++ordinal) {
        const DSL_REGION_INTERFACE_RECORD *binding = NULL;
        for (UINT32 i = 0; i < inputs.size(); ++i) {
            if (inputs[i]->ordinal == ordinal)
                binding = inputs[i];
        }
        if (binding == NULL || binding->st != external[ordinal])
            return DSL_Region_Report
                       (diagnostic, "DOPC_LLAMA_TOPOLOGY input interface "
                        "is not in deterministic first-use order",
                        region.image.region_id);
    }

    ST_IDX last = DSL_Region_Last_Result(WN_region_body(region.wn));
    if (output == NULL || output->ordinal != 0 || output->st != last ||
        ST_IDX_index(last) == 0 ||
        ST_tensor_attribute
            (last, TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS)) ==
            NULL ||
        strcmp(ST_tensor_attribute
                   (last,
                    TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS)),
               "true") != 0)
        return DSL_Region_Report
                   (diagnostic, "DOPC_LLAMA_TOPOLOGY result must be the "
                    "outer-owned no-alias final value",
                    region.image.region_id);
    return TRUE;
}

static BOOL
DSL_Region_Verify_Decoder_Topology
        (const dsl_region_runtime &region, FILE *diagnostic)
{
    static const DSL_OPERATOR expected[] = {
        OPR_DSLRMSNORM,
        OPR_DSLLINEAR, OPR_DSLLINEAR, OPR_DSLLINEAR,
        OPR_DSLRESHAPE, OPR_DSLTRANSPOSE,
        OPR_DSLROTARYEMBEDDING, OPR_DSLROTARYEMBEDDING,
        OPR_DSLATTENTION,
        OPR_DSLTRANSPOSE, OPR_DSLRESHAPE,
        OPR_DSLLINEAR, OPR_DSLRESIDUALADD,
        OPR_DSLRMSNORM,
        OPR_DSLLINEAR, OPR_DSLLINEAR, OPR_DSLSWIGLU,
        OPR_DSLLINEAR, OPR_DSLRESIDUALADD
    };
    UINT32 ordinal = 0;
    for (WN *stmt = WN_first(WN_region_body(region.wn)); stmt != NULL;
         stmt = WN_next(stmt)) {
        if (WN_operator(stmt) != OPR_STID || WN_kid_count(stmt) != 1 ||
            !DSL_WN_Is_Native(WN_kid0(stmt)) ||
            ordinal >= sizeof(expected) / sizeof(expected[0]) ||
            DSL_WN_operator(WN_kid0(stmt)) != expected[ordinal])
            return DSL_Region_Report
                       (diagnostic, "DOPC_LLAMA_TOPOLOGY decoder operator "
                        "sequence mismatch", region.image.region_id);
        ++ordinal;
    }
    if (ordinal != sizeof(expected) / sizeof(expected[0]))
        return DSL_Region_Report
                   (diagnostic, "DOPC_LLAMA_TOPOLOGY incomplete decoder "
                    "operator sequence", region.image.region_id);
    return TRUE;
}

static BOOL
DSL_Region_Verify_Prefill_Topology
        (const DSL_REGION_STORE &store,
         const dsl_region_runtime &region,
         FILE *diagnostic)
{
    static const DSL_OPERATOR tail[] = {
        OPR_DSLRMSNORM, OPR_DSLLINEAR, OPR_DSLOUTPUTLOGITS
    };
    WN *stmt = WN_first(WN_region_body(region.wn));
    if (stmt == NULL || WN_operator(stmt) != OPR_STID ||
        !DSL_WN_Is_Native(WN_kid0(stmt)) ||
        DSL_WN_operator(WN_kid0(stmt)) != OPR_DSLTOKENEMBEDDING)
        return DSL_Region_Report
                   (diagnostic, "DOPC_LLAMA_TOPOLOGY prefill must begin "
                    "with token embedding", region.image.region_id);

    UINT32 decoder_count = 0;
    stmt = WN_next(stmt);
    while (stmt != NULL && WN_operator(stmt) == OPR_REGION) {
        BOOL matched = FALSE;
        for (UINT32 i = 0; i < store.regions.size(); ++i) {
            const dsl_region_runtime &child = *store.regions[i];
            if (child.wn == stmt &&
                child.image.parent_region_id == region.image.region_id &&
                child.image.contract_name != STR_IDX_ZERO &&
                child.image.contract_name < STR_Table_Size() &&
                strcmp(Index_To_Str(child.image.contract_name),
                       "transformer.decoder_layer") == 0 &&
                child.image.contract_version == 1)
                matched = TRUE;
        }
        if (!matched)
            return DSL_Region_Report
                       (diagnostic, "DOPC_LLAMA_TOPOLOGY invalid prefill "
                        "child region", region.image.region_id);
        ++decoder_count;
        stmt = WN_next(stmt);
    }
    if (decoder_count == 0)
        return DSL_Region_Report
                   (diagnostic, "DOPC_LLAMA_TOPOLOGY prefill has no decoder "
                    "region", region.image.region_id);
    for (UINT32 i = 0; i < sizeof(tail) / sizeof(tail[0]); ++i) {
        if (stmt == NULL || WN_operator(stmt) != OPR_STID ||
            !DSL_WN_Is_Native(WN_kid0(stmt)) ||
            DSL_WN_operator(WN_kid0(stmt)) != tail[i])
            return DSL_Region_Report
                       (diagnostic, "DOPC_LLAMA_TOPOLOGY invalid prefill "
                        "final operator sequence", region.image.region_id);
        stmt = WN_next(stmt);
    }
    if (stmt != NULL)
        return DSL_Region_Report
                   (diagnostic, "DOPC_LLAMA_TOPOLOGY trailing prefill "
                    "statement", region.image.region_id);
    return TRUE;
}

static BOOL
DSL_Region_Find_State_Object
        (PU_Info *pu,
         ST_IDX st,
         DSL_STATE_OBJECT_RECORD *state)
{
    for (UINT32 i = 1; i <= DSL_Effect_Image_State_Object_Count(); ++i) {
        DSL_STATE_OBJECT_RECORD candidate;
        if (!DSL_Effect_Image_Get_State_Object(i, &candidate))
            return FALSE;
        if (candidate.owner_pu_st == PU_Info_proc_sym(pu) &&
            candidate.st == st) {
            if (state != NULL)
                *state = candidate;
            return TRUE;
        }
    }
    return FALSE;
}

static DSL_IR_NODE_ID
DSL_Region_Node_Id (WN *statement)
{
    if (statement == NULL || WN_operator(statement) != OPR_STID ||
        WN_kid_count(statement) != 1 ||
        !DSL_WN_Is_Native(WN_kid0(statement)))
        return DSL_IR_NODE_INVALID_ID;

    DSL_IR_IMAGE_HEADER header;
    DSL_IR_Image_Get_Header(&header);
    for (UINT32 i = 1; i <= header.value_count; ++i) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(i, &value))
            return DSL_IR_NODE_INVALID_ID;
        if (value.st == WN_st_idx(statement))
            return value.producer_node_id;
    }
    return DSL_IR_NODE_INVALID_ID;
}

static INT32
DSL_Region_Node_Position
        (const dsl_region_runtime &region,
         DSL_IR_NODE_ID node_id)
{
    INT32 position = 0;
    for (WN *statement = WN_first(WN_region_body(region.wn));
         statement != NULL; statement = WN_next(statement), ++position) {
        if (DSL_Region_Node_Id(statement) == node_id)
            return position;
    }
    return -1;
}

static BOOL
DSL_Region_Verify_State_Interface
        (const DSL_REGION_STORE &store,
         const dsl_region_runtime &region,
         FILE *diagnostic,
         UINT32 *layer_state_count)
{
    UINT32 count = 0;
    for (UINT32 i = 0; i < store.interfaces.size(); ++i) {
        const DSL_REGION_INTERFACE_RECORD &binding = store.interfaces[i];
        if (binding.region_id != region.image.region_id ||
            (binding.flags & DSL_REGION_INTERFACE_ABSTRACT_STATE) == 0)
            continue;

        const UINT32 state_flags =
            DSL_REGION_INTERFACE_ABSTRACT_STATE |
            DSL_REGION_INTERFACE_UNIQUE_OWNERSHIP |
            DSL_REGION_INTERFACE_STATE_READ |
            DSL_REGION_INTERFACE_STATE_MODIFY |
            DSL_REGION_INTERFACE_LAYER_OWNED;
        const BOOL reads =
            (binding.flags & DSL_REGION_INTERFACE_STATE_READ) != 0;
        const BOOL modifies =
            (binding.flags & DSL_REGION_INTERFACE_STATE_MODIFY) != 0;
        const BOOL layer_owned =
            (binding.flags & DSL_REGION_INTERFACE_LAYER_OWNED) != 0;
        if ((binding.flags & ~state_flags) != 0 || reads == modifies ||
            (reads && binding.roles != DSL_REGION_VALUE_INPUT) ||
            (modifies && binding.roles != DSL_REGION_VALUE_INOUT))
            return DSL_Region_Report
                       (diagnostic, "DDECODE_STATE invalid state interface",
                        region.image.region_id);

        DSL_STATE_OBJECT_RECORD state;
        if (!DSL_Region_Find_State_Object(store.pu, binding.st, &state) ||
            state.kind != DSL_STATE_KIND_MUTABLE_BUFFER)
            return DSL_Region_Report
                       (diagnostic, "DDECODE_STATE cache state is not a "
                        "declared mutable buffer", region.image.region_id);
        if ((binding.flags & DSL_REGION_INTERFACE_UNIQUE_OWNERSHIP) != 0 &&
            (state.flags & DSL_STATE_OBJECT_UNIQUE_OWNERSHIP) == 0)
            return DSL_Region_Report
                       (diagnostic, "DDECODE_ALIAS unique region state lacks "
                        "unique state ownership", region.image.region_id);
        if (layer_owned &&
            (binding.flags & DSL_REGION_INTERFACE_UNIQUE_OWNERSHIP) == 0)
            return DSL_Region_Report
                       (diagnostic, "DDECODE_ALIAS layer state is not "
                        "uniquely owned", region.image.region_id);
        if (layer_owned && (!modifies || binding.ordinal >= 2))
            return DSL_Region_Report
                       (diagnostic, "DDECODE_STATE layer cache state must be "
                        "key ordinal 0 or value ordinal 1 with MODIFY",
                        region.image.region_id);

        for (UINT32 j = 0; j < i; ++j) {
            const DSL_REGION_INTERFACE_RECORD &previous = store.interfaces[j];
            if (previous.region_id == binding.region_id &&
                (previous.flags & DSL_REGION_INTERFACE_ABSTRACT_STATE) != 0 &&
                previous.st == binding.st)
                return DSL_Region_Report
                           (diagnostic, "DDECODE_ALIAS duplicate state "
                            "identity", region.image.region_id);
        }

        UINT32 effect_count = 0;
        UINT32 modify_count = 0;
        INT32 previous_position = -1;
        for (UINT32 j = 1; j <= DSL_Effect_Image_State_Effect_Count(); ++j) {
            DSL_STATE_EFFECT_RECORD effect;
            if (!DSL_Effect_Image_Get_State_Effect(j, &effect))
                return FALSE;
            if (effect.state_object_id != state.id)
                continue;
            INT32 position = DSL_Region_Node_Position(region,
                                                       effect.owner_node_id);
            if (position < 0) {
                if (layer_owned)
                    return DSL_Region_Report
                               (diagnostic, "DDECODE_STATE layer-owned state "
                                "effect escapes its region",
                                region.image.region_id);
                continue;
            }
            if (position <= previous_position)
                return DSL_Region_Report
                           (diagnostic, "DDECODE_ORDER state effects do not "
                            "follow region statement order",
                            region.image.region_id);
            previous_position = position;
            ++effect_count;
            if (effect.effect_kind == DSL_STATE_EFFECT_MODIFY)
                ++modify_count;
            if (reads && effect.effect_kind != DSL_STATE_EFFECT_READ)
                return DSL_Region_Report
                           (diagnostic, "DDECODE_STATE read-only state is "
                            "modified", region.image.region_id);
        }
        if ((layer_owned && effect_count == 0) ||
            (effect_count != 0 && modifies && modify_count == 0))
            return DSL_Region_Report
                       (diagnostic, "DDECODE_STATE missing declared state "
                        "effect", region.image.region_id);

        if (layer_owned)
            ++count;
    }
    if (layer_state_count != NULL)
        *layer_state_count = count;
    return TRUE;
}

static BOOL
DSL_Region_Verify_Store (DSL_REGION_STORE *store, FILE *diagnostic)
{
    if (store == NULL)
        return TRUE;

    for (UINT32 i = 0; i < store->regions.size(); ++i) {
        const dsl_region_runtime &region = *store->regions[i];
        if (region.wn == NULL || WN_operator(region.wn) != OPR_REGION ||
            WN_region_id(region.wn) != (INT32)region.image.region_id ||
            WN_region_kind(region.wn) != (REGION_KIND)region.image.kind ||
            region.image.contract_name == STR_IDX_ZERO ||
            region.image.contract_name >= STR_Table_Size() ||
            region.image.contract_version == 0)
            return DSL_Region_Report
                       (diagnostic, "invalid region", region.image.region_id);
        for (UINT32 j = 0; j < i; ++j) {
            if (store->regions[j]->image.region_id == region.image.region_id)
                return DSL_Region_Report
                           (diagnostic, "duplicate region",
                            region.image.region_id);
        }
        if (region.image.parent_region_id == 0) {
            if (region.image.depth != 1)
                return DSL_Region_Report
                           (diagnostic, "invalid root depth",
                            region.image.region_id);
        } else {
            BOOL found_parent = FALSE;
            for (UINT32 j = 0; j < store->regions.size(); ++j) {
                const DSL_REGION_IMAGE_RECORD &parent =
                    store->regions[j]->image;
                if (parent.region_id == region.image.parent_region_id &&
                    parent.depth + 1 == region.image.depth)
                    found_parent = TRUE;
            }
            if (!found_parent)
                return DSL_Region_Report
                           (diagnostic, "invalid parent",
                            region.image.region_id);
        }

        const char *contract = Index_To_Str(region.image.contract_name);
        BOOL decoder = strcmp(contract, "transformer.decoder_layer") == 0;
        BOOL decoder_v1 = decoder && region.image.contract_version == 1;
        BOOL decoder_v2 = decoder && region.image.contract_version == 2;
        BOOL prefill = strcmp(contract, "transformer.prefill") == 0;
        if ((decoder || prefill) &&
            ((!decoder_v1 && !decoder_v2 && !prefill) ||
             (prefill && region.image.contract_version != 1) ||
             WN_Get_Linenum(region.wn) == 0 ||
             !DSL_Region_Source_Positions_Valid
                  (WN_region_body(region.wn)) ||
             DSL_Region_Metadata_Value(region, "module_path") == NULL))
            return DSL_Region_Report
                       (diagnostic, "DOPC_LLAMA_TOPOLOGY incomplete source "
                        "or module context", region.image.region_id);
        if (decoder) {
            const char *ordinal =
                DSL_Region_Metadata_Value(region, "layer_ordinal");
            if (ordinal == NULL || ordinal[0] == '\0')
                return DSL_Region_Report
                           (diagnostic, "DOPC_LLAMA_TOPOLOGY missing decoder "
                            "layer ordinal", region.image.region_id);
            for (const char *cursor = ordinal; *cursor != '\0'; ++cursor) {
                if (!isdigit((unsigned char)*cursor))
                    return DSL_Region_Report
                               (diagnostic, "DOPC_LLAMA_TOPOLOGY invalid "
                                "decoder layer ordinal",
                                region.image.region_id);
            }
            if (decoder_v1 &&
                !DSL_Region_Verify_Decoder_Topology(region, diagnostic))
                return FALSE;
        }
        if (prefill &&
            !DSL_Region_Verify_Prefill_Topology(*store, region, diagnostic))
            return FALSE;
        if ((decoder_v1 || prefill) &&
            !DSL_Region_Verify_Value_Interface(*store, region, diagnostic))
            return FALSE;
        UINT32 layer_state_count = 0;
        if (!DSL_Region_Verify_State_Interface
                 (*store, region, diagnostic, &layer_state_count))
            return FALSE;
        if (decoder_v2 && layer_state_count != 2)
            return DSL_Region_Report
                       (diagnostic, "DDECODE_STATE decoder layer requires "
                        "distinct key and value cache states",
                        region.image.region_id);
    }
    for (UINT32 i = 0; i < store->interfaces.size(); ++i) {
        const DSL_REGION_INTERFACE_RECORD &binding = store->interfaces[i];
        BOOL found = FALSE;
        for (UINT32 j = 0; j < store->regions.size(); ++j)
            found = found || store->regions[j]->image.region_id ==
                             binding.region_id;
        const UINT32 valid_roles = DSL_REGION_VALUE_INPUT |
                                   DSL_REGION_VALUE_OUTPUT |
                                   DSL_REGION_VALUE_INOUT |
                                   DSL_REGION_VALUE_RESULT;
        const UINT32 valid_flags =
            DSL_REGION_INTERFACE_ABSTRACT_STATE |
            DSL_REGION_INTERFACE_UNIQUE_OWNERSHIP |
            DSL_REGION_INTERFACE_STATE_READ |
            DSL_REGION_INTERFACE_STATE_MODIFY |
            DSL_REGION_INTERFACE_LAYER_OWNED;
        if (!found || ST_IDX_index(binding.st) == 0 || binding.roles == 0 ||
            (binding.roles & ~valid_roles) != 0 ||
            (binding.flags & ~valid_flags) != 0 ||
            ((binding.flags & DSL_REGION_INTERFACE_ABSTRACT_STATE) == 0 &&
             binding.flags != 0) ||
            ((binding.roles & DSL_REGION_VALUE_RESULT) != 0 &&
             (binding.roles & DSL_REGION_VALUE_OUTPUT) == 0))
            return DSL_Region_Report
                       (diagnostic, "invalid interface", binding.region_id);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_REGION_INTERFACE_RECORD &previous =
                store->interfaces[j];
            const UINT32 input_roles = DSL_REGION_VALUE_INPUT |
                                       DSL_REGION_VALUE_INOUT;
            const UINT32 output_roles = DSL_REGION_VALUE_OUTPUT |
                                        DSL_REGION_VALUE_INOUT |
                                        DSL_REGION_VALUE_RESULT;
            if (previous.region_id == binding.region_id &&
                previous.st == binding.st)
                return DSL_Region_Report
                           (diagnostic, "duplicate interface symbol",
                            binding.region_id);
            if (previous.region_id == binding.region_id &&
                previous.ordinal == binding.ordinal &&
                (((previous.roles & input_roles) != 0 &&
                  (binding.roles & input_roles) != 0) ||
                 ((previous.roles & output_roles) != 0 &&
                  (binding.roles & output_roles) != 0)))
                return DSL_Region_Report
                           (diagnostic, "duplicate interface ordinal",
                            binding.region_id);
        }
    }
    return TRUE;
}

BOOL
DSL_Region_Verify_PU (PU_Info *pu, FILE *diagnostic)
{
    return DSL_Region_Verify_Store(DSL_Region_Find_Store(pu), diagnostic);
}

UINT32
DSL_Region_Symbol_Use_Count (PU_Info *pu, ST_IDX st)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (store == NULL || ST_IDX_index(st) == 0)
        return 0;
    UINT32 count = 0;
    for (UINT32 i = 0; i < store->interfaces.size(); ++i) {
        if (store->interfaces[i].st == st)
            ++count;
    }
    return count;
}

BOOL
DSL_Region_Can_Redirect_Symbol (PU_Info *pu, ST_IDX old_st, ST_IDX new_st)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (store == NULL)
        return TRUE;
    if (ST_IDX_index(old_st) == 0 || ST_IDX_index(new_st) == 0)
        return FALSE;
    DSL_REGION_STORE candidate = *store;
    for (UINT32 i = 0; i < candidate.interfaces.size(); ++i) {
        if (candidate.interfaces[i].st == old_st)
            candidate.interfaces[i].st = new_st;
    }
    return DSL_Region_Verify_Store(&candidate, NULL);
}

BOOL
DSL_Region_Redirect_Symbol (PU_Info *pu, ST_IDX old_st, ST_IDX new_st)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (store == NULL ||
        !DSL_Region_Can_Redirect_Symbol(pu, old_st, new_st))
        return FALSE;
    for (UINT32 i = 0; i < store->interfaces.size(); ++i) {
        if (store->interfaces[i].st == old_st)
            store->interfaces[i].st = new_st;
    }
    return TRUE;
}

void
DSL_Region_Print_PU (FILE *file, PU_Info *pu)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (file == NULL || store == NULL)
        return;
    fprintf (file, "\nDSL REGION TABLE:\n");
    for (UINT32 i = 0; i < store->regions.size(); ++i) {
        const dsl_region_runtime &region = *store->regions[i];
        fprintf (file, "REGION id=%u parent=%u depth=%u kind=%u "
                 "contract=%s.v%u\n", region.image.region_id,
                 region.image.parent_region_id, region.image.depth,
                 region.image.kind, Index_To_Str(region.image.contract_name),
                 region.image.contract_version);
        for (WN *wn = WN_first(WN_region_pragmas(region.wn)); wn != NULL;
             wn = WN_next(wn)) {
            if (!WN_Is_DSL_Comment(wn))
                continue;
            const char *comment = Index_To_Str(WN_GetComment(wn));
            const char *prefix = WN_DSL_Comment_Prefix();
            const char *metadata = comment + strlen(prefix);
            if (strncmp(metadata, "region_metadata:", 16) == 0)
                fprintf (file, "  METADATA %s\n", metadata + 16);
        }
        for (UINT32 j = 0; j < store->interfaces.size(); ++j) {
            const DSL_REGION_INTERFACE_RECORD &binding = store->interfaces[j];
            if (binding.region_id == region.image.region_id)
                fprintf (file, "  VALUE ordinal=%u st=<%u,%u> roles=0x%x "
                         "flags=0x%x\n", binding.ordinal,
                         ST_IDX_level(binding.st), ST_IDX_index(binding.st),
                         binding.roles, binding.flags);
        }
    }
}

BOOL
DSL_Region_Write_PU (PU_Info *pu, WN_MAP off_map, Output_File *output)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (store == NULL || store->regions.empty())
        return TRUE;
    if (off_map == WN_MAP_UNDEFINED || output == NULL)
        return FALSE;

    output->file_size = ir_b_align(output->file_size, sizeof(mUINT32), 0);
    off_t base = output->file_size;
    DSL_REGION_IMAGE_HEADER header;
    memset (&header, 0, sizeof(header));
    header.magic = DSL_REGION_IMAGE_MAGIC;
    header.version = DSL_REGION_IMAGE_VERSION;
    header.region_count = store->regions.size();
    header.interface_count = store->interfaces.size();
    ir_b_save_buf(&header, sizeof(header), sizeof(mUINT32), 0, output);

    for (UINT32 i = 0; i < store->regions.size(); ++i) {
        DSL_REGION_IMAGE_RECORD record = store->regions[i]->image;
        record.wn_offset = WN_MAP32_Get(off_map, store->regions[i]->wn);
        if (record.wn_offset == 0)
            return FALSE;
        ir_b_save_buf(&record, sizeof(record), sizeof(mUINT32), 0, output);
    }
    if (!store->interfaces.empty())
        ir_b_save_buf(&store->interfaces[0],
                      store->interfaces.size() * sizeof(store->interfaces[0]),
                      sizeof(mUINT32), 0, output);

    PU_Info_subsect_offset(pu, WT_REGIONS) =
        base - output->cur_section->shdr.sh_offset;
    PU_Info_subsect_size(pu, WT_REGIONS) = output->file_size - base;
    Set_PU_Info_state(pu, WT_REGIONS, Subsect_Written);
    return TRUE;
}

INT
DSL_Region_Load_Mapped_PU (PU_Info *pu, const void *tree_base,
                           UINT64 tree_size, const void *section_base,
                           UINT64 section_size)
{
    if (pu == NULL || tree_base == NULL || section_base == NULL ||
        tree_size == 0 || section_size < sizeof(DSL_REGION_IMAGE_HEADER)) {
        errno = EINVAL;
        return -1;
    }

    const char *base = (const char *)section_base;
    const DSL_REGION_IMAGE_HEADER *header =
        (const DSL_REGION_IMAGE_HEADER *)base;
    UINT64 expected = sizeof(*header) +
                      (UINT64)header->region_count *
                          sizeof(DSL_REGION_IMAGE_RECORD) +
                      (UINT64)header->interface_count *
                          sizeof(DSL_REGION_INTERFACE_RECORD);
    if (header->magic != DSL_REGION_IMAGE_MAGIC ||
        header->version != DSL_REGION_IMAGE_VERSION ||
        expected != section_size) {
        errno = EINVAL;
        return -1;
    }

    DSL_REGION_STORE *store = DSL_Region_Get_Store(pu, TRUE);
    const DSL_REGION_IMAGE_RECORD *records =
        (const DSL_REGION_IMAGE_RECORD *)(base + sizeof(*header));
    for (UINT32 i = 0; i < header->region_count; ++i) {
        if (records[i].wn_offset == 0 || records[i].wn_offset >= tree_size) {
            errno = EINVAL;
            return -1;
        }
        dsl_region_runtime *runtime = new dsl_region_runtime;
        runtime->image = records[i];
        runtime->wn = (WN *)((const char *)tree_base + records[i].wn_offset);
        store->regions.push_back(runtime);
    }
    const DSL_REGION_INTERFACE_RECORD *interfaces =
        (const DSL_REGION_INTERFACE_RECORD *)(records + header->region_count);
    store->interfaces.insert(store->interfaces.end(), interfaces,
                             interfaces + header->interface_count);
    Set_PU_Info_state(pu, WT_REGIONS, Subsect_InMem);
    return DSL_Region_Verify_PU(pu, stderr) ? 0 : -1;
}
