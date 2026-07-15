/*
 * Copyright (C) 2026 Open64 Project
 */

#include <errno.h>
#include <string.h>
#include <vector>

#include "dsl_region.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"
#include "strtab.h"

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
DSL_Region_Append_To_PU (DSL_REGION region)
{
    if (region == NULL)
        return FALSE;
    DSL_REGION_STORE *store = NULL;
    for (UINT32 i = 0; i < DSL_region_stores.size(); ++i) {
        for (UINT32 j = 0; j < DSL_region_stores[i]->regions.size(); ++j) {
            if (DSL_region_stores[i]->regions[j] == region) {
                store = DSL_region_stores[i];
                break;
            }
        }
    }
    if (store == NULL)
        return FALSE;
    WN *entry = PU_Info_tree_ptr(store->pu);
    WN_INSERT_BlockLast(WN_func_body(entry), region->wn);
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

    DSL_REGION_STORE *store = NULL;
    for (UINT32 i = 0; i < DSL_region_stores.size(); ++i) {
        for (UINT32 j = 0; j < DSL_region_stores[i]->regions.size(); ++j) {
            if (DSL_region_stores[i]->regions[j] == region)
                store = DSL_region_stores[i];
        }
    }
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

static BOOL
DSL_Region_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf (diagnostic, "DSL region error: %s id=%u\n", message, id);
    return FALSE;
}

BOOL
DSL_Region_Verify_PU (PU_Info *pu, FILE *diagnostic)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
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
        if (!found || ST_IDX_index(binding.st) == 0 || binding.roles == 0 ||
            (binding.roles & ~valid_roles) != 0 ||
            ((binding.roles & DSL_REGION_VALUE_RESULT) != 0 &&
             (binding.roles & DSL_REGION_VALUE_OUTPUT) == 0))
            return DSL_Region_Report
                       (diagnostic, "invalid interface", binding.region_id);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_REGION_INTERFACE_RECORD &previous =
                store->interfaces[j];
            if (previous.region_id == binding.region_id &&
                previous.ordinal == binding.ordinal)
                return DSL_Region_Report
                           (diagnostic, "duplicate interface ordinal",
                            binding.region_id);
        }
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
