/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Image-owned REGION registry, lifecycle and WT_REGIONS mapped-image I/O.
 * Runtime wrappers borrow PU/WN objects; only unchanged schema rows enter .B.
 * Preserve the existing late writer rejection and partial loader mutation;
 * physical/domain validation uses existing backend-free be/com support with
 * no optimizer or code-generation linkage. The registry is authoritative here.
 * Design: doc/COMMON-COM-DSL-PERSISTENCE-OWNERSHIP-AUDIT.md and
 * doc/WHIRL-DSL-INFRASTRUCTURE.md.
 */

#include <errno.h>
#include <string.h>

#include "dsl_region_image.h"
#include "dsl_region_store_internal.h"
#include "wn_map.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"

typedef char DSL_Region_Header_Size_Check
    [sizeof(DSL_REGION_IMAGE_HEADER) == 24 ? 1 : -1];
typedef char DSL_Region_Record_Size_Check
    [sizeof(DSL_REGION_IMAGE_RECORD) == 40 ? 1 : -1];
typedef char DSL_Region_Interface_Size_Check
    [sizeof(DSL_REGION_INTERFACE_RECORD) == 24 ? 1 : -1];

static std::vector<DSL_REGION_STORE *> DSL_region_stores;

/*
 * Find the first PU-pointer match in the sole image-owned registry without
 * creating or activating a PU. Return a borrowed store or NULL; its lifetime
 * ends on reset, discard, or consumption of its last region.
 */
DSL_REGION_STORE *
DSL_Region_Find_Store (PU_Info *pu)
{
    for (UINT32 i = 0; i < DSL_region_stores.size(); ++i) {
        if (DSL_region_stores[i]->pu == pu)
            return DSL_region_stores[i];
    }
    return NULL;
}

/*
 * Return an existing borrowed store, or NULL when absent and creation is
 * disabled. Creation requires a valid PU, registers an empty owned store,
 * clears its overlapping regions pointer and marks WT_REGIONS in memory;
 * it neither owns the PU/tree nor catches allocation failures.
 */
DSL_REGION_STORE *
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

/*
 * Find the registered store containing this exact borrowed runtime handle.
 * NULL or no match returns NULL without dereferencing the handle or mutating
 * the registry; returned storage remains owned by the image service.
 */
DSL_REGION_STORE *
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

/*
 * Delete every owned runtime wrapper and store and clear the registry,
 * invalidating all handles. Borrowed WNs, PUs and shared strings survive;
 * PU subsection pointers/states are deliberately not repaired here.
 */
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

/*
 * Locate target by parallel traversal of caller-owned source/clone trees.
 * Return the corresponding borrowed clone WN or NULL for no match/shape
 * mismatch; compare operators and kid counts, not full clone equivalence.
 * The initial source==target test can return clone even for NULL inputs.
 */
static WN *
DSL_Region_Corresponding_WN
        (WN *source, WN *clone, const WN *target)
{
    if (source == target)
        return clone;
    if (source == NULL || clone == NULL ||
        WN_operator(source) != WN_operator(clone))
        return NULL;
    if (WN_operator(source) == OPR_BLOCK) {
        WN *source_stmt = WN_first(source);
        WN *clone_stmt = WN_first(clone);
        while (source_stmt != NULL && clone_stmt != NULL) {
            WN *found = DSL_Region_Corresponding_WN
                            (source_stmt, clone_stmt, target);
            if (found != NULL)
                return found;
            source_stmt = WN_next(source_stmt);
            clone_stmt = WN_next(clone_stmt);
        }
        return NULL;
    }
    if (WN_kid_count(source) != WN_kid_count(clone))
        return NULL;
    for (INT i = 0; i < WN_kid_count(source); ++i) {
        WN *found = DSL_Region_Corresponding_WN
                        (WN_kid(source, i), WN_kid(clone, i), target);
        if (found != NULL)
            return found;
    }
    return NULL;
}

/*
 * Remove one registered PU store and delete its owned wrappers, preserving
 * borrowed tree nodes. When found, clear the regions pointer and mark the
 * subsection missing; no registered store is a no-op, including its PU state.
 */
void
DSL_Region_Discard_PU_Store (PU_Info *pu)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (store == NULL)
        return;
    for (UINT32 i = 0; i < DSL_region_stores.size(); ++i) {
        if (DSL_region_stores[i] == store) {
            DSL_region_stores.erase(DSL_region_stores.begin() + i);
            break;
        }
    }
    for (UINT32 i = 0; i < store->regions.size(); ++i)
        delete store->regions[i];
    delete store;
    Set_PU_Info_regions_ptr(pu, NULL);
    Set_PU_Info_state(pu, WT_REGIONS, Subsect_Missing);
}

/*
 * Stage independent runtime/interface vectors over an already cloned PU tree;
 * borrow PUs/WNs and copy rows, clearing stored offsets. Reject invalid or
 * already registered clones without writes; a source with no store succeeds
 * only when its subsection is missing. Correspondence/verification failure
 * discards the new clone store and marks it missing, not its original state.
 * Allocation exceptions are not caught or rolled back.
 */
BOOL
DSL_Region_Clone_PU_Store (PU_Info *source, PU_Info *clone)
{
    if (source == NULL || clone == NULL || source == clone ||
        PU_Info_tree_ptr(source) == NULL ||
        PU_Info_tree_ptr(clone) == NULL ||
        DSL_Region_Find_Store(clone) != NULL)
        return FALSE;
    DSL_REGION_STORE *source_store = DSL_Region_Find_Store(source);
    if (source_store == NULL)
        return PU_Info_state(source, WT_REGIONS) == Subsect_Missing;
    /* Wrapper ownership is independent; cloned WNs stay in caller pools. */
    DSL_REGION_STORE *clone_store = DSL_Region_Get_Store(clone, TRUE);
    if (clone_store == NULL)
        return FALSE;
    for (UINT32 i = 0; i < source_store->regions.size(); ++i) {
        dsl_region_runtime *source_region = source_store->regions[i];
        WN *clone_wn = DSL_Region_Corresponding_WN
                           (PU_Info_tree_ptr(source),
                            PU_Info_tree_ptr(clone), source_region->wn);
        if (clone_wn == NULL || WN_operator(clone_wn) != OPR_REGION) {
            DSL_Region_Discard_PU_Store(clone);
            return FALSE;
        }
        dsl_region_runtime *copy = new dsl_region_runtime;
        copy->image = source_region->image;
        copy->image.wn_offset = 0;
        copy->wn = clone_wn;
        clone_store->regions.push_back(copy);
    }
    /* Rejection discards the staged store, not the caller's cloned tree. */
    clone_store->interfaces = source_store->interfaces;
    if (!DSL_Region_Verify_PU(clone, NULL)) {
        DSL_Region_Discard_PU_Store(clone);
        return FALSE;
    }
    return TRUE;
}

/*
 * Remove one matching wrapper and all of its interface rows, leaving the
 * borrowed WN/tree untouched for caller-owned lowering. No match rejects
 * unchanged; consuming the last wrapper deletes the store, clears the PU
 * regions pointer and marks WT_REGIONS missing. Other handles survive.
 */
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

/*
 * Finalize existing WT_REGIONS rows into the caller's mapped-image output,
 * using its WN offset map while keeping runtime row offsets unchanged. No
 * store/empty regions succeeds without touching output. Invalid map/output
 * rejects before writes; a zero offset rejects after header/prior rows may
 * already be written, without output rollback or subsection publication.
 * Success publishes subsection offset/size and Written state. Caller owns
 * the output/map/tree and any outer file transaction; no verifier runs here.
 */
BOOL
DSL_Region_Write_PU (PU_Info *pu, WN_MAP off_map, Output_File *output)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (store == NULL || store->regions.empty())
        return TRUE;
    if (off_map == WN_MAP_UNDEFINED || output == NULL)
        return FALSE;

    /* Keep the historical write order: late offset failure leaves bytes. */
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

    /* Publish the subsection only after every row has been emitted. */
    PU_Info_subsect_offset(pu, WT_REGIONS) =
        base - output->cur_section->shdr.sh_offset;
    PU_Info_subsect_size(pu, WT_REGIONS) = output->file_size - base;
    Set_PU_Info_state(pu, WT_REGIONS, Subsect_Written);
    return TRUE;
}

/*
 * Copy header-checked WT_REGIONS rows and attach borrowed WNs at stored tree
 * offsets, appending to an existing or newly registered store. Caller keeps
 * aligned mapped bytes, relocated tree and shared strings/symbols readable.
 * Bad arguments/header/size return -1 with EINVAL before mutation; a bad later
 * offset also sets EINVAL but retains store creation and all prior appended
 * wrappers. After all appends, verification failure retains rows/InMem state
 * and does not explicitly set errno. No rollback, whole-WN bounds validation,
 * PU activation or mapped-tree ownership transfer is provided.
 */
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

    /* From this point, rejection retains creation and prior row appends. */
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
    /* Physical verification diagnoses the accumulated store, without rollback. */
    Set_PU_Info_state(pu, WT_REGIONS, Subsect_InMem);
    return DSL_Region_Verify_PU(pu, stderr) ? 0 : -1;
}
