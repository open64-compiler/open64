/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * PU-local REGION construction, metadata/interface rewrites, physical and
 * recognized transformer/decode-state verification owned by be/com. Shared
 * store ownership stays in common/ir_image; all existing executable bodies
 * and schema identities are retained, without optimization, implicit scope
 * changes, code generation or additional .B state. Direct-object frontend and
 * reader consumers retain the existing backend-free physical support boundary.
 * Design: doc/COMMON-COM-DSL-PERSISTENCE-OWNERSHIP-AUDIT.md and
 * doc/WHIRL-DSL-INFRASTRUCTURE.md.
 */

#include <ctype.h>
#include <string.h>
#include <vector>

#include "dsl_region.h"
#include "dsl_region_internal.h"
#include "../../common/ir_image/dsl_region_store_internal.h"
#include "../../common/ir_image/dsl_image_service.h"
#include "dsl_ir_image.h"
#include "dsl_ir_transaction_internal.h"
#include "strtab.h"
#include "symtab.h"

/*
 * Borrow the first matching region_metadata pragma value from shared strings.
 * Require a live REGION with a pragma BLOCK and a readable nonnull key;
 * return NULL when absent, without interning strings or changing the WN.
 */
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

/*
 * Create a detached pragma REGION and register an owned runtime handle for
 * a caller-owned in-memory PU. Reject missing PU/name/version/tree state
 * before mutation; otherwise allocate blocks/classifier, intern the contract
 * and derive parent IDs/depth. Caller must supply a live compatible parent
 * and later attach/verify the tree; no parent-store or version-range proof,
 * allocation rollback, RID reconstruction or optimization is performed.
 */
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

/*
 * Link a caller-supplied non-BLOCK statement last in a live region body.
 * Reject NULL/block input unchanged; caller must provide an unattached WN
 * and preserve strict tree ownership. This does not verify its semantics.
 */
BOOL
DSL_Region_Append_Statement (DSL_REGION region, WN *statement)
{
    if (region == NULL || statement == NULL ||
        WN_operator(statement) == OPR_BLOCK)
        return FALSE;
    WN_INSERT_BlockLast(WN_region_body(region->wn), statement);
    return TRUE;
}

/*
 * Attach a registered child last in its same-store parent body after checking
 * the recorded parent ID and direct duplicate attachment. Rejection leaves
 * links unchanged; caller must ensure neither WN is otherwise attached and
 * must maintain depth/tree invariants beyond these limited checks.
 */
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

/*
 * Append a live registered region to its owning PU function body. NULL or
 * unregistered handles reject unchanged; caller supplies a valid FUNC_ENTRY
 * and unattached region, since duplicate attachment is not checked here.
 */
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

/*
 * Append one region_metadata pragma with the region source position after
 * checking a nonempty alphanumeric/underscore/dot/hyphen key and no existing
 * binding. NULL/invalid/duplicate inputs reject before mutation; caller
 * retains strings, while shared strings/WN pool own newly created metadata.
 * Allocation failure has no rollback guarantee.
 */
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

/*
 * Append a copied interface row to a registered region, checking only a
 * nonzero ST index and nonempty supported role mask. Caller supplies valid
 * symbol lifetime, ordinals, flags and uniqueness for later verification;
 * reported rejection changes nothing, allocation exceptions are uncaught.
 */
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

/*
 * Set the live region, its three BLOCKs and existing pragma statements to
 * the supplied source position. NULL rejects unchanged; body statements and
 * symbols are not visited, and zero positions are accepted at this layer.
 */
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

/*
 * Borrow the runtime handle's physical WN, or return NULL for a NULL handle.
 * Caller must keep nonnull handles live; no ownership transfer or validation.
 */
WN *
DSL_Region_WN (DSL_REGION region)
{
    return region == NULL ? NULL : region->wn;
}

/*
 * Test pointer membership in this PU's registered region wrappers without
 * tree traversal or mutation; absent stores and NULL WNs return FALSE.
 */
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

/*
 * Optionally emit one region-ID diagnostic to a borrowed FILE and return
 * FALSE for verifier composition; no store mutation or stream ownership.
 */
static BOOL
DSL_Region_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf (diagnostic, "DSL region error: %s id=%u\n", message, id);
    return FALSE;
}

/*
 * Test exact ST_IDX membership in a borrowed vector without resolving the
 * symbol or changing caller-owned storage.
 */
static BOOL
DSL_Region_Vector_Has_ST (const std::vector<ST_IDX> &values, ST_IDX st)
{
    for (UINT32 i = 0; i < values.size(); ++i) {
        if (values[i] == st)
            return TRUE;
    }
    return FALSE;
}

/*
 * Append STID symbol identities from a valid BLOCK and nested REGION bodies
 * to caller-owned scratch storage. Preserve duplicates and encounter order;
 * do not inspect arbitrary control-flow subtrees or mutate the borrowed WNs.
 */
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

/*
 * Append first-seen LDID operands of native DSL STID expressions, recursing
 * through REGION bodies and excluding symbols defined anywhere in the supplied
 * definition set. Borrow valid BLOCKs/vectors; mutate only scratch output.
 * This is the transformer interface census, not general def-use analysis.
 */
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

/*
 * Find the last native-DSL STID result in BLOCK/REGION encounter order, or
 * ST_IDX_ZERO. Borrow a valid BLOCK without mutation; ordinary STIDs and
 * other control-flow subtrees do not establish a result for this contract.
 */
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

/*
 * Require nonzero positions on statements in a valid BLOCK and recursively
 * on nested REGION body statements. Read only; this does not validate columns,
 * filenames, expression kids, symbols, or every possible control-flow subtree.
 */
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

/*
 * Read the transformer v1/prefill interface against the native-DSL first-use
 * census: plain inputs must follow encounter ordinals and one output/result
 * must be the final value with no_alias=true. Borrow valid WNs/shared strings
 * and symbols, optionally diagnose failure, and mutate nothing. These checks
 * are specific to this topology, not a generic alias or ownership proof.
 */
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

/*
 * Check the fixed transformer.decoder_layer.v1 native operator sequence in
 * direct STID statements. Borrow an established REGION, optionally diagnose
 * the first mismatch, and change nothing; operand wiring, numerical legality
 * and decoder v2 topology are outside this sequence check.
 */
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

/*
 * Check transformer.prefill.v1 embedding, one or more registered v1 decoder
 * children, and the fixed final operator sequence. Borrow valid region trees
 * and store/shared strings; require expected STID kid shape from construction.
 * Diagnose mismatch without mutation; do not prove complete model semantics.
 */
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

/*
 * Find the first global state row matching this valid PU's procedure symbol
 * and ST identity. Copy to optional caller output only on success; row-read
 * failure/no match returns FALSE unchanged. No PU activation or state mutation.
 */
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

/*
 * Map a native DSL STID result to the first stored value's producer node ID,
 * or return INVALID_ID on bad shape/read/no match. Borrow tree/global image
 * rows without writes; this lookup does not filter owner PU or prove uniqueness.
 */
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

/*
 * Return the first direct body statement ordinal whose derived node ID
 * matches, or -1. Borrow a valid REGION; no nested search, mutation, or
 * independent validation of the requested node identity is performed.
 */
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

/*
 * Check abstract mutable-buffer interfaces using decode-state flag/role,
 * unique-ownership, layer-cache ordinal, and effect-order rules. Borrow valid
 * state/core rows and REGION WNs; require recorded layer-owned effects to be
 * direct body statements, and enforce read/modify presence as implemented.
 * Only successful completion writes optional layer_state_count. Failures may
 * diagnose but never mutate rows; this is not general alias/effect analysis
 * or a proof of arbitrary domain or FHE numerical/key-state correctness.
 */
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

/*
 * Read structural region IDs, WN kind/identity, parent-depth relations and
 * interface masks/uniqueness, plus named transformer v1/v2/prefill and decode
 * state contracts. NULL stores succeed. Borrow valid WN/string/state views;
 * diagnose the first failure without mutation. Parent IDs do not prove actual
 * tree nesting, and arbitrary contracts (including FHE contracts) receive only
 * these structural/state checks, not domain, cryptographic or numerical proof.
 */
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

        /* Only recognized transformer contracts receive topology checks. */
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
        /* Abstract-state checks apply independently of the contract name. */
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

/*
 * Verify the PU's registered store with structural and recognized domain
 * checks, treating no store as valid. Borrow its context and optional stream;
 * do not activate a PU, repair state, or prove terminal tree attachment.
 */
BOOL
DSL_Region_Verify_PU (PU_Info *pu, FILE *diagnostic)
{
    return DSL_Region_Verify_Store(DSL_Region_Find_Store(pu), diagnostic);
}

/*
 * Read a borrowed strict tree for exact WN pointer reachability, traversing
 * BLOCK statement lists or ordinary kids. NULL roots fail; no ownership or
 * cycle proof is made and no node is changed.
 */
static BOOL
DSL_Region_Tree_Contains (const WN *root, const WN *target)
{
    if (root == NULL)
        return FALSE;
    if (root == target)
        return TRUE;
    if (WN_operator(root) == OPR_BLOCK) {
        for (WN *stmt = WN_first(root); stmt != NULL; stmt = WN_next(stmt)) {
            if (DSL_Region_Tree_Contains(stmt, target))
                return TRUE;
        }
        return FALSE;
    }
    for (INT kid = 0; kid < WN_kid_count(root); ++kid) {
        if (DSL_Region_Tree_Contains(WN_kid(root, kid), target))
            return TRUE;
    }
    return FALSE;
}

/*
 * Read a borrowed strict tree for any STID to the exact symbol, visiting
 * BLOCK lists and ordinary kids. NULL roots fail; this is existence, not
 * dominance, definite assignment, symbol validity or value-equivalence proof.
 */
static BOOL
DSL_Region_Tree_Defines_ST (const WN *root, ST_IDX st)
{
    if (root == NULL)
        return FALSE;
    if (WN_operator(root) == OPR_STID && WN_st_idx(root) == st)
        return TRUE;
    if (WN_operator(root) == OPR_BLOCK) {
        for (WN *stmt = WN_first(root); stmt != NULL; stmt = WN_next(stmt)) {
            if (DSL_Region_Tree_Defines_ST(stmt, st))
                return TRUE;
        }
        return FALSE;
    }
    for (INT kid = 0; kid < WN_kid_count(root); ++kid) {
        if (DSL_Region_Tree_Defines_ST(WN_kid(root, kid), st))
            return TRUE;
    }
    return FALSE;
}

/*
 * Require a nonempty in-memory managed store/tree and caller-established
 * current PU ownership, then structural/domain checks, unique attached REGION
 * WNs, current-scope interface symbols and a body STID for each output role.
 * Read only and optionally diagnose; existence does not prove dominance or
 * semantic correctness, and this function never switches compiler scope.
 */
BOOL
DSL_Region_Verify_Terminal_PU (PU_Info *pu, FILE *diagnostic)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (pu == NULL || store == NULL || store->regions.empty() ||
        PU_Info_state(pu, WT_TREE) != Subsect_InMem ||
        PU_Info_tree_ptr(pu) == NULL ||
        PU_Info_state(pu, WT_REGIONS) != Subsect_InMem ||
        !DSL_IR_Image_Current_PU_Is(PU_Info_proc_sym(pu)) ||
        !DSL_Region_Verify_Store(store, diagnostic))
        return DSL_Region_Report(diagnostic,
                                 "terminal REGION store or owner invalid", 0);

    for (UINT32 i = 0; i < store->regions.size(); ++i) {
        const dsl_region_runtime &region = *store->regions[i];
        if (!DSL_Region_Tree_Contains(PU_Info_tree_ptr(pu), region.wn))
            return DSL_Region_Report(diagnostic,
                                     "terminal REGION is detached",
                                     region.image.region_id);
        for (UINT32 j = 0; j < i; ++j) {
            if (store->regions[j]->wn == region.wn)
                return DSL_Region_Report(diagnostic,
                                         "terminal REGION WN is duplicated",
                                         region.image.region_id);
        }
    }
    for (UINT32 i = 0; i < store->interfaces.size(); ++i) {
        const DSL_REGION_INTERFACE_RECORD &binding = store->interfaces[i];
        if (ST_IDX_level(binding.st) != CURRENT_SYMTAB ||
            ST_IDX_index(binding.st) >= ST_Table_Size(CURRENT_SYMTAB))
            return DSL_Region_Report(diagnostic,
                                     "terminal REGION symbol is invalid",
                                     binding.region_id);
        if ((binding.roles & (DSL_REGION_VALUE_OUTPUT |
                              DSL_REGION_VALUE_INOUT |
                              DSL_REGION_VALUE_RESULT)) == 0)
            continue;
        BOOL defined = FALSE;
        for (UINT32 j = 0; j < store->regions.size(); ++j) {
            const dsl_region_runtime &region = *store->regions[j];
            if (region.image.region_id == binding.region_id)
                defined = DSL_Region_Tree_Defines_ST
                              (WN_region_body(region.wn), binding.st);
        }
        if (!defined)
            return DSL_Region_Report(diagnostic,
                                     "terminal REGION result is not defined",
                                     binding.region_id);
    }
    return TRUE;
}

/*
 * Count exact interface ST matches in this PU's store, returning zero for
 * no store or a zero-index symbol. Read only; this is not a tree use count.
 */
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

/*
 * Read this PU's interface vector length, or zero without a store; no
 * validation, filtering or transfer of the store's owned rows occurs.
 */
UINT32
DSL_Region_Interface_Count (PU_Info *pu)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    return store == NULL ? 0 : store->interfaces.size();
}

/*
 * Copy a one-based interface row to nonnull caller storage. Missing store,
 * invalid ID or NULL output returns FALSE without writes; identities remain
 * borrowed references to externally owned symbols and regions.
 */
BOOL
DSL_Region_Get_Interface
        (PU_Info *pu, UINT32 id, DSL_REGION_INTERFACE_RECORD *record)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (store == NULL || record == NULL || id == 0 ||
        id > store->interfaces.size())
        return FALSE;
    *record = store->interfaces[id - 1];
    return TRUE;
}

/*
 * Preflight complete current-PU output redirects and plain-input removals
 * into caller-owned scratch, borrowing region wrappers from the live store.
 * Reject invalid/duplicate/overlapping requests and unmatched rows; verify the
 * candidate before commit. Failure may leave scratch partial, never live rows.
 * An empty request succeeds after context checks without filling candidate;
 * allocation exceptions are uncaught. This does not rewrite physical WNs.
 */
static BOOL
DSL_Region_Build_Lowering_Store
        (PU_Info *pu, const DSL_REGION_SYMBOL_REDIRECT *redirects,
         UINT32 redirect_count, const ST_IDX *pruned_inputs,
         UINT32 pruned_input_count, DSL_REGION_STORE *candidate)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (pu == NULL || candidate == NULL ||
        !DSL_IR_Image_Current_PU_Is(PU_Info_proc_sym(pu)) ||
        (redirect_count != 0 && redirects == NULL) ||
        (pruned_input_count != 0 && pruned_inputs == NULL))
        return FALSE;
    if (redirect_count == 0 && pruned_input_count == 0)
        return TRUE;
    if (store == NULL)
        return FALSE;
    for (UINT32 i = 0; i < redirect_count; ++i) {
        ST_IDX old_st = redirects[i].old_st;
        ST_IDX new_st = redirects[i].new_st;
        if (old_st == new_st || ST_IDX_level(old_st) != CURRENT_SYMTAB ||
            ST_IDX_level(new_st) != CURRENT_SYMTAB ||
            ST_IDX_index(old_st) == 0 ||
            ST_IDX_index(new_st) == 0 ||
            ST_IDX_index(old_st) >= ST_Table_Size(CURRENT_SYMTAB) ||
            ST_IDX_index(new_st) >= ST_Table_Size(CURRENT_SYMTAB))
            return FALSE;
        for (UINT32 j = 0; j < i; ++j) {
            if (redirects[j].old_st == old_st ||
                redirects[j].new_st == new_st)
                return FALSE;
        }
    }
    for (UINT32 i = 0; i < pruned_input_count; ++i) {
        ST_IDX st = pruned_inputs[i];
        if (ST_IDX_level(st) != CURRENT_SYMTAB ||
            ST_IDX_index(st) == 0 ||
            ST_IDX_index(st) >= ST_Table_Size(CURRENT_SYMTAB))
            return FALSE;
        for (UINT32 j = 0; j < redirect_count; ++j) {
            if (redirects[j].old_st == st || redirects[j].new_st == st)
                return FALSE;
        }
        for (UINT32 j = 0; j < i; ++j) {
            if (pruned_inputs[j] == st)
                return FALSE;
        }
    }

    /* Copy interfaces for preflight; runtime wrappers remain borrowed. */
    *candidate = *store;
    std::vector<BOOL> redirected(redirect_count, FALSE);
    std::vector<BOOL> pruned(pruned_input_count, FALSE);
    for (UINT32 i = 0; i < candidate->interfaces.size(); ) {
        DSL_REGION_INTERFACE_RECORD &binding = candidate->interfaces[i];
        BOOL removed = FALSE;
        for (UINT32 j = 0; j < pruned_input_count; ++j) {
            if (binding.st != pruned_inputs[j])
                continue;
            if (binding.roles != DSL_REGION_VALUE_INPUT ||
                binding.flags != DSL_REGION_INTERFACE_FLAG_NONE)
                return FALSE;
            pruned[j] = TRUE;
            candidate->interfaces.erase(candidate->interfaces.begin() + i);
            removed = TRUE;
            break;
        }
        if (removed)
            continue;
        for (UINT32 j = 0; j < redirect_count; ++j) {
            if (binding.st != redirects[j].old_st)
                continue;
            if ((binding.roles &
                 (DSL_REGION_VALUE_INPUT | DSL_REGION_VALUE_INOUT)) != 0 ||
                (binding.roles &
                 (DSL_REGION_VALUE_OUTPUT | DSL_REGION_VALUE_RESULT)) == 0)
                return FALSE;
            binding.st = redirects[j].new_st;
            redirected[j] = TRUE;
            break;
        }
        ++i;
    }
    for (UINT32 i = 0; i < redirect_count; ++i) {
        if (!redirected[i])
            return FALSE;
    }
    for (UINT32 i = 0; i < pruned_input_count; ++i) {
        if (!pruned[i])
            return FALSE;
    }
    return DSL_Region_Verify_Store(candidate, NULL);
}

/*
 * Run lowering-transition preflight in local scratch and discard it, with
 * no live row or WN mutation. Require current PU for context checks even for
 * empty requests; return FALSE for rejected requests without rollback work.
 */
BOOL
DSL_Region_Can_Apply_Lowering_Transitions
        (PU_Info *pu, const DSL_REGION_SYMBOL_REDIRECT *redirects,
         UINT32 redirect_count, const ST_IDX *pruned_inputs,
         UINT32 pruned_input_count)
{
    DSL_REGION_STORE candidate;
    return DSL_Region_Build_Lowering_Store
               (pu, redirects, redirect_count, pruned_inputs,
                pruned_input_count, &candidate);
}

/*
 * For a nonempty request, rebuild and verify scratch under the current PU,
 * then swap only the live interface vector. Reported FALSE leaves live rows
 * unchanged; an empty request succeeds without context checks. Caller owns
 * physical lowering and cross-service rollback; allocation failures are uncaught.
 */
BOOL
DSL_Region_Apply_Lowering_Transitions
        (PU_Info *pu, const DSL_REGION_SYMBOL_REDIRECT *redirects,
         UINT32 redirect_count, const ST_IDX *pruned_inputs,
         UINT32 pruned_input_count)
{
    if (redirect_count == 0 && pruned_input_count == 0)
        return TRUE;
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    DSL_REGION_STORE candidate;
    if (store == NULL ||
        !DSL_Region_Build_Lowering_Store
             (pu, redirects, redirect_count, pruned_inputs,
              pruned_input_count, &candidate))
        return FALSE;
    /* Commit only verified interface rows; physical WNs are caller-owned. */
    store->interfaces.swap(candidate.interfaces);
    return TRUE;
}

/*
 * Test exact membership in a readable count-sized caller pruning array.
 * Read only; a zero count performs no dereference or symbol validation.
 */
static BOOL
DSL_Region_Prune_Request_Has_ST
        (const ST_IDX *symbols, UINT32 symbol_count, ST_IDX st)
{
    for (UINT32 i = 0; i < symbol_count; ++i) {
        if (symbols[i] == st)
            return TRUE;
    }
    return FALSE;
}

/*
 * Require a nonempty distinct current-scope symbol set and matching active
 * PU/store; copy rows to scratch, remove only unflagged plain inputs, require
 * every requested symbol to match, then verify the candidate. Failures leave
 * live rows/WNs unchanged but scratch may be partial; wrappers stay borrowed.
 */
static BOOL
DSL_Region_Build_Pruned_Input_Store
        (PU_Info *pu, const ST_IDX *symbols, UINT32 symbol_count,
         DSL_REGION_STORE *candidate)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    if (pu == NULL || symbols == NULL || symbol_count == 0 ||
        candidate == NULL || store == NULL ||
        !DSL_IR_Image_Current_PU_Is(PU_Info_proc_sym(pu)))
        return FALSE;
    for (UINT32 i = 0; i < symbol_count; ++i) {
        if (ST_IDX_level(symbols[i]) != CURRENT_SYMTAB ||
            ST_IDX_index(symbols[i]) == 0 ||
            ST_IDX_index(symbols[i]) >= ST_Table_Size(CURRENT_SYMTAB))
            return FALSE;
        for (UINT32 j = 0; j < i; ++j) {
            if (symbols[i] == symbols[j])
                return FALSE;
        }
    }

    /* Erasure is confined to scratch until the complete request verifies. */
    *candidate = *store;
    std::vector<BOOL> matched(symbol_count, FALSE);
    for (UINT32 i = 0; i < candidate->interfaces.size(); ) {
        DSL_REGION_INTERFACE_RECORD &binding = candidate->interfaces[i];
        if (!DSL_Region_Prune_Request_Has_ST
                 (symbols, symbol_count, binding.st)) {
            ++i;
            continue;
        }
        if (binding.roles != DSL_REGION_VALUE_INPUT ||
            binding.flags != DSL_REGION_INTERFACE_FLAG_NONE)
            return FALSE;
        for (UINT32 j = 0; j < symbol_count; ++j) {
            if (symbols[j] == binding.st)
                matched[j] = TRUE;
        }
        candidate->interfaces.erase(candidate->interfaces.begin() + i);
    }
    for (UINT32 i = 0; i < symbol_count; ++i) {
        if (!matched[i])
            return FALSE;
    }
    return DSL_Region_Verify_Store(candidate, NULL);
}

/*
 * Preflight a complete nonempty input-pruning request in disposable scratch.
 * Return candidate verification status without live mutations; caller must
 * maintain the current PU/symbol context and repeat preflight before commit.
 */
BOOL
DSL_Region_Can_Prune_Input_Symbols
        (PU_Info *pu, const ST_IDX *symbols, UINT32 symbol_count)
{
    DSL_REGION_STORE candidate;
    return DSL_Region_Build_Pruned_Input_Store
               (pu, symbols, symbol_count, &candidate);
}

/*
 * Rebuild and verify the full nonempty input-pruning candidate, then swap
 * its interfaces into the registered store. Reported failure leaves live rows
 * unchanged; no WN/symbol mutation or cross-service rollback is provided.
 */
BOOL
DSL_Region_Prune_Input_Symbols
        (PU_Info *pu, const ST_IDX *symbols, UINT32 symbol_count)
{
    DSL_REGION_STORE *store = DSL_Region_Find_Store(pu);
    DSL_REGION_STORE candidate;
    if (store == NULL ||
        !DSL_Region_Build_Pruned_Input_Store
             (pu, symbols, symbol_count, &candidate))
        return FALSE;
    /* No failing checks remain after this interface-only commit. */
    store->interfaces.swap(candidate.interfaces);
    return TRUE;
}

/*
 * Preflight replacing every matching interface ST in a copied store and
 * verify it; absent stores succeed even for zero symbols. Existing stores
 * reject zero indices; no source-match, type, active-PU or role proof is added.
 * Live rows/WNs remain unchanged, including on candidate rejection.
 */
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

/*
 * Require a registered store and repeat redirect preflight before replacing
 * all matching interface STs in place. FALSE leaves rows unchanged; no match
 * may succeed, no physical WNs are rewritten, and no other service is rolled back.
 */
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

/*
 * Print stored region/metadata/interface facts to a borrowed FILE, requiring
 * live REGION trees and readable contract/pragma strings. NULL stream or no
 * store is a no-op; this does not validate, mutate, free or activate anything.
 */
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
