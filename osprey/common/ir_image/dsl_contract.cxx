/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/ir_image owns insertion-ordered contract records and copied strings in a
 * process-local registry. Legacy identity ignores versions; versioned APIs
 * extend lookup without changing that behavior. Seeds describe required checks
 * but do not implement gatekeeping, lowering, optimization or mapped-image
 * changes or .B layout changes. Callers coordinate access and domain/contract
 * reset lifetimes; registry payloads never become mapped-image rows.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, item 29.
 */

#include <string.h>
#include <vector>

#include "dsl_contract.h"

struct DSL_CONTRACT_RECORD {
    DSL_CONTRACT_ID id;
    DSL_DOMAIN_ID source_domain_id;
    DSL_DOMAIN_ID target_domain_id;
    const char *name;
    UINT32 version;
    UINT32 flags;
    std::vector<const char *> required_checks;
    std::vector<const char *> diagnostic_codes;

    /* Start unpublished, with no owned strings and empty check/code vectors. */
    DSL_CONTRACT_RECORD() :
        id(DSL_CONTRACT_INVALID_ID),
        source_domain_id(DSL_DOMAIN_INVALID_ID),
        target_domain_id(DSL_DOMAIN_INVALID_ID),
        name(NULL),
        version(0),
        flags(0) {}
};

static std::vector<DSL_CONTRACT_RECORD> DSL_contract_registry;

struct DSL_CONTRACT_SEED {
    const char *name;
    UINT32 version;
    UINT32 flags;
    const char *const *required_checks;
    UINT32 required_check_count;
    const char *const *diagnostic_codes;
    UINT32 diagnostic_code_count;
};

/* Count static seed arrays at compile time; never pass a decayed pointer. */
#define DSL_ARRAY_COUNT(array) (sizeof(array) / sizeof((array)[0]))

static const char *DSL_rotary_embedding_v1_checks[] = {
    "tensor_descriptors_complete",
    "full_sequence_position_mode",
    "position_offset_static"
};

static const char *DSL_rotary_embedding_v1_diagnostics[] = {
    "DROTARY101",
    "DROTARY102"
};

static const char *DSL_rotary_embedding_v2_checks[] = {
    "tensor_descriptors_complete",
    "cache_position_is_semantic_operand",
    "cache_position_in_rope_capacity",
    "query_key_position_consistent"
};

static const char *DSL_rotary_embedding_v2_diagnostics[] = {
    "DROTARY201",
    "DROTARY202",
    "DROTARY203"
};

static const char *DSL_attention_v1_checks[] = {
    "query_key_value_descriptors_complete",
    "full_sequence_causal_shape_valid",
    "cache_mode_none"
};

static const char *DSL_attention_v1_diagnostics[] = {
    "DATTENTION101",
    "DATTENTION102"
};

static const char *DSL_attention_v2_checks[] = {
    "query_and_cache_descriptors_complete",
    "cached_attention_shape_valid",
    "cache_read_effect_declared",
    "cache_update_effect_declared",
    "cache_position_matches_update_location"
};

static const char *DSL_attention_v2_diagnostics[] = {
    "DATTENTION201",
    "DATTENTION202",
    "DATTENTION203",
    "DATTENTION204"
};

static const char *DSL_decoder_layer_v1_checks[] = {
    "prefill_operator_order_valid",
    "single_activation_result",
    "cache_interface_absent"
};

static const char *DSL_decoder_layer_v1_diagnostics[] = {
    "DDECODER101",
    "DDECODER102"
};

static const char *DSL_decoder_layer_v2_checks[] = {
    "decode_operator_order_valid",
    "key_value_state_inputs_declared",
    "key_value_state_results_declared",
    "layer_state_ownership_valid",
    "state_update_order_valid"
};

static const char *DSL_decoder_layer_v2_diagnostics[] = {
    "DDECODER201",
    "DDECODER202",
    "DDECODER203",
    "DDECODER204"
};

static const char *DSL_decode_v1_checks[] = {
    "input_protocol_valid",
    "result_protocol_valid",
    "cache_position_valid",
    "per_layer_cache_ownership_valid",
    "prefill_contract_unchanged"
};

static const char *DSL_decode_v1_diagnostics[] = {
    "DDECODE001",
    "DDECODE002",
    "DDECODE003",
    "DDECODE004"
};

static const char *DSL_kv_cache_state_v1_checks[] = {
    "cache_rank_four",
    "cache_layout_bhsd",
    "key_value_roles_distinct",
    "valid_length_and_capacity_consistent",
    "state_identity_declared",
    "no_unintended_alias"
};

static const char *DSL_kv_cache_state_v1_diagnostics[] = {
    "DKVCACHE001",
    "DKVCACHE002",
    "DKVCACHE003",
    "DKVCACHE004"
};

static const DSL_CONTRACT_SEED DSL_transformer_decode_contracts[] = {
    { DSL_CONTRACT_TRANSFORMER_ROTARY_EMBEDDING, 1,
      DSL_CONTRACT_FLAG_EXPRESSION,
      DSL_rotary_embedding_v1_checks,
      DSL_ARRAY_COUNT(DSL_rotary_embedding_v1_checks),
      DSL_rotary_embedding_v1_diagnostics,
      DSL_ARRAY_COUNT(DSL_rotary_embedding_v1_diagnostics) },
    { DSL_CONTRACT_TRANSFORMER_ROTARY_EMBEDDING, 2,
      DSL_CONTRACT_FLAG_EXPRESSION,
      DSL_rotary_embedding_v2_checks,
      DSL_ARRAY_COUNT(DSL_rotary_embedding_v2_checks),
      DSL_rotary_embedding_v2_diagnostics,
      DSL_ARRAY_COUNT(DSL_rotary_embedding_v2_diagnostics) },
    { DSL_CONTRACT_TRANSFORMER_ATTENTION, 1,
      DSL_CONTRACT_FLAG_EXPRESSION,
      DSL_attention_v1_checks, DSL_ARRAY_COUNT(DSL_attention_v1_checks),
      DSL_attention_v1_diagnostics,
      DSL_ARRAY_COUNT(DSL_attention_v1_diagnostics) },
    { DSL_CONTRACT_TRANSFORMER_ATTENTION, 2,
      DSL_CONTRACT_FLAG_EXPRESSION | DSL_CONTRACT_FLAG_STATE,
      DSL_attention_v2_checks, DSL_ARRAY_COUNT(DSL_attention_v2_checks),
      DSL_attention_v2_diagnostics,
      DSL_ARRAY_COUNT(DSL_attention_v2_diagnostics) },
    { DSL_CONTRACT_TRANSFORMER_DECODER_LAYER, 1,
      DSL_CONTRACT_FLAG_REGION,
      DSL_decoder_layer_v1_checks,
      DSL_ARRAY_COUNT(DSL_decoder_layer_v1_checks),
      DSL_decoder_layer_v1_diagnostics,
      DSL_ARRAY_COUNT(DSL_decoder_layer_v1_diagnostics) },
    { DSL_CONTRACT_TRANSFORMER_DECODER_LAYER, 2,
      DSL_CONTRACT_FLAG_REGION | DSL_CONTRACT_FLAG_STATE,
      DSL_decoder_layer_v2_checks,
      DSL_ARRAY_COUNT(DSL_decoder_layer_v2_checks),
      DSL_decoder_layer_v2_diagnostics,
      DSL_ARRAY_COUNT(DSL_decoder_layer_v2_diagnostics) },
    { DSL_CONTRACT_TRANSFORMER_DECODE, 1,
      DSL_CONTRACT_FLAG_REGION | DSL_CONTRACT_FLAG_STATE,
      DSL_decode_v1_checks, DSL_ARRAY_COUNT(DSL_decode_v1_checks),
      DSL_decode_v1_diagnostics, DSL_ARRAY_COUNT(DSL_decode_v1_diagnostics) },
    { DSL_CONTRACT_TRANSFORMER_KV_CACHE_STATE, 1,
      DSL_CONTRACT_FLAG_STATE,
      DSL_kv_cache_state_v1_checks,
      DSL_ARRAY_COUNT(DSL_kv_cache_state_v1_checks),
      DSL_kv_cache_state_v1_diagnostics,
      DSL_ARRAY_COUNT(DSL_kv_cache_state_v1_diagnostics) }
};

/* Allocate an owned string copy for a record; normalize null inputs to empty. */
static const char *
DSL_Contract_Save_String (const char *str)
{
    const char *safe_str = str ? str : "";
    size_t len = strlen(safe_str) + 1;
    char *saved = new char[len];

    memcpy(saved, safe_str, len);
    return saved;
}

/* Check the one-based contract ID range without consulting domain validity. */
static BOOL
DSL_Contract_Valid_Id (DSL_CONTRACT_ID id)
{
    return id != DSL_CONTRACT_INVALID_ID && id <= DSL_contract_registry.size();
}

/*
 * Release strings of an owning record during registry reset and clear its
 * vectors. The caller discards the record next: name is left dangling, so this
 * is not an idempotent release or a reusable-record initialization service.
 */
static void
DSL_Contract_Free_Record (DSL_CONTRACT_RECORD &record)
{
    delete [] record.name;

    for (UINT32 i = 0; i < record.required_checks.size(); ++i)
        delete [] record.required_checks[i];
    record.required_checks.clear();

    for (UINT32 i = 0; i < record.diagnostic_codes.size(); ++i)
        delete [] record.diagnostic_codes[i];
    record.diagnostic_codes.clear();
}

/* Free owned payloads before clearing records; do not reset referenced domains. */
void
DSL_Contract_Registry_Reset (void)
{
    for (UINT32 i = 0; i < DSL_contract_registry.size(); ++i)
        DSL_Contract_Free_Record(DSL_contract_registry[i]);

    DSL_contract_registry.clear();
}

/*
 * Share legacy/versioned registration while preserving first-writer metadata.
 * Reject invalid names/versions and new-entry domains before copying strings;
 * duplicate hits bypass domain validation. Publish only after payload copying.
 * Ordinary validation failures do not mutate the table; allocation exceptions
 * are neither caught nor rolled back, including partially allocated payloads.
 */
static DSL_CONTRACT_ID
DSL_Contract_Register_Internal
        (const char *name,
         DSL_DOMAIN_ID source_domain_id,
         DSL_DOMAIN_ID target_domain_id,
         UINT32 version,
         UINT32 flags,
         const char *const *required_checks,
         UINT32 required_check_count,
         const char *const *diagnostic_codes,
         UINT32 diagnostic_code_count,
         BOOL versioned)
{
    const char *safe_name = name ? name : "";
    DSL_CONTRACT_ID existing =
        versioned ?
        DSL_Contract_Find_Version(safe_name, source_domain_id,
                                  target_domain_id, version) :
        DSL_Contract_Find(safe_name, source_domain_id, target_domain_id);

    if (safe_name[0] == '\0' || (versioned && version == 0))
        return DSL_CONTRACT_INVALID_ID;

    /* Existing identity wins even when newly supplied metadata differs. */
    if (existing != DSL_CONTRACT_INVALID_ID)
        return existing;

    if (!DSL_Domain_Get_Info(source_domain_id, NULL) ||
            !DSL_Domain_Get_Info(target_domain_id, NULL))
        return DSL_CONTRACT_INVALID_ID;

    DSL_CONTRACT_RECORD record;
    record.id = DSL_contract_registry.size() + 1;
    record.source_domain_id = source_domain_id;
    record.target_domain_id = target_domain_id;
    record.name = DSL_Contract_Save_String(safe_name);
    record.version = version;
    record.flags = flags;

    for (UINT32 i = 0; i < required_check_count; ++i)
        record.required_checks.push_back
            (DSL_Contract_Save_String(required_checks == NULL ? NULL :
                                required_checks[i]));

    for (UINT32 i = 0; i < diagnostic_code_count; ++i)
        record.diagnostic_codes.push_back
            (DSL_Contract_Save_String(diagnostic_codes == NULL ? NULL :
                                diagnostic_codes[i]));

    /* Record copies share payload pointers; registry reset frees them once. */
    DSL_contract_registry.push_back(record);
    return record.id;
}

/* Delegate with legacy identity: version is payload, not a duplicate key. */
DSL_CONTRACT_ID
DSL_Contract_Register (const char *name,
                       DSL_DOMAIN_ID source_domain_id,
                       DSL_DOMAIN_ID target_domain_id,
                       UINT32 version,
                       UINT32 flags,
                       const char *const *required_checks,
                       UINT32 required_check_count,
                       const char *const *diagnostic_codes,
                       UINT32 diagnostic_code_count)
{
    return DSL_Contract_Register_Internal
               (name, source_domain_id, target_domain_id, version, flags,
                required_checks, required_check_count, diagnostic_codes,
                diagnostic_code_count, FALSE);
}

/* Delegate with exact-version identity and the shared nonzero-version check. */
DSL_CONTRACT_ID
DSL_Contract_Register_Versioned
        (const char *name,
         DSL_DOMAIN_ID source_domain_id,
         DSL_DOMAIN_ID target_domain_id,
         UINT32 version,
         UINT32 flags,
         const char *const *required_checks,
         UINT32 required_check_count,
         const char *const *diagnostic_codes,
         UINT32 diagnostic_code_count)
{
    return DSL_Contract_Register_Internal
               (name, source_domain_id, target_domain_id, version, flags,
                required_checks, required_check_count, diagnostic_codes,
                diagnostic_code_count, TRUE);
}

/*
 * Scan in insertion order and return the first name/domain-pair match. Do not
 * rank versions, validate domains or modify the metadata during legacy lookup.
 */
DSL_CONTRACT_ID
DSL_Contract_Find (const char *name,
                   DSL_DOMAIN_ID source_domain_id,
                   DSL_DOMAIN_ID target_domain_id)
{
    const char *safe_name = name ? name : "";

    for (UINT32 i = 0; i < DSL_contract_registry.size(); ++i) {
        const DSL_CONTRACT_RECORD &record = DSL_contract_registry[i];
        if (record.source_domain_id == source_domain_id &&
        record.target_domain_id == target_domain_id &&
        strcmp(record.name, safe_name) == 0)
            return record.id;
    }

    return DSL_CONTRACT_INVALID_ID;
}

/* Compare the complete versioned key without mutation; zero is a valid query. */
DSL_CONTRACT_ID
DSL_Contract_Find_Version (const char *name,
                           DSL_DOMAIN_ID source_domain_id,
                           DSL_DOMAIN_ID target_domain_id,
                           UINT32 version)
{
    const char *safe_name = name ? name : "";

    for (UINT32 i = 0; i < DSL_contract_registry.size(); ++i) {
        const DSL_CONTRACT_RECORD &record = DSL_contract_registry[i];
        if (record.source_domain_id == source_domain_id &&
            record.target_domain_id == target_domain_id &&
            record.version == version && strcmp(record.name, safe_name) == 0)
            return record.id;
    }

    return DSL_CONTRACT_INVALID_ID;
}

/*
 * Track the greatest matching numeric version, retaining the first on ties.
 * This is metadata selection only, not a support or legality decision.
 */
DSL_CONTRACT_ID
DSL_Contract_Find_Current (const char *name,
                           DSL_DOMAIN_ID source_domain_id,
                           DSL_DOMAIN_ID target_domain_id)
{
    const char *safe_name = name ? name : "";
    DSL_CONTRACT_ID current = DSL_CONTRACT_INVALID_ID;
    UINT32 current_version = 0;

    for (UINT32 i = 0; i < DSL_contract_registry.size(); ++i) {
        const DSL_CONTRACT_RECORD &record = DSL_contract_registry[i];
        if (record.source_domain_id == source_domain_id &&
            record.target_domain_id == target_domain_id &&
            strcmp(record.name, safe_name) == 0 &&
            (current == DSL_CONTRACT_INVALID_ID ||
             record.version > current_version)) {
            current = record.id;
            current_version = record.version;
        }
    }

    return current;
}

/*
 * Validate before any output writes; copy scalar metadata/counts and a borrowed
 * name, not owned storage. NULL output succeeds for a live contract ID.
 */
BOOL
DSL_Contract_Get_Info (DSL_CONTRACT_ID id, DSL_CONTRACT_INFO *info)
{
    if (!DSL_Contract_Valid_Id(id))
        return FALSE;

    if (info != NULL) {
        const DSL_CONTRACT_RECORD &record = DSL_contract_registry[id - 1];
        info->id = record.id;
        info->source_domain_id = record.source_domain_id;
        info->target_domain_id = record.target_domain_id;
        info->name = record.name;
        info->version = record.version;
        info->flags = record.flags;
        info->required_check_count = record.required_checks.size();
        info->diagnostic_code_count = record.diagnostic_codes.size();
    }

    return TRUE;
}

/* Expose vector size through the public count type without changing records. */
UINT32
DSL_Contract_Count (void)
{
    return DSL_contract_registry.size();
}

/* Bounds-check the zero-based ordinal before sharing Get_Info's output rules. */
BOOL
DSL_Contract_At (UINT32 ordinal, DSL_CONTRACT_INFO *info)
{
    if (ordinal >= DSL_contract_registry.size())
        return FALSE;

    return DSL_Contract_Get_Info(DSL_contract_registry[ordinal].id, info);
}

/* Validate both indices and borrow a stored check name; never run the check. */
const char *
DSL_Contract_Required_Check_At (DSL_CONTRACT_ID id, UINT32 ordinal)
{
    if (!DSL_Contract_Valid_Id(id))
        return NULL;

    const DSL_CONTRACT_RECORD &record = DSL_contract_registry[id - 1];
    if (ordinal >= record.required_checks.size())
        return NULL;

    return record.required_checks[ordinal];
}

/* Validate both indices and borrow a stored code; do not emit a diagnostic. */
const char *
DSL_Contract_Diagnostic_Code_At (DSL_CONTRACT_ID id, UINT32 ordinal)
{
    if (!DSL_Contract_Valid_Id(id))
        return NULL;

    const DSL_CONTRACT_RECORD &record = DSL_contract_registry[id - 1];
    if (ordinal >= record.diagnostic_codes.size())
        return NULL;

    return record.diagnostic_codes[ordinal];
}

/*
 * Look up, but never create, the transformer domain and register each seed as
 * a self-domain versioned contract. Count successful IDs, including duplicates;
 * earlier entries remain if a later registration fails or throws.
 */
UINT32
DSL_Contract_Register_Transformer_Decode (void)
{
    DSL_DOMAIN_ID transformer_id = DSL_Domain_Find("transformer");
    UINT32 registered = 0;

    if (transformer_id == DSL_DOMAIN_INVALID_ID)
        return 0;

    for (UINT32 i = 0;
         i < DSL_ARRAY_COUNT(DSL_transformer_decode_contracts); ++i) {
        const DSL_CONTRACT_SEED &seed = DSL_transformer_decode_contracts[i];
        if (DSL_Contract_Register_Versioned
                (seed.name, transformer_id, transformer_id, seed.version,
                 seed.flags, seed.required_checks, seed.required_check_count,
                 seed.diagnostic_codes, seed.diagnostic_code_count) !=
            DSL_CONTRACT_INVALID_ID)
            ++registered;
    }

    return registered;
}

/*
 * Dump registration-order metadata and stored check/code strings without
 * executing them. NULL streams are skipped; the caller handles I/O failures.
 */
void
DSL_Contract_fprint_registry (FILE *f)
{
    if (f == NULL)
        return;

    fprintf(f, "DSL Contract Registry: entries=%u\n", DSL_Contract_Count());
    for (UINT32 i = 0; i < DSL_contract_registry.size(); ++i) {
        const DSL_CONTRACT_RECORD &record = DSL_contract_registry[i];
        fprintf(f,
            "  [%u] id=%u name=%s source=%u target=%u version=%u flags=0x%x\n",
            i,
            record.id,
            record.name,
            record.source_domain_id,
            record.target_domain_id,
            record.version,
            record.flags);

        for (UINT32 j = 0; j < record.required_checks.size(); ++j)
            fprintf(f, "      check[%u]=%s\n", j, record.required_checks[j]);

        for (UINT32 j = 0; j < record.diagnostic_codes.size(); ++j)
            fprintf(f, "      diagnostic[%u]=%s\n", j, record.diagnostic_codes[j]);
    }
}
