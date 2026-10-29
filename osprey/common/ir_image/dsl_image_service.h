/*
 * Managed DSL image service: one authoritative row store, mapped views,
 * call-WN associations and structural validation. Callers retain physical
 * PU ownership; these APIs select no optimization or compilation scope.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef DSL_IMAGE_SERVICE_INCLUDED
#define DSL_IMAGE_SERVICE_INCLUDED

#include <stdio.h>
#include "dsl_lowered_relation.h"
#include "../com/dsl_ir_image.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

/*
 * Runtime-only request for replacing one logical operation while retaining
 * its node and result identities.  Referenced arrays are borrowed for the
 * duration of the call and are copied into the existing WHIRL image tables.
 */
typedef struct {
    DSL_IR_NODE_ID node_id;
    DSL_IR_OPCODE_DESCRIPTOR_ID opcode_descriptor_id;
    STR_IDX payload;
    const DSL_IR_VALUE_ID *operand_value_ids;
    UINT32 operand_count;
    const DSL_IR_ATTRIBUTE_RECORD *attributes;
    UINT32 attribute_count;
    UINT32 result_value_kind;
} DSL_IR_NODE_REWRITE_REQUEST;

/*
 * At a caller-controlled image lifetime boundary, discard core and state/effect
 * rows and reset external-source handles, call/ABI/PU/runtime/program interface
 * images and CKKS events through their owning services. Old row ID meanings and
 * table-backed views expire; IDs may be reused. This is not PU-local rollback
 * and does not reset shared strings, symbols/types or opcode/domain registries.
 */
extern void DSL_IR_Image_Reset (void);

/*
 * Fill a caller-owned core header with current version, fixed capabilities and
 * five table counts; zero remaining fields. NULL is a no-op. This snapshots
 * counts only, without validating rows or counting auxiliary image families.
 */
extern void DSL_IR_Image_Get_Header (DSL_IR_IMAGE_HEADER *header);

/*
 * Test whether any of the five core tables has rows, without mutation. FALSE
 * does not imply call, effect or other auxiliary image families are empty.
 */
extern BOOL DSL_IR_Image_Has_Records (void);

/*
 * Validate a copied snapshot of the five core tables against current shared
 * string/type context. Check row/link/flag/provenance rules, generated-constant
 * profiles and producer-graph cycles; return FALSE on the first reported error.
 * NULL diagnostic suppresses messages. Live tables are never modified; scratch
 * allocation failures are not converted to FALSE or given exception cleanup.
 * This does not validate auxiliary images, physical WN trees, every descriptor
 * semantic field, arbitrary TY/ST identities or domain-specific legality.
 */
extern BOOL DSL_IR_Image_Validate (FILE *diagnostic);

/* Print current core/effect/call/ABI/PU/runtime/program tables and delegated
 * FHE/CKKS images to a borrowed stream; NULL is a no-op. Require valid row links,
 * shared strings/types and applicable local-symbol context: this is inspection,
 * not malformed-image validation. Includes retired/lowered provenance and
 * resolved relation names without changing rows or PU context. Does not close
 * or flush the stream or report I/O failure; partial text may remain on stream
 * or allocation failure, and scratch allocation exceptions propagate. */
extern void DSL_IR_Image_Print (FILE *file);

/*
 * Require an aligned, readable, independent section buffer for the whole call,
 * with referenced shared strings/types already available. Reject NULL, short,
 * size-mismatched or invalid core sections before modifying live tables; absent
 * sections are handled by the reader, not by passing NULL here. On success,
 * reset the whole image (including auxiliary families) and copy core rows into
 * managed storage, retaining no section pointers. Even a valid empty section
 * resets the image. Allocation failure during commit has no rollback guarantee.
 * TRUE certifies the core validator's checks only; NULL diagnostic is silent.
 */
extern BOOL DSL_IR_Image_Load_Mapped (const void *section_base,
                                      UINT64 section_size,
                                      FILE *diagnostic);

/* Fill a zero-initialized call header from current table counts, without
 * validation or table mutation. NULL output is a no-op. */
extern void DSL_Call_Image_Get_Header (DSL_CALL_IMAGE_HEADER *header);

/* Discard identity/callsite rows and borrowed WN associations, not WN trees
 * or dependent ABI/interface/core rows. Callers coordinate dependent resets;
 * row IDs may be reused after reset. */
extern void DSL_Call_Image_Reset (void);

/* Test persisted identity/callsite table occupancy, not WN associations. */
extern BOOL DSL_Call_Image_Has_Records (void);

/* Check row IDs, required strings/nonzero symbol indices, identity uniqueness
 * and association-or-offset presence against current shared tables. This is
 * not a CALL/tree/offset-bound, symbol-liveness or full flag validator. No
 * mutation; FALSE reports the first error, with NULL diagnostic silent. */
extern BOOL DSL_Call_Image_Validate (FILE *diagnostic);

/* Require an aligned, readable section independent of live tables, with shared
 * strings available. Header/size rejection preserves state; after that, replace
 * call rows and discard all WN associations. Row-validation failure leaves call
 * tables empty, not restored. Success copies rows without retaining section
 * pointers; Load_PU must establish borrowed WN associations separately. NULL is
 * invalid, not an absent-section request. Dependent tables are not reset, and
 * allocation failures have no rollback guarantee. NULL diagnostic is silent. */
extern BOOL DSL_Call_Image_Load_Mapped (const void *section_base,
                                        UINT64 section_size,
                                        FILE *diagnostic);

/* Copy a source identity with a fresh one-based ID; do not intern duplicates.
 * Input is unchanged and its STR_IDX fields still refer to shared strings.
 * Return INVALID_ID on preflight or whole-call-image validation failure; the
 * latter removes the appended row. Allocation exceptions are not rolled back. */
extern DSL_PU_SOURCE_IDENTITY_ID DSL_Call_Image_Add_PU_Identity
                                (const DSL_PU_SOURCE_IDENTITY_RECORD *record);

/* Copy a callsite with a fresh ID and borrow call under matching owner_pu_st.
 * Caller owns the WN/tree lifetime and physical CALL/callee correctness. Whole
 * call-image validation failure removes the new row/association and returns
 * INVALID_ID; input is unchanged. Allocation exceptions have no such rollback. */
extern DSL_CALLSITE_METADATA_ID DSL_Call_Image_Add_Callsite
                                (ST_IDX owner_pu_st, WN *call,
                                 const DSL_CALLSITE_METADATA_RECORD *record);

/* Find the first exact owner identity. Copy scalars/shared string IDs when
 * record is nonnull, or probe with NULL; FALSE leaves output unchanged. */
extern BOOL DSL_Call_Image_Find_PU_Identity
                                (ST_IDX owner_pu_st,
                                 DSL_PU_SOURCE_IDENTITY_RECORD *record);

/* Find the first pointer-equal runtime association, not a stored-offset match.
 * Copy its row or probe with NULL; FALSE leaves output unchanged. Mapped rows
 * need Load_PU associations before this lookup can find their WN pointers. */
extern BOOL DSL_Call_Image_Find_Callsite
                                (const WN *call,
                                 DSL_CALLSITE_METADATA_RECORD *record);

/* Return the first borrowed WN association for id, or NULL if absent. Neither
 * table bounds nor tree legality is checked; caller maintains WN lifetime. */
extern const WN *DSL_Call_Image_Get_Call_WN
                                (DSL_CALLSITE_METADATA_ID id);

/* Return the current identity row count without validation or mutation. */
extern UINT32 DSL_Call_Image_PU_Identity_Count (void);

/* Return the current callsite row count, not the runtime association count. */
extern UINT32 DSL_Call_Image_Callsite_Count (void);

/* Copy the one-based identity row, or probe with NULL output. Invalid IDs
 * return FALSE without changing output; copied strings remain shared IDs. */
extern BOOL DSL_Call_Image_Get_PU_Identity
                                (DSL_PU_SOURCE_IDENTITY_ID id,
                                 DSL_PU_SOURCE_IDENTITY_RECORD *record);

/* Copy the one-based callsite row, or probe with NULL output. Invalid IDs
 * leave output unchanged; this does not resolve wn_offset into a WN pointer. */
extern BOOL DSL_Call_Image_Get_Callsite
                                (DSL_CALLSITE_METADATA_ID id,
                                 DSL_CALLSITE_METADATA_RECORD *record);

/* Test callsite row ownership without walking a PU or checking associations.
 * A zero symbol index returns FALSE; no PU context is activated. */
extern BOOL DSL_Call_Image_PU_Has_Calls (ST_IDX owner_pu_st);

/* Publish selected owner's association offsets using the writer-established
 * Current_Map_Tab and valid INT32 off_map, then validate the whole call image.
 * Caller supplies live WNs/valid association IDs and a completed tree map.
 * An owner with no call rows returns TRUE without checking the map. FALSE
 * does not undo offsets already assigned; this does not switch PU context. */
extern BOOL DSL_Call_Image_Finalize_PU (ST_IDX owner_pu_st, WN_MAP off_map);

/* Append borrowed WN associations for the selected owner's nonzero offsets
 * below tree_size. Require nonzero owner index and readable, live tree storage;
 * caller verifies WN boundaries, CALL/callee legality and full node extents.
 * Existing associations are not cleared/deduplicated; FALSE can leave earlier
 * additions in place. No tree ownership transfer or PU activation occurs. */
extern BOOL DSL_Call_Image_Load_PU (ST_IDX owner_pu_st,
                                    const void *tree_base,
                                    UINT64 tree_size);

/* Fill a cleared ABI header from the current argument count; NULL is a no-op.
 * Does not validate or modify the argument table. */
extern void DSL_Call_ABI_Image_Get_Header (DSL_CALL_ABI_IMAGE_HEADER *header);

/* Clear only argument rows; callsite/core rows and physical trees survive. */
extern void DSL_Call_ABI_Image_Reset (void);

/* Test whether argument rows exist, without validation or mutation. */
extern BOOL DSL_Call_ABI_Image_Has_Records (void);

/* Check argument IDs, call/value references, non-sentinel ordinals, zero flags,
 * role strings, unique actual slots and role consistency for each callee/formal.
 * Uses current call/core/string tables; does not prove physical slot bounds,
 * ST/TY ownership or passing modes. No mutation; FALSE reports the first error
 * and NULL diagnostic is silent. Use Validate_PU for active-PU physical checks. */
extern BOOL DSL_Call_ABI_Image_Validate (FILE *diagnostic);

/* Require an aligned, readable section independent of live tables, with call,
 * core value and string context already loaded. Header/size rejection preserves
 * old arguments; subsequent replacement copies rows, and validation failure
 * leaves the ABI table empty. Other tables are unchanged, no section pointer is
 * retained, and allocation failure has no rollback guarantee. NULL is invalid,
 * not an absent-section request; NULL diagnostic suppresses messages. */
extern BOOL DSL_Call_ABI_Image_Load_Mapped (const void *section_base,
                                            UINT64 section_size,
                                            FILE *diagnostic);

/* Resolve call through its borrowed association and require matching callsite
 * ID, then copy the argument under a fresh ID. Input/shared string IDs are not
 * changed. Return INVALID_ID on preflight or whole-ABI validation failure;
 * the latter removes the append. No physical actual rewrite or exception
 * rollback is provided. */
extern DSL_CALL_ARGUMENT_ID DSL_Call_ABI_Image_Add_Argument
                                (const WN *call,
                                 const DSL_CALL_ARGUMENT_RECORD *record);

/* Return the full argument table size without validation or mutation. */
extern UINT32 DSL_Call_ABI_Image_Argument_Count (void);

/* Copy the one-based argument row, or probe with NULL output. Invalid IDs
 * return FALSE with output unchanged; semantic_role remains a shared string ID. */
extern BOOL DSL_Call_ABI_Image_Get_Argument
                                (DSL_CALL_ARGUMENT_ID id,
                                 DSL_CALL_ARGUMENT_RECORD *record);

/* Resolve a pointer-equal call association, then find its actual_ordinal row.
 * NULL output probes presence; failure leaves output unchanged. No physical
 * actual or ordinal-bound validation occurs. */
extern BOOL DSL_Call_ABI_Image_Find_Argument
                                (const WN *call, UINT32 actual_ordinal,
                                 DSL_CALL_ARGUMENT_RECORD *record);

/* Find the first exact callsite/actual pair without revalidating the keys.
 * Copy the row or probe with NULL; FALSE leaves output unchanged. */
extern BOOL DSL_Call_ABI_Image_Find_Argument_By_Id
                                (DSL_CALLSITE_METADATA_ID callsite_id,
                                 UINT32 actual_ordinal,
                                 DSL_CALL_ARGUMENT_RECORD *record);

/* Count metadata arguments whose callsite names this numeric callee/formal
 * pair, skipping missing callsites. No opposite-PU tree analysis or activation. */
extern UINT32 DSL_Call_ABI_Image_Callee_Formal_Count
                                (ST_IDX callee_pu_st,
                                 UINT32 callee_formal_ordinal);

/* Select the zero-based index among matching callee/formal metadata rows in
 * table order, not a physical formal index. Skip missing callsites, copy or
 * probe with NULL, and leave output unchanged on FALSE. No PU activation. */
extern BOOL DSL_Call_ABI_Image_Get_Callee_Formal_Argument
                                (ST_IDX callee_pu_st,
                                 UINT32 callee_formal_ordinal,
                                 UINT32 index,
                                 DSL_CALL_ARGUMENT_RECORD *record);

/* Clear the supplied header and fill image identity/current formal count.
 * NULL is a no-op; no row validation or table mutation occurs. */
extern void DSL_PU_Interface_Image_Get_Header
                                (DSL_PU_INTERFACE_IMAGE_HEADER *header);

/* Discard only PU-formal rows, not source values, dependent runtime/program
 * interface rows or physical formals. IDs may be reused; caller coordinates
 * dependent invalidation or the broader image reset. */
extern void DSL_PU_Interface_Image_Reset (void);

/* Test whether stored formal rows exist, including retired/projected sources;
 * this is not a count or validation of physical formals in an active PU. */
extern BOOL DSL_PU_Interface_Image_Has_Records (void);

/* In current global ST/PU and core-value context, check row identity, live
 * function owners, source symbol/value/TY agreement, non-sentinel ordinals,
 * zero flags/reserved and per-owner uniqueness of ordinal/value/ST. Does not
 * prove local ST residency, TY validity beyond a nonzero index, value owner
 * metadata, contiguous ordinals or physical interface coverage. Read-only;
 * FALSE reports the first error, NULL diagnostic is silent. Implemented in
 * dsl_ir_rewrite.cxx; no callee PU activation is needed. */
extern BOOL DSL_PU_Interface_Image_Validate (FILE *diagnostic);

/* Require an aligned, readable section independent of live formal storage,
 * with global ST/PU and core-value context loaded. Header/size rejection leaves
 * old formals intact; subsequent replacement copies rows and validates them.
 * Row failure leaves the formal table empty, not restored. Other tables/trees
 * are unchanged; no input pointers are retained or allocation-exception
 * rollback promised. NULL is invalid, not the absent-section protocol (the
 * reader resets absent sections); NULL diagnostic suppresses messages. */
extern BOOL DSL_PU_Interface_Image_Load_Mapped
                                (const void *section_base,
                                 UINT64 section_size,
                                 FILE *diagnostic);

/* Copy a nonnull source row with a fresh one-based ID, then structurally
 * validate all formals in current global ST/PU and core-value context. Input
 * is unchanged; INVALID_ID on validation failure removes only this append.
 * Not interning or physical formal construction/verification; no allocation
 * exception rollback. Per-owner duplicate ordinal/value/ST identities are
 * rejected. */
extern DSL_PU_FORMAL_ID DSL_PU_Interface_Image_Add_Formal
                                (const DSL_PU_FORMAL_RECORD *record);

/* Return all stored formal rows, including retired or projected source rows,
 * without validation or a physical FUNC_ENTRY traversal. */
extern UINT32 DSL_PU_Interface_Image_Formal_Count (void);

/* Copy the one-based formal row, or probe with NULL output. Invalid IDs return
 * FALSE leaving output unchanged; copied ST/TY/value IDs are not resolved. */
extern BOOL DSL_PU_Interface_Image_Get_Formal
                                (DSL_PU_FORMAL_ID id,
                                 DSL_PU_FORMAL_RECORD *record);

/* Find the first exact owner/ordinal metadata pair, without validating keys or
 * activating that PU. Copy the row or probe with NULL; FALSE leaves output
 * unchanged. The source ordinal need not equal a current physical position. */
extern BOOL DSL_PU_Interface_Image_Find_Formal
                                (ST_IDX owner_pu_st,
                                 UINT32 formal_ordinal,
                                 DSL_PU_FORMAL_RECORD *record);

/* Clear the supplied header and fill image identity and current projection
 * counts without validation or table mutation. NULL output is a no-op. */
extern void DSL_Runtime_Interface_Image_Get_Header
                                (DSL_RUNTIME_INTERFACE_IMAGE_HEADER *header);

/* Clear only value/call projections; source values, ABI/formal/program tables
 * and physical trees survive. IDs may be reused; caller coordinates dependent
 * invalidation and physical reconstruction or the broader image reset. */
extern void DSL_Runtime_Interface_Image_Reset (void);

/* Test either projection table for rows, including retired source provenance.
 * FALSE does not imply structural validation will pass with current call/formal
 * context, nor that a physical PU has no runtime interface. */
extern BOOL DSL_Runtime_Interface_Image_Has_Records (void);

/* Validate copied projection rows against current shared core/ST/TY/string,
 * call/ABI/formal and retired-formal context, without activating another PU.
 * Check source/handle identities, retirement provenance, unique relationships,
 * compatible call directions/handle types and complete nonretired call/formal
 * projection coverage. Caller supplies valid referenced core/type/string state;
 * this is not physical WN/local-ST proof or full program-interface validation.
 * No live mutation; FALSE reports the first error, NULL diagnostic is silent.
 * Scratch allocation exceptions are not converted to FALSE. */
extern BOOL DSL_Runtime_Interface_Image_Validate (FILE *diagnostic);

/* Require an aligned/readable section independent of live projection storage
 * and valid loaded shared core/ST/TY/string/call/ABI/formal context. Parse and
 * validate against live retired-formal state; if program rows exist, also run
 * their cross-check against the proposed projections, all before mutation.
 * Boolean rejection preserves live rows. Success resets/copies only projection
 * tables and retains no section pointers; allocation failure during commit has
 * no rollback guarantee. NULL is invalid, not an absent-section request (reader
 * handles absence). NULL diagnostic suppresses messages; no physical PU proof. */
extern BOOL DSL_Runtime_Interface_Image_Load_Mapped
                                (const void *section_base,
                                 UINT64 section_size,
                                 FILE *diagnostic);

/* Return all value projection rows, including retired provenance; no filtering,
 * validation or physical-handle traversal occurs. */
extern UINT32 DSL_Runtime_Interface_Image_Value_Count (void);

/* Return the stored call projection count, not a physical CALL census. */
extern UINT32 DSL_Runtime_Interface_Image_Call_Count (void);

/* Copy a one-based value projection, including retired rows, or probe with
 * NULL output. Invalid IDs leave output unchanged; scalar IDs are not resolved. */
extern BOOL DSL_Runtime_Interface_Image_Get_Value
                                (DSL_RUNTIME_VALUE_PROJECTION_ID id,
                                 DSL_RUNTIME_VALUE_PROJECTION_RECORD *record);

/* Copy a one-based call projection or probe with NULL output. Invalid IDs
 * return FALSE leaving output unchanged; no physical call is resolved. */
extern BOOL DSL_Runtime_Interface_Image_Get_Call
                                (DSL_RUNTIME_CALL_PROJECTION_ID id,
                                 DSL_RUNTIME_CALL_PROJECTION_RECORD *record);

/* Find the first exact owner/source-value pair not flagged RETIRED_VIEW; no key
 * validation or PU activation. Copy or probe with NULL, leaving output unchanged
 * on FALSE. Use Get_Value/count enumeration to inspect retired provenance. */
extern BOOL DSL_Runtime_Interface_Image_Find_Value
                                (ST_IDX owner_pu_st,
                                 DSL_IR_VALUE_ID source_value_id,
                                 DSL_RUNTIME_VALUE_PROJECTION_RECORD *record);

/* Find the first exact callsite/actual-ordinal metadata pair without key or WN
 * validation. Copy or probe with NULL; FALSE leaves output unchanged. No caller
 * or callee PU context is activated. */
extern BOOL DSL_Runtime_Interface_Image_Find_Call
                                (DSL_CALLSITE_METADATA_ID callsite_id,
                                 UINT32 actual_ordinal,
                                 DSL_RUNTIME_CALL_PROJECTION_RECORD *record);

/* Clear the supplied header and fill image identity and all five current row
 * counts, without validation or table mutation. NULL output is a no-op. */
extern void DSL_Program_Interface_Image_Get_Header
                                (DSL_PROGRAM_INTERFACE_IMAGE_HEADER *header);

/* Drop all five program tables and process-local prepared-plan/PU commit
 * evidence. IDs may be reused; source, call, formal, runtime-projection and WN
 * state survive. Caller coordinates dependent invalidation/reconstruction. */
extern void DSL_Program_Interface_Image_Reset (void);

/* Test the five program tables for any rows, not commit eligibility or physical
 * interface state. An empty image need not satisfy current projection coverage. */
extern BOOL DSL_Program_Interface_Image_Has_Records (void);

/* Snapshot program and runtime-projection rows and check retirement/coverage,
 * input profiles, canonical binding order and rooted acyclic input flow. Require
 * valid shared core/ST/TY/TCON/string/call/ABI/formal context; if an input's
 * source PU is already active, its matching local context is also required.
 * This does not activate PUs, prove deadness/physical ABI, or fully validate
 * runtime projections. No live mutation; FALSE reports the first error, NULL
 * diagnostic is silent. Scratch allocation exceptions are not Boolean errors. */
extern BOOL DSL_Program_Interface_Image_Validate (FILE *diagnostic);

/* Require an aligned/readable section independent of live program storage and
 * valid shared source/type/TCON/string/call/formal context, including matching
 * local state for an already-active external-input owner. Validate structure,
 * retirement coverage and input flow before mutation, but do not check runtime
 * projection exclusivity or lowered relations here; use the paired loader for
 * coordinated images. FALSE preserves old rows/commit evidence. Success resets
 * and copies the five program tables and marks mapped-commit eligibility, not
 * physical correctness. No retained section pointers or allocation rollback;
 * NULL is invalid (reader handles absence); NULL diagnostic is silent. */
extern BOOL DSL_Program_Interface_Image_Load_Mapped
                                (const void *section_base,
                                 UINT64 section_size,
                                 FILE *diagnostic);

/* Load coordinated program/runtime images with all Boolean preflight before
 * either commit: parse present sections, use staged retired formals for runtime
 * checks, cross-check program/projection coverage and lowered/dead relations.
 * With no program section, runtime parsing still uses live retired-formal state
 * before the later reset; it does not stage an empty retirement context.
 * NULL means an absent section and its size is ignored; successful preflight
 * resets absent families. Nonnull sections must remain aligned/readable and
 * independent of both live families throughout commit. Require valid shared
 * core/ST/TY/TCON/string/call/formal/CKKS state and any already-active source-PU
 * local context. FALSE preserves both families; allocation exceptions during
 * sequential reset/copy need not. No input pointers retained, PU activation or
 * physical ABI proof; NULL diagnostic suppresses messages. */
extern BOOL DSL_Program_Runtime_Interface_Images_Load_Mapped
                                (const void *program_section_base,
                                 UINT64 program_section_size,
                                 const void *runtime_section_base,
                                 UINT64 runtime_section_size,
                                 FILE *diagnostic);

/* Count stored formal-retirement provenance, not remaining physical formals. */
extern UINT32 DSL_Program_Interface_Image_Retired_Formal_Count (void);

/* Count stored call-argument retirements without scanning physical CALL kids. */
extern UINT32 DSL_Program_Interface_Image_Retired_Call_Count (void);

/* Count runtime-input records, independently of binding reachability. */
extern UINT32 DSL_Program_Interface_Image_Runtime_Input_Count (void);

/* Count all runtime bindings, roots and threaded formals, without validation. */
extern UINT32 DSL_Program_Interface_Image_Runtime_Binding_Count (void);

/* Count input-flow call records, not physical calls or dynamic invocations. */
extern UINT32 DSL_Program_Interface_Image_Runtime_Call_Count (void);

/* Copy one one-based retirement row or probe with NULL; invalid IDs return
 * FALSE leaving output unchanged. Scalar provenance IDs are not resolved. */
extern BOOL DSL_Program_Interface_Image_Get_Retired_Formal
                                (DSL_RETIRED_FORMAL_ID id,
                                 DSL_RETIRED_FORMAL_RECORD *record);

/* Copy one one-based call-retirement row or probe with NULL. Invalid IDs leave
 * output unchanged; no physical actual or source symbol is resolved. */
extern BOOL DSL_Program_Interface_Image_Get_Retired_Call
                                (DSL_RETIRED_CALL_ARGUMENT_ID id,
                                 DSL_RETIRED_CALL_ARGUMENT_RECORD *record);

/* Copy one one-based input row or probe with NULL; invalid IDs leave output
 * unchanged. Shared role/type/TCON/value IDs retain their existing lifetimes. */
extern BOOL DSL_Program_Interface_Image_Get_Runtime_Input
                                (DSL_RUNTIME_INPUT_ID id,
                                 DSL_RUNTIME_INPUT_RECORD *record);

/* Copy one one-based binding row or probe with NULL; invalid IDs leave output
 * unchanged. Does not resolve handle ST/TY or activate the owner PU. */
extern BOOL DSL_Program_Interface_Image_Get_Runtime_Binding
                                (DSL_RUNTIME_INPUT_BINDING_ID id,
                                 DSL_RUNTIME_INPUT_BINDING_RECORD *record);

/* Copy one one-based input-flow call row or probe with NULL; invalid IDs leave
 * output unchanged. No physical call lookup or caller/callee activation. */
extern BOOL DSL_Program_Interface_Image_Get_Runtime_Call
                                (DSL_RUNTIME_INPUT_CALL_ID id,
                                 DSL_RUNTIME_INPUT_CALL_RECORD *record);

/* Find the first canonical-formal ID match, with optional copy or NULL probe;
 * failure leaves output unchanged. This flat-view implementation assumes the
 * entire populated table is contiguous, which segmented growth does not ensure.
 * For segmented storage use indexed Get_Retired_Formal/count traversal. */
extern BOOL DSL_Program_Interface_Image_Find_Retired_Formal
                                (DSL_PU_FORMAL_ID pu_formal_id,
                                 DSL_RETIRED_FORMAL_RECORD *record);

/* Find the first canonical-argument ID match, with optional copy or NULL probe;
 * failure leaves output unchanged. The current flat view requires contiguous
 * populated storage; segmented growth is not generally safe for this scan.
 * Use indexed Get_Retired_Call/count traversal for segmented storage. */
extern BOOL DSL_Program_Interface_Image_Find_Retired_Call
                                (DSL_CALL_ARGUMENT_ID call_argument_id,
                                 DSL_RETIRED_CALL_ARGUMENT_RECORD *record);

/* Find the first owner/final-ordinal match, with optional copy or NULL probe;
 * failure leaves output unchanged. This assumes contiguous populated binding
 * storage, not generally guaranteed after segmented growth. Use indexed
 * Get_Runtime_Binding/count traversal for segmented storage. No PU activation. */
extern BOOL DSL_Program_Interface_Image_Find_Runtime_Binding
                                (ST_IDX owner_pu_st,
                                 UINT32 final_formal_ordinal,
                                 DSL_RUNTIME_INPUT_BINDING_RECORD *record);

/* Fill caller-owned header storage with current effect-table counts and the
 * fixed magic/version, zeroing flags/reserved; NULL is a no-op. Used by the
 * writer/printer, not a validation or mapped-section publication operation. */
extern void DSL_Effect_Image_Get_Header (DSL_EFFECT_IMAGE_HEADER *header);

/* Discard all live state/effect rows, invalidating their IDs and borrowed views;
 * later appends may reuse IDs. Core rows, shared strings, ST/TY and physical
 * state remain unchanged. Reader/lifetime owners must retire dependent handles;
 * this whole-effect-image reset has no savepoint or restoration service. */
extern void DSL_Effect_Image_Reset (void);

/* Test whether either live effect-family table is nonempty, even if invalid;
 * lets the writer omit an absent family without proving row consistency. */
extern BOOL DSL_Effect_Image_Has_Records (void);

/*
 * With core node/descriptor links already valid, check current state/effect
 * rows: sequential IDs, state kind/name/flags and uniqueness, effect bounds,
 * per-node ordinals/uniqueness and non-pure owner descriptors. Shared strings
 * must be available. ST/owner indices are tested for nonzero, not resolved to
 * live symbols or used to prove PU ownership. Return FALSE at the first error;
 * NULL diagnostic is silent and no table is modified.
 */
extern BOOL DSL_Effect_Image_Validate (FILE *diagnostic);

/*
 * Require an aligned, readable section independent of effect table storage,
 * available shared strings and valid core node/descriptor links. Reject NULL,
 * short or invalid header/size without mutation. After those checks, discard
 * old effect rows, copy new rows, then validate them: row rejection clears both
 * effect tables and returns FALSE, without restoring old rows. Success retains
 * no input pointers and leaves core tables alone. Allocation failure has no
 * rollback guarantee; missing sections are handled by reader-owned reset.
 */
extern BOOL DSL_Effect_Image_Load_Mapped (const void *section_base,
                                          UINT64 section_size,
                                          FILE *diagnostic);

/* Zero caller-owned state storage or ignore NULL; no row/symbol is created.
 * The resulting UNKNOWN kind and zero identities are not a valid state. */
extern void DSL_State_Object_Record_Init (DSL_STATE_OBJECT_RECORD *record);

/* Zero caller-owned effect storage or ignore NULL; no edge is attached.
 * Caller must supply endpoints, supported kind and per-node ordinal. */
extern void DSL_State_Effect_Record_Init (DSL_STATE_EFFECT_RECORD *record);

/* Copy and append a state row, replacing its supplied ID with the one-based
 * slot; caller retains its input and all referenced string/ST identities.
 * Require readable shared strings and valid core node/descriptor links for
 * validation of the entire effect family, including pre-existing effects.
 * Reject invalid input or any resulting family error with INVALID_ID; a
 * post-append rejection truncates only this new row, preserving prior rows.
 * Does not resolve STs, prove physical PU/unique ownership or undo caller
 * allocations. Allocation failures are uncaught; no general rollback. */
extern DSL_STATE_OBJECT_ID DSL_Effect_Image_Add_State_Object
                                (const DSL_STATE_OBJECT_RECORD *record);

/* Copy and append a node/state edge with a freshly assigned one-based ID;
 * caller supplies ordinal (count of prior edges for that node), not this API.
 * Require valid core descriptor links and shared strings for whole-family
 * validation; endpoints must exist and the owner descriptor must not be PURE.
 * Duplicate node/state pairs or other family errors return INVALID_ID after
 * removing only the attempted append. No physical-PU, scheduling or effect-
 * flags proof; supplied flags survive. Input remains caller-owned; no caller
 * allocation rollback or recovery from uncaught allocation failure. */
extern DSL_STATE_EFFECT_ID DSL_Effect_Image_Add_State_Effect
                                (const DSL_STATE_EFFECT_RECORD *record);

/* Count all stored states, not active-PU states or proven live symbols. */
extern UINT32 DSL_Effect_Image_State_Object_Count (void);

/* Count stored edges without filtering node retirement or checking legality. */
extern UINT32 DSL_Effect_Image_State_Effect_Count (void);

/* Copy a one-based state row or probe its slot with NULL. Invalid IDs return
 * FALSE without writing output; shared string/ST identities are not resolved
 * or validated and retain their external lifetimes. No borrowed row returned. */
extern BOOL DSL_Effect_Image_Get_State_Object
                                (DSL_STATE_OBJECT_ID id,
                                 DSL_STATE_OBJECT_RECORD *record);

/* Copy a one-based edge or probe with NULL. Invalid IDs leave output unchanged;
 * success checks only slot bounds, not endpoint liveness or effect legality. */
extern BOOL DSL_Effect_Image_Get_State_Effect
                                (DSL_STATE_EFFECT_ID id,
                                 DSL_STATE_EFFECT_RECORD *record);

/* Return a static diagnostic spelling; UNKNOWN or out-of-range kinds yield
 * "unknown". No allocation or state validation; caller must not free it. */
extern const char *DSL_State_Kind_Name (DSL_STATE_KIND kind);

/* Return a static effect spelling, falling back to "unknown" for unsupported
 * values; no edge validation/allocation and no ownership transfer. */
extern const char *DSL_State_Effect_Kind_Name
                                (DSL_STATE_EFFECT_KIND effect_kind);

/*
 * Zero a caller-owned descriptor, then set operand_count to -1; NULL is a
 * no-op. This allocates no ID and does not produce a complete valid descriptor.
 */
extern void DSL_IR_Opcode_Descriptor_Record_Init
                                (DSL_IR_OPCODE_DESCRIPTOR_RECORD *record);

/* Zero a caller-owned node, or ignore NULL; no table entry/links are created. */
extern void DSL_IR_Node_Record_Init (DSL_IR_NODE_RECORD *record);

/* Zero a caller-owned attribute, or ignore NULL; owner/name/kind remain unset. */
extern void DSL_IR_Attribute_Record_Init (DSL_IR_ATTRIBUTE_RECORD *record);

/* Zero a caller-owned value, or ignore NULL; kind/identity must still be set. */
extern void DSL_IR_Value_Record_Init (DSL_IR_VALUE_RECORD *record);

/* Zero a caller-owned reference, or ignore NULL; no owner/value is resolved. */
extern void DSL_IR_Value_Reference_Record_Init
                                (DSL_IR_VALUE_REFERENCE_RECORD *record);

/*
 * Copy a descriptor into table storage and replace its supplied ID with the
 * next one-based ID. Reject only NULL, zero stable_name or zero version here;
 * do not deduplicate or certify strings, logical semantics or enum fields.
 * STR_IDX ownership stays with the shared string table. Rejection returns zero
 * unchanged; allocation uses the existing table service, not a batch rollback.
 */
extern DSL_IR_OPCODE_DESCRIPTOR_ID DSL_IR_Image_Add_Opcode_Descriptor
                                (const DSL_IR_OPCODE_DESCRIPTOR_RECORD *record);

/*
 * Return the first stored logical-operator/version match, or zero. This query
 * neither validates the key/row semantics nor mutates or interns a descriptor.
 */
extern DSL_IR_OPCODE_DESCRIPTOR_ID DSL_IR_Image_Find_Opcode_Descriptor
                                (UINT32 logical_operator,
                                 UINT32 version);

/*
 * Reuse the first existing key without revalidation; otherwise query static
 * operator metadata, intern its strings and append a copied descriptor. Supply
 * a supported UINT16-range version: the metadata lookup narrows this UINT32
 * argument without a range check. Missing metadata returns zero before string
 * interning. Allocation failures have no string/table transaction rollback.
 */
extern DSL_IR_OPCODE_DESCRIPTOR_ID DSL_IR_Image_Ensure_Opcode_Descriptor
                                (UINT32 logical_operator,
                                 UINT32 version);

/*
 * Copy a node with a live descriptor ID, assigning a fresh one-based node ID.
 * Reject NULL/invalid descriptor IDs unchanged with zero. Other fields and
 * descriptor semantics are not verified; finish links before full validation.
 */
extern DSL_IR_NODE_ID DSL_IR_Image_Add_Node
                                (const DSL_IR_NODE_RECORD *record);

/*
 * Copy an attribute and assign a fresh ID. Require nonnull input, nonzero name,
 * a kind other than UNKNOWN and a live owner node. These are shallow checks,
 * not string/kind-range/schema validation; zero rejection leaves tables intact.
 */
extern DSL_IR_ATTRIBUTE_ID DSL_IR_Image_Add_Attribute
                                (const DSL_IR_ATTRIBUTE_RECORD *record);

/*
 * Copy a value and assign a fresh ID. Require nonnull input, kind other than
 * UNKNOWN and either zero producer or a live node. No TY/ST, descriptor, kind
 * range or producer-role proof is made; shallow rejection returns zero unchanged.
 */
extern DSL_IR_VALUE_ID DSL_IR_Image_Add_Value
                                (const DSL_IR_VALUE_RECORD *record);

/*
 * Copy a reference with live nonzero owner-node and value IDs, assigning a new
 * ID; NULL/invalid IDs return zero unchanged. Ordinal and semantic compatibility
 * are not checked here. The input row remains caller-owned and unmodified.
 */
extern DSL_IR_VALUE_REFERENCE_ID DSL_IR_Image_Add_Value_Reference
                                (const DSL_IR_VALUE_REFERENCE_RECORD *record);

/*
 * Require an existing node's descriptor link to be valid, as established by
 * checked construction or mapped-image validation. Preflight contiguous ranges,
 * zero-count/zero-start consistency, fixed nonnegative descriptor arity,
 * reference owners and zero-based ordinals, attribute owners and result producer.
 * On success assign the five link fields only; rejection writes nothing and
 * does not remove prior appends. This is not an acyclicity, type/schema or
 * domain verifier and does not alter physical WHIRL.
 */
extern BOOL DSL_IR_Image_Set_Node_Links
                                (DSL_IR_NODE_ID node_id,
                                 DSL_IR_VALUE_REFERENCE_ID first_operand_id,
                                 UINT32 operand_count,
                                 DSL_IR_ATTRIBUTE_ID first_attribute_id,
                                 UINT32 attribute_count,
                                 DSL_IR_VALUE_ID result_value_id);

/* Replace one logical node's opcode/operands/attributes/payload and result kind
 * without changing its node/result IDs, ST/TY or physical WN. Require valid
 * existing core rows/ranges, representable table sizes, a stable caller-owned
 * request and readable count-sized arrays. The owning transaction supplies
 * schema/type/effect/owner legality and coordinates any physical replacement.
 * All reported FALSE paths precede mutation. TRUE rebuilds reference/attribute
 * tables for every node, renumbering those row IDs and updating all range starts;
 * old reference/attribute IDs and views must not be retained across the call.
 * No final verifier or general rollback is provided. Allocation failures are
 * not caught and may leave partial replacement once live-table commit begins. */
extern BOOL DSL_IR_Image_Rewrite_Node
                                (const DSL_IR_NODE_REWRITE_REQUEST *request);

/* Read one immediate target operand of REDIRECTED/RETIRED provenance. Require
 * nonnull output and prevalidated producer/operand-reference ranges; this does
 * not validate their upper bounds or follow a redirect chain. Boolean failure
 * leaves output unchanged; TRUE returns the stored value ID, not a runtime
 * handle or a fully validated target. No mutation or owner-PU activation. */
extern BOOL DSL_IR_Image_Value_Redirect_Target
                                (DSL_IR_VALUE_ID value_id,
                                 DSL_IR_VALUE_ID *target_value_id);

/* Find one globally unambiguous stored ST/name match, ignoring owner metadata.
 * Names/shared string IDs must be readable; zero ST index or NULL name rejects.
 * NULL record probes uniqueness, otherwise copy the POD row; FALSE leaves it
 * unchanged. Local ST ordinals reused in different PUs can be ambiguous: use
 * Find_PU_Value with the exact owner name when that distinction is required.
 * No flags/type/physical ownership proof, mutation or PU activation occurs. */
extern BOOL DSL_IR_Image_Find_Value
                                (ST_IDX st,
                                 const char *name,
                                 DSL_IR_VALUE_RECORD *record);

/* Find exactly one stored ST/name match, optionally requiring the entire
 * metadata string to equal "owner_pu=<owner_pu>"; NULL owner disables that
 * filter, not a request to use Current_pu. Require readable names/shared string
 * IDs; zero ST index, NULL name, no match or multiple matches returns FALSE
 * without changing output. NULL record probes presence/uniqueness, otherwise
 * copy the POD row. Retired/lowered/dead flags are not filtered, and numeric
 * ST/name/metadata matching is not physical ownership or ST/TY validity proof.
 * No table writes, string interning or PU activation is performed. */
extern BOOL DSL_IR_Image_Find_PU_Value
                                (ST_IDX st,
                                 const char *name,
                                 const char *owner_pu,
                                 DSL_IR_VALUE_RECORD *record);

/* Read the stored descriptor row count, not a count of unique logical keys. */
extern UINT32 DSL_IR_Image_Opcode_Descriptor_Count (void);

/* Read all stored node rows, including retired/lowered/dead-elided provenance. */
extern UINT32 DSL_IR_Image_Node_Count (void);

/*
 * Count rows lacking RETIRED, LOWERED and DEAD_ELIDED node flags. This is a
 * global table scan, not a PU-filtered count or physical executable-tree walk.
 */
extern UINT32 DSL_IR_Image_Executable_Node_Count (void);

/* Read the stored attribute row count, including rows outside active links. */
extern UINT32 DSL_IR_Image_Attribute_Count (void);

/* Read all stored value rows regardless of lowering/retirement flags. */
extern UINT32 DSL_IR_Image_Value_Count (void);

/* Read all stored reference rows, not only references in live node ranges. */
extern UINT32 DSL_IR_Image_Value_Reference_Count (void);

/*
 * Copy the row for a live one-based ID; NULL output tests ID bounds only.
 * Invalid IDs return FALSE without writes. No descriptor verification or string
 * ownership transfer occurs; copied STR_IDX fields require the shared strings.
 */
extern BOOL DSL_IR_Image_Get_Opcode_Descriptor
                                (DSL_IR_OPCODE_DESCRIPTOR_ID id,
                                 DSL_IR_OPCODE_DESCRIPTOR_RECORD *record);

/* Copy a bounded node row, or probe with NULL; invalid IDs leave output intact. */
extern BOOL DSL_IR_Image_Get_Node (DSL_IR_NODE_ID id,
                                   DSL_IR_NODE_RECORD *record);

/*
 * Copy a bounded attribute row or probe with NULL; invalid IDs do not write.
 * Referenced STR_IDX values stay in the shared string table, not this copy.
 */
extern BOOL DSL_IR_Image_Get_Attribute
                                (DSL_IR_ATTRIBUTE_ID id,
                                 DSL_IR_ATTRIBUTE_RECORD *record);

/* Copy a bounded value row or probe with NULL; do not resolve/validate TY/ST IDs. */
extern BOOL DSL_IR_Image_Get_Value (DSL_IR_VALUE_ID id,
                                    DSL_IR_VALUE_RECORD *record);

/* Copy a bounded reference row or probe with NULL; do not follow its value ID. */
extern BOOL DSL_IR_Image_Get_Value_Reference
                                (DSL_IR_VALUE_REFERENCE_ID id,
                                 DSL_IR_VALUE_REFERENCE_RECORD *record);

#endif /* DSL_IMAGE_SERVICE_INCLUDED */
