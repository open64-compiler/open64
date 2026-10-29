/* VHO runtime compute-realization joins for one driver-established active PU.
 * Immutable profiles belong to common/targ_info/access; this phase owns the
 * nonpersistent plan/protocol/resource records and structural joins. No image
 * state, candidate generation, timing, selection or emission is introduced.
 * Design: doc/AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md, MMA1. */
#ifndef dsl_compute_realization_INCLUDED
#define dsl_compute_realization_INCLUDED

#include "dsl_compute_profile.h"
#include "dsl_accumulation.h"
#include "dsl_fetch_pipeline.h"

typedef UINT32 DSL_COMPUTE_REALIZATION_ID;
/* An unresolved typed fallback requirement is distinct from an executable
 * OptimizationPlanIR. Inventory joins keep the plan unresolved; the materializer
 * must separately verify an executable same-contract fallback before lowering. */
typedef struct {
    UINT32 id;
    UINT32 family;
    UINT32 numerical_contract_id;
    UINT32 source_format;
    UINT32 output_format;
    UINT32 support;
    DSL_OPT_PLAN_ID plan_id;
    UINT64 generation;
    UINT32 reserved;
} DSL_COMPUTE_FALLBACK_RECORD;

/* Required event references only. These are not an event graph or a proof of
 * completion/progress. Existing copy-ready stages cannot represent MMA done. */
typedef struct {
    UINT32 id;
    UINT32 recipe;
    UINT32 obligations;
    UINT32 operand_ready;
    UINT32 issue;
    UINT32 completion;
    UINT32 result_read;
    UINT32 reuse_join;
    UINT32 allocation;
    UINT32 deallocation;
    UINT32 permit_release;
    UINT32 outstanding_groups;
    UINT32 reserved;
} DSL_COMPUTE_PROTOCOL_RECORD;

/* Extension beside the existing RF/SMEM ledger; TMEM is neither RF nor SMEM.
 * Quantities are caller-supplied planning requirements, not native allocations. */
typedef struct {
    DSL_FETCH_RESOURCE_RECORD storage;
    UINT32 tmem_lanes;
    UINT32 tmem_columns;
    UINT32 tmem_allocation_columns;
    UINT32 allocation_scope;
    UINT32 reserved;
} DSL_COMPUTE_RESOURCE_RECORD;

typedef struct {
    DSL_COMPUTE_REALIZATION_ID id;
    UINT32 version;
    ST_IDX owner_pu_st;
    DSL_PU_SOURCE_IDENTITY_ID source_identity_id;
    UINT32 region_id;
    DSL_IR_NODE_ID semantic_node_id;
    DSL_OPT_PLAN_ID plan_id;
    DSL_TILE_PLAN_ID tile_plan_id;
    DSL_COMPUTE_PROFILE_ID profile_id;
    UINT32 descriptor_recipe;
    UINT32 a_source_tier;
    UINT32 b_source_tier;
    UINT32 fragment_mapping;
    DSL_TENSOR_EVOLUTION_NODE_ID operand_nodes[2];
    DSL_TENSOR_EVOLUTION_NODE_ID accumulator_node;
    DSL_TENSOR_EVOLUTION_NODE_ID result_node;
    UINT32 numerical_contract_id;
    UINT32 numerical_acceptance_id;
    UINT32 protocol_id;
    UINT32 fallback_requirement_id;
    UINT64 generation;
    UINT32 required_ptx;
    UINT32 required_toolkit;
    UINT32 lowering_support;
    UINT32 native_support;
    UINT32 reserved;
    UINT32 source_line;
} DSL_COMPUTE_REALIZATION_RECORD;

/* All references are borrowed for this call, in driver-established PU scope.
 * Four evolution nodes are A, B, accumulator, result; descriptor IDs retain
 * real tensor identities. Full descriptor/alias/ISA proofs belong to the VHO
 * materializer, not this structural join service. */
typedef struct {
    ST_IDX owner_pu_st;
    UINT32 arch_id;
    UINT32 region_id;
    DSL_PU_SOURCE_IDENTITY_ID source_identity_id;
    UINT64 generation;
    UINT32 numerical_acceptance_id;
    const DSL_TILE_SITE_RECORD *site;
    const DSL_TILE_PLAN_RECORD *tile;
    const DSL_OPT_PLAN_RECORD *plan;
    const DSL_TENSOR_EVOLUTION_NODE_RECORD *representations;
    UINT32 representation_count;
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical;
    const DSL_COMPUTE_PROTOCOL_RECORD *protocol;
    const DSL_COMPUTE_RESOURCE_RECORD *resources;
    const DSL_COMPUTE_FALLBACK_RECORD *fallback;
} DSL_COMPUTE_REALIZATION_LINKS;

/* Structural joins, inventory restrictions and generation validation only.
 * Returns no selected plan and mutates no inputs or WHIRL on any path. */
extern BOOL DSL_compute_realization_verify(
    const DSL_COMPUTE_REALIZATION_RECORD *record,
    const DSL_COMPUTE_REALIZATION_LINKS *links, FILE *diagnostic);

/* Separate versioned inspection stream, never AI_OPT/model6 reinterpretation.
 * Null stream or rejected input returns FALSE before printing; I/O failure
 * returns FALSE after potentially partial output. No input or WHIRL mutation.
 * Candidate source line remains last. */
extern BOOL DSL_compute_realization_print(FILE *file,
    const DSL_COMPUTE_REALIZATION_RECORD *record,
    const DSL_COMPUTE_REALIZATION_LINKS *links);

#endif
