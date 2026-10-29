/* VHO-owned runtime-only typed matmul source contract for MMA3.
 * No binary image fields, target selection or expression simplification;
 * dsl_typed_simt_opt.cxx owns executable lowering.
 * Design: doc/AI-COMPILER-MMA3-TYPED-SIMT-BASELINE.md. */
#ifndef dsl_typed_simt_INCLUDED
#define dsl_typed_simt_INCLUDED

#include "dsl_accumulation.h"

typedef struct {
    UINT32 id;
    UINT32 version;
    ST_IDX owner_pu_st;
    UINT32 numerical_contract_id;
    UINT32 numerical_acceptance_id;
    UINT32 generation;
    UINT32 problem_m;
    UINT32 problem_n;
    UINT32 problem_k;
    UINT32 threads_per_cta;
    UINT32 disjoint_output;
    UINT32 reserved;
} DSL_TYPED_SIMT_RECORD;

/* Validate only the accepted numerical, storage-alias and static shape
 * contract for borrowed records. Require F16/BF16 inputs with explicit FP32
 * product/partial/outer/output formats; return FALSE with optional diagnostics
 * on rejection. No permissions are inferred and no input is mutated. */
extern BOOL DSL_typed_simt_verify(const DSL_TYPED_SIMT_RECORD *record,
    const DSL_NUMERICAL_CONTRACT_RECORD *numerical, FILE *diagnostic);

#endif
