/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_contract_INCLUDED
#define dsl_contract_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_domain.h"

#define DSL_CONTRACT_INVALID_ID 0

#define DSL_CONTRACT_FLAG_EXPRESSION 0x00000001
#define DSL_CONTRACT_FLAG_REGION     0x00000002
#define DSL_CONTRACT_FLAG_STATE      0x00000004

#define DSL_CONTRACT_TRANSFORMER_ROTARY_EMBEDDING \
    "transformer.rotary_embedding"
#define DSL_CONTRACT_TRANSFORMER_ATTENTION "transformer.attention"
#define DSL_CONTRACT_TRANSFORMER_DECODER_LAYER "transformer.decoder_layer"
#define DSL_CONTRACT_TRANSFORMER_DECODE "transformer.decode"
#define DSL_CONTRACT_TRANSFORMER_KV_CACHE_STATE \
    "transformer.kv_cache_state"

typedef UINT32 DSL_CONTRACT_ID;

typedef struct {
    DSL_CONTRACT_ID id;
    DSL_DOMAIN_ID source_domain_id;
    DSL_DOMAIN_ID target_domain_id;
    const char *name;
    UINT32 version;
    UINT32 flags;
    UINT32 required_check_count;
    UINT32 diagnostic_code_count;
} DSL_CONTRACT_INFO;

extern void DSL_Contract_Registry_Reset (void);
extern DSL_CONTRACT_ID DSL_Contract_Register
				    (const char *name,
				     DSL_DOMAIN_ID source_domain_id,
				     DSL_DOMAIN_ID target_domain_id,
				     UINT32 version,
				     UINT32 flags,
				     const char *const *required_checks,
				     UINT32 required_check_count,
				     const char *const *diagnostic_codes,
				     UINT32 diagnostic_code_count);
extern DSL_CONTRACT_ID DSL_Contract_Register_Versioned
                                    (const char *name,
                                     DSL_DOMAIN_ID source_domain_id,
                                     DSL_DOMAIN_ID target_domain_id,
                                     UINT32 version,
                                     UINT32 flags,
                                     const char *const *required_checks,
                                     UINT32 required_check_count,
                                     const char *const *diagnostic_codes,
                                     UINT32 diagnostic_code_count);
extern DSL_CONTRACT_ID DSL_Contract_Find (const char *name,
					  DSL_DOMAIN_ID source_domain_id,
					  DSL_DOMAIN_ID target_domain_id);
extern DSL_CONTRACT_ID DSL_Contract_Find_Version
                                         (const char *name,
                                          DSL_DOMAIN_ID source_domain_id,
                                          DSL_DOMAIN_ID target_domain_id,
                                          UINT32 version);
extern DSL_CONTRACT_ID DSL_Contract_Find_Current
                                         (const char *name,
                                          DSL_DOMAIN_ID source_domain_id,
                                          DSL_DOMAIN_ID target_domain_id);
extern BOOL DSL_Contract_Get_Info (DSL_CONTRACT_ID id,
				   DSL_CONTRACT_INFO *info);
extern UINT32 DSL_Contract_Count (void);
extern BOOL DSL_Contract_At (UINT32 ordinal, DSL_CONTRACT_INFO *info);
extern const char *DSL_Contract_Required_Check_At (DSL_CONTRACT_ID id,
						  UINT32 ordinal);
extern const char *DSL_Contract_Diagnostic_Code_At (DSL_CONTRACT_ID id,
						   UINT32 ordinal);
extern UINT32 DSL_Contract_Register_Transformer_Decode (void);
extern void DSL_Contract_fprint_registry (FILE *f);

#endif /* dsl_contract_INCLUDED */
