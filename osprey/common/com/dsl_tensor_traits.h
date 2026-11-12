/* Copyright (C) 2026 Open64 Project */
/* Policy-free names for semantic tensor content roles. These use the existing
 * TensorTraitSet string carrier, not new TY flags or mapped-image records.
 * No role implies a unique allocation, residency, readiness, or immutability
 * across invocations. See doc/AI-COMPILER-MLA-CACHE-TRAITS-v1.0.md. */
#ifndef dsl_tensor_traits_INCLUDED
#define dsl_tensor_traits_INCLUDED
#include <cstring>

#define DSL_TENSOR_TRAIT_LATENT_KV_CACHE "attention.latent_kv_cache.v1"
#define DSL_TENSOR_TRAIT_ROTARY_KEY_CACHE "attention.rotary_key_cache.v1"

/* Recognize only the two published single-role contracts; unknown combinations
 * remain unsupported at the current tensor-expression lowering boundary. */
static inline bool DSL_tensor_cache_trait_known(const char *trait) {
    return trait && (!strcmp(trait, DSL_TENSOR_TRAIT_LATENT_KV_CACHE) ||
                     !strcmp(trait, DSL_TENSOR_TRAIT_ROTARY_KEY_CACHE));
}
#endif
