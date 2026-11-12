/* Copyright (C) 2026 Open64 Project */
/* Detached LNO search input/report; native pointers never cross this interface.
 * Design: doc/AI-COMPILER-SEARCH-M2C-OBSERVER-v1.0.md. No selected application,
 * candidate lowering, or mapped-image representation is provided here. */
#ifndef dsl_high_observer_INCLUDED
#define dsl_high_observer_INCLUDED
#include "dsl_high_snapshot.h"
#include "../vho/tensor_attention_search.h"

struct DSL_HIGH_SEARCH_INPUT {
    std::string unit, function, source_key, facts_key, numeric, host_target;
    VHO_TENSOR_PROGRAM algorithm;
    VHO_TENSOR_GPU_PROFILE gpu;
    VHO_MLA_COST_PROFILE model;
    std::vector<unsigned> buffers;
    bool allow_reassociate;
    /* Missing permission never defaults to authorization. */
    DSL_HIGH_SEARCH_INPUT() : allow_reassociate(false) {}
};
struct DSL_HIGH_SEARCH_REPORT {
    VHO_MLA_SEARCH_RESULT exploration;
    std::string resource_key, model_key, manifest_key;
};

/* Strict, bounded JSON adapter into owned records. Reject duplicate/unknown
 * fields, coercions and unsupported versions; clear output on any error. */
extern bool LNO_DSL_read_search_input(const char *, DSL_HIGH_SEARCH_INPUT &, std::string &);
/* Validate every shape/ABI/profile/model field and exact source/fact bindings.
 * Effective numerical permission and native producer/ABI checks are supplied
 * separately by the owning native caller before this detached service runs. */
extern bool LNO_DSL_validate_search_input(const DSL_HIGH_SNAPSHOT &, const DSL_HIGH_BINDINGS &,
    const DSL_HIGH_SEARCH_INPUT &, std::string &);
/* Explore only owned records using the existing finite MLA portfolio. This
 * service has no native handles and never applies or materializes a winner. */
extern bool LNO_DSL_observe_search(const DSL_HIGH_SEARCH_INPUT &, DSL_HIGH_SEARCH_REPORT &, std::string &);
/* Native active-PU wrapper: validate lineage/ABI, guard physical tree/table
 * state, run detached exploration, revalidate analyses and retain fallback. */
extern void LNO_DSL_observe_high(WN *, DU_MANAGER *, ALIAS_MANAGER *, ARRAY_DIRECTED_GRAPH16 *,
    const DSL_HIGH_EPOCH &, const DSL_HIGH_BINDINGS &, const DSL_HIGH_SNAPSHOT &, uint64_t, FILE *);
/* Monotonic host collection/validation/exploration accounting, never GPU time. */
extern uint64_t LNO_DSL_observer_time_us();
#endif
