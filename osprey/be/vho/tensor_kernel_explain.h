/* Copyright (C) 2026 Open64 Project */
/* Detached VHO-selected explanation records shared by WHIRL annotation and
 * source presentation. Design: doc/AI-COMPILER-TENSOR-SCHEDULE-SOURCE-v0.2.md.
 * No IR layout, optimization policy, or compilation scope is changed here. */
#ifndef tensor_kernel_explain_INCLUDED
#define tensor_kernel_explain_INCLUDED
#include "tensor_kernel_lower.h"
enum VHO_TENSOR_EXPLANATION_KIND {
    VHO_TENSOR_EXPLAIN_OVERVIEW,
    VHO_TENSOR_EXPLAIN_STORAGE,
    VHO_TENSOR_EXPLAIN_STAGE,
    VHO_TENSOR_EXPLAIN_PRODUCER
};
struct VHO_TENSOR_EXPLANATION {
    UINT32 kernel_id,kind,value;
    std::string text; /* exact WN COMMENT content, without enclosing delimiters */
};
/* Describe only a validated selected schedule; no WN/ST/TY allocation occurs.
 * Records are ordered, deterministic, and owned by the returned vector. */
extern std::vector<VHO_TENSOR_EXPLANATION> VHO_tensor_explanations(const VHO_TENSOR_SCHEDULE &);
#endif
