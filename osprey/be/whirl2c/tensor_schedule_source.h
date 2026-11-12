/* Copyright (C) 2026 Open64 Project */
/* Read-only presentation of a VHO-selected detached tensor schedule.
 * Design: doc/AI-COMPILER-TENSOR-SCHEDULE-SOURCE-v0.1.md. This printer owns
 * no selection, WHIRL mutation, driver scope, or production GPU emission. */
#ifndef tensor_schedule_source_INCLUDED
#define tensor_schedule_source_INCLUDED
#include <string>
struct VHO_TENSOR_SCHEDULE;
/* Return compilable host/device review CUDA for the supplied selected stages.
 * Invalid or unsupported presentation contracts throw before publication. */
extern std::string W2C_tensor_schedule_source(const VHO_TENSOR_SCHEDULE &,
    const std::string &recipe_fingerprint);
#endif
