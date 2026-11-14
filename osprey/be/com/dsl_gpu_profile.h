/* Copyright (C) 2026 Open64 Project */
/* Backend driver-owned H1b profile import service. Reads a bounded, pinned
 * external artifact once, validates static bindings and freezes owned facts.
 * No PU traversal, optimizer policy, native mutation or hardware probing.
 * Design: doc/AI-COMPILER-H1B-GPU-PROFILE-IMPORT-v1.0.md. */
#ifndef dsl_gpu_profile_INCLUDED
#define dsl_gpu_profile_INCLUDED
#include <stdio.h>
#include "dsl_memory_hierarchy.h"
#include "dsl_device_profile.h"

/* Resolve validated H1a identity plus optional file/digest/toolchain inputs.
 * With no file, companions must also be absent and all facts stay unknown.
 * An import requires all three nonempty inputs and explicit device identity.
 * On success atomically replace output with a sealed owned snapshot; previous
 * borrowed views expire. Failure preserves the previous context. Diagnostics
 * contain no profile data and output is never a mapped-WHIRL artifact. */
extern BOOL DSL_gpu_profile_resolve(const DSL_HARDWARE_TARGET_SELECTION &target,
    const char *path, const char *expected_sha256, const char *expected_toolchain,
    DSL_GPU_DEVICE_CONTEXT_OWNER *output, FILE *diagnostics);
#endif
