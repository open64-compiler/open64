/*
 * Copyright (C) 2026 Open64 Project
 */

/* Numerical ISA contract and symbolic storage/event binding for the direct
 * ordered fallback. Design: doc/AI-COMPILER-REDUCTION-DIRECT-BINDING-v0.1.md.
 * No MMA capability, performance estimate, runtime assertion or transformation. */
#include <string.h>

#include "opt_dsl_reduction.h"
#include "dsl_memory_hierarchy.h"
#include "symtab.h"

/* Reject stale cross-node/type/shape/order requests before accepting contracts. */
static BOOL
WOPT_DSL_reduction_source_equal
    (const WOPT_DSL_REDUCTION_CHARACTERIZATION &a,
     const WOPT_DSL_REDUCTION_CHARACTERIZATION &b)
{
  return a.recipe == b.recipe && a.flags == b.flags && a.owner_pu == b.owner_pu &&
      a.node_id == b.node_id && a.input_value_id == b.input_value_id &&
      a.result_value_id == b.result_value_id && a.input_ty == b.input_ty &&
      a.result_ty == b.result_ty && a.rows == b.rows && a.width == b.width &&
      a.source_line == b.source_line;
}

/* The initial fallback admits only sealed contiguous row-major source meaning.
 * Runtime allocations must implement this same layout; a label is not a lease. */
static BOOL
WOPT_DSL_reduction_contiguous_type(TY_IDX ty)
{
  const char *layout = TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_LAYOUT);
  return layout != NULL &&
      (strcmp(layout, "row_major") == 0 || strcmp(layout, "contiguous") == 0);
}

/* Check exact accessed byte interval, without overflowing or assuming noalias.
 * Runtime canonical allocation/base-address guarantees remain obligations. */
static BOOL
WOPT_DSL_reduction_range_valid
    (const WOPT_DSL_REDUCTION_BUFFER_BINDING &range, UINT64 bytes)
{
  return range.memory_space == WOPT_DSL_REDUCE_BUFFER_DEVICE_GLOBAL &&
      range.allocation_id != 0 && range.base_alignment >= 4 &&
      (range.base_alignment & (range.base_alignment - 1)) == 0 &&
      range.offset_bytes % 4 == 0 && range.offset_bytes <= range.allocation_bytes &&
      bytes <= range.allocation_bytes - range.offset_bytes;
}

/* Fixed six-role partial order: ready < launch < complete < consume < output
 * release, and complete < input release. Validate closure and absence of cycles,
 * not event timestamps, stream numbering or the textual order of declarations. */
static BOOL
WOPT_DSL_reduction_events_valid(const UINT32 *before)
{
  UINT32 reach[WOPT_DSL_REDUCE_EVENT_COUNT];
  const UINT32 allowed = (1U << WOPT_DSL_REDUCE_EVENT_COUNT) - 1;
  for (UINT32 i = 0; i < WOPT_DSL_REDUCE_EVENT_COUNT; ++i) {
    if ((before[i] & ~allowed) != 0) return FALSE;
    reach[i] = before[i];
  }
  for (UINT32 k = 0; k < WOPT_DSL_REDUCE_EVENT_COUNT; ++k)
    for (UINT32 i = 0; i < WOPT_DSL_REDUCE_EVENT_COUNT; ++i)
      if ((reach[i] & (1U << k)) != 0) reach[i] |= reach[k];
  for (UINT32 i = 0; i < WOPT_DSL_REDUCE_EVENT_COUNT; ++i)
    if ((reach[i] & (1U << i)) != 0) return FALSE;
  return (reach[WOPT_DSL_REDUCE_INPUT_READY] & (1U << WOPT_DSL_REDUCE_LAUNCH)) &&
      (reach[WOPT_DSL_REDUCE_LAUNCH] & (1U << WOPT_DSL_REDUCE_COMPLETE)) &&
      (reach[WOPT_DSL_REDUCE_COMPLETE] & (1U << WOPT_DSL_REDUCE_CONSUME)) &&
      (reach[WOPT_DSL_REDUCE_COMPLETE] & (1U << WOPT_DSL_REDUCE_INPUT_RELEASE)) &&
      (reach[WOPT_DSL_REDUCE_CONSUME] & (1U << WOPT_DSL_REDUCE_OUTPUT_RELEASE));
}

/* A refusal never grants later stages or runtime obligations as proved facts.
 * Stage bits mean source=1, ISA=2, storage=4, events/owner=8 are contract-bound. */
static UINT32
WOPT_DSL_reduction_direct_status
    (const WOPT_DSL_REDUCTION_DIRECT_REQUEST &request,
     WOPT_DSL_REDUCTION_DIRECT_BINDING *binding)
{
  if (!WOPT_DSL_reduction_source_equal(request.expected_source, binding->source))
    return WOPT_DSL_REDUCE_DIRECT_STALE_SOURCE;
  if (binding->source.recipe != WOPT_DSL_REDUCTION_SUM_F32_ORDERED_V2)
    return WOPT_DSL_REDUCE_DIRECT_SOURCE_UNBOUND;
  binding->stages_bound = 1;
  DSL_HARDWARE_DESCRIPTION_RECORD hardware;
  if (!DSL_hardware_description_find_arch(request.architecture, &hardware))
    return WOPT_DSL_REDUCE_DIRECT_TARGET_UNKNOWN;
  binding->architecture_id = hardware.arch_id;
  if (request.instruction != WOPT_DSL_REDUCE_PTX_ADD_RN_F32)
    return WOPT_DSL_REDUCE_DIRECT_INSTRUCTION_UNSUPPORTED;
  if (request.fp_options != 0)
    return WOPT_DSL_REDUCE_DIRECT_FP_OPTIONS_UNSAFE;
  binding->stages_bound |= 2;
  if (!WOPT_DSL_reduction_contiguous_type(binding->source.input_ty) ||
      !WOPT_DSL_reduction_contiguous_type(binding->source.result_ty))
    return WOPT_DSL_REDUCE_DIRECT_LAYOUT_UNSUPPORTED;
  const UINT64 maximum = ~(UINT64)0;
  if (binding->source.rows > maximum / 4 ||
      binding->source.width > maximum / (binding->source.rows * 4))
    return WOPT_DSL_REDUCE_DIRECT_RANGE_INVALID;
  binding->output_bytes = binding->source.rows * 4;
  binding->input_bytes = binding->output_bytes * binding->source.width;
  if (!WOPT_DSL_reduction_range_valid(request.input, binding->input_bytes) ||
      !WOPT_DSL_reduction_range_valid(request.output, binding->output_bytes))
    return WOPT_DSL_REDUCE_DIRECT_RANGE_INVALID;
  if (request.runtime_contract_id == 0 || request.context_id == 0 ||
      request.input.lease != WOPT_DSL_REDUCE_INPUT_SNAPSHOT ||
      request.output.lease != WOPT_DSL_REDUCE_OUTPUT_EXCLUSIVE)
    return WOPT_DSL_REDUCE_DIRECT_LEASE_MISSING;
  if (request.input.allocation_id == request.output.allocation_id) {
    if (request.input.allocation_bytes != request.output.allocation_bytes)
      return WOPT_DSL_REDUCE_DIRECT_RANGE_INVALID;
    if (request.input.offset_bytes < request.output.offset_bytes + binding->output_bytes &&
        request.output.offset_bytes < request.input.offset_bytes + binding->input_bytes)
      return WOPT_DSL_REDUCE_DIRECT_ALIAS_OVERLAP;
  }
  binding->stages_bound |= 4;
  if (request.row_mapping != WOPT_DSL_REDUCE_ROW_LINEAR_THREAD ||
      request.launched_threads < binding->source.rows)
    return WOPT_DSL_REDUCE_DIRECT_OWNER_INVALID;
  if (!WOPT_DSL_reduction_events_valid(request.before))
    return WOPT_DSL_REDUCE_DIRECT_EVENTS_INVALID;
  binding->stages_bound |= 8;
  binding->runtime_obligations = WOPT_DSL_REDUCE_RUNTIME_CANONICAL_ALLOCATIONS |
      WOPT_DSL_REDUCE_RUNTIME_LIVE_LEASES | WOPT_DSL_REDUCE_RUNTIME_EVENT_ENFORCEMENT |
      WOPT_DSL_REDUCE_RUNTIME_LAUNCH_CONFORMANCE;
  return WOPT_DSL_REDUCE_DIRECT_BOUND;
}

/* The concise row includes all symbolic range, policy and event inputs so the
 * independent auditor can recompute legality, rather than trusting a status. */
static void
WOPT_DSL_reduction_direct_trace
    (const WOPT_DSL_REDUCTION_DIRECT_REQUEST &request,
     const WOPT_DSL_REDUCTION_DIRECT_BINDING &binding, FILE *trace)
{
  static const char *reason[] = {
    "bound_runtime_obligations", "source_binding_missing", "stale_source",
    "unknown_architecture", "unsupported_instruction", "unsafe_fp_options",
    "unsupported_layout", "invalid_range", "runtime_lease_missing",
    "overlapping_ranges", "invalid_owner_mapping", "invalid_event_order"
  };
  const WOPT_DSL_REDUCTION_CHARACTERIZATION &s = binding.source;
  fprintf(trace, "AI_REDUCE_DIRECT 1 HEADER node input result rows width recipe source_match contiguous arch instruction "
      "fp_options contract context in_alloc in_capacity in_offset in_align in_lease "
      "in_space out_alloc out_capacity out_offset out_align out_lease out_space mapping threads "
      "e0 e1 e2 e3 e4 e5 in_bytes out_bytes stages obligations status selected source_line\n");
  fprintf(trace, "AI_REDUCE_DIRECT 1 BIND %u %u %u %llu %llu %u %u %u %u %u %u "
      "%llu %llu %llu %llu %llu %u %u %u %llu %llu %llu %u %u %u %u %llu "
      "%u %u %u %u %u %u %llu %llu %u %u %s 0 %u\n",
      s.node_id, s.input_value_id, s.result_value_id,
      (unsigned long long)s.rows, (unsigned long long)s.width, s.recipe,
      (UINT32)WOPT_DSL_reduction_source_equal(request.expected_source, s),
      (UINT32)(WOPT_DSL_reduction_contiguous_type(s.input_ty) &&
               WOPT_DSL_reduction_contiguous_type(s.result_ty)),
      binding.architecture_id, request.instruction, request.fp_options,
      (unsigned long long)request.runtime_contract_id, (unsigned long long)request.context_id,
      (unsigned long long)request.input.allocation_id, (unsigned long long)request.input.allocation_bytes,
      (unsigned long long)request.input.offset_bytes, request.input.base_alignment, request.input.lease,
      request.input.memory_space,
      (unsigned long long)request.output.allocation_id, (unsigned long long)request.output.allocation_bytes,
      (unsigned long long)request.output.offset_bytes, request.output.base_alignment, request.output.lease,
      request.output.memory_space,
      request.row_mapping, (unsigned long long)request.launched_threads,
      request.before[0], request.before[1], request.before[2], request.before[3], request.before[4], request.before[5],
      (unsigned long long)binding.input_bytes, (unsigned long long)binding.output_bytes,
      binding.stages_bound, binding.runtime_obligations, reason[binding.status], s.source_line);
}

/* Native WHIRL remains authoritative; the caller supplies target/host contracts,
 * not facts inferred from pointer samples, Python or diagnostic metadata. */
BOOL
WOPT_DSL_bind_reduction_direct
    (WN *scope, WN *assignment, const WOPT_DSL_REDUCTION_DIRECT_REQUEST *request,
     WOPT_DSL_REDUCTION_DIRECT_BINDING *output, FILE *diagnostic, FILE *trace)
{
  if (request == NULL || output == NULL) return FALSE;
  WOPT_DSL_REDUCTION_DIRECT_BINDING binding;
  memset(&binding, 0, sizeof(binding));
  if (!WOPT_DSL_characterize_reduction(scope, assignment, &binding.source, diagnostic))
    return FALSE;
  binding.status = WOPT_DSL_reduction_direct_status(*request, &binding);
  binding.instruction = request->instruction;
  binding.fp_options = request->fp_options;
  binding.runtime_contract_id = request->runtime_contract_id;
  binding.context_id = request->context_id;
  binding.input = request->input;
  binding.output = request->output;
  binding.row_mapping = request->row_mapping;
  binding.launched_threads = request->launched_threads;
  memcpy(binding.before, request->before, sizeof(binding.before));
  if (trace != NULL) WOPT_DSL_reduction_direct_trace(*request, binding, trace);
  *output = binding;
  return TRUE;
}
