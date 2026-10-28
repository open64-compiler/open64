/* Native staged ordered-reduction binding and exact slice derivation.
 * Design: doc/AI-COMPILER-REDUCTION-STAGED-BINDING-v0.1.md.
 * Reuses source/ISA/root/event binding and numerical recipe discovery. No MMA1,
 * runtime ABI extension, selected WHIRL mutation or generated kernel body. */
#include <string.h>
#include "opt_dsl_reduction_staged.h"
#include "dsl_memory_hierarchy.h"

/* Checked arithmetic for scratch extents, resource reservation and CTA coverage. */
static BOOL
WOPT_DSL_staged_add(UINT64 a, UINT64 b, UINT64 *out)
{
  if (b > (~(UINT64)0) - a) return FALSE;
  *out = a + b;
  return TRUE;
}

/* Reject an unrepresentable product without publishing a wrapped footprint. */
static BOOL
WOPT_DSL_staged_mul(UINT64 a, UINT64 b, UINT64 *out)
{
  if (a != 0 && b > (~(UINT64)0) / a) return FALSE;
  *out = a * b;
  return TRUE;
}

/* Synchronous five-event protocol is a total order. Every adjacent edge must
 * exist; extra forward shortcuts are legal, backward/self/unknown edges are not.
 * This specialized proof is not a second general-purpose event-DAG solver. */
static BOOL
WOPT_DSL_staged_events(const UINT32 *before)
{
  for (UINT32 i = 0; i < 5; ++i) {
    UINT32 allowed = 31U & ~((1U << (i + 1)) - 1);
    if (before[i] & ~allowed) return FALSE;
    if (i < 4 && !(before[i] & (1U << (i + 1)))) return FALSE;
  }
  return TRUE;
}

/* Reuse needs a last-read release before the next generation may acquire.
 * Other carry protocols require a separately reviewed producer/consumer model. */
static BOOL
WOPT_DSL_staged_reuse(const UINT32 *next)
{
  return next[0] == 0 && next[1] == 0 && next[2] == 0 && next[3] == 0 && next[4] == 1;
}

/* Bind physical scratch under the existing target's conservative planning
 * envelope; real launch limits, symbol allocation and code conformance remain
 * obligations. No timing estimate or MMA capability participates in legality. */
static UINT32
WOPT_DSL_staged_status(WN *scope, WN *assignment,
    WOPT_DSL_REDUCTION_STAGED_BINDING *b, FILE *diagnostic)
{
  const WOPT_DSL_REDUCTION_STAGING &s = b->staging;
  if (b->base.status != WOPT_DSL_REDUCE_DIRECT_BOUND)
    return WOPT_DSL_REDUCE_STAGED_BASE_REFUSED;
  if (!s.chunk_elements) return WOPT_DSL_REDUCE_STAGED_CHUNK_INVALID;
  WOPT_DSL_REDUCTION_ALTERNATIVES recipe;
  if (!WOPT_DSL_query_reduction_alternatives(scope, assignment, s.chunk_elements,
      s.per_row_budget, &recipe, diagnostic, NULL)) return WOPT_DSL_REDUCE_STAGED_CHUNK_INVALID;
  b->chunk = recipe.chunk_elements;
  b->row_bytes = recipe.scratch_bytes;
  if (s.copy_mode != WOPT_DSL_REDUCE_COPY_SYNC_BITS32 ||
      s.visibility != WOPT_DSL_REDUCE_VISIBLE_SAME_THREAD)
    return WOPT_DSL_REDUCE_STAGED_COPY_UNSUPPORTED;
  if (s.slots != 1 || s.memory_space != WOPT_DSL_REDUCE_STAGED_SHARED ||
      !s.scratch_object || s.scratch_offset % 4 || s.row_stride % 4 ||
      s.row_stride < b->row_bytes) return WOPT_DSL_REDUCE_STAGED_STORAGE_INVALID;
  DSL_HARDWARE_DESCRIPTION_RECORD hw;
  if (!DSL_hardware_description_get_arch(b->base.architecture_id, &hw))
    return WOPT_DSL_REDUCE_STAGED_BASE_REFUSED;
  b->hardware_budget = hw.resources.shared_budget;
  b->headroom = hw.resources.shared_reserved;
  b->quantum = hw.resources.shared_quantum;
  b->max_threads = hw.resources.max_threads_per_cta;
  UINT64 launched;
  if (!s.rows_per_cta || s.rows_per_cta > b->max_threads ||
      s.cta_count != 1 + (b->base.source.rows - 1) / s.rows_per_cta ||
      !WOPT_DSL_staged_mul(s.rows_per_cta, s.cta_count, &launched) ||
      launched != b->base.launched_threads) return WOPT_DSL_REDUCE_STAGED_OWNER_INVALID;
  UINT64 rows_bytes, total, rounded;
  if (recipe.staged_status != WOPT_DSL_REDUCE_LEGAL_RECIPE ||
      !WOPT_DSL_staged_mul(s.rows_per_cta - 1, s.row_stride, &rows_bytes) ||
      !WOPT_DSL_staged_add(s.scratch_offset, rows_bytes, &b->scratch_end) ||
      !WOPT_DSL_staged_add(b->scratch_end, b->row_bytes, &b->scratch_end) ||
      b->scratch_end > s.allocation_bytes || !s.cta_budget || s.cta_budget > b->hardware_budget ||
      !WOPT_DSL_staged_add(s.allocation_bytes, s.other_shared_bytes, &total) ||
      !WOPT_DSL_staged_add(total, b->quantum - 1, &rounded))
    return WOPT_DSL_REDUCE_STAGED_CAPACITY;
  rounded = rounded / b->quantum * b->quantum;
  if (!WOPT_DSL_staged_add(rounded, b->headroom, &b->reserved_bytes) ||
      b->reserved_bytes > s.cta_budget) return WOPT_DSL_REDUCE_STAGED_CAPACITY;
  if (!WOPT_DSL_staged_events(s.before)) return WOPT_DSL_REDUCE_STAGED_EVENTS_INVALID;
  if (!WOPT_DSL_staged_reuse(s.next)) return WOPT_DSL_REDUCE_STAGED_REUSE_INVALID;
  if (s.tail_policy != WOPT_DSL_REDUCE_TAIL_EXACT_COPY_READ)
    return WOPT_DSL_REDUCE_STAGED_TAIL_UNSUPPORTED;
  b->chunks = 1 + (b->base.source.width - 1) / b->chunk;
  b->last_count = 1 + (b->base.source.width - 1) % b->chunk;
  b->last_active_rows = 1 + (b->base.source.rows - 1) % s.rows_per_cta;
  b->runtime_obligations = b->base.runtime_obligations |
      WOPT_DSL_REDUCE_RUNTIME_SCRATCH_ALLOCATION | WOPT_DSL_REDUCE_RUNTIME_COPY_VISIBILITY |
      WOPT_DSL_REDUCE_RUNTIME_SLOT_LIFETIME | WOPT_DSL_REDUCE_RUNTIME_EXACT_TAIL_ORDER;
  return WOPT_DSL_REDUCE_STAGED_BOUND;
}

/* One positional row retains choices, resource provenance and exact bounds;
 * no selected, executed-copy or runtime-discharge claim is emitted. */
static void
WOPT_DSL_staged_trace(const WOPT_DSL_REDUCTION_STAGED_BINDING &b, FILE *trace)
{
  const WOPT_DSL_REDUCTION_STAGING &s = b.staging;
  fprintf(trace, "AI_REDUCE_STAGE 1 HEADER node input result rows width version base_status arch chunk_request "
      "slots scratch_object space offset stride allocation row_budget cta_budget other rows_cta ctas threads "
      "copy visibility tail e0 e1 e2 e3 e4 n0 n1 n2 n3 n4 chunk chunks last_count last_active "
      "row_bytes scratch_end reserved hw_budget headroom quantum max_threads obligations status selected source_line\n");
  fprintf(trace, "AI_REDUCE_STAGE 1 BIND %u %u %u %llu %llu %u %u %u %llu %u %llu %u "
      "%llu %llu %llu %llu %llu %llu %llu %llu %llu %u %u %u %u %u %u %u %u %u %u %u %u %u "
      "%llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %u %u 0 %u\n",
      b.base.source.node_id, b.base.source.input_value_id, b.base.source.result_value_id,
      (unsigned long long)b.base.source.rows, (unsigned long long)b.base.source.width,
      b.base.source.recipe, b.base.status, b.base.architecture_id,
      (unsigned long long)s.chunk_elements, s.slots, (unsigned long long)s.scratch_object, s.memory_space,
      (unsigned long long)s.scratch_offset, (unsigned long long)s.row_stride,
      (unsigned long long)s.allocation_bytes, (unsigned long long)s.per_row_budget,
      (unsigned long long)s.cta_budget, (unsigned long long)s.other_shared_bytes,
      (unsigned long long)s.rows_per_cta, (unsigned long long)s.cta_count,
      (unsigned long long)b.base.launched_threads,
      s.copy_mode, s.visibility, s.tail_policy,
      s.before[0], s.before[1], s.before[2], s.before[3], s.before[4],
      s.next[0], s.next[1], s.next[2], s.next[3], s.next[4],
      (unsigned long long)b.chunk, (unsigned long long)b.chunks,
      (unsigned long long)b.last_count, (unsigned long long)b.last_active_rows,
      (unsigned long long)b.row_bytes, (unsigned long long)b.scratch_end,
      (unsigned long long)b.reserved_bytes, (unsigned long long)b.hardware_budget,
      (unsigned long long)b.headroom, (unsigned long long)b.quantum, (unsigned long long)b.max_threads,
      b.runtime_obligations, b.status, b.base.source.source_line);
}

/* Reuse the direct source/ISA/storage/host-DAG gate before adding staging.
 * Native validation fails transactionally; source-valid refusal is inspectable. */
BOOL
WOPT_DSL_bind_reduction_staged(WN *scope, WN *assignment,
    const WOPT_DSL_REDUCTION_STAGED_REQUEST *request,
    WOPT_DSL_REDUCTION_STAGED_BINDING *output, FILE *diagnostic, FILE *trace)
{
  if (!request || !output) return FALSE;
  WOPT_DSL_REDUCTION_STAGED_BINDING b;
  memset(&b, 0, sizeof(b));
  if (!WOPT_DSL_bind_reduction_direct(scope, assignment, &request->base, &b.base, diagnostic, NULL))
    return FALSE;
  b.staging = request->staging;
  b.status = WOPT_DSL_staged_status(scope, assignment, &b, diagnostic);
  if (trace) WOPT_DSL_staged_trace(b, trace);
  *output = b;
  return TRUE;
}

/* Reconstruct a request without retaining the caller's architecture string. */
static BOOL
WOPT_DSL_staged_revalidate(WN *scope, WN *assignment,
    const WOPT_DSL_REDUCTION_STAGED_BINDING &old,
    WOPT_DSL_REDUCTION_STAGED_BINDING *fresh, FILE *diagnostic)
{
  if (old.status != WOPT_DSL_REDUCE_STAGED_BOUND || old.runtime_obligations != 255) return FALSE;
  DSL_HARDWARE_DESCRIPTION_RECORD hw;
  if (!DSL_hardware_description_get_arch(old.base.architecture_id, &hw)) return FALSE;
  WOPT_DSL_REDUCTION_STAGED_REQUEST req;
  memset(&req, 0, sizeof(req));
  req.base.expected_source = old.base.source;
  req.base.architecture = hw.architecture;
  req.base.instruction = old.base.instruction;
  req.base.fp_options = old.base.fp_options;
  req.base.runtime_contract_id = old.base.runtime_contract_id;
  req.base.context_id = old.base.context_id;
  req.base.input = old.base.input;
  req.base.output = old.base.output;
  req.base.row_mapping = old.base.row_mapping;
  req.base.launched_threads = old.base.launched_threads;
  memcpy(req.base.before, old.base.before, sizeof(req.base.before));
  req.staging = old.staging;
  if (!WOPT_DSL_bind_reduction_staged(scope, assignment, &req, fresh, diagnostic, NULL) ||
      fresh->status != WOPT_DSL_REDUCE_STAGED_BOUND) return FALSE;
  return old.base.status == fresh->base.status && old.base.stages_bound == fresh->base.stages_bound &&
      old.base.runtime_obligations == fresh->base.runtime_obligations &&
      old.base.input_bytes == fresh->base.input_bytes && old.base.output_bytes == fresh->base.output_bytes &&
      old.chunk == fresh->chunk && old.chunks == fresh->chunks && old.last_count == fresh->last_count &&
      old.last_active_rows == fresh->last_active_rows && old.row_bytes == fresh->row_bytes &&
      old.scratch_end == fresh->scratch_end && old.reserved_bytes == fresh->reserved_bytes &&
      old.hardware_budget == fresh->hardware_budget && old.headroom == fresh->headroom &&
      old.quantum == fresh->quantum && old.max_threads == fresh->max_threads;
}

/* Use fresh bounded extents: generation*chunk < N and row*N < R*N; the direct
 * byte-footprint/range gate already proves every multiply/add fits UINT64. */
BOOL
WOPT_DSL_reduction_staged_slice(WN *scope, WN *assignment,
    const WOPT_DSL_REDUCTION_STAGED_BINDING *binding, UINT64 row, UINT64 generation,
    WOPT_DSL_REDUCTION_STAGED_SLICE *output, FILE *diagnostic, FILE *trace)
{
  if (!binding || !output) return FALSE;
  WOPT_DSL_REDUCTION_STAGED_BINDING b;
  if (!WOPT_DSL_staged_revalidate(scope, assignment, *binding, &b, diagnostic) ||
      row >= b.base.source.rows || generation >= b.chunks) return FALSE;
  WOPT_DSL_REDUCTION_STAGED_SLICE s;
  memset(&s, 0, sizeof(s));
  s.row = row; s.generation = generation; s.base = generation * b.chunk;
  s.count = generation + 1 == b.chunks ? b.last_count : b.chunk;
  s.input_offset = b.base.input.offset_bytes + 4 * (row * b.base.source.width + s.base);
  s.scratch_offset = b.staging.scratch_offset + (row % b.staging.rows_per_cta) * b.staging.row_stride;
  s.output_offset = b.base.output.offset_bytes + 4 * row;
  if (trace) {
    fprintf(trace, "AI_REDUCE_SLICE 1 HEADER node row generation base count input_offset scratch_offset output_offset source_line\n");
    fprintf(trace, "AI_REDUCE_SLICE 1 PLAN %u %llu %llu %llu %llu %llu %llu %llu %u\n",
        b.base.source.node_id, (unsigned long long)s.row, (unsigned long long)s.generation,
        (unsigned long long)s.base, (unsigned long long)s.count, (unsigned long long)s.input_offset,
        (unsigned long long)s.scratch_offset, (unsigned long long)s.output_offset, b.base.source.source_line);
  }
  *output = s;
  return TRUE;
}
