/* Guarded ordered-reduction leases and event enforcement.
 * Design: doc/AI-COMPILER-REDUCTION-RUNTIME-GUARDS-v0.1.md.
 * Runtime-only library: never linked into be.so/wopt.so; no CUDA dependency. */
#include "open64_reduction_runtime.h"
#include <atomic>
#include <mutex>
#include <new>
#include <string.h>
#include <limits.h>

namespace {
const unsigned OPEN64_REDUCTION_LIMIT = 64;
struct OPEN64_REDUCTION_ALLOCATION {
    uint64_t id, backing_object;
    OPEN64_TARGET_BUFFER_VIEW view;
};
struct OPEN64_REDUCTION_TICKET {
    uint64_t id, input, output;
    OPEN64_REDUCTION_ENTRY entry;
    OPEN64_REDUCTION_INVOCATION invocation;
    uint32_t predecessors[6], done, leases;
    bool busy, attempted, launch_failed, consumer_started, aborted;
};
std::atomic<uint64_t> open64_reduction_next_id(1);

/* Never wrap/reuse an ID, including across contexts. Exhaustion fails closed. */
uint64_t open64_reduction_mint()
{
    uint64_t id = open64_reduction_next_id.load();
    while (id != UINT64_MAX) {
        if (open64_reduction_next_id.compare_exchange_weak(id, id + 1)) return id;
    }
    return 0;
}

/* Compare half-open device-address intervals after registration overflow checks. */
bool open64_reduction_overlap(const OPEN64_TARGET_BUFFER_VIEW &a,
                              const OPEN64_TARGET_BUFFER_VIEW &b)
{
    uintptr_t x = reinterpret_cast<uintptr_t>(a.address);
    uintptr_t y = reinterpret_cast<uintptr_t>(b.address);
    return x < y + b.bytes && y < x + a.bytes;
}

/* Guard equality ignores C padding; all declared fields participate. */
bool open64_reduction_range_equal(const OPEN64_REDUCTION_RANGE_V1 &a,
                                  const OPEN64_REDUCTION_RANGE_V1 &b)
{
    return a.role == b.role && a.capacity == b.capacity && a.offset == b.offset &&
        a.alignment == b.alignment && a.lease == b.lease &&
        a.memory_space == b.memory_space && a.reserved == b.reserved;
}

/* A trusted entry's manifest must implement this exact compiler contract. */
bool open64_reduction_guard_equal(const OPEN64_REDUCTION_GUARD &a,
                                  const OPEN64_REDUCTION_GUARD &b)
{
    return a.abi_version == b.abi_version && a.size == b.size &&
        a.source_version == b.source_version && a.node == b.node &&
        a.input_value == b.input_value && a.result_value == b.result_value &&
        a.source_line == b.source_line && a.architecture == b.architecture &&
        a.instruction == b.instruction && a.fp_options == b.fp_options &&
        a.obligations == b.obligations && a.row_mapping == b.row_mapping &&
        a.rows == b.rows && a.width == b.width && a.threads == b.threads &&
        a.contract == b.contract && a.context == b.context &&
        a.input_bytes == b.input_bytes && a.output_bytes == b.output_bytes &&
        open64_reduction_range_equal(a.input, b.input) &&
        open64_reduction_range_equal(a.output, b.output) &&
        memcmp(a.before, b.before, sizeof(a.before)) == 0;
}

/* Validate closure, including extra edges; derive every predecessor mask. */
bool open64_reduction_graph(const uint32_t *before, uint32_t *predecessors)
{
    uint32_t reach[6];
    memset(predecessors, 0, 6 * sizeof(uint32_t));
    for (unsigned i = 0; i < 6; ++i) {
        if (before[i] & ~63U) return false;
        reach[i] = before[i];
    }
    for (unsigned k = 0; k < 6; ++k)
        for (unsigned i = 0; i < 6; ++i)
            if (reach[i] & (1U << k)) reach[i] |= reach[k];
    for (unsigned i = 0; i < 6; ++i) {
        if (reach[i] & (1U << i)) return false;
        for (unsigned j = 0; j < 6; ++j)
            if (reach[i] & (1U << j)) predecessors[j] |= 1U << i;
    }
    return (reach[0] & 2) && (reach[1] & 4) && (reach[2] & 8) &&
        (reach[2] & 16) && (reach[3] & 32);
}

/* Read unaligned blobs via memcpy; validate all referenced shape/stride bytes. */
bool open64_reduction_descriptor(const void *blob, size_t bytes,
                                 uint64_t rows, uint64_t width, bool input)
{
    if (!blob || bytes < sizeof(OPEN64_DSL_TENSOR_DESCRIPTOR_V1)) return false;
    OPEN64_DSL_TENSOR_DESCRIPTOR_V1 d;
    memcpy(&d, blob, sizeof(d));
    unsigned rank = input ? 2 : 1;
    if (d.abi_version != 1 || d.header_size != sizeof(d) || d.total_size > bytes ||
        d.total_size < sizeof(d) || d.dtype != OPEN64_DSL_DTYPE_F32 ||
        d.rank != static_cast<int>(rank) || d.shape_count != rank ||
        d.shape_offset < sizeof(d) || d.shape_offset > d.total_size ||
        8 * rank > d.total_size - d.shape_offset || d.reserved || (d.flags & ~7U) ||
        d.layout != OPEN64_DSL_LAYOUT_CONTIGUOUS ||
        d.memory != OPEN64_DSL_MEMORY_DEVICE ||
        d.placement != OPEN64_DSL_PLACEMENT_DEVICE ||
        d.sharding != OPEN64_DSL_SHARDING_REPLICATED ||
        d.quantization != OPEN64_DSL_QUANTIZATION_NONE ||
        d.runtime_state != OPEN64_DSL_RUNTIME_STATE_STATIC ||
        (!input && (d.flags & OPEN64_DSL_TENSOR_FLAG_CONSTANT))) return false;
    int64_t shape[2] = {0, 0}, stride[2] = {0, 0};
    memcpy(shape, static_cast<const char *>(blob) + d.shape_offset, 8 * rank);
    if (shape[0] <= 0 || static_cast<uint64_t>(shape[0]) != rows ||
        (input && (shape[1] <= 0 || static_cast<uint64_t>(shape[1]) != width))) return false;
    if (d.flags & OPEN64_DSL_TENSOR_FLAG_HAS_STRIDES) {
        if (d.stride_count != rank || d.stride_offset < sizeof(d) ||
            d.stride_offset > d.total_size || 8 * rank > d.total_size - d.stride_offset)
            return false;
        memcpy(stride, static_cast<const char *>(blob) + d.stride_offset, 8 * rank);
        return stride[0] > 0 && static_cast<uint64_t>(stride[0]) == (input ? width : 1) &&
            (!input || stride[1] == 1);
    }
    return d.stride_count == 0 && d.stride_offset == 0;
}

/* Canonical role ranges must have exact capacity/alignment and correct leases. */
bool open64_reduction_range_valid(const OPEN64_REDUCTION_RANGE_V1 &r,
                                  uint64_t bytes, uint32_t lease)
{
    return r.role && r.alignment >= 4 && !(r.alignment & (r.alignment - 1)) &&
        r.lease == lease && r.memory_space == 1 && !r.reserved &&
        r.offset % 4 == 0 && r.offset <= r.capacity && bytes <= r.capacity - r.offset;
}
}

struct OPEN64_REDUCTION_RUNTIME {
    std::mutex mutex;
    OPEN64_TARGET_BUFFER_SERVICE service;
    OPEN64_REDUCTION_ROOT_RESOLVER resolve_root;
    uint32_t architecture, flags;
    uint64_t context, seq;
    FILE *trace;
    OPEN64_REDUCTION_ALLOCATION allocations[OPEN64_REDUCTION_LIMIT];
    OPEN64_REDUCTION_TICKET tickets[OPEN64_REDUCTION_LIMIT];
    /* Only plain state is cleared; never byte-clear a mutex. */
    OPEN64_REDUCTION_RUNTIME() : resolve_root(NULL), architecture(0), flags(0), context(0), seq(0), trace(NULL)
    {
        memset(&service, 0, sizeof(service));
        memset(allocations, 0, sizeof(allocations));
        memset(tickets, 0, sizeof(tickets));
    }
};

namespace {
/* Trace while holding the context lock. Source line is always last. */
int open64_reduction_report(OPEN64_REDUCTION_RUNTIME *r, uint64_t id,
    const char *action, int status, const OPEN64_REDUCTION_TICKET *t = NULL)
{
    if (r->trace) fprintf(r->trace, "AI_REDUCE_RUN 1 EVENT %llu %llu %s %d %u %u %u\n",
        static_cast<unsigned long long>(++r->seq), static_cast<unsigned long long>(id),
        action, status, t ? t->done : 0, t ? t->leases : 0,
        t ? t->invocation.guard.source_line : 0);
    return status;
}

/* IDs are looked up only under the owning runtime lock. */
OPEN64_REDUCTION_TICKET *open64_reduction_ticket(OPEN64_REDUCTION_RUNTIME *r, uint64_t id)
{
    for (unsigned i = 0; i < OPEN64_REDUCTION_LIMIT; ++i)
        if (r->tickets[i].id == id && id) return &r->tickets[i];
    return NULL;
}

/* Allocation lookup does not accept another context's otherwise valid handle. */
OPEN64_REDUCTION_ALLOCATION *open64_reduction_allocation(OPEN64_REDUCTION_RUNTIME *r, uint64_t id)
{
    for (unsigned i = 0; i < OPEN64_REDUCTION_LIMIT; ++i)
        if (r->allocations[i].id == id && id) return &r->allocations[i];
    return NULL;
}

/* Bind the guard's exact root capacity and base guarantee to a live allocation. */
bool open64_reduction_subview(const OPEN64_REDUCTION_ALLOCATION *a,
    const OPEN64_REDUCTION_RANGE_V1 &range, uint64_t bytes, OPEN64_TARGET_BUFFER_VIEW *v)
{
    if (!a || a->view.bytes != range.capacity ||
        reinterpret_cast<uintptr_t>(a->view.address) % range.alignment) return false;
    *v = a->view;
    v->address = reinterpret_cast<void *>(reinterpret_cast<uintptr_t>(v->address) + range.offset);
    v->bytes = static_cast<size_t>(bytes);
    return true;
}
}

/* Configure a trusted target provider without introducing compiler dependencies. */
OPEN64_REDUCTION_RUNTIME *open64_reduction_runtime_create(
    const OPEN64_TARGET_BUFFER_SERVICE *s, OPEN64_REDUCTION_ROOT_RESOLVER resolve_root,
    uint32_t architecture,
    uint64_t context, uint32_t flags, FILE *trace)
{
    if (!s || s->abi_version != 1 || s->target_id != OPEN64_TARGET_NVIDIA ||
        !s->validate_import || !resolve_root || !context || architecture < 1 || architecture > 4 || flags > 1)
        return NULL;
    OPEN64_REDUCTION_RUNTIME *r = new (std::nothrow) OPEN64_REDUCTION_RUNTIME;
    if (!r) return NULL;
    r->service = *s; r->architecture = architecture; r->context = context;
    r->resolve_root = resolve_root;
    r->flags = flags; r->trace = trace;
    if (trace) {
        fprintf(trace, "AI_REDUCE_RUN 1 HEADER seq ticket action status done leases source_line\n");
        fprintf(trace, "AI_REDUCE_RUN 1 BIND_HEADER ticket contract arch in_root out_root in_offset in_bytes out_offset out_bytes e0 e1 e2 e3 e4 e5 source_line\n");
    }
    return r;
}

/* Destruction is externally synchronized; live tickets/allocations block it. */
int open64_reduction_runtime_destroy(OPEN64_REDUCTION_RUNTIME *r)
{
    if (!r) return OPEN64_REDUCTION_INVALID;
    {
        std::lock_guard<std::mutex> lock(r->mutex);
        for (unsigned i = 0; i < OPEN64_REDUCTION_LIMIT; ++i)
            if (r->allocations[i].id || r->tickets[i].id) return OPEN64_REDUCTION_BUSY;
    }
    delete r;
    return OPEN64_REDUCTION_OK;
}

/* Provider verifies the actual device/context before canonical registration.
 * Registration/retirement/free of the same root must be externally serialized. */
int open64_reduction_register(OPEN64_REDUCTION_RUNTIME *r,
    const OPEN64_TARGET_BUFFER_VIEW *view, uint64_t *allocation)
{
    if (!r || !view || !allocation) return OPEN64_REDUCTION_INVALID;
    OPEN64_TARGET_BUFFER_VIEW v = *view;
    uint64_t backing_object = 0;
    bool invalid = !v.address || !v.bytes || v.tier != OPEN64_TARGET_BUFFER_DEVICE ||
        v.device_id != r->service.device_id ||
        v.bytes > UINTPTR_MAX - reinterpret_cast<uintptr_t>(v.address) ||
        r->service.validate_import(r->service.context, &v, v.bytes, 4) ||
        r->resolve_root(r->service.context, &v, &backing_object) || !backing_object;
    std::lock_guard<std::mutex> lock(r->mutex);
    if (invalid) return open64_reduction_report(r, 0, "register", OPEN64_REDUCTION_INVALID);
    OPEN64_REDUCTION_ALLOCATION *slot = NULL;
    for (unsigned i = 0; i < OPEN64_REDUCTION_LIMIT; ++i) {
        if (!r->allocations[i].id) slot = &r->allocations[i];
        else if (backing_object == r->allocations[i].backing_object ||
                 open64_reduction_overlap(v, r->allocations[i].view))
            return open64_reduction_report(r, 0, "register", OPEN64_REDUCTION_CONFLICT);
    }
    if (!slot) return open64_reduction_report(r, 0, "register", OPEN64_REDUCTION_CAPACITY);
    uint64_t id = open64_reduction_mint();
    if (!id) return OPEN64_REDUCTION_CAPACITY;
    slot->id = id; slot->backing_object = backing_object; slot->view = v; *allocation = id;
    return open64_reduction_report(r, 0, "register", OPEN64_REDUCTION_OK);
}

/* Cooperative free/rebind is prohibited while either accessed range is pinned. */
int open64_reduction_retire(OPEN64_REDUCTION_RUNTIME *r, uint64_t allocation)
{
    if (!r) return OPEN64_REDUCTION_INVALID;
    std::lock_guard<std::mutex> lock(r->mutex);
    OPEN64_REDUCTION_ALLOCATION *a = open64_reduction_allocation(r, allocation);
    if (!a) return open64_reduction_report(r, 0, "retire", OPEN64_REDUCTION_INVALID);
    for (unsigned i = 0; i < OPEN64_REDUCTION_LIMIT; ++i) {
        const OPEN64_REDUCTION_TICKET &t = r->tickets[i];
        if (t.id && (((t.leases & 1) && t.input == allocation) ||
                     ((t.leases & 2) && t.output == allocation)))
            return open64_reduction_report(r, t.id, "retire", OPEN64_REDUCTION_BUSY, &t);
    }
    a->id = 0;
    return open64_reduction_report(r, 0, "retire", OPEN64_REDUCTION_OK);
}

/* Preflight shape/ISA/manifest; under lock bind roots and atomically acquire
 * both leases. Refusal never publishes a ticket or invokes the kernel. */
int open64_reduction_admit(OPEN64_REDUCTION_RUNTIME *r,
    const OPEN64_REDUCTION_GUARD *guard, const OPEN64_REDUCTION_ENTRY *entry,
    uint64_t input, uint64_t output, const void *in_desc, size_t in_size,
    const void *out_desc, size_t out_size, uint64_t *ticket)
{
    if (!r || !guard || !entry || !ticket) return OPEN64_REDUCTION_INVALID;
    OPEN64_REDUCTION_TICKET t = {};
    t.entry = *entry; t.invocation.guard = *guard;
    const OPEN64_REDUCTION_GUARD &g = t.invocation.guard;
    bool valid = g.abi_version == 1 && g.size == sizeof(g) && g.source_version == 2 &&
        g.node && g.input_value && g.result_value && g.rows && g.width &&
        g.rows <= INT64_MAX && g.width <= INT64_MAX &&
        g.rows <= UINT64_MAX / 4 && g.width <= UINT64_MAX / (g.rows * 4) &&
        g.output_bytes == g.rows * 4 && g.input_bytes == g.rows * 4 * g.width &&
        g.architecture == r->architecture && g.instruction == 1 && !g.fp_options &&
        g.obligations == 15 && g.row_mapping == 1 && g.threads >= g.rows &&
        g.contract && g.context == r->context && entry->perform && entry->flags == r->flags &&
        open64_reduction_guard_equal(g, t.entry.manifest) &&
        open64_reduction_range_valid(g.input, g.input_bytes, 1) &&
        open64_reduction_range_valid(g.output, g.output_bytes, 2) &&
        open64_reduction_graph(g.before, t.predecessors) &&
        open64_reduction_descriptor(in_desc, in_size, g.rows, g.width, true) &&
        open64_reduction_descriptor(out_desc, out_size, g.rows, g.width, false);
    std::lock_guard<std::mutex> lock(r->mutex);
    if (!valid) return open64_reduction_report(r, 0, "admit", OPEN64_REDUCTION_INVALID, &t);
    if ((g.input.role == g.output.role) != (input == output) ||
        !open64_reduction_subview(open64_reduction_allocation(r, input), g.input,
                                 g.input_bytes, &t.invocation.input) ||
        !open64_reduction_subview(open64_reduction_allocation(r, output), g.output,
                                 g.output_bytes, &t.invocation.output) ||
        open64_reduction_overlap(t.invocation.input, t.invocation.output))
        return open64_reduction_report(r, 0, "admit", OPEN64_REDUCTION_INVALID, &t);
    OPEN64_REDUCTION_TICKET *slot = NULL;
    for (unsigned i = 0; i < OPEN64_REDUCTION_LIMIT; ++i) {
        const OPEN64_REDUCTION_TICKET &other = r->tickets[i];
        if (!other.id) { slot = &r->tickets[i]; continue; }
        if (((other.leases & 2) &&
             (open64_reduction_overlap(t.invocation.input, other.invocation.output) ||
              open64_reduction_overlap(t.invocation.output, other.invocation.output))) ||
            ((other.leases & 1) && open64_reduction_overlap(t.invocation.output, other.invocation.input)))
            return open64_reduction_report(r, 0, "admit", OPEN64_REDUCTION_CONFLICT, &t);
    }
    if (!slot || !(t.id = open64_reduction_mint()))
        return open64_reduction_report(r, 0, "admit", OPEN64_REDUCTION_CAPACITY, &t);
    t.input = input; t.output = output; t.leases = 3; t.invocation.ticket = t.id;
    *slot = t; *ticket = t.id;
    if (r->trace) fprintf(r->trace,
        "AI_REDUCE_RUN 1 BIND %llu %llu %u %llu %llu %llu %llu %llu %llu %u %u %u %u %u %u %u\n",
        static_cast<unsigned long long>(t.id), static_cast<unsigned long long>(g.contract),
        g.architecture, static_cast<unsigned long long>(input), static_cast<unsigned long long>(output),
        static_cast<unsigned long long>(g.input.offset), static_cast<unsigned long long>(g.input_bytes),
        static_cast<unsigned long long>(g.output.offset), static_cast<unsigned long long>(g.output_bytes),
        g.before[0], g.before[1], g.before[2], g.before[3], g.before[4], g.before[5], g.source_line);
    return open64_reduction_report(r, t.id, "admit", OPEN64_REDUCTION_OK, slot);
}

/* Reserve a transition before calling the adapter unlocked. A failed launch
 * is ambiguous, so only completion/drain and abort may follow it. */
int open64_reduction_advance(OPEN64_REDUCTION_RUNTIME *r, uint64_t id, uint32_t event)
{
    static const char *names[] = {"ready", "launch", "complete", "consume", "input_release", "output_release"};
    if (!r || event >= 6) return OPEN64_REDUCTION_INVALID;
    std::unique_lock<std::mutex> lock(r->mutex);
    OPEN64_REDUCTION_TICKET *t = open64_reduction_ticket(r, id);
    if (!t) return open64_reduction_report(r, id, names[event], OPEN64_REDUCTION_INVALID);
    if (t->busy) return open64_reduction_report(r, id, names[event], OPEN64_REDUCTION_BUSY, t);
    if (t->aborted || (t->launch_failed && event != OPEN64_REDUCTION_COMPLETE))
        return open64_reduction_report(r, id, names[event], OPEN64_REDUCTION_FAILED, t);
    if (t->done & (1U << event))
        return open64_reduction_report(r, id, names[event], OPEN64_REDUCTION_OK, t);
    uint32_t done = t->done;
    /* Ambiguous failed launch grants only permission to wait for drain. */
    if (t->launch_failed && event == OPEN64_REDUCTION_COMPLETE) done |= 2;
    if ((done & t->predecessors[event]) != t->predecessors[event])
        return open64_reduction_report(r, id, names[event], OPEN64_REDUCTION_ORDER, t);
    if (event >= OPEN64_REDUCTION_INPUT_RELEASE) {
        t->done |= 1U << event;
        t->leases &= ~(event == OPEN64_REDUCTION_INPUT_RELEASE ? 1U : 2U);
        return open64_reduction_report(r, id, names[event], OPEN64_REDUCTION_OK, t);
    }
    t->busy = true;
    if (event == OPEN64_REDUCTION_LAUNCH) t->attempted = true;
    if (event == OPEN64_REDUCTION_CONSUME) t->consumer_started = true;
    OPEN64_REDUCTION_ENTRY adapter = t->entry;
    OPEN64_REDUCTION_INVOCATION invocation = t->invocation;
    lock.unlock();
    int result = adapter.perform(adapter.context, event, &invocation);
    lock.lock();
    t->busy = false;
    if (event == OPEN64_REDUCTION_LAUNCH && result) t->launch_failed = true;
    if (!result) t->done |= 1U << event;
    return open64_reduction_report(r, id, names[event],
        result ? OPEN64_REDUCTION_BACKEND : OPEN64_REDUCTION_OK, t);
}

/* Cancellation never releases storage under outstanding device/consumer work. */
int open64_reduction_abort(OPEN64_REDUCTION_RUNTIME *r, uint64_t id)
{
    if (!r) return OPEN64_REDUCTION_INVALID;
    std::lock_guard<std::mutex> lock(r->mutex);
    OPEN64_REDUCTION_TICKET *t = open64_reduction_ticket(r, id);
    if (!t) return open64_reduction_report(r, id, "abort", OPEN64_REDUCTION_INVALID);
    if (t->busy || (t->attempted && !(t->done & 4)) ||
        (t->consumer_started && !(t->done & 8)))
        return open64_reduction_report(r, id, "abort", OPEN64_REDUCTION_BUSY, t);
    t->aborted = true; t->leases = 0;
    return open64_reduction_report(r, id, "abort", OPEN64_REDUCTION_OK, t);
}

/* Only terminal tickets may be forgotten; IDs never become valid again. */
int open64_reduction_close(OPEN64_REDUCTION_RUNTIME *r, uint64_t id)
{
    if (!r) return OPEN64_REDUCTION_INVALID;
    std::lock_guard<std::mutex> lock(r->mutex);
    OPEN64_REDUCTION_TICKET *t = open64_reduction_ticket(r, id);
    if (!t) return open64_reduction_report(r, id, "close", OPEN64_REDUCTION_INVALID);
    if (t->busy || t->leases) return open64_reduction_report(r, id, "close", OPEN64_REDUCTION_BUSY, t);
    open64_reduction_report(r, id, "close", OPEN64_REDUCTION_OK, t);
    t->id = 0;
    return OPEN64_REDUCTION_OK;
}
