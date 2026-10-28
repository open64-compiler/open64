/* Compiler-exported guard -> host-mock runtime enforcement, NOT GPU execution.
 * Design: doc/AI-COMPILER-REDUCTION-RUNTIME-GUARDS-v0.1.md. */
#include "open64_reduction_runtime.h"
#include <atomic>
#include <thread>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned assertions;
/* Fail immediately with an auditable scenario name. */
static void check(bool value, const char *name)
{
    ++assertions;
    if (!value) { fprintf(stderr, "FAIL %s\n", name); exit(1); }
}

struct DESCRIPTOR {
    OPEN64_DSL_TENSOR_DESCRIPTOR_V1 header;
    int64_t shape[2], strides[2];
};

/* Use the existing tensor ABI; no parallel shape representation at admission. */
static DESCRIPTOR descriptor(const OPEN64_REDUCTION_GUARD &g, bool input)
{
    DESCRIPTOR d = {};
    d.header.abi_version = 1; d.header.header_size = sizeof(d.header);
    d.header.total_size = sizeof(d); d.header.dtype = OPEN64_DSL_DTYPE_F32;
    d.header.rank = input ? 2 : 1; d.header.shape_count = d.header.rank;
    d.header.shape_offset = offsetof(DESCRIPTOR, shape);
    d.header.layout = OPEN64_DSL_LAYOUT_CONTIGUOUS;
    d.header.memory = OPEN64_DSL_MEMORY_DEVICE;
    d.header.placement = OPEN64_DSL_PLACEMENT_DEVICE;
    d.header.sharding = OPEN64_DSL_SHARDING_REPLICATED;
    d.header.quantization = OPEN64_DSL_QUANTIZATION_NONE;
    d.header.runtime_state = OPEN64_DSL_RUNTIME_STATE_STATIC;
    d.shape[0] = g.rows; d.shape[1] = g.width;
    return d;
}

struct MOCK {
    OPEN64_REDUCTION_RUNTIME *runtime;
    unsigned calls[4];
    int fail_event;
    bool reenter;
};

struct ROOTS {
    void *alias, *canonical;
};

/* Test backing-store authority can deliberately map distinct virtual roots to
 * the same storage. Production must query its actual allocation authority. */
static int resolve_root(void *context, const OPEN64_TARGET_BUFFER_VIEW *v, uint64_t *key)
{
    ROOTS *roots = static_cast<ROOTS *>(context);
    *key = reinterpret_cast<uintptr_t>(v->address == roots->alias ? roots->canonical : v->address);
    return 0;
}

/* Host pointers stand in for imported device memory ONLY in test contexts. */
static int validate(void *, const OPEN64_TARGET_BUFFER_VIEW *v, size_t n, size_t align)
{
    return !v || v->bytes < n || reinterpret_cast<uintptr_t>(v->address) % align;
}

/* Record/wait-only mock. Reentry proves the context mutex is not held across
 * callbacks; failures deliberately provide no completion proof. */
static int perform(void *context, uint32_t event, const OPEN64_REDUCTION_INVOCATION *inv)
{
    MOCK *m = static_cast<MOCK *>(context);
    check(event < 4 && inv->input.bytes == inv->guard.input_bytes &&
          inv->output.bytes == inv->guard.output_bytes, "exact_callback_snapshot");
    ++m->calls[event];
    if (m->reenter) {
        check(open64_reduction_advance(m->runtime, inv->ticket, event) == OPEN64_REDUCTION_BUSY,
              "reentrant_busy_not_deadlocked");
        check(open64_reduction_abort(m->runtime, inv->ticket) == OPEN64_REDUCTION_BUSY,
              "reentrant_abort_pinned");
    }
    return m->fail_event == static_cast<int>(event);
}

/* Keep admission syntax small without omitting real descriptor/blob checks. */
static int admit(OPEN64_REDUCTION_RUNTIME *r, const OPEN64_REDUCTION_GUARD &g,
    const OPEN64_REDUCTION_ENTRY &e, uint64_t a, uint64_t b,
    const DESCRIPTOR &x, const DESCRIPTOR &y, uint64_t *ticket)
{
    return open64_reduction_admit(r, &g, &e, a, b, &x, sizeof(x), &y, sizeof(y), ticket);
}

/* Each accepted ticket explicitly traverses the ordered join/release protocol. */
static void finish(OPEN64_REDUCTION_RUNTIME *r, uint64_t t)
{
    for (unsigned event = 0; event < 6; ++event)
        check(open64_reduction_advance(r, t, event) == 0, "normal_event");
    check(open64_reduction_close(r, t) == 0, "normal_close");
}

/* Exercise actual WOPT-exported contracts; no hand-written successful guard.
 * Contract mutations are intentionally labeled negative/alternate scenarios. */
int main(int argc, char **argv)
{
    if (argc != 3) return 2;
    FILE *file = fopen(argv[1], "rb");
    OPEN64_REDUCTION_GUARD g;
    check(file && fread(&g, sizeof(g), 1, file) == 1 && fgetc(file) == EOF,
          "read_native_guard");
    fclose(file);
    FILE *trace = strcmp(argv[2], "off") ? fopen(argv[2], "w") : NULL;
    check(!strcmp(argv[2], "off") || trace, "trace_open");
    OPEN64_TARGET_BUFFER_SERVICE service = {};
    ROOTS roots = {};
    service.abi_version = 1; service.target_id = OPEN64_TARGET_NVIDIA;
    service.validate_import = validate; service.context = &roots;
    MOCK mock = {}; mock.fail_event = -1;
    OPEN64_REDUCTION_RUNTIME *r = open64_reduction_runtime_create(
        &service, resolve_root, g.architecture, g.context, OPEN64_REDUCTION_HOST_TEST, trace);
    check(r != NULL, "create_test_runtime"); mock.runtime = r;
    alignas(16) unsigned char a_data[1024], b_data[1024], c_data[1024];
    OPEN64_TARGET_BUFFER_VIEW av = {a_data, sizeof(a_data), OPEN64_TARGET_BUFFER_DEVICE, 0};
    OPEN64_TARGET_BUFFER_VIEW bv = {b_data, sizeof(b_data), OPEN64_TARGET_BUFFER_DEVICE, 0};
    OPEN64_TARGET_BUFFER_VIEW cv = {c_data, sizeof(c_data), OPEN64_TARGET_BUFFER_DEVICE, 0};
    uint64_t a = 0, b = 0, c = 0, untouched = UINT64_MAX;
    check(open64_reduction_register(r, &av, &a) == 0 &&
          open64_reduction_register(r, &bv, &b) == 0 &&
          open64_reduction_register(r, &cv, &c) == 0, "canonical_register");
    check(open64_reduction_register(r, &av, &untouched) == OPEN64_REDUCTION_CONFLICT &&
          untouched == UINT64_MAX, "duplicate_canonical_root_refused");
    alignas(16) unsigned char alias_data[1024];
    OPEN64_TARGET_BUFFER_VIEW alias = {alias_data, sizeof(alias_data), OPEN64_TARGET_BUFFER_DEVICE, 0};
    roots.alias = alias_data; roots.canonical = a_data;
    check(open64_reduction_register(r, &alias, &untouched) == OPEN64_REDUCTION_CONFLICT,
          "different_virtual_address_same_backing_refused");
    OPEN64_TARGET_BUFFER_VIEW sub = av; sub.address = a_data + 16; sub.bytes = 16;
    check(open64_reduction_register(r, &sub, &untouched) == OPEN64_REDUCTION_CONFLICT,
          "alias_registration_refused");
    sub = av; sub.device_id = 1;
    check(open64_reduction_register(r, &sub, &untouched) == OPEN64_REDUCTION_INVALID,
          "wrong_device_refused");
    DESCRIPTOR x = descriptor(g, true), y = descriptor(g, false);
    OPEN64_REDUCTION_ENTRY e = {g, OPEN64_REDUCTION_HOST_TEST, &mock, perform};
    for (unsigned test = 0; test < 27; ++test) {
        OPEN64_REDUCTION_GUARD bad = g;
        DESCRIPTOR xx = x, yy = y;
        switch (test) {
        case 0: bad.abi_version = 2; break;
        case 1: bad.source_version = 1; break;
        case 2: bad.fp_options = 1; break;
        case 3: bad.instruction = 0; break;
        case 4: bad.obligations = 0; break;
        case 5: bad.context++; break;
        case 6: bad.architecture = 999; break;
        case 7: bad.rows = UINT64_MAX; break;
        case 8: bad.input_bytes++; break;
        case 9: bad.input.offset = UINT64_MAX; break;
        case 10: bad.output.capacity = 11; break;
        case 11: bad.input.alignment = 3; break;
        case 12: bad.output.lease = 1; break;
        case 13: bad.before[0] = 0; break;
        case 14: bad.before[5] = 1; break;
        case 15: bad.before[5] = 1U << 31; break;
        case 16: bad.threads = 0; break;
        case 17: xx.header.dtype = OPEN64_DSL_DTYPE_F16; break;
        case 18: xx.shape[0]++; break;
        case 19: yy.header.rank = 2; break;
        case 20: xx.header.shape_offset = UINT32_MAX; break;
        case 21: xx.header.total_size = 1; break;
        case 22: xx.header.stride_count = 1; break;
        case 23: yy.header.flags = OPEN64_DSL_TENSOR_FLAG_CONSTANT; break;
        case 24: xx.header.layout = OPEN64_DSL_LAYOUT_STRIDED; break;
        case 25: xx.header.reserved = 1; break;
        case 26: xx.header.memory = OPEN64_DSL_MEMORY_HOST; break;
        }
        OPEN64_REDUCTION_ENTRY ee = e; ee.manifest = bad;
        check(admit(r, bad, ee, a, b, xx, yy, &untouched) == OPEN64_REDUCTION_INVALID &&
              untouched == UINT64_MAX, "bad_admission_no_ticket");
    }
    OPEN64_REDUCTION_ENTRY mismatch = e; ++mismatch.manifest.width;
    check(admit(r, g, mismatch, a, b, x, y, &untouched) == OPEN64_REDUCTION_INVALID,
          "entry_manifest_mismatch");
    check(open64_reduction_admit(r, &g, &e, a, b, &x, 63, &y, sizeof(y), &untouched) ==
          OPEN64_REDUCTION_INVALID, "truncated_blob_refused");
    check(admit(r, g, e, a, a, x, y, &untouched) == OPEN64_REDUCTION_INVALID,
          "actual_alias_despite_distinct_roles_refused");
    check(mock.calls[0] == 0 && mock.calls[1] == 0, "no_callback_on_guard_failure");
    OPEN64_REDUCTION_RUNTIME *other = open64_reduction_runtime_create(
        &service, resolve_root, g.architecture, g.context, 0, NULL);
    uint64_t oa = 0, ob = 0;
    check(open64_reduction_register(other, &av, &oa) == 0 &&
          open64_reduction_register(other, &bv, &ob) == 0, "other_context_handles");
    check(admit(r, g, e, oa, ob, x, y, &untouched) == OPEN64_REDUCTION_INVALID,
          "cross_context_handle_refused");
    check(admit(other, g, e, oa, ob, x, y, &untouched) == OPEN64_REDUCTION_INVALID,
          "test_entry_cannot_enter_production");
    check(open64_reduction_retire(other, oa) == 0 && open64_reduction_retire(other, ob) == 0 &&
          open64_reduction_runtime_destroy(other) == 0, "other_cleanup");
    uint64_t t = 0, t2 = 0;
    check(admit(r, g, e, a, b, x, y, &t) == 0, "admit_normal");
    check(admit(r, g, e, a, b, x, y, &untouched) == OPEN64_REDUCTION_CONFLICT,
          "writer_conflict_refused");
    check(admit(r, g, e, a, c, x, y, &t2) == 0, "read_read_shared_snapshot");
    check(open64_reduction_abort(r, t2) == 0 && open64_reduction_close(r, t2) == 0,
          "prelaunch_abort");
    OPEN64_REDUCTION_ENTRY saved_entry = e;
    e.manifest.fp_options = 1; e.perform = NULL;
    x.shape[0]++; y.shape[0]++;
    check(open64_reduction_retire(r, a) == OPEN64_REDUCTION_BUSY &&
          open64_reduction_retire(r, b) == OPEN64_REDUCTION_BUSY &&
          open64_reduction_runtime_destroy(r) == OPEN64_REDUCTION_BUSY, "live_buffers_pinned");
    for (unsigned event = 1; event < 6; ++event)
        check(open64_reduction_advance(r, t, event) == OPEN64_REDUCTION_ORDER, "early_event_refused");
    mock.reenter = true;
    check(open64_reduction_advance(r, t, 0) == 0, "ready_with_reentry");
    mock.reenter = false;
    unsigned ready_calls = mock.calls[0];
    check(open64_reduction_advance(r, t, 0) == 0 && mock.calls[0] == ready_calls,
          "event_idempotent");
    finish(r, t);
    e = saved_entry; x = descriptor(g, true); y = descriptor(g, false);
    check(open64_reduction_advance(r, t, 0) == OPEN64_REDUCTION_INVALID, "closed_ticket_stale");

    check(admit(r, g, e, a, b, x, y, &t) == 0, "input_release_first_admit");
    for (unsigned event = 0; event < 3; ++event)
        check(open64_reduction_advance(r, t, event) == 0, "input_release_first_prefix");
    check(open64_reduction_advance(r, t, 4) == 0, "input_release_before_consumer");
    check(open64_reduction_retire(r, b) == OPEN64_REDUCTION_BUSY, "output_remains_pinned");
    check(admit(r, g, e, c, a, x, y, &t2) == 0, "released_input_allows_new_writer");
    check(open64_reduction_abort(r, t2) == 0 && open64_reduction_close(r, t2) == 0,
          "new_writer_cleanup");
    check(open64_reduction_advance(r, t, 3) == 0 && open64_reduction_advance(r, t, 5) == 0 &&
          open64_reduction_close(r, t) == 0, "input_release_first_finish");

    /* Additional graph edges must constrain runtime release as well. */
    OPEN64_REDUCTION_GUARD transitive = g;
    transitive.before[2] = 8; transitive.before[3] = 16 | 32;
    OPEN64_REDUCTION_ENTRY te = e; te.manifest = transitive;
    check(admit(r, transitive, te, a, b, x, y, &t) == 0, "extra_edge_admit");
    for (unsigned event = 0; event < 3; ++event)
        check(open64_reduction_advance(r, t, event) == 0, "extra_edge_prefix");
    check(open64_reduction_advance(r, t, 4) == OPEN64_REDUCTION_ORDER, "extra_edge_enforced");
    finish(r, t);

    /* Legal disjoint intervals may share a single canonical allocation. */
    OPEN64_REDUCTION_GUARD adjacent = g;
    adjacent.output.role = adjacent.input.role; adjacent.output.offset = g.input_bytes;
    OPEN64_REDUCTION_ENTRY ae = e; ae.manifest = adjacent;
    check(admit(r, adjacent, ae, a, a, x, y, &t) == 0, "same_root_adjacent_admit");
    finish(r, t);
    adjacent.output.offset -= 4; ae.manifest = adjacent;
    check(admit(r, adjacent, ae, a, a, x, y, &untouched) == OPEN64_REDUCTION_INVALID,
          "same_root_overlap_refused");
    /* Reject writes into another ticket's read snapshot, even with no overlap
     * between the new ticket's own input and output. */
    check(admit(r, g, e, a, b, x, y, &t) == 0, "snapshot_conflict_admit");
    check(admit(r, g, e, c, a, x, y, &untouched) == OPEN64_REDUCTION_CONFLICT,
          "cross_ticket_read_write_conflict");
    check(open64_reduction_abort(r, t) == 0 && open64_reduction_close(r, t) == 0,
          "snapshot_conflict_cleanup");
    /* Explicit canonical strides are accepted, nonunit columns refused. */
    x.header.flags = OPEN64_DSL_TENSOR_FLAG_HAS_STRIDES;
    x.header.stride_count = 2; x.header.stride_offset = offsetof(DESCRIPTOR, strides);
    x.strides[0] = g.width; x.strides[1] = 1;
    check(admit(r, g, e, a, b, x, y, &t) == 0, "explicit_contiguous_strides");
    finish(r, t);
    x.strides[1] = 2;
    check(admit(r, g, e, a, b, x, y, &untouched) == OPEN64_REDUCTION_INVALID,
          "noncontiguous_stride_refused");
    x = descriptor(g, true);

    /* Each failed callback has a separate, deliberately conservative recovery. */
    for (int failure = 0; failure < 4; ++failure) {
        check(admit(r, g, e, a, b, x, y, &t) == 0, "failure_case_admit");
        for (int event = 0; event < failure; ++event)
            check(open64_reduction_advance(r, t, event) == 0, "failure_prefix");
        mock.fail_event = failure;
        check(open64_reduction_advance(r, t, failure) == OPEN64_REDUCTION_BACKEND,
              "backend_failure_reported");
        if (failure != 0) check(open64_reduction_abort(r, t) == OPEN64_REDUCTION_BUSY,
                               "ambiguous_work_blocks_abort");
        check(open64_reduction_retire(r, b) == OPEN64_REDUCTION_BUSY, "failure_retains_output");
        mock.fail_event = -1;
        if (failure == 1) {
            check(open64_reduction_advance(r, t, 1) == OPEN64_REDUCTION_FAILED,
                  "failed_launch_never_reissued");
            mock.fail_event = 2;
            check(open64_reduction_advance(r, t, 2) == OPEN64_REDUCTION_BACKEND &&
                  open64_reduction_abort(r, t) == OPEN64_REDUCTION_BUSY, "failed_drain_pins");
            mock.fail_event = -1;
            check(open64_reduction_advance(r, t, 2) == 0 &&
                  open64_reduction_advance(r, t, 3) == OPEN64_REDUCTION_FAILED,
                  "drained_failed_launch_never_publishes");
        } else if (failure >= 2) {
            check(open64_reduction_advance(r, t, failure) == 0, "wait_retry");
        }
        check(open64_reduction_abort(r, t) == 0 && open64_reduction_close(r, t) == 0,
              "safe_failure_cleanup");
    }
    /* Competing admissions must acquire BOTH leases atomically. */
    std::atomic<unsigned> start(0);
    int status[2]; uint64_t ids[2] = {0, 0};
    std::thread first([&]() {
        ++start; while (start.load() != 2) std::this_thread::yield();
        status[0] = admit(r, g, e, a, b, x, y, &ids[0]);
    });
    std::thread second([&]() {
        ++start; while (start.load() != 2) std::this_thread::yield();
        status[1] = admit(r, g, e, a, b, x, y, &ids[1]);
    });
    first.join(); second.join();
    check((status[0] == 0 && status[1] == OPEN64_REDUCTION_CONFLICT) ||
          (status[1] == 0 && status[0] == OPEN64_REDUCTION_CONFLICT), "atomic_racing_admission");
    t = ids[status[0] == 0 ? 0 : 1];
    check(open64_reduction_abort(r, t) == 0 && open64_reduction_close(r, t) == 0, "race_cleanup");
    check(open64_reduction_retire(r, a) == 0 && open64_reduction_retire(r, b) == 0 &&
          open64_reduction_retire(r, c) == 0, "retire_unpinned");
    check(admit(r, g, e, a, b, x, y, &untouched) == OPEN64_REDUCTION_INVALID,
          "retired_handle_refused");
    check(open64_reduction_runtime_destroy(r) == 0, "destroy_clean");
    if (trace) fclose(trace);
    printf("PASS assertions=%u arch=%u rows=%llu width=%llu host_mock=1 gpu=0\n", assertions,
           g.architecture, static_cast<unsigned long long>(g.rows), static_cast<unsigned long long>(g.width));
    return 0;
}
