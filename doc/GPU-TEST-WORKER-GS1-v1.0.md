# GPU testing worker adapter GS1 v1.0

November 11, 2026. Implemented local worker and durable state. HTTP service,
remote client and NVIDIA qualification remain GS2/GS3. OpenCTI stays deferred.
Controlling direction: API v0.1 (`GPU-TEST-SERVER-API-v0.1.md`, historical design reference) and
plan v1.30 (`AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.30.md`, historical design reference).

## Ownership and compatibility

`osprey/be/lno/tests/gpu_test_worker_state.py` owns a host-local SQLite store,
immutable registered-packet bindings, requests, idempotency keys, ordered events,
worker claims and physical GPU lease disposition. `gpu_test_worker.py` owns
supervision, cancellation, recovery, closed artifact publication and a local
administrative CLI. Use Python >=3.9 on a POSIX host, local storage with working
SQLite WAL/file locks, and one shared store for all workers on that GPU host.
Do not create independent stores for the same physical GPU or use a network
filesystem as the ownership authority. Store policy is bound to its host.

The adapter invokes these existing entries from the authenticated registered
source packet, with explicit argv and an allowlisted child environment:

| Entry | Responsibility retained |
| --- | --- |
| `tools/run_gpu_test_suite.py` | All original host/build/numerical/sanitizer/timing/profiling gates, independent workload continuation, original exit codes and automatic native/combined collection |
| `tools/package_gpu_test_suite_results.py` | Bounded recovery collection when a closed run lacks its original return pair; partial/raw/native evidence and read-only environment capture |
| `tools/validate_gpu_test_suite.py` | Integrity-first offline review and the original shared/native Q7 acceptance; never execute returned code |

It changes no kernel, WHIRL, oracle, tolerance, workload ABI or baseline policy.
No compiler search, JIT, AOT dispatch or source upload is added. Wall/device
timing and optional diagnostic profiling remain in the existing packet's
separate processes and batches. The original combined v1.0 archive and all
271 preserved scripts remain unchanged. The worker ships as a companion.

## Durable state and physical leases

Every accepted request is detached, type-checked and stored in a FULL-synchronous
SQLite transaction. `(principal, idempotency-key)` is unique: parsed member order
does not matter, identical replay returns the same job, and changed reuse is
rejected. Explicit retry lineage requires an accessible terminal job and creates
a fresh job/run. These are local administrative principals, not an implemented
network authentication layer.

A claim transaction chooses ready queued work, moves it to preparing and writes
its non-expiring GPU lease/token before any executable child starts. Only one
job owns a physical GPU UUID; ordinal/MIG aliases are not accepted. Different
registered physical GPUs can be claimed concurrently, and check-only needs no
GPU. Queue expiry never launches work, and lease age never authorizes reuse.

A per-job file lock prevents recovery from stealing an active supervisor. A
spawned child waits on a pipe until its PID/start identity and command have been
persisted. Parent death before gate release produces EOF without executing the
target. Parent death after release leaves a durable child identity. Linux boot/
start records or POSIX process-start observations distinguish reused PIDs;
recovery never sends signals to a discovered PID.

The real device adapter makes read-only `nvidia-smi` observations: a Linux
x86_64 physical capability-9.0 device, MIG disabled, matching UUID and no active
compute contexts. The packet still owns its detailed CUDA/Driver/tool checks.
Absent observations refuse admission/release rather than proving compatibility.
A service lease does not exclude external actors; MIG/MPS/system-wide isolation
and actual NVIDIA tool behavior still need GS3 evidence.

## Cancellation, deadlines and restart

Queued cancellation is terminal without launching work. Active cancellation or
worker SIGINT/SIGTERM signals the owned runner cooperatively. The original
runner stops its independently grouped children and reaches its finalizer. The
adapter gives collection its reserved grace instead of immediately killing the
runner. Cancellation during an already announced finalizer leaves collection
running. A new request is not claimed after a worker shutdown signal.

The execution budget includes preparation. The fixed combined runner's existing
`Q7 step: q7-results` progress line identifies entry into automatic collection;
this line is not acceptance evidence. One monotonic collection/review deadline
covers the runner finalizer, any recovery collector and receiver work in that
attempt. The runner's individual collector limits are bounded within that
grace. A later explicit recovery is a new bounded recovery attempt, not a rerun
of the original workload.

Only owned new-session process groups may be force-stopped. The runner itself
can create separate sessions; therefore a forced stop does not prove all its
descendants or GPU work have drained. A trusted closed `completion.json`, a
reaped runner and a subsequent idle-device observation are needed for ordinary
lease release. Forced termination or uncertain drain quarantines the lease.
Closed supervisor logs remain available; potentially live workload files are
not snapshotted into a misleading finalized archive.

On restart, `run-once` first reconciles abandoned active claims. A live original
worker is left alone. If its orphaned child still lives, recovery records
pending collection and quarantines the GPU, then waits for a later recovery
invocation; it neither signals a recovered PID nor retries execution. If the
original runner has closed evidence, recover/validate it and finish interrupted
without executing it again. If closure cannot be established, finish interrupted
with explicit partial evidence. The raw workspace remains retained. Recovered
GPU leases stay quarantined even when the closed evidence passes review.

`release-gpu` requires an operator reason, a fresh device observation and a
terminal associated job. Re-registering a UUID cannot clear quarantine. For an
unknown live orphan, establish device/process safety externally before this
operation; a missing original finalizer is never rewritten as successful drain.

## Evidence and independent outcomes

The store preserves request, packet/archive/producer/runner identities,
timestamps, original signed process returns and normalized exits, termination
reason, collection status, review status, receiver verdict, diagnostic request,
lease binding and ordered events. The worker copies closed artifacts to fresh
opaque IDs with byte count and SHA-256; it never replaces an earlier artifact.
It records source transport references rather than duplicating the source tar
for every job. Raw run/work directories and child logs are also retained.

An ordinary failed run still gets the existing result collection and review.
If a closed run has no return pair, recovery uses a fresh destination and leaves
all original outputs untouched. Authentication refusal never executes collector
code from a modified packet. Unproven live closure prevents workload snapshotting.
These cases retain the job's failure/partial metadata rather than claim that a
result tar was created.

Collection is separate from review: receiver timeout/failure does not erase a
successfully published tar. Receiver exit and `summary.json` must agree. The
worker retains PASS_REPORTED/FAIL/INCOMPLETE/INVALID exactly, without redoing
numerical arithmetic. Returned execution GPU identity, when available, must
match the lease; mismatch fails the worker and quarantines its assigned GPU.
Missing identity stays unknown. Optional diagnostic details stay in original
reports and are not synthesized as GPU measurements by the adapter.

A check-only job can finish completed with runner exit 0, collection complete
and receiver INCOMPLETE. Job completion is not qualification. A failed original
test remains failed even when collection succeeds. A recovered job remains
interrupted even if its retained evidence is internally consistent.

## Local operator commands

The companion places the CLI under `tools/`. In a source checkout use
`osprey/be/lno/tests/` instead. Paths below are operator-controlled, not request
fields. Source installation, archive, store and output paths must be separate.

```bash
python3 tools/gpu_test_worker.py --store /path/to/worker-state register-packet \
  --packet /path/to/Open64_GPU_Test_Suite_v1.0 \
  --archive /path/to/Open64_GPU_Test_Suite_v1.0.tar.gz \
  --archive-sha256 849d123c1801e8bdd7142fe2f8f84ee51138a5c439df4072075c10c91d43ba8d
```

Registration checks source and archive file sets/hashes, rejects links/unlisted
code and binds both identities. Keep the archive and installation available.
On an actual compatible GPU host, also register its physical UUID:

```bash
python3 tools/gpu_test_worker.py --store /path/to/worker-state register-gpu \
  --uuid GPU-xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
```

Use a complete typed request JSON from the companion examples. Check-only works
without that GPU registration. Submission prints the stable `job_id`:

```bash
python3 tools/gpu_test_worker.py --store /path/to/worker-state submit \
  --request examples/check-only.json --key first-check
python3 tools/gpu_test_worker.py --store /path/to/worker-state run-once
python3 tools/gpu_test_worker.py --store /path/to/worker-state status JOB_ID
python3 tools/gpu_test_worker.py --store /path/to/worker-state events JOB_ID
python3 tools/gpu_test_worker.py --store /path/to/worker-state cancel JOB_ID
python3 tools/gpu_test_worker.py --store /path/to/worker-state recover JOB_ID
python3 tools/gpu_test_worker.py --store /path/to/worker-state gpus
```

Replace `JOB_ID` with the printed ID. Cancel runs from a second local process.
`run-once --job-id JOB_ID` selects a specific ready job. A busy/quarantined GPU
leaves its job queued; `run-once` prints null when no work is ready. Run multiple
worker processes against this same store to use different GPUs. Scheduling a
continuous daemon and HTTP/remote client behavior are GS2 work.

CLI exit 0 means the administrative command completed; inspect the returned job
state, original runner exit and receiver verdict for test outcomes. It does not
mean GPU PASS. The original validator's exit semantics remain unchanged. Child
environments omit service tokens and PYTHONPATH; credentials are not request
fields. Store and its paths require trusted local operator access.

## Qualification and remaining work

`test_gpu_test_worker.py` uses real subprocesses, SQLite/file locks and synthetic
worker/device adapters. It covers concurrent claims, persistent idempotency,
typed refusal, queued/active cancellation, worker SIGTERM, gated launch, execution
and collection budgets, ordinary/collector/receiver failures, stale ownership,
source bindings, GPU mismatch, uncertain drain and killed-worker/orphan recovery.
The separate real-packet check-only replay uses the released runner/collector/
receiver and confirms all three profiles remain INCOMPLETE without a GPU lease.
Existing combined/native/shared receiver regressions remain required.

Synthetic PASS labels in process-control fixtures are not GPU qualification.
GS3 still needs actual sm_90 execution, query/tool compatibility, original
correctness/race gates, timing/profiling isolation and device cleanup under
failure. GS2 adds authenticated network admission, bounded event pagination,
artifact transport and the easy remote client. GS4 new-source admission and
OpenCTI integration remain deferred. Personnel roles remain TBD.
