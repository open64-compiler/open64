# GPU performance testing server API and usability v0.1

November 11, 2026. Status: proposed contract; no network service or client is
implemented by this document. The machine-readable companion is
[OpenAPI v0.1](GPU-TEST-SERVER-API-v0.1.openapi.json). API paths use `/v1`; the
document version remains 0.1 until implementation and contract qualification.

## Objective and work boundaries

Make a compiler-produced test packet easy to discover, submit, monitor and
retrieve on a compatible GPU host. Reuse the existing combined runner,
automatic collector and shared/native validators. Server transport and queue
management must not create a second numerical or performance acceptance engine.

OpenCTI integration is deferred until its modernization completes. This service
is developed within AI optimization and can operate independently. The released
[combined package](GPU-TEST-COMBINED-PACKAGE-v1.0.md) remains the manual/offline
path; [packaging closure](GPU-TEST-PACKAGING-CLOSURE-v1.0.md) records its exact
identity and open hardware gate. AOT selection remains deferred. No server
submission invokes compiler search, JIT or alternative WHIRL lowering.

Two submission scopes must remain distinct:

| Scope | Initial behavior |
| --- | --- |
| Registered combined packet | First implementation: execute one exact installed source packet, all three profiles, existing tests/oracles/sanitizers and measurements. This includes build checks already owned by that packet. |
| New compiler candidate sources | Subsequent extension: per-workload source submission against a server-owned workload/header/corpus/reference registry. Requires a separately versioned admission and authenticated source-snapshot contract before acceptance. |

The second scope preserves the scaffold ABI `workspace_bytes(args)` and
`run(args, workspace, stream)`. Keep its current package-local baseline support;
later registered baselines are explicit registry entries. Do not accept client
replacement of golden tensors, tolerances, useful-FLOP formulas or harnesses.
The full Q7/Q3 boundary needs its own reviewed candidate contract; the BF16
MLA-core ABI cannot stand in for its six-output/publication/lifetime semantics.
No new-source upload endpoint is exposed in the first API revision.

## Architecture and identity

```text
client -> authenticated API -> persistent job/evidence store -> worker
                                  |                            |
                            status + artifacts           one leased GPU
                                                               |
                                                   existing packet runner
                                                               |
                                             collector -> existing validators
```

Install packets through operator-controlled registration, verifying transport
SHA-256, source PACKET_ID and inner bindings before marking them runnable.
Registration is separate from job submission. A client supplies PACKET_ID;
the registry resolves it to one immutable source archive and installed tree.
Return both identities, registry version, producer commit, runner identity and
result-schema version. Jobs pin that record, never a mutable `latest` alias.

Persist requests, transitions, lease ownership and artifact metadata before
acknowledging them. The server chooses absolute working paths, tools and argv.
Map only typed options to existing CLI flags; accept no shell fragments, client
paths, arbitrary environment overrides or download URLs. Execute from a fresh
run directory outside the immutable installation; retain existing writable
scaffold staging and build-log collection. The installed packet must satisfy
the server's execution trust/isolation policy: manifest integrity alone does
not make executable source safe. This initial service is for authorized users
and registered code, rather than anonymous arbitrary-code execution.

## Initial HTTP contract

JSON objects use explicit schema versions. Unknown request fields are rejected.
All endpoints require a configured principal. The initial deployment binds to
loopback behind an authenticated SSH tunnel and uses an operator-provisioned
bearer credential. A later direct remote deployment must provide HTTPS and
equivalent principal/job ownership checks. Never capture credentials in returned
environment/log bundles. Credential provisioning is an operator action.

| Operation | Endpoint | Behavior |
| --- | --- | --- |
| Discover | `GET /v1/capabilities` | API/registry versions, packet modes, limits, worker availability, GPU UUID/resources/sharing state and tool/profiling availability. Discovery is not GPU qualification. |
| List packets | `GET /v1/packets` | Registered immutable packet identities, profile/requirements and downloadable source artifacts. |
| Submit | `POST /v1/jobs` | Validate request and capability compatibility, persist one job, return 202 with job identity and links. Requires `Idempotency-Key`. |
| Inspect | `GET /v1/jobs/{job_id}` | Durable state, phase progress, timestamps, original runner exit, collection and validation statuses, structured failures and artifact links. |
| Observe | `GET /v1/jobs/{job_id}/events?after=N&limit=M` | Ordered durable events after sequence N; return next cursor. Bounded polling, not a live GPU measurement stream. |
| Cancel | `POST /v1/jobs/{job_id}/cancel` | Idempotently request cancellation; return current state. Acknowledgment is not proof the worker/GPU has stopped. |
| Download | `GET /v1/artifacts/{artifact_id}` | Authorized immutable bytes with expected SHA-256 and byte count from artifact metadata. Artifact IDs are opaque, never filesystem paths. |

Accepted submissions pin complete semantics, for example:

```json
{
  "schema": "open64.gpu-test.job-request.v1",
  "packet_id": "b070675941adcfb2d55a791f7ce87c874c2b444bd165092c644cedc3b155d032",
  "profile": "combined",
  "mode": "qualify",
  "retry_of": null,
  "target_gpu_uuid": null,
  "samples": 20,
  "warmup": 3,
  "profiling": "off",
  "profile_timeline": false,
  "execution_timeout_seconds": 21600
}
```

`target_gpu_uuid: null` requests any compatible exclusively leased GPU. The
server returns the selected UUID before device work. Check-only uses no GPU
lease and rejects a GPU UUID or requested profiling. `profile_timeline: true`
requires `profiling: auto`. Samples/warmup map to existing runner controls;
capabilities supplies narrower server quotas when needed. The initial profile
is the complete combined packet: case filtering, per-profile omission, arbitrary
baseline selection and client-set tolerances are not accepted.

`retry_of` is null for an original job or names an accessible terminal job for
an explicit rerun. It creates lineage only; the rerun has its own key, workspace,
session and complete new evidence. It cannot resume or mutate the earlier job.

Idempotency is scoped to principal and key. The same parsed request (object
member order ignored) returns the same durable job ID without another run;
a changed request with the same key returns 409. Keep the mapping for the job's
retention period. A client retries an uncertain submission with the original
key; a deliberate rerun uses a new key and fresh run/session identity. No GPU
results cache substitutes for a fresh requested measurement.
An identical replay returns 200 with the original job's current state; a newly
accepted job returns 202.

Return a structured error with code, message, retryable flag and request ID.
Use 400 for malformed/invalid options, 401/403 for authentication/authorization,
404 for absent or inaccessible resources, 409 for identity/idempotency conflicts,
422 for an unregistered or incompatible packet/target, 429 for admission quota
and 503 for service unavailability. Busy compatible GPUs can queue; do not
mislabel a permanently incompatible GPU as merely busy. No rejected request
creates a runnable job or launches compilation/device work.

## Execution, cancellation and recovery

| State | Meaning |
| --- | --- |
| queued | Request is durable; no worker or GPU work has begun. |
| preparing | Worker authenticates inputs, creates its workspace and preflights tools/device. |
| running | Existing runner owns host checks, build, correctness/race gates, timing and optional diagnostic phases. Phase events identify the active work. |
| cancelling | Cancellation/deadline observed; stop owned child groups and establish device completion before reuse. |
| collecting | Runner finalizer and/or bounded recovery collector close logs and publish any complete or partial evidence. |
| validating | Existing offline receiver audits the closed result archive; no returned code is executed. |
| completed | Requested execution ended normally; verdicts are independent fields, not an implied pass. |
| failed | Build/test/service failure ended execution; retained evidence may still be complete. |
| cancelled | Stop/drain and bounded evidence handling finished; missing evidence remains explicit. |
| interrupted | Worker loss/restart prevented a normal completion record; evidence and GPU recovery are tracked explicitly. |

Ordinary execution follows queued -> preparing -> running -> collecting ->
validating -> completed/failed. Collection failure can finish failed without
validation. Queued cancellation finishes cancelled without device work;
active cancellation passes through cancelling and bounded collection/validation
where possible. Terminal jobs are immutable; repeated cancellation returns
their current record. Event sequence numbers increase per job, and a compacted
cursor must return an explicit cursor-expired error rather than silently skip
events. Events contain summaries/phase exit facts and bounded log artifact
references; they do not expose unrestricted live filesystem access.

The execution deadline starts when preparing begins, excluding queue delay.
The scheduler also enforces its disclosed maximum queue wait. Cancellation and
deadline reserve a separate bounded collection grace; timeout never terminates
collection merely to pretend it met the execution budget. Record timestamps,
original exit/signal, cancellation reason and collection outcome separately.

On worker restart, reconcile persisted ownership before running more work.
Do not blindly retry a job that might still own a GPU. Recover closed evidence,
mark interrupted work and quarantine an uncertain GPU until drain/ownership
recovery succeeds. A rerun creates a new job linked by `retry_of`; it does not
rewrite the interrupted job. API/client disconnect leaves accepted work running.

## Measurement and evidence contracts

Hold an exclusive physical-GPU lease across all combined device timing and
diagnostics. Queue other jobs for that GPU; different physical GPUs may run
concurrently. Report external load, MIG/MPS/sharing and throttling observations;
the service's lease alone does not prove system-wide isolation. The first full
Q7 route requires its existing capability-9.0/tool/runtime contract; unsupported
partition/sharing modes are rejected or remain an explicit unqualified state.

Within one job, keep existing correctness -> sanitizer -> unprofiled timing
gates and separate wall/device batches. Diagnostics are off by default and
run in separate processes after the relevant workload's timing; they cannot
overlap another timed job on the leased GPU. Optional profiling failure changes
diagnostic status, not original numerical/benchmark measurements. Preserve
measured/derived/proxy/estimate/unknown labels and explicit counter capabilities.
Do not infer local-versus-peer wait percentages from traffic counts alone.

Job transport state, process exit, per-profile correctness/race/performance,
diagnostic availability, collection and receiver verdict are separate fields.
Receiver verdict is one of NOT_RUN, PASS_REPORTED, FAIL, INCOMPLETE, INVALID.
A check-only job can finish completed with runner exit 0 and INCOMPLETE review.
A correctly rejected slower candidate is not a service fault: preserve its
workload decision and existing evidence. Q7 baseline/FLOP unknowns stay null.

Always attempt the existing automatic return collector after ordinary failures
and catchable interruption. Expose the unchanged result tar and checksum,
environment, logs/build diagnostics and receiver summary as separate immutable
artifacts. Return source tar/checksum references and external PACKET_ID so a
client can run the same validator offline. Server job metadata is a companion
envelope; do not modify the packet's authenticated source or returned manifest.

Publish artifacts only after files close and hashes/lengths are recorded. Use
bounded staged downloads; the client checks bytes/hash before accepting or
extracting, and the receiver still applies archive budgets/path/type checks.
Record retention expiry and any missing artifacts. Until expiry, reconnecting
must recover the same immutable bytes without rerunning the test. API success
does not imply GPU attestation, reference recomputation or transfer acceptance.

## Ease of use and implementation order

The intended client is one command after one-time server credential setup:

```text
gpu-test submit --server NAME --packet PACKET_ID --wait --output DIRECTORY
```

This is a proposed CLI, not a shipped command. `gpu-test doctor --server NAME`
should explain compatibility before submission. `status`, `watch`, `cancel`,
`fetch` and `review` operate on the printed stable job ID. `--json` supplies
machine output; interactive progress shows queue, selected GPU, phase, elapsed
time and the exact result directory. Print unknown/missing evidence explicitly.
An interrupted watch can resume by job ID without another submission.

Submission without `--wait` exits successfully only when the job is durably
accepted; it prints that this is not qualification. With `--wait`, the client
downloads and independently reviews evidence using the original receiver,
preserving its 0/1/2/3 PASS_REPORTED/FAIL/INCOMPLETE/INVALID exit codes. Reserve
4 for client/transport/pre-admission failure and 5 for acknowledged cancellation
without an assessable return. A broken watch reports the existing job ID for
resumption and must not submit another job automatically.

| Milestone | Deliverable and acceptance |
| --- | --- |
| GS0 contract | This design/OpenAPI plus packaging closure and versioned plan; source/schema/example/link checks, no runtime-service claim. |
| GS1 worker adapter | Translate registered jobs to existing CLI, persist state, enforce lease/deadline/cancellation and invoke existing validator; fake-worker failure/restart/finalization tests and check-only parity. |
| GS2 API and client | Authenticated endpoints, durable idempotency/events/artifacts, doctor/submit/wait/fetch/review; disconnect/retry/quota/cross-principal and download-corruption checks. |
| GS3 NVIDIA qualification | Actual full combined run, same-GPU attribution, required sanitizer coverage, separate timing/profiling and cancellation/drain on supported hardware. Pending host access. |
| GS4 compiler submissions | Version source admission and baseline/corpus registry, preserve current ABI and create authenticated snapshots; reject oracle/harness overrides and separately qualify Q7 candidate support. |
| GS5 usability closure | A fresh operator runs a documented install/doctor flow; a second user submits and retrieves/reviews evidence with one command, including an ordinary failure and reconnect. |

GS1/GS2 CPU control work proceeds while CP6/GS3 wait for hardware. GS4 may be
specified concurrently but cannot claim new-candidate execution via the initial
registered-packet endpoints. OpenCTI modernization is not a dependency of these
milestones; any later integration is separately requested and qualified.

Risks requiring acceptance evidence are stale packet/tool bindings, duplicate
execution on retries/restart, premature lease release, timing contamination,
unbounded collection/storage, source-admission scope drift and misleading
status aggregation. Owner/successor/reviewer/approver remain TBD. Existing
fallback, Q3 publication, COMMENT explanations and two-phase search isolation
remain governed by their current compiler contracts.
