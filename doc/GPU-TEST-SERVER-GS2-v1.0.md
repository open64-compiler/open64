# GPU testing HTTP service and remote client GS2 v1.0

November 11, 2026. Implemented authenticated loopback HTTP service, managed GS1
worker pool and reconnectable client. The supported wire contract is
[OpenAPI v0.2](GPU-TEST-SERVER-API-v0.2.openapi.json). It makes the earlier
v0.1 proposal (`GPU-TEST-SERVER-API-v0.1.md`, historical design reference) concrete, including explicit unknown
capability facts and separate GS1 collection/review status. OpenCTI is deferred.

## Implementation and ownership

| Component | Responsibility |
| --- | --- |
| `gpu_test_http.py` | Bearer authentication, principal ownership, typed endpoints, quotas, bounded event pages, integrity-checked artifact streaming and managed worker slots |
| `gpu_test_client.py` / `gpu-test` | Discovery, durable submission receipt, status/watch/cancel, verified downloads and independent local receiver review |
| `gpu_test_worker_state.py` | Existing GS1 transactions plus optional atomic active-job quota and created/replayed admission result; old GS1 callers retain their behavior |
| Existing GS1 worker and registered packet | Process/GPU ownership, cancellation/drain, recovery, correctness/races, measurements, collectors and original receiver semantics |

The service accepts only installed operator-registered combined packets. There
is no source upload, arbitrary argv/environment, oracle/tolerance replacement,
compiler search, JIT or new AOT dispatch. Full quantized Q7 and BF16 MLA core
retain their different operation boundaries and acceptance requirements. Original
combined v1.0 and GS1 companion archives remain unchanged.

## Authentication, API and disclosure

The initial listener binds only `127.0.0.1`. Reach it through an authenticated
SSH tunnel; the client refuses plain HTTP to a non-loopback host. HTTPS origins
are supported by the client with normal certificate validation for deployments
that supply a trusted reverse proxy. Direct public HTTP/TLS listener deployment
is outside this increment. Client redirects are refused, and environment proxy
settings are not used to forward credentials.

Operator credential setup creates fresh mode-0600 files: a client bearer and a
server configuration containing only its SHA-256/principal binding. Tokens are
not printed, logged, persisted in client receipts or forwarded to workers. A
server configuration may contain multiple independently issued principal/hash
entries. Local operator access to the store and registered code remains trusted,
as in GS1; this is not anonymous code execution or a tenant filesystem sandbox.

All seven v1 operations are implemented: capability and packet discovery,
submission, job inspection, paged events, cancellation and artifact retrieval.
Job inspection/cancellation/events and job artifacts require the owning principal;
inaccessible resources return 404. Registered source packets are available to all
configured principals. Responses expose opaque IDs, source/runner identities and
original review facts, excluding worker paths, ownership tokens and argv.

POST admission rejects unknown fields, duplicate JSON keys/nonfinite numbers,
unsupported options, missing keys and bodies over 64 KiB. Its principal-scoped
idempotency check and active-job quota run in the same GS1 transaction. New jobs
return 202; identical replays return 200 even when the quota is full. Conflicting
key reuse returns 409; quota returns 429. Rejected requests launch no work.
Cross-principal retry lineage is inaccessible. A changed deliberate run uses a
new key/output and can name an accessible terminal `retry_of` job.

Capability requests read registry and process ownership only; they never launch
GPU/tool probes during a benchmark. Unknown device resources, sharing facts,
tool versions and timeline availability are null/unknown, not inferred as
qualified. The registered packet still enforces its complete platform/tool
requirements. A busy or quarantined registered GPU leaves compatible jobs queued.

## Persistent progress, workers and cancellation

The API stores a stable server ID in the GS1 database. Accepted jobs survive
client/API disconnects. Events use durable increasing per-job sequences and
bounded SQL pages. Negative, duplicate, excessive or future cursor requests
are refused. Missing retained history returns an explicit expired-cursor error;
the client does not silently skip it. No event compaction is performed here.
Progress reports preparation/execution/drain/collection/review phases; it does
not invent a GPU duration from queue, build or HTTP time.

Each managed worker uses its own process and an exclusive local slot lock,
then calls the existing GS1 worker repeatedly. Slots prevent a replacement API
from duplicating a surviving pool after a crash; GS1 claims/leases independently
prevent duplicate jobs/device ownership. Lost workers are reconciled through
GS1 without blindly rerunning execution. `--workers 0` supports separately
managed workers. One shared host-local store remains mandatory.
After API loss, a managed orphan finishes its current GS1 call and retires;
the replacement service can then take the slot. It does not keep claiming jobs
indefinitely under a dead supervisor. Separately managed workers remain explicit.

Explicit cancel sets the existing durable GS1 request. It is idempotent; an
acknowledgment is not proof of device release. API/client disconnect does not
cancel work. Graceful server shutdown signals its owned workers, allowing GS1
drain/finalization; forced worker loss leaves its persistent ownership for
conservative restart recovery/quarantine. Source/runtime/GPU safety remains the
[GS1 contract](GPU-TEST-WORKER-GS1-v1.0.md).

## Client submission, retrieval and independent review

The client binds an output directory to server ID, origin and principal. It
persists request and idempotency key before POST. A lost acceptance response can
be resolved by repeating the same submit/output; a different request or identity
is refused. `watch` resumes a saved cursor and never submits a job. Interrupting
watch leaves accepted work running. Status/cancel operate directly by stable ID.

Fetch retrieves only closed immutable artifacts. It checks advertised length,
SHA-256 and response headers, streams to fresh `.part` files and publishes with
no-replace behavior after verification. A failed transfer is not published under
a valid filename; changed existing evidence is never overwritten. Redirects and
artifact URLs outside the configured origin are refused. Repeated fetch reuses
verified local bytes and can retry an interrupted transfer.

With `--packet-dir`, review uses a separately held exact source packet. Otherwise
fetch downloads the registered source archive, verifies transport and submitted
PACKET_ID, preflights all paths/types/count/expansion budgets, then extracts
regular inert files and rechecks them. No downloaded validator, module, shared
library or executable is run/imported. The client invokes its own trusted
`validate_gpu_test_suite.py`, which delegates the unchanged shared/native audits.
Offline `review` works without server credentials. Retained source is data for
that validator; raw GPU numerical outputs are still not supplied by the package.

Job state, original runner exit, collection/review status and receiver verdict
remain separate. `submit` without `--wait` acknowledges durable acceptance and
prints the job ID. With `--wait`, the client waits, fetches and reviews. Exit codes
0/1/2/3 preserve PASS_REPORTED/FAIL/INCOMPLETE/INVALID; a failed remote execution
cannot become client success merely because its evidence is well formed. Code
4 reports configuration/transport/client failure, and 5 cancellation without an
assessable return. Successful check-only deliberately exits 2 after review.

## Operator and compiler-user runbook

Install the companion on the GPU host and client. Use Python >=3.9/POSIX and
the existing local GS1 store. Register the exact source packet using the GS1
`register-packet` command and its external digest. Register a real physical GPU
through GS1 before qualification requests; check-only requires none.

Create a separate credential for each authorized principal, transferring its
token file to that user over a protected channel:

```bash
python3 tools/gpu_test_http.py init-credentials \
  --credentials /secure/gpu-server.json \
  --token-file /secure/compiler-user.token --principal compiler-user
python3 tools/gpu_test_http.py serve --store /path/to/worker-state \
  --credentials /secure/gpu-server.json --port 8765 --workers 1
```

Use additional workers for distinct GPUs while sharing the same store. Quotas
default to four active jobs per principal and can be set with `--quota`. Secrets
are excluded from released/returned evidence. No automatic deletion/expiry is
performed; retention metadata is null and durable evidence remains retained.

From the client machine, establish the tunnel, then configure its origin and
token-file path once:

```bash
ssh -N -L 8765:127.0.0.1:8765 gpu-host
```

In a separate terminal:

```bash
export GPU_TEST_SERVER=http://127.0.0.1:8765
export GPU_TEST_TOKEN_FILE=/secure/compiler-user.token
tools/gpu-test doctor
tools/gpu-test submit \
  --packet b070675941adcfb2d55a791f7ce87c874c2b444bd165092c644cedc3b155d032 \
  --wait --output run-001
```

Alternatively use `python3 tools/gpu_test_client.py`; explicit `--server` and
`--token-file` are global flags before the command. Add `--mode check-only` for
CPU-only validation, or `--profiling auto` for the original separate diagnostic
lane. `--profile-timeline` requires profiling auto. `--packet-dir PATH` avoids
downloading another copy of the original source archive. `--json` supplies JSON
with progress kept off stdout. The default source archive is about 199 MB and
expands to about 1.9 GB; retain sufficient client storage.

```bash
tools/gpu-test status JOB_ID
tools/gpu-test cancel JOB_ID
tools/gpu-test watch JOB_ID --output run-001
tools/gpu-test fetch JOB_ID --output run-001
tools/gpu-test review --output run-001
```

Read `submission.json`, `watch.json`, `job.json`, `fetch.json`, `artifacts/` and
the fresh `review-*/summary.md` plus per-profile reviews. `server.json` pins the
remote identity. The client can reconnect using the same directory; a new
experiment uses a fresh directory/key. An acknowledged job continues when watch
times out; use its printed ID. The source checksum and original packet identity
remain distinct from the results transport checksum.

## Qualification and remaining gates

Focused tests use real loopback HTTP, concurrent requests, SQLite and GS1 fake
workers for authentication/ownership, atomic idempotency/quotas, cancellation,
durable cursors, lost responses, reconnects, immutable downloads, corrupted data,
redirect refusal and inert source extraction/review. Existing GS1 and receiver
tests remain required. A separate actual server/pool/client CLI flow registers
the unchanged released packet, runs check-only, transfers/authenticates source
and results, and independently reviews all three profiles as INCOMPLETE.

These checks do not certify NVIDIA execution, race freedom, counter support or
performance. GS3 still needs a real supported host and full original reference/
sanitizer/timing/profiling qualification. GS4 new-source admission and OpenCTI
integration remain deferred. Two-phase search isolation, Q3 publication,
COMMENT explanations and compiler fallback are unchanged; roles remain TBD.
