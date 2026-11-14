# MLA Q7 NVIDIA qualification package v1.0

November 10, 2026. Continues Q6d device bindings, commit `da0f8ca3`.
Q7 delivers the hardware qualification harness and a transferable tar archive.
No NVIDIA machine is available to the producer. Preparation and offline checks
do not close NVIDIA execution, numerical, race or performance gates.

## Fixed-code scope and authority

The packet retains all 14 accepted Q6c staging products from the authenticated
Q6d receipt, including compiler-generated prepare/attention/finalize `.cu`,
native `.ptx`, fixed linked cubins, stage manifests, Low binary WHIRL, full
before/High/Mid/Low traces and raw Mid-to-Low diffs. CUDA/WHIRL COMMENT
explanations are byte-preserved. No CUDA kernel is independently templated.
Q6c's original <=48 KiB layout/resource envelope and strict source folds remain.

The private qualification decoder now accepts caller-supplied actual context,
producer/execution and consumer streams. The old Q6d synthetic-handle entries
remain for the delayed Driver model. `libmla_serving_cuda_q7.so` links the same
Q6d runtime and production Q3 registry to the actual CUDA Driver, excluding the
model Driver and test-counter APIs. The SDK stub is link-only, never a runtime
search path or device implementation. No backend dependencies/exports change.

Preparation authenticates the entire upstream Q6d receipt and the unchanged
Q6c parent before extracting fixed products. On the GPU host, the externally
identified packet manifest is verified before any native binding or image load.
The outer tar checksum authenticates the delivery including PACKET_ID and the
manifest. A checksum is integrity evidence within the trusted owner's delivery,
not a new compiler-provenance signer or serving-engine packaging ABI.

The approved code record binds original program/source/mapping keys, all three
Low native identities, actual emitted entry names, launch/shared extents and
the retained cubin digest. Each request obtains fresh device Q3 facts, never
uses historic host identities as leases, and uses original status, controls,
cache/scale versions, page maps, parent CAS and whole-tuple publication.
Actual Driver completion gates publication. Consumer copies use a distinct
nondefault stream, pinned host destinations and the original Q3 reader until
actual consumer completion. No source fallback is launched by this harness.

## Recipient requirements and commands

Use Linux x86_64 with Python >=3.8, a compute-capability-9.0 NVIDIA GPU (for
example H100/H800), and a real Driver exposing CUDA >=12.8 APIs. The retained
Linux native binding/compiler dependency was built with Ubuntu 20.04/GCC 9.4;
use glibc >=2.31 and compatible libstdc++. CUDA 12.8.1
development tools are needed for compile checks, not to execute retained cubins.
Compute Sanitizer with the capability checks below is needed for full race
qualification. No PyTorch, model download, Open64 rebuild or JIT is needed to
execute. The tar includes the relevant host source/header closure and exact
relocatable C++ compile commands, plus retained pure compiler link dependencies.

Verify the separately delivered archive checksum, then extract:

```bash
sha256sum -c Open64_MLA_Q7_NVIDIA_v1.0.tar.gz.sha256
tar -xzf Open64_MLA_Q7_NVIDIA_v1.0.tar.gz
cd Open64_MLA_Q7_NVIDIA_v1.0
export CUDA_HOME=/usr/local/cuda
export PATH="$CUDA_HOME/bin:$PATH"
python3 -m unittest -v test_mla_q7
```

Compile the compiler-generated CUDA and reconstruct native PTX bundles. The
script logs exact argument vectors and outputs without modifying the packet:

```bash
python3 build_mla_q7.py --packet . --expected-sha256 "$(cat PACKET_ID)" \
  --cuda-home "$CUDA_HOME" --mode generated --output ../q7-compile-generated
```

For each fixed case this runs strict sm_90 nvcc `-c` on the three generated
CUDA files, ptxas `-c` on the three native PTX files, nvcc `-dc` on the saved
device-math provider and nvlink on the PTX/math objects. The original explicit
fmad/ftz/prec-div/prec-sqrt flags are retained. These are two compiler-emitted
representations of the same selected WHIRL; CUDA compilation is a compile check,
and the executable bundle is the native PTX route. The script records cubin
byte equality. A different toolchain's rebuilt image is not silently authorized
or substituted. Execution below always uses the retained approved image.

Optionally rebuild the host binding against the local SDK. This rebuilds three
source objects and reuses the retained pure contract object and be.so. It does
not claim a full Open64 compiler source rebuild. The build receipt binds the
new host library hash to the exact packet; append
`--library ../q7-host/libmla_serving_cuda_q7.so` to run commands to use it.

```bash
python3 build_mla_q7.py --packet . --expected-sha256 "$(cat PACKET_ID)" \
  --cuda-home "$CUDA_HOME" --mode host --output ../q7-host
```

Run correctness first or the complete sequence. Output directories must be new:

```bash
python3 run_mla_q7.py --packet . --expected-sha256 "$(cat PACKET_ID)" \
  --device 0 --mode correctness --output ../q7-correctness
python3 run_mla_q7.py --packet . --expected-sha256 "$(cat PACKET_ID)" \
  --device 0 --mode all --warmup 3 --iterations 20 --output ../q7-all
```

`all` runs correctness, memcheck, racecheck, synccheck, initcheck and then an
uninstrumented performance child in separate processes. Use `--timeout` to set
the per-child wall-time budget; a timeout is a failure, never a convergence or
correctness result. `--case dense_s8_decode--async` explicitly limits a smoke
run; it is reported as a subset, never automatic AOT selection. `--prefix 40`
or `--prefix 256` explicitly limits address-residue coverage. Without overrides,
correctness/sanitizers cover both residues for all 14 products, while performance
uses prefix 256. Never compare sanitizer-instrumented times to normal timing.

The source for a representative long decode case is
`cases/dense_s8_decode_long--async/attention.cu`. Each case has the other two
launch stages, annotated traces, PTX and original source identities beside it.
The large paged U4 case is `cases/paged_u4_prefill_long--async/attention.cu`.

## Numerical, publication and race acceptance

Preparation saves deterministic dense/paged/append S8/U4 fixtures, tails,
zero-active queries and genuine 8193-token workloads. All six source outputs
are saved against selected host source, PyTorch and independent scalar reference.
The GPU host consumes typed portable bytes. First-two-result F32 tolerances
remain rtol=2e-5/atol=2e-6; remaining cache/scale results require exact values.
Nonfinite classes/signs must match; unexpected NaN is a failure. No tolerance
is relaxed after observing a device result. Reports retain max absolute/relative
and RMS errors, and fail at the first incorrect element.

Every case verifies immutable inputs, prefix/suffix allocation guards, six
outputs, the complete Q3 publication, and exact consumer-stream tuple contents.
Prepared cancellation must release once; input mutation under a prepared lease
and duplicate Submit must refuse. The small dense async case also executes the
actual emitted NaN guard, requires no publication, restores input cooperatively
and proves a fresh positive invocation can proceed after completed refusal.
Nonblocking completion is not required to be pending: GPU speed makes that
observation nondeterministic. Injected Driver failure recovery remains covered
by Q6d; hardware Q7 does not fake allocation recycling or context loss.

Scratch/output interiors are not prefilled: initcheck can observe missing
initialization. Inputs and guards alone are uploaded. Guards are read separately
so unused scratch padding is not copied to the host. Consumer pinned buffers
survive until observed consumer completion. Any live/uncertain failure stops
the qualification process without in-process reclamation or source fallback.
Failure is reported with attempted/completed execution state; no successful
cleanup is inferred from an error. External code must not reclaim Q3 buffers.

Sanitizer help/version/logs and exact commands are retained. Bulk-copy checking
is explicit. Racecheck requires async-copy and proxy-fence checking and detects
info-level hazards; initcheck requires both global/shared coverage. Missing
features produce TOOL_COVERAGE_INCOMPLETE, not a clean race report. Require
zero summaries, no tracking overflow/unsupported instrumentation, successful
GPU child reports and the exact same case/reference matrix. Expected resource
arity probes return INVALID_VALUE, so automatic API-error reports are disabled;
the adapter checks every required API result. Memory/race/synchronization
diagnostics are not suppressed. Tool logs are not proof of all possible races.

NVIDIA documents [bulk/proxy checking and tool options](https://docs.nvidia.com/compute-sanitizer/ComputeSanitizer/index.html)
and [version-dependent bulk-copy coverage](https://docs.nvidia.com/compute-sanitizer/ReleaseNotes/index.html).
This is why a clean run of an older tool is insufficient for the full gate.

## Performance evidence and remaining gates

Warmup and samples operate on persistent device buffers, with fresh tickets and
publication for each independent replay of the same source request. Paged
parents are retired/reimported between samples; this is not an evolving serving
decode chain. Allocation, initial upload, reference downloads, consumer checks
and retirement are excluded from the reported request interval. Every case is
reference-tested before and after sampling. Raw samples, median, p95, min, mean
and standard deviation are retained with device UUID, Driver, SM count, clocks,
memory bus facts, environment and nvidia-smi snapshots; settings are not changed.

`host_begin_submit_complete_us` includes Begin, Submit and Q3 completion/status/
control/publication. `submission_stream_span_ms` brackets Submit on the execution
stream and includes admission/control transfers and host submission gaps; it
is explicitly **not** pure kernel duration or serving-engine latency. NVIDIA's
[Driver event timing contract](https://docs.nvidia.com/cuda/archive/12.8.0/cuda-driver-api/group__CUDA__EVENT.html)
governs this measurement. Use an external Nsight Compute/Systems invocation on
an explicit fixed case for per-kernel time, stalls and traffic; profiler replay
results must stay separate from normal samples. No Nsight metric is invented
from event time, and no speedup is asserted without a matched baseline.

Preparation can PASS locally; hardware reports remain GPU_UNAVAILABLE (exit 77)
without a suitable Driver/device. Missing/insufficient tools exit 78. Malformed
packets, numerical errors, hazards, CUDA failures and timeouts exit 1. Only
actual execution and all required tool/reference gates can produce a hardware
PASS. Preserve original logs/reports when returning results. Primary owner,
successor, reviewer and approving lead remain TBD.

Two-phase search is unchanged: Q7 measurements never occur inside candidate
exploration, never trigger new lowering and never choose another configuration.
Any feedback into a future model needs a separately versioned offline profile.
The PREOPT COMMENT workaround and fallback semantics remain. AOT selection,
phase dispatch, serving-engine import/packaging and JIT policies stay deferred.
