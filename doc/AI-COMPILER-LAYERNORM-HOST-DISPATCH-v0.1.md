# LN4 Selected Host Dispatch And Mode Packaging

Version v0.1 | 2026-10-29

## Contract And Scope

The explicit LN4 checkpoint now completes its compiler-side handoff in two
modes. `-DSL:normalization_mode=service` (the compatibility default) emits no
host PU. `standalone` additionally creates a program-specific ordinary host PU.
Both use the same cost-selected device WHIRL. Performance harnesses remain
separately requested; neither mode creates one or changes the source v1 recipe.
An unsupported mode refuses before PREOPT or publication.

| Building block | Owner and obligation |
| --- | --- |
| Analysis PU | Existing source scope; PREOPT and WOPT characterize/select it. |
| Device PU | Existing LN4 materializer; owns executable kernel semantics. |
| Host dispatch PU | Driver-created ordinary WHIRL wrapper with two opaque pointer formals, selected plan/body constants, effectful runtime call, immediate return-preg capture and status return. |
| Execution consumer | Runtime implements allocation, guarded copies, leases, launch, drain, publication and retry/cancel, using deployment services. |
| Deployment | Creates context/stream/shared registry, provides hash/buffer/join/quiescence callbacks, opens the packaged compiled entry and owns its lifetime. |
| Service session | Owns compilation admission and request scheduling; a general concurrent compiler session manager is not part of LN4. |

The wrapper calls `open64_normalization_execution_run_selected` with its exact
plan and device-tree seals. Under the consumer's operation token this checks the
already-opened immutable descriptor before reading roots, allocating or moving
data. Seals detect mismatched compiler products, not adversarial tampering;
the existing compiled-entry SHA-256 and resource admission remain required.
Failure retains the consumer's documented resume/cancel/destroy obligations.

```text
compiler-selected plan
  device PU: selected LN-REF-1 C0/C1/C2 kernel
  optional host PU: int dispatch(void *execution, void *whole_root_requests)
    status = run_selected(execution, selected_plan_seal,
                          selected_device_tree_seal, whole_root_requests)
    return status
```

Driver owns PU creation, local scopes, maps and image publication. No WOPT pass
traverses another PU and no new mapped WHIRL encoding is introduced. Use ordinary
CALL/PARM, immediate STID from Return_Val_Preg, RETURN_VAL, ST/TY and complete
physical-formal metadata, following the WHIRL reference CALL/return rules
(https://www.mcs.anl.gov/OpenAD/open64A.pdf). No frontend builder/runtime linkage
is added to `be.so`.

## Emission And Packaging

Keep analysis, device and host PUs in the full mapped image for interface
integrity. `whirl2c -cuda` still emits only device PUs. Ordinary `whirl2c` with
`-DSL:normalization_host_entry=<manifest host_dispatch>` emits only the verified
selected host wrapper, not unlowered analysis PUs. The filter refuses absent or
malformed wrappers; it does not infer a plan or generate a source template.
Retain the emitted host C with the device `.cu`, cubin, entry descriptor, native
guard, mode manifest and before/after mapped evidence. Host C uses the ordinary
Open64 support header and links to the optional normalization runtime library;
service callers use the same checked runtime API without this wrapper.

## Required Evidence

`AI_NORM_HOST 1` compile rows name mode, host/device entries, exact selected
constants and source line last. Runtime `selected` observations report identity
acceptance/refusal before any movement. `host-dispatch.log` starts with
`# What it summarizes:` and joins mode, source/plan/body, mapped call/ABI,
emission, status propagation, wrong-entry refusals and outstanding deployment
obligations. Include it in full `optimization.log` and phase coverage summary.

Validation must independently reopen before/after images with `-st -src`, verify
the actual host call/formals/constants/capture/return, compile emitted host C,
exercise it with the real consumer plus CUDA API mocks, and retain raw traces.
Test service absence, standalone presence, malformed mode, trace-off parity,
wrong selected identities and status/cleanup propagation. Offline CUDA resource
checks remain distinct from GPU numerical/performance certification (LN6).

## Developer Workflow

The normal request/profile, O3/n64 and strict-FP gates still apply. For example,
with a source image and its explicit source-bound request already prepared:

```sh
be -fB,source.B -O3 -TARG:abi=n64 \
  -OPT:roundoff=0:IEEE_arith=1:fast_math=off \
  -DSL:gpu_arch=sm_90:normalization_mode=standalone \
  -DSL:normalization_request=request.json:normalization_output=selected.after.B source.cxx
whirl2c -O0 -fB,selected.after.B \
  -DSL:normalization_host_entry=open64_ln_dispatch_12801_1_c2 \
  -CLIST:dotc_file=host.c:doth_file=host.h source.cxx
whirl2c -cuda -fB,selected.after.B -CLIST:dotc_file=kernel.cu source.cxx
```

Use the actual `host_dispatch` from the manifest, not the illustrative symbol
above. The existing component search-path options remain necessary outside an
installed compiler. Extraction validates the high-level host tree immediately
after reading the PU, before ordinary backend return lowering replaces negative
return pregs and RETURN_VAL. It then delegates to normal whirl2c emission.

After offline CUDA assembly and resource auditing, the existing compiled-entry
packager also preserves standalone `host.c`, `host.h` and `whirl2c.h` with SHA-256
identities in `entry.json`. It never fabricates host or device source. Service
packages retain no generated host body. The source include is relative, so these
review/deployment components can be relocated together.

Build/install the optional runtime SDK explicitly in a dedicated build directory:

```sh
make -f /path/to/open64/osprey/libopen64rt/tests/Makefile.normalization_cuda \
  SOURCE=/path/to/open64/osprey CUDA_HOME=/path/to/cuda \
  PREFIX=/usr/local DESTDIR=/path/to/staging install
```

The SDK installs the optional static archive and five public/support headers.
Link host C or a service client against that archive, CUDA Driver `libcuda` and
the C++/thread runtimes. The opaque compiler call uses `unsigned long long`
for U8, exactly matching whirl2c's `_UINT64` on supported n64 deployments; native
request/guard structures are not a portable wire format.

The application/service opens the audited compiled entry once using
`open64_normalization_execution_open`, supplying its shared registry, buffer
service, real SHA-256 and readiness/publication/quiescence hooks. Invoke the
emitted host function with that execution handle and four whole-root requests,
or call `open64_normalization_execution_run_selected` directly in service mode.
On failure, follow resume/cancel/destroy ownership rules rather than dropping
the handle or blindly invoking the wrapper again. Deployment must implement or
adopt these service contracts; installing the SDK does not create those policies.

LN4 completion means the bounded explicit compiler checkpoint, selected executable
WHIRL, emitted host/device source, compiled-entry handoff and runtime enforcement
are available and reviewable. It does not mean a general `openpy -o a.out`
linking driver, production inference service, calibrated timing table, benchmark
harness or GPU-certified deployment exists. Those broader execution-mode tasks
and LN6 remain explicitly tracked, not silently declared complete.

## Accepted Compiler-Side Closure

Implementation `046b198b` passes all 36 selections on the four exact targets:
C0 4 / C1 4 / C2 28. Normal and leak-checked ASan/UBSan runs each pass 792
scenarios, 30,368 consumer checks and 10,152 CUDA API mock checks through the
actual emitted host C. The 144 mapped dumps cover both standalone/service
before/after boundaries; independent native reopens verify selected bodies,
host ABI/constants/status propagation and 108 deliberate host mutations.
Forty mode/extraction and nine additional scope/ABI/FP/target/v1/publication
refusals pass. All 135 focused tests and the installed public SDK ABI pass.

Standalone/service device sources are identical except their timestamp banner;
all 72 PTX/cubin files match the authenticated previous numerical producer.
Its 288 datasets / 26,240 oracle checks are inherited, not new execution.
The prior consumer/adapter/base-runtime suites also pass both modes with 720
byte-exact prior artifacts. No v1, reduction, binary-image layout or compiler
runtime-library dependency change is claimed.

Accepted immutable final-path replay repeats compiler output, host/runtime
builds, offline CUDA, sanitizer matrices, refusals and focused tests: 864 files
are byte-exact, 648 use only recorded banner/review-root normalization, and 216
joins compare all compact compile records or exact entry semantics with fresh
evidence-hash verification. It uses authenticated frozen compiler binaries;
compiler-build/shared-boundary proof is inherited from primary validation.
The [progress ledger](AI-COMPILER-OPTIMIZATION-PROGRESS.md) records both receipt
paths and the preserved source-bundle correction. Raw `.t` and complete mapped
dumps/diffs remain reviewable. Deployment integration and LN6 GPU numerical/
resource/performance certification remain open, not replaced by sentinel tests.
