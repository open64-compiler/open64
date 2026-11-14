# H1d device helper and host/serving contracts v1.0

2026-11-12. Implements the H1d software handoff for `h20_96gb` and
`h20_141gb`, following [H1c](AI-COMPILER-H1C-PLANNER-BINDINGS-v1.0.md).
Actual H20 inventory, NVIDIA execution, race checking and measured performance
remain qualification gates. Synthetic fixtures establish contract behavior;
matching digests do not attest real hardware. Transfer roles remain TBD.

## 1. Ownership and phase ordering

1. The backend driver resolves the H1a identity and imports H1b evidence once.
   The immutable context retains the original bounded, digest-pinned bytes.
2. H1c snapshots those facts before estimation-only search. Exploration stays
   detached and creates no WN/ST/TY or executable candidates. Only the selected
   plan is applied and verified; its complete device key is in the High seal.
3. After selected High admission, the driver checks that key against its current
   context, captures the accepted PU in a private child, and writes the owned
   bytes to an exclusive private staging file. It never reopens the original
   mutable profile pathname. The driver preserves the ordinary host product.
4. The separately linked device helper receives a fixed argv, reimports the
   staged bytes with the exact SHA-256/toolchain/device binding, and compares its
   snapshot to `tensor.high.selected.device` before constructing any workers.
   The original production manifest/resource/model/control key must also match.
   Stripping the transport from an imported source is an error. Supplying an
   explicit device context to an unbound source is also an error.
5. Worker construction copies the optional device key to `tensor.bridge.device`.
   Existing native seals bind that field at High, structured-memory Mid, and
   GPU-mapped Low. Missing/changed metadata fails the next seal check. Existing
   selected arithmetic, ownership, synchronization and COMMENT text persist.
6. PTX is emitted from verified native Low WHIRL. Host launch/completion records
   carry the same owned requirements; the completion manifest is published
   last with the existing no-replace protocol. The staged profile accompanies
   imported products for independent audit. The helper invokes search zero times.

This extends backend orchestration and existing metadata; it adds no WN layout,
WHIRL opcode, type kind, tensor trait, mapped-image table, or be.so dependency.
There are no MLIR interface/trait assumptions. WHIRL opcodes, tensor traits,
DO_LOOP/ARRAY and owning phase analyses retain their existing roles.

## 2. Helper command and artifacts

The old command remains valid for a source selected without a device:

```text
tensor_selected_bridge bridge-production selected-high.B bindings.v2.json PREFIX
```

For a selected explicit device append:

```text
--gpu-context h20_96gb PRIVATE_PROFILE_JSON EXACT_SHA256 TARGET_TOOLCHAIN
--gpu-context h20_141gb PRIVATE_PROFILE_JSON EXACT_SHA256 TARGET_TOOLCHAIN
```

Identity-only selection uses three `-` companions. It carries exact device
identity, with unknown resources and no performance claim. Architecture-only
selection adds no transport, optional metadata or requirement fields. Old seal
spellings remain unchanged. A helper that cannot accept the extended command
refuses normally; the backend retains its host fallback.

`open64.gpu-host-requirements.v1` in launch/completion records contains exact
device/ISA, device revision, partition, resource/execution/budget IDs and
revisions, toolchain, raw input digest, complete selection key and separate
resource/execution/budget keys. `minimum_backing_bytes` is the checked sum of
unique native launch storage extents. It is a floor for the product's backing;
the runtime owner must also reserve metadata, other live allocations and runtime
overhead. This is not a query of currently free memory or proof of native RF
allocation. `performance_qualified` remains false. Imported plans require a
runtime budget reservation. Failed staging is diagnostic evidence, never a
completed device product.

## 3. Runtime compatibility and performance evidence

`DSL_GPU_HOST_REQUIREMENTS` is an owned embedding contract. A trusted owner binds
it to the authenticated fixed code record; accepting a caller's arbitrary JSON
or claimed digest is insufficient. Its observation authenticates the actual
device and ISA, partition/inventory mapping, toolchain applicability, current
context generation, budget generation, and an owner-held reservation identity.
The reservation must cover the aggregate invocation ABI and metadata, plus any
additional allocations charged by the owner, and remain held through completion.
Callbacks must not throw or reenter the registry and must outlive it.

| Change | Execution outcome | Performance-evidence reuse |
| --- | --- | --- |
| Exact observed device/resources/budget and live sufficient reservation | Eligible, subject to existing loaded-kernel/ABI/completion checks | Matching nonempty execution evidence may be reused; never hardware qualification |
| Changed execution calibration only | Code may execute | False; do not transfer old timing/optimality claims |
| Wrong device, ISA, partition, inventory key, toolchain applicability or budget key | Refuse before arm/publication | False |
| Unknown observation, expired/undersized reservation | Refuse | False |
| Context, budget lease generation or reservation changes during invocation | Refuse before launch/publication | False |
| Fresh reservation generation at a new Begin | May admit if other requirements match | Independently checked |
| Identity-only device selection | Exact identity observation required, no imported resource tuning | False |
| Architecture-only selection | Existing runtime behavior and loaded-code checks | No device-specific tuning or qualification claim |

Code admission is conservative: a changed resource/budget key requires a new
validated binding even if a larger device might execute the same instructions.
Cross-device portability and AOT prefill/decode selection remain separate work.
Physical device identity never follows from `sm_90` alone or from reported HBM
capacity. H20 variants are distinct from each other and H200.

## 4. Q3 and CUDA owner integration

An optional constructor overload copies `DSL_GPU_SERVING_BINDING`; existing Q3
constructors and the `VHO_SERVING_MEMORY` layout remain unchanged. Before Import
or Begin, Q3 checks current requirements and sufficient reserved backing. It
captures the lease key in owned facts. Before arm, costing export/revalidation
and completion publication it checks the same lease, with fresh observations.
The live owner also checks the binding before detached costing export or
revalidation. Detached Q4 keys and existing fact/bundle layouts remain unchanged;
optional binding diagnostics use a separate owned sidecar and additive lookup.
Calibration reuse diagnostics do not change code compatibility or introduce runtime search.

The complete published bundle carries the selection/lease keys and the latest
performance-reuse result alongside its unchanged cache/scale allocation
identities, content versions, page mappings and valid lengths. Failure publishes
no bundle. Existing synchronous callback or proven CUDA event/drain completion
remains mandatory before releasing leases; a requirement failure cannot infer
completion. A caller may use its existing compatible fallback after proper
cleanup, using fresh output storage where needed.

The optional CUDA adapter adds an explicit `Open` overload taking the owned
binding. Existing `VHO_MLA_CUDA_CODE`, `VHO_MLA_CUDA_HOOKS` and legacy `Open`
layouts/signatures are preserved. The embedding owner authenticates the
requirements-to-cubin association. The new overload checks observation on open
and supplies the binding to the existing Q3 registry; existing actual CUDA
context/stream IDs, allocation buffer IDs, loaded-kernel resource/ABI queries,
completion events and cleanup retry checks remain necessary. This is an opt-in
embedding facility, not automatic discovery of serving hardware or an engine
adapter. The current quantized Q4/Q5 imported-device optimization route remains
refused until its dedicated profile adapter is implemented; H1d does not claim
that work complete. AOT selection stays deferred.

## 5. Milestones, tests and remaining risks

- H1d-T: owned profile bytes and exact helper reimport, no stripped/replaced
  context, source and worker seal checks, three native boundaries and PTX reemit.
- H1d-H: host launch/completion requirements, backing floor, no-replace output,
  unknown-profile skip and failed/tampered/incomplete-helper host preservation.
- H1d-S: owned optional serving binding, mismatch checks at admission/arm/
  completion, reservation freshness, cache/scale publication and calibration-only
  compatibility; scalar/Torch reference checks remain outside search.
- H1d-C: preserve legacy embedding ABIs and test the new CUDA overload with the
  existing delayed Driver model and real SDK declarations. Model execution is
  not NVIDIA execution, performance measurement, or a race-check qualification.

Focused tests live in `run_gpu_host_contract.py` (C++98 importer/owned contracts,
fatal ASan/UBSan), `run_gpu_transport_review.py` (actual driver/helper/native
products), `run_mla_serving_review.py --gpu-device DEVICE` (real host entries and
synthetic observations), and `run_mla_q6d_review.py` with
`OPEN64_Q6D_GPU_DEVICE=DEVICE` (focused Driver-model overload). Existing unbound
Q3/Q4 and estimation-only portfolio regressions remain acceptance checks.

The main remaining risk is authority supplied by external serving owners:
profile integrity cannot certify inventory authenticity, runtime reservation
exclusion or owner behavior. Exact-key admission is intentionally conservative.
Native RF allocation, real Hopper execution, asynchronous race checks,
calibration validity and performance are still separate gates. H2-H8 remain
responsible for broader memory optimization and measured profitability.

## 6. Accepted software evidence

The acceptance run passes 24 host-contract mismatch cases and both H20 identity
contracts under fatal ASan/UBSan; 97 existing importer cases and 18 planner
fixtures also pass those sanitizers. The actual backend/helper suite passes
14 context refusals, six native worker-seal refusals and ten fallback cases.
The ordinary host trace is unchanged by successful or failed device handoff.
Both imported devices and architecture-only selection reopen at High/Mid/Low,
reemit identical PTX, and retain explanatory comments in native whirl2c CUDA.

Serving acceptance passes 132 mismatch/freshness cases and eight native host
reference executions across the two variants; 570 existing unbound Q3/Q4 checks
pass. Each focused CUDA Driver-model overload run passes 6,060 checks, including
fixed generated arithmetic, three-stage ordering, admission/submit/completion
changes and uncertain-drain/cleanup regressions. These are model checks with
real retained SDK declarations, not NVIDIA hardware execution. Four existing
search/permission/selected-short-long correctness regressions pass; be.so
dependencies are unchanged and lw_inline remains link-closed.

The immutable receipt under `artifacts/review/ai-optimization/research/h1d-gpu-transport/`
records exact commands, source/producer hashes, prior receipt identities,
source-interleaved B/T pairs and raw diffs. Earlier H1a-H1c receipts are preserved.
