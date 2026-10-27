# MMA1 Compute Contract And Exact Target Inventory

Date: 2026-10-27. Revision 1. Scope: caller-owned runtime analysis records.
MMA0 is user-approved for MMA1. This increment implements inventory and
structural verification; MMA2/MMA3 and all native lowering remain open.

## Evidence And Exact Recipes

The independently retained primary reference is NVIDIA's
[CUDA 12.8.1 PTX ISA 8.7 manual](https://docs.nvidia.com/cuda/archive/12.8.1/parallel-thread-execution/index.html).
Downloaded 2026-10-27; raw HTML SHA256:
`9f9c58a62e0963ee3e001132b986e1ae55af4ef1691812ff834a4a98512df2c8`.
The immutable receipt retains these bytes. Later manual revisions do not
silently broaden this inventory. Profile revision and reviewed evidence must
change together when facts or supported recipes change.

| IDs / exact architecture | Concrete variant spellings | Atom M,N,K | Participation / accumulator |
| --- | --- | --- | --- |
| 1,2 / `sm_90a` | `wgmma_m64n64k16_f16_ss`, `wgmma_m64n64k16_bf16_ss` | 64,64,16 | Four consecutive warps, warpgroup-aligned; 128 collective issuers. FP32 register fragment, 32 registers/thread. |
| 3,4 / `sm_100a` | `tcgen05_m128n64k16_f16_ss`, `tcgen05_m128n64k16_bf16_ss` | 128,64,16 | `cta_group::1`, one elected MMA issuer; four warps read result, one complete warp allocates/frees. FP32 TMEM fragment, 128 lanes x 64 columns. |

These four recipes are a deliberately bounded subset, not an exhaustive ISA
matrix. A/B both originate in SMEM; the result format is FP32. Other atoms,
register/TMEM A, sparse/WS/collector-reuse variants, CTA pairs, F32/TF32,
FP8/FP4 and SM120 are absent. Exact lookup never strips suffixes or falls
back from `sm_90`/`sm_100`/`sm_100f`/`sm_120a` to an `a` profile.

WGMMA's ISA floor is PTX 8.0 (section 9.7.15.5.2); tcgen05's is 8.6
(9.7.16.9.9.1). Profile toolkit floors are conservatively CUDA 12.0 and
12.8 respectively, not a claim of earliest unpublished toolchain support.
Encoded versions are PTX `100*major+minor` and toolkit
`1000*major+10*minor+patch`; evidence is 807/12081. Record fields state
minimum requirements, not a probe of the installed assembler. Assembler
compatibility/actual instruction evidence is a later gate.

Fragment mapping 1 references PTX 9.7.15.5.1.1's `m64nNk16` FP32 D register
fragment; mapping 2 references 9.7.16.9.5.4's M=128/cta_group::1 layout D
and 9.7.16.7.1's per-warp TMEM lane access restrictions. These IDs identify
concrete ISA maps. Bit-accurate WN construction and register/TMEM extraction
are not implemented by storing their IDs.

## Descriptor And Completion Obligations

Descriptor recipe 1 names the WGMMA no-swizzle shared-matrix descriptor;
recipe 2 names the distinct tcgen05 no-swizzle descriptor. Initial operand
major modes are K-major A and MN-major B. Do not interchange descriptor bit
formats: see 9.7.15.5.1.2 and 9.7.16.3.1. Addresses and leading/stride byte
offsets require 16-byte alignment and representable encoded ranges; tcgen05
also requires its fixed descriptor bits. Scale/transpose/type bits must match
the exact variant. Scalar row-major tile geometry alone does not prove the
required core-matrix layout. No descriptor bits or addresses are constructed
or certified by MMA1. Exact layout, offsets, alias ranges and alignment must
be verified before an MMA4 lowering can be admitted.

| Recipe | Mandatory symbolic requirements; every event ID is distinct |
| --- | --- |
| Hopper / 1 | Operand publication and generic-to-async proxy ordering; register-order `wgmma.fence`; issue, commit group, fully waited group 0; result read; complete reader join before overwrite. |
| SM100 / 2 | Same operand/proxy requirements; owned TMEM allocation; one MMA issue; `tcgen05.commit` completion mbarrier and wait, thread synchronization/`tcgen05.fence` requirements; result load and `tcgen05.wait::ld`; reader join, deallocation and allocation-permit release. |

See PTX 9.7.15.7 and 9.7.16.5/.6/.7/.10/.11. TMA-ready, MMA-done, result-read
and slot-reusable are separate requirements. Protocol rows are unresolved
requirements, not a dependency graph, uniform-participation proof or progress
proof. One outstanding completion window is this fully waited recipe's
restriction, not an asserted ISA queue capacity or a cost estimate.

TMEM allocation is in powers of two columns, 32 through 512, covering all
128 lanes; see 9.7.16.1 and .6. The initial D needs 64 columns. Allocation and
deallocation are warp collective. Complete all relevant accesses before
freeing; no allocation may follow permit relinquishment, and successive
allocation sizes cannot increase. This inventory does not derive SM
occupancy by dividing a per-CTA TMEM capacity. Scope is a single CTA with
one owned allocation; no circular-wait/progress guarantee is inferred.

## Runtime Records And Existing Services

`osprey/common/com/dsl_compute_realization.h/.cxx` owns policy-free exact
lookup, structural joins and `AI_COMPUTE 1` inspection. It references existing
`DSL_TILE_SITE_RECORD`, `DSL_TILE_PLAN_RECORD`, `DSL_OPT_PLAN_RECORD`,
`DSL_TENSOR_EVOLUTION_NODE_RECORD`, `DSL_NUMERICAL_CONTRACT_RECORD` and
`DSL_FETCH_RESOURCE_RECORD`. F16/BF16 append values 5/6 to the runtime
numerical-format enum, preserving values 1-4. The existing segmented
accumulation verifier/materializer retains its supported subset; it does not
become a typed fallback. No existing AI_ACC stream is changed.

Caller-owned links establish PU, source, region, semantic node/value, plan,
tile, representation and generation namespaces. Borrowed references remain
valid only during the call. Changing canonical source, geometry,
representation, numerical policy or target invalidates the view; caller bumps
generation and rebuilds all dependent records. Mismatched generations fail.
Source/region/acceptance identities must come from the owning phase's known
namespace; this utility does not traverse other PUs or create those identities.

The tile is a borrowed geometry view, not a newly registered scalar tile
plan. Its instruction family stays UNKNOWN because existing tile-family
verification is scalar-only; exact MMA identity belongs to the compute
profile. Admitting an executable MMA tile to shared tile services remains
an MMA4 integration requirement, rather than falsely labeling it scalar FMA.

Resource requirements reuse the existing RF/SMEM ledger plus a separate TMEM
extent/allocation record. This proves structural bounds only.
The caller supplies RF/SMEM budgets; common verification checks demand/budget
consistency without selecting a budget or proving configured availability.
The fixture reuses the prior conservative scalar envelope for inspection.
Native RF,
address/control temporaries, spills, configured carveout and actual ownership
are unknown. No Tensor Core timing or rate is supplied; inspection says
`unknown`. Existing scalar rates are not borrowed as Tensor Core rates.

The numerical reference preserves F16 or BF16 source/input, FP32 product,
partial/outer accumulator and result, with explicit reassociation and a
separate acceptance identity. No narrowing permission is inferred. ISA
accumulation order/rounding/subnormal freedoms require numerical acceptance;
a nonzero identity is a reference, not numerical certification. A future
phase must resolve and prove it. Fallback references the exact same numerical
contract and source/output formats. Its status is REQUIRED and plan ID zero:
MMA3 has not implemented it. Existing F32-buffer kernels cannot fulfill it.

Hardware capability is documented; lowering and native certification are
REQUIRED. Structurally valid records cannot claim either completed status,
registered cost, selected plan, or an executable fallback. Common code never
generates candidates, decides legality/profitability, mutates WHIRL or emits
CUDA. The new module is linked only into its focused inventory test until
a reviewed phase consumer exists. No backend library dependency is added.

## Executable Carrier Inventory: Still Open

| Required meaning | Existing service reused / missing work before MMA4 |
| --- | --- |
| Dense typed buffers and same-contract scalar computation | Existing symbol/type/descriptor and scalar loops; actual F16/BF16 representation, conversion, accumulation semantics and typed fallback remain MMA3. |
| Exact atom, participants, fragments, descriptor bits and target features | Existing tile/evolution/selected-kernel records provide identity. Reviewed executable MMA and fragment carriers, logical printing, structural/target verifiers and exact target emission are missing. |
| WGMMA issue/fence/commit/wait and operand access completion | Existing copy pipeline carries copy readiness only. Distinct executable compute operations and access joins must be added/verified; a comment or TMA wait is insufficient. |
| TMEM allocation, issue/result-read/fence/wait/free/permit | Existing RF/SMEM model cannot represent TMEM by renaming it. Typed TMEM carrier, lifetime/progress verification, extraction, emitter and launch obligations remain open. |
| Selected output and launch guards/fallback | Existing WHIRL image and driver publication remain authoritative. Add exact-target launch/guard/ABI support only after all typed compute/fallback operations are lowerable. |

No new opcode, mapped-image row, ELF section, symbol/type attribute encoding
or production option is introduced. Future carriers require WHIRL architectural,
binary compatibility, reader/writer, verifier and emitter review together.
`whirl2c -cuda` remains a consumer of selected/lowered WHIRL; there is no new
template selector or scalar-loop-to-MMA inference.

## Inspection And Acceptance

`AI_COMPUTE 1` is separate from `AI_OPT 7`/model6 and `AI_ACC 1`. It reports
exact profile/revision/evidence, participant/fragment and descriptor recipe,
joined identities, required events, RF/SMEM/TMEM requirements, unimplemented
typed fallback, unknown timing and absent lowerability/certification. Every
candidate row ends with the source line. Invalid input emits no stream.
The stream begins `# What it summarizes:`. There are no cost/select/action
rows because MMA1 does not execute P7/P8/P9 candidate selection or rewriting.

MMA2 must extend semantic/tile/residency/fetch-pipeline/physical-plan and
aggregate reports with the owning phase's actual analysis, uncertainty,
complete costs and unchanged SIMT shadow outcome. MMA4 action reports require
actual generated WHIRL. Neither a contract ID nor this test fixture counts
as a phase transformation.

Focused C++ tests use actual common headers/records and synthetic caller
identities. They cover all four profiles, exact-name/output-preserving lookup,
276 malformed cases and empty-output rejection. An independent Python reader
checks the printed inventory and malformed inspection streams. A linked
accumulation fixture proves typed inventory does not admit the F32-buffer
planner, preserves existing scalar materialization and independently reopens
its before/after binary images with `ir_b2a -st -src`. Authenticated M9 D/S1/T2
artifacts are reused with a fresh reader audit; they are never relabeled MMA.
Accepted evidence retains source snapshots, commands, raw logs, manual bytes,
hashes and receipts. No host/structural test certifies GPU numerical behavior
or performance. MMA2, MMA3, MMA4-H/B, MMA5-H/B, MMA6 and MMA7 remain open.

Final focused acceptance: all four Linux C++ profiles, 276 malformed contract
cases, seven independent malformed-stream audits, the linked Linux numerical
fixture (including both typed-planner rejections), and 109 host regression
tests pass. All 1384 M9 file hashes and fourteen cached readiness traces pass
fresh audit; current be.so/lw_inline hashes match that producer and their
symbol boundaries remain clean. No unrelated FHE producer is invalidated:
new inventory code is test-linked only, and existing format IDs, segmented
verification, driver options, lowering and image code are unchanged.

Broader Linux-image Python attempts retain one existing environment failure:
the image's Python 3.8 lacks `Path.is_relative_to` in `test_package`; the other
108 tests pass. The supported host Python 3.9 runs all 109 successfully. This
does not weaken the Linux C++/linked contract boundary and is not repaired
by an unrelated package change. Failed attempt logs remain in the receipt.

Reproduce within the supported Linux build (mount this source at both
`/open64` and `/repo/open64`, and the build at `/build`):

```sh
make -C /build/osprey/targdir/ir_tools -j4 dsl_compute_realization_contract_test dsl_accumulation_contract_test ir_b2a
/build/osprey/targdir/ir_tools/dsl_compute_realization_contract_test > /repo/open64/artifacts/ai_optimization/mma1/fresh-compute.log
python3 osprey/be/vho/tests/ai_matmul_cuda/test_accumulation_contract.py --build-dir /build --output-dir /repo/open64/artifacts/ai_optimization/mma1/fresh-accumulation
```

On the host, independently audit with a fresh output directory:

```sh
PYTHONDONTWRITEBYTECODE=1 python3 osprey/common/com/tests/dsl_compute_realization_review.py --inventory-log artifacts/ai_optimization/mma1/fresh-compute.log --legacy-receipt <authenticated-M9-receipt> --output artifacts/ai_optimization/mma1/fresh-review
```

The runner verifies every M9 file before reuse. It does not invoke a binary
producer or edit accepted artifacts. Commands assume the parent output
directory exists; runs never clean earlier accepted receipts.

## Accepted Local Checkpoint

Implementation commit `761bf24d` on `codex/ai-mma1-contracts`, based on merged
PR194 develop `40b07e25`. Approved common/R0, six-fixture and MMA design
commits were incorporated as `e11ffc68`, `e109116f`, `26fde777`; the occupancy
commits were already merged and were not replayed. No push or PR.

Primary immutable receipt (113 checksum-verified files):

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma1/compute-contracts/sha256-08313f5fa1742136596852e542d9fc471348fb667ef05533b788aca6123a520b/
```

Entry points: `summary.log`, `acceptance/compute.log`,
`acceptance/review-final/report.json`, `commands.json`, `evidence/`,
`source/`, `binaries/` and `receipt.json`. The fresh linked scalar fixture
retains `acceptance/accumulation/accumulation.before.B/.T`,
`accumulation.after.B/.T`, the full `accumulation.before-after.diff` and
WHIRL-derived CUDA under `before/` and `after/`. It exercises the established
F32 segmentation adapter, not MMA or a new F16/BF16 transformation.
`acceptance/review-final/cached-simt/` retains authenticated D/S1/T2 evidence
with its M9 parent receipt reference; those are cached producer artifacts.

Independent post-promotion receipt:

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma1/reopen/sha256-75206d07f4b9629da92d6148ea563f37c32438bbce6a00dfd8abf3eab98d7b0d/
```

From the read-only durable receipt, the copied Linux contract binary replays
byte-for-byte, two separate `ir_b2a -st -src` processes reopen the scalar
before/after images with identical traces, and the independent reader's
summary matches. The reader uses the retained MMA1 source plus unchanged,
authenticated M9 helper sources. Source interleave uses the retained source
mounted at `/open64`. Both receipts are indexed under the primary review
root. The no-tab scan and `git diff --check` pass.
