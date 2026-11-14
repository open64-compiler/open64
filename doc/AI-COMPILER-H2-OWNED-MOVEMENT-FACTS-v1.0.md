# H2 owned data movement and reuse facts v1.0

2026-11-12. This specification implements the owned-fact portion of H2 over
existing admitted GEMM, MLA, LayerNorm and Reduction owners. It preserves the
WHIRL opcode/tensor-trait architecture, numerical contracts and selected-only
two-phase search. It adds no algorithm, native tensor type, mapped layout,
operation interface, runtime ABI field or library dependency.

## Authority and phase placement

Very High WHIRL carries domain operations. High WHIRL carries common algorithms,
DO_LOOP/OPR_ARRAY indexing, Open64 dependence analysis and scheduling. Structured
memory lowering follows at Mid WHIRL, GPU mapping at Low WHIRL and PTX emission
at the backend. H2 records describe already admitted source/contract evidence;
they do not move these boundaries or grant a new canonical lowering route.

| Existing owner | Captured scope | Admission and recapture |
| --- | --- | --- |
| VHO common GEMM/locality | F32 nontransposed A[i,k], B[k,j], C[i,j], exact dimensions, storage and layout/alias proof | Active PU/site/managed operand joins at O3; capture before joint search, fresh capture before fixed-pair emission |
| LNO common MLA | MR1 latent/rotary cache accesses and versions; other source storage is inventory | Current canonical High snapshot, source key, owned-memory key, dependence/reuse proof and activation/mutation epoch; fresh selected preflight |
| WOPT LayerNorm LN2 | Centered, ordered LN-REF-1; X/gamma/beta/Y ranges and leases; C1 retained X and C2 X/d shared-row generations and mu/q slots | Existing native source/target/plan binder; no tree/epoch authority retained; fresh bind for every capture |
| WOPT Reduction R2 | Ordered F32 direct fold; staged row/chunk/within-chunk work, byte stride, capacity, active tails and slot generations | Existing direct/staged binder, same-thread copy/consumer/release event and next-generation proof |
| Q3 serving registry | Registered allocation/view/version/scale/page/index/publication joins under a prepared ticket | Freeze costing, then registry lock and current-ticket/device-lease checks; revalidation reacquires actual owner facts |
| Selected Q6 | Raw slots and decoded tile, exact padding/ranges, waits, publication, all-head consumption and release | Already selected qualified mapping plus full verified action trace; no candidate staging expansion |

General LNO executable admission/materialization for all four families is not
introduced by these projections. The original H2 canonical-route preparation
remains a separate open gate for unsupported forms; future H4-H6 materializers
must explicitly admit those algorithms. A successful R2/LN2 binding is still
conditional on its existing runtime obligations. A reuse opportunity is never
an executable optimization certificate.

## Owned records and identity

`DSL_MOVEMENT_FACTS` owns a validated copy of the capture. No WN, ST, TY,
analysis-map, runtime-address, request or registry pointer survives in it.
Work Partition records explicit axes/extents, owners, instances, reduction or
tile width, active/inactive tail and numerical permissions. Storage records
carry canonical allocation/view identities, byte range/capacity/alignment,
encoding, shape, byte strides, version, alias proof and lifetime boundary labels.
Access records retain exact storage/version joins, relation, dynamic source
byte count when known, multiplicity and separately proved reuse scope.

Bindings include the complete source/proof/control identity and immutable H1
GPU snapshot: exact device variant, resource/partition, execution/profile,
model/toolchain and workload-budget dependencies. Runtime Q3 projections retain
the H1d owner sidecar key without inventing a numeric device profile. No-import
architecture-only behavior remains supported. Matching `sm_90`, shapes or nominal
memory capacity never equates H20 96 GB, H20 141 GB or H200.

Full framed content comparison is authoritative for recapture equality; SHA-256
is a compact diagnostic/cache key. Existing selected scalar native keys retain
their original spelling. An explicit source-only native-key mode additionally
copies existing logical opcodes and tensor attributes through public readers;
it does not authorize scalar emission of an unlowered domain operation.

LNO facts retain known activation/mutation epochs. VHO, WOPT and serving facts
without a supplied compiler epoch deliberately cannot pass `Is_current`.
Their callers must recapture and compare complete content. Owned data remain
readable after native/registry cleanup, but do not retain a live ticket, lease,
CUDA completion, application authorization or analysis validity.

## Byte ledger and evidence meaning

All quantities are bytes with explicit known/unknown state. Unknown differs
from proved zero. Useful unique operand footprint, canonical source reads and
writes, selected recipe requests, physical transactions, L2, HBM, shared-memory
requests, spills, scale/page/index payload and partial results are separate.
H2 refuses supplied physical transaction/L2/HBM/spill counts: these source
projections cannot establish bus service, cache hits, register allocation or
stall percentages. H3 models and H7 measurements must remain separately labeled.

For GEMM M×K times K×N in F32:

- Useful input payload = 4(MK + KN); output = 4MN.
- Canonical scalar source reads = 4MKN + 4MKN; source output writes = 4MN.
- A reuse across N output columns and B reuse across M output rows require
  existing unique-storage and known-layout proof. Unknown layout gives no credit.
- Tile/TMA transactions, cache service and realized reuse are not source facts.

For LN rows R and width N, useful input = 4RN + 8N, source reads =
12RN + 8RN, and output writes = 4RN. These describe three X sweeps and
per-row gamma/beta accesses in the ordered centered reference. C1/C2 requests
may differ; they are not substituted into these source counts. C2 records two
versions at one physical row allocation. LN2 retains its per-element overwrite
proof: H2 does not assert an extra all-X-read barrier before d writes.

For R2 rows R, width N, source input = 4RN and output = 4R. The admitted
same-thread staged recipe separately records 8RN shared copy/read request
bytes, exact scratch row stride and tail. The per-row logical accumulator is
recorded without asserting its physical register placement or absence of spills. It does not claim physical SMEM
bandwidth or improved throughput. No partial-result buffer is required there.

MLA's MR1 ledger covers cache operands only, including source materialization
writes where present; it is not whole-invocation traffic. Q3 inventory counts
payload/scale/page/index extents, with dynamic accesses explicitly unknown.
Selected Q6 records three ordered passes, raw request bytes across its actual
grid, and exact scale bytes. Padding is storage, not a padded global read.

## Events and lifetime validation

The bounded owned event graph validates exact storage/version and participant
joins, acyclicity, readiness before consumption, readers-complete before reuse,
and explicit next generations. Decoded buffers require raw-ready → decoded-ready
→ consumer → readers-complete, followed by reuse when present. Raw arrival alone
never grants decoded readiness or safe overwrite. Checked arithmetic refuses
sentinel/overflow and out-of-capacity views or byte strides.

Q3 cache/scale/page versions and publication lifetimes remain controlled by Q3;
role names or source annotations alone grant no live lease. Identical scale
bytes after a cooperative write still advance the content version. Changed page
contents, cache generations, budget generations, expired/cancelled/completed
tickets and changed views invalidate revalidation.

Q6's complete existing action trace is verified before projection. The owned
DAG retains a representative prefix of up to three tiles in pass zero, joined
to the full mapping/trace identity. This bounds the generic graph to 64 events
without pretending the prefix proves all generations. Raw wait, dequantization,
decoded publication, all-head consumption and release remain distinct. Additional
boundary labels describe existing actions and insert no instruction/barrier.

## Search, application and fallback

1. The owning phase admits source and captures owned facts outside candidate
   exploration, under existing scope/options/optimization-level controls.
2. Phase one explores estimates over detached input only. No WN/ST/TY creation,
   native transformation, probe, compilation, reference execution or profiling
   occurs in its candidate loop. H2 itself changes no ranking/model or winner.
3. Before phase two, recapture actual native source/proof/profile and require
   equality/current epoch where available. Stale selected facts decline to the
   existing family fallback. Native application retains existing atomicity,
   rollback, invalidation, analysis rebuild and exit checks.
4. Only the selected plan follows structured memory lowering, GPU mapping,
   whirl2c/CUDA and PTX. Existing COMMENT explanations and PREOPT workaround
   remain unchanged. H2 diagnostics use `AI_MOVEMENT_*`, not invented COMMENT
   transformations. No new transformation means no new transformation comment.

Missing optional H2 facts preserve the previous admitted planner/fallback and
provide no new reuse or physical-service credit. Missing required family proofs
still refuse through the existing binder. Unknown physical service never means
zero cost, and a retained owned snapshot never substitutes for live authority.

## Validation and transfer

The focused suite uses real native mapped inputs authenticated by prior
receipts, separate-process `ir_b2a -st -src`, exact-stem B/T pairs and raw full
no-op diffs. Current captures must preserve native tables/analysis images and
selected native output. Tests cover detached lifetime, same-shaped distinct
allocation, offsets/strides, epochs, unknown aliases, source/control/profile
mismatch, scale/page/budget generations, raw/decoded/readers ordering, generation
reuse, tails, overflow, lower-level refusal and existing ordinary fallback.

Reference execution is separate from exploration: selected short/long MLA
native references and quantized serving callbacks remain required. Shared
library dependency and linker-closure checks protect frontend/backend isolation.
Synthetic profiles validate both H20 variants but establish no physical hardware
specification, GPU execution, race-check result, bandwidth result or speedup.

Primary owner, successor, independent reviewer and approving lead remain TBD.
Unassigned roles block formal knowledge-transfer acceptance. Durable local
receipt location: `artifacts/review/ai-optimization/research/h2-movement-facts/`;
accepted reports bind exact source/producer hashes, commands, original inputs,
B/T diffs, test scope and independent final reopen. Earlier receipts/documents
are preserved.
