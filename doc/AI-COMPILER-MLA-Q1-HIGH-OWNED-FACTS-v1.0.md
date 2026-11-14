# Q1 canonical High lowering and owned cache facts v1.0

2026-11-12 architecture terminology correction:
[native WHIRL contract](AI-COMPILER-WHIRL-OPCODE-AND-TENSOR-TRAIT-CONTRACT-v1.0.md)
governs opcode properties, tensor traits, phase legality and scheduling
services. Embedded earlier revisions include these editorial corrections;
historical implementation status and acceptance evidence retain their original
scope.

November 10, 2026. Extends [Q1 native ingestion v1.0](AI-COMPILER-MLA-Q1-NATIVE-INGESTION-v1.0.md)
and [plan v1.14](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.14.md).
Earlier documents and immutable source receipts remain historical evidence.

## Scope and architecture

The opt-in `q1-prepare` / `q1-high` route expands all ten admitted Q0/Q1
quantized, paged, functional-append and fake-quantized source programs into a
straightforward common High algorithm. This is required semantic expansion at
O0, with ordered F32 reductions and signed zero-based unit DO_LOOP / OPR_ARRAY
indexing. It performs no fusion, tiling, vectorization, cross-head reuse,
reassociation, GPU mapping, candidate search or runtime compilation.

Very High domain operators retain their frontend/native gatekeeper boundary.
The common High algorithm exposes byte loads, conversions, encoder arithmetic,
index guards, gathers, fresh functional updates and ordered attention folds.
Mid WHIRL structured memory lowering, Low WHIRL GPU mapping and PTX emission
remain later phases. WHIRL opcodes define operation semantics; tensor
descriptors and tensor traits carry semantic tensor properties. Existing
compiler metadata retains provenance and phase information separately.

VHO owns semantic construction in the driver's active empty host PU. LNO owns
the read-only copied fact provider at the resulting High boundary. The standalone
driver alone selects source/target PUs, creates the explicit buffer ABI, writes
checkpoints and selects the observer lifetime. Neither service traverses another
PU. No native handle escapes into detached exploration.

The original FP32 `prepare` / `high`, source schemas and production fallback
remain unchanged. New quantized source registry entries retain marker-only
capability for that old route. This milestone provides an explicitly requested
semantic tool path, not automatic `openpy`/`be_driver` quantized dispatch. The
new fact service is built into LNO and exercised on independently reopened High
images; the legacy full-affine-dependence admission is not broadened to admit
indirect or guarded accesses as affine. A future production Q3 binding must
select this qualified form and preserve its obligations explicitly.

## Admission, lowering and publication order

1. Read one genuine source .B in a fresh process. Run the native gatekeeper and
   independently rederive the source cache/scale associations before capture.
2. Copy the source operations, native versions/attributes, scalar encodings,
   shapes, reshape storage aliases and cache-role/scale SSA metadata. Native
   WN/ST/TY/DST ownership remains in C++; Python constructs no native records.
3. Publish the separately versioned
   `open64.quantized-common-high.recipe.v1` semantic handoff. Revalidate its
   bounded SSA, shapes, types, exact attribute schemas, source mapping, storage
   and scale references before target initialization. Unknown fields, versions,
   schedules and inconsistent aliases refuse. Existing recipe versions 1–8 are
   unchanged. There is no launch geometry or selected-plan authority here.
4. In a new process, the driver creates an empty host PU. Strict FP controls,
   valid source mapping and unscheduled O0 semantics must pass before allocation.
   Invalid context, unsafe math and nonempty-target refusals leave tables intact.
5. Retain the immediate empty-target before.B. Materialize the common High
   algorithm, verify no native DSL operation remains, then publish after.B and
   the disjoint-buffer/status manifest. The original source is never rewritten.
   A recoverable refusal publishes no after image or manifest. An exceptional
   construction failure discards the fresh target process, not a live source PU.
6. Independently reopen both images with `ir_b2a -st -src`, retain the raw diff,
   and collect owned facts before ARRAY lowering. Comments describe the actual
   scalar algorithm and runtime checks. Existing selected-plan COMMENT/PREOPT
   workarounds remain unchanged; general PREOPT COMMENT repair is separate.
   Always give whirl2c a distinct `-ft,<case>/w2c.phase.t` path: its default
   `.t` would collide with the retained `.T` on case-insensitive host mounts.
   The review asserts the final after.T still contains DO_LOOP/ARRAY/COMMENT.
7. Estimation-only exploration consumes qualified copied facts in a future
   binding; it must not invoke these producers, a compiler, profiler or oracle.
   Only a qualified selected optimized configuration reaches transformation and
   regression tests. Q1 qualification tests occur outside exploration.

No mapped-image layout, ELF section, physical opcode, type-kind or reader/writer
format changes. The exact native-tree identity helper additionally recognizes
TRUNC, SHL and LSHR using its existing typed/payload-aware key machinery.

## Scalar semantics and guard ABI

Each unique logical allocation has one caller-provided disjoint buffer formal;
reshape views share their root buffer. Inputs remain immutable. Scalar pointer
metadata preserves cache role, encoding, original value, scale SSA and source
evidence; scale names do not enter pointer type identity. Shapes/extents and
actual element sizes remain explicit in ARRAY nodes and the semantic manifest.
The caller must honor this separate-buffer ABI; ordinary serving pool aliases
or in-place updates cannot be passed to it without an adapter and proof.

The final argument is a separate disjoint int32 status location: 0 succeeds,
1 refuses the runtime input. Scratch can contain partial work on refusal; the
caller discards every result. This is a semantic failure ABI, not a complete
serving fallback dispatcher or an allocator publication protocol.

| Source contract | High expansion and checks |
| --- | --- |
| int8 / uint8 storage | I1/U1 memory descriptors and byte-size ARRAY; ILOAD sign/zero extension into I4/U4 arithmetic. I8 still means int64 |
| abs / clamp | F32 abs and comparisons/selects with exact finite bound bits; retain source order |
| round_even | Explicit binary32 ties-to-even independent of integer-conversion rounding mode. Only abs(x)<2^23 enters safe truncation; larger finite F32 values are integral. Preserve signed zero. Nonfinite encoder ratios refuse |
| checked_integral conversion | Check range and ordered comparisons before truncation, then exact integral equality; no overflow, wrapping or NaN-to-integer conversion |
| U4 packing | Even low nibble, odd high nibble; guard each code <=15; promoted shifts/bit operations; exact byte output |
| U4 unpacking | Unsigned shifts and mask, all 16 codes valid; offset8 subtraction/scaling remain original F32 operations |
| int64 page arithmetic | Check nonnegative inputs and positive divisor. Check add/multiply overflow before the operation. Floor DIV/REM is exact for this nonnegative domain |
| gather | Bounds guard before loading the selected row/column; load-derived scalar coordinates remain explicitly indirect |
| functional scatter.v2 | Fresh result, copy untouched rows, scan matching updates, reject out-of-range or repeated targets, branch before a potentially negative update-row load; never mutate input pools |
| matmul / softmax / weighted latent | Existing ordered source expansion, including masked-head output zero and LSE negative infinity; no newly permitted F32 reassociation |

The scatter scan is intentionally a simple semantic baseline and can be costly.
It is not the selected quantized optimization or a claimed efficient kernel.
Decoder scale positivity/calibration, allocator alignment/generation and serving
phase are not inferred from dtype. Q2/Q3 retain those separate obligations.

## Owned access, scale-version and lifetime facts

`LNO_DSL_capture_quantized_high` consumes only the driver-selected FUNC_ENTRY,
the admitted semantic handoff and a live existing `DSL_HIGH_EPOCH`. Its atomic
output contains copied strings, numbers and vectors; refusal clears the output.
`LNO_DSL_revalidate_quantized_high` first checks the epoch and then recaptures
actual native content. No WN, ST, TY, map, manager, pool or runtime address is
stored. Collection neither allocates native compiler products nor changes IR.

The provider reuses Open64's `WN_LOOP_LowerBound`, `WN_LOOP_UpperBound` and
`WN_LOOP_Increment` accessors to verify canonical signed static unit loops.
Loop and memory IDs are stable lexical tree paths. Full native identity includes
opcode/result/descriptor types, memory offsets, fields, pointer types/qualifiers,
constants, structured control and comments. A partial fingerprint is never an
application certificate.

For each root it copies shape, dense strides, physical payload bytes, formal
identity, local version, declared encoding/role, immutable-input/result status,
lexical first/last memory access, and gather-copy/functional-successor origin.
Reshape values resolve to the same root. Fresh results receive a new local
version tied to their producer operation, not the incoming version. Scale SSA
resolves independently to its native storage and local version. Matching page
indices and functional-update versions are checked; an encoder's actual source
scale operand joins its produced byte state. Functional successors also check
the update encoder/scale pair. Returned cache copies join the separately returned
scale copies, retaining both new local versions and their source origins. A
`scale_pair_ready_group` records when both producers have completed; this is
not atomic serving publication. Missing or incompatible source,
scale, formal/type, extent or alias evidence refuses rather than filling gaps.

Each access copies its read/write identity, operation group, loop nesting,
physical element type/bytes, full address key, conditional status and local
storage version. Data/control provenance through local scalar temporaries
conservatively marks indirect page or update coordinates. Loading an index into
a scalar does not make it affine. Unknown cross-access dependence, GPU cacheline
transactions, cache residency and cross-head reuse legality remain unproved.

The disjointness fact is an explicit buffer-ABI precondition, checked in native
formal qualifiers; it is not an observation of runtime addresses or a fabricated
alias-manager result. Removing the qualifier refuses. Local scratch is fresh by
ABI and source construction; serving-cache unique ownership is not inferred.

Lifetime endpoints are lexical within this invocation. Inputs and returned
state can outlive the observed scope; this provider does not authorize freeing,
recycling, in-place append or publishing them. It does not invent engine cache
generations, inter-call persistence, concurrent-reader events or serving phase.
Epoch mutation invalidates every access/scale/lifetime join together. Rebuild
rederives them; epoch close, another same-name activation, changed source/scale
bindings or native edits invalidate the old facts. Owned records remain readable
after expiry while losing all live-analysis authority.

The observer requires the producer's operation explanations to map groups.
Missing/dropped comments therefore refuse safely. Post-PREOPT Q1 fact rebinding
needs a separately qualified persistent join; it cannot reuse an expired
pre-PREOPT observation or bypass the existing full-dependence admission.

## Focused validation and transfer package

`osprey/be/lno/tests/run_quantized_high_review.py` reuses authenticated Q1 source
.B and input payloads, then produces fresh High before/after B/T and raw diffs.
It independently reopens the High image for owned facts, emits ordinary C through
whirl2c, compiles that exact C with strict F32 settings and executes it. No
handwritten kernel or source-graph interpreter substitutes for the lowered code.

The ten cases cover dense S8 decode/prefill-tail/prefill, dense U4 decode, paged
S8 decode/U4 prefill, fresh S8 decode/prefill and U4 append/verification, and fake
quantization. All six results are compared with saved eager source and the
independent scalar-quantizer/page-walk/expanded-FP64 reference. Attention/LSE use
rtol 2e-5 and atol 2e-6; returned bytes and scales use exact equality. Canaries
and immutable inputs are checked for successful and refused executions.

Additional checks cover signed -128, all U4 nibble codes, exact positive/negative
half ties at scale=1, nonfinite encoder input, negative/out-of-range pages,
int64 overflow and duplicate writes. Owned-fact checks cover epoch/activation
isolation, rebuild, scale-binding change, close/expiry, native loop corruption,
unknown alias qualifiers and restoration. Corruption-only tests retain separate
before/after native artifacts and never execute the deliberately damaged image.

Qualification on November 10 passed all ten compiled High cases and all sixty
returned results. The independently reopened records contain 767 storage roots,
1761 static memory sites and 37 conservatively indirect sites in aggregate.
Both returned cache/scale pairs are joined for every case. Seven malformed
semantic handoffs refuse without publishing checkpoints or a manifest.

The seven impact-selected legacy/native/search tests pass, including genuine
selected short/long WHIRL-to-CUDA host simulation and estimation isolation.
Separate current-source standalone controls pass 37 search assertions and 144
movement assertions. The rebuilt backend processes two legacy High programs at
O3 and two Q1 High programs at O0/O3 (four Q1 form checks); these backend checks
are compilation/form evidence, not post-LNO numerical or GPU execution.

Legacy recipes, complete ST/TY/source-interleaved dumps and typed executable
keys match the retained baseline exactly. Whole binary images differ in raw
mapped-table bytes even when rerunning the unchanged retained producer; preserve
the old-tool control images and empty textual diffs rather than claiming whole
binary byte determinism. Backend NEEDED libraries and undefined symbols are
unchanged. Shared transient value-layout dependencies now rebuild the owned
reuse provider and movement-test object together, preventing stale ABI mixtures.
This is focused qualification, not a claim that an entire repository suite ran.

Retain exact source/tools/options, old source receipt identity, numerical
payloads, generated C, compilation logs, owned facts, raw diffs, compatibility
and search-isolation results in a new immutable content-addressed review packet.
Promote without replacement and replay from its final path before indexing it.
The packet's README records measured counts, commit and final commands.

## Remaining gates and risks

- Automatic production quantized routing and post-PREOPT owned rebinding remain
  open; legacy fallback and full dependence admission are preserved.
- Q2 phase detection, Q3 serving guards/allocator contracts, Q4 movement costing,
  Q5 selected synchronous quantized optimization, Q6 TMA/dequantization and Q7
  device correctness/performance remain separate milestones.
- Guarded O0 scalar execution proves this bounded semantic baseline. It does not
  prove a reordered, grouped or asynchronous version legal or faster.
- Fresh native producer processes remain required; same-process multi-export
  lifetime robustness is not claimed fixed.
- No NVIDIA GPU is available. Host and compiler evidence cannot certify Hopper
  correctness, bandwidth, launch overhead or performance.
- Primary owner, successor, independent reviewer and approving lead remain TBD;
  independent personnel transfer acceptance remains open.
