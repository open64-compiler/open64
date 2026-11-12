# Quantized and paged MLA ingestion plan v1.1

November 9, 2026 | Q1 native source ingestion and inferred traits delivered; High-provider exit pending

This frontend plan follows [WHIRL DSL infrastructure](../../doc/WHIRL-DSL-INFRASTRUCTURE.md)
and its native-builder ownership rules. The Q0 requirements supplement that
infrastructure in [native contracts v1.0](../../doc/AI-COMPILER-MLA-Q0-NATIVE-CONTRACTS-v1.0.md);
the [serving contract](../../doc/AI-COMPILER-MLA-Q0-SERVING-CONTRACT-v1.0.md)
and [audited census](../../doc/AI-COMPILER-MLA-Q0-OPERATOR-CENSUS-v1.0.md)
complete this bounded interface definition. Those documents define proposed
schemas; only native common/com review and implementation can admit them.

## Q1 native source implementation update

Adopt [Q1 native ingestion v1.0](../../doc/AI-COMPILER-MLA-Q1-NATIVE-INGESTION-v1.0.md)
and [plan v1.14](../../doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.14.md).
All ten source cases now produce actual native .B/.T with thirteen new/versioned
common source schemas. The source frontend's native C++ query derives latent /
rotary content and conservative decoder encodings from QDQ/contraction uses,
paired page gathers and successor byte/scale SSA. These facts are retained in
canonical descriptors and ST metadata and independently rederived after mapped
reopening. No class/argument name, dtype alone or serving phase label proves a
role. Generic decoder facts do not assert an external producer's RNE algorithm.

The source mirror and opaque native builder preflight agree on exact shapes,
carriers and attributes. Complete nonescaping nibble patterns are canonicalized;
general truncating/wrapping casts and unmatched bit operations refuse. Fake
quantization remains float storage. Unknown joins remain unknown, and a stale
scale-update join or contradictory declaration refuses before PU publication.

The original Q0 census/golden/CPU oracle remain drift detectors. Its current
source probe now reports the published source contracts as supported; immutable
Q0 receipts still record their historical refusal at the Q0 producer commit.
The source-only Q0 capture runner still does not write quantized .B. Use the
new native runner for native certification:

```sh
python3 -m unittest test_mla_q1_native_optional
python3 mla_q1_native_review.py --artifact-dir /out/mla-q1 --ir-b2a /tools/ir_b2a --observer /tools/tensor_kernel_lower
```

Each producer, reader and q1-inspect observation owns a fresh process. The
runner checks six results from independently reopened operations against eager
source, while retaining Q0's independent expanded reference. Its interpretation
is numerical ingestion evidence, not compiled quantized execution. Guard words
are explicit preconditions; production guards/allocator dispatch remain open.
No optimized candidate or materializer is enabled, and old FP32 fallback still
works. Raw packed data cannot enter the FP32 family.

The requested native-ingestion/source-trait boundary is delivered. The broader
Q1 High-provider exit is still open: straightforward semantic lowering of the
new operators to canonical High DO_LOOP/OPR_ARRAY and owned access/version facts
must precede Q3/Q4's loop binding. Q5 remains the selected synchronous optimization
rule, not a substitute for that unoptimized algorithm boundary. Q2 phase facts
can progress independently from this source boundary. Q3 engine state/guards,
Q5 execution and Q6/Q7 async/hardware acceptance remain unimplemented.

## Preserved Q0 source baseline and evidence (historical)

`python/tests/models/mla_quantized_paged.py` contains ten deterministic actual
PyTorch CPU sources: dense S8 decode/prefill-tail/prefill, dense packed-U4 decode,
paged S8 decode/U4 prefill, functional S8 decode/prefill and U4 verification
append, and dense float fake quantization. No cache trait annotation conceals
source arithmetic. Q is explicit; nonmonotonic page mapping, shared prefix,
private updates, masks and partial pages appear in the programs/data.

`mla_q0_capture.py` captures FX and normalized torch.export ATen graphs, exact
arguments, shape/dtype metadata, per-node classifications and original .pt2
programs. It runs an independent scalar F32 quantizer/page oracle followed by
expanded FP64 K/V attention (`mla_quantized_reference.py` plus the existing
`mla_reference.py`), retains values, and reloads saved ATen programs in another
process. The graph census golden is a drift detector, never a reason to accept
new operators silently. Keep this lane after Q1 becomes native-capable.

`mla_q0_contract.py` is a review-only exact-schema constructor/validator, not a
production admission hook or alternate tensor type system. Actual per-case
records declare no native/TMA/inference capability and no GPU alignment.
`test_mla_q0_capture_optional.py` covers numerical parity, state/byte/phase
distinctions, shared-write/schema refusal, immutable inputs, receipt replay
and no-replace publication. Unknown census targets fail closed.

All ten production native/mock ingestions refuse. Source .pt2 is a Torch
program, not .B. Q0 cannot retain a native quantized original that cannot yet
be constructed. This is an explicit Q0 acceptance qualification of the earlier
plan's B/T requirement: it is deferred to Q1/Q3, not satisfied with text or a
fake image. Fresh FP32 short/long baseline .B/.T prove the existing path remains
usable after refusals. Native producers run one per process, independently
from the reader, because the multi-export diagnostic failed for long context.
No compiler/global-state repair is part of Q0.

## Original Q1 work and ordering (native source items now delivered)

1. Review/implement native common schemas in the contract table, retaining
   old version meanings and appending logical IDs only after review. Add
   constructors, native predicates, descriptors, shape/effect rules, printer,
   mapped writer/reader and reopened gatekeeper tests together. Use int8/uint8
   descriptor encoding and promoted WHIRL arithmetic; do not confuse byte
   storage with MTYPE_I8/U8. Check numerical finite/range/scale assumptions.
2. Admit opaque native API construction only after whole-graph preflight.
   Prove complete pack/unpack patterns; preserve unmatched bit operations as
   unsupported. Treat functional scatter.v2 separately from modifying v1.
   Tensor values, scale versions and page/workload joins remain explicit.
3. Derive undeclared latent/rotary cache roles and encoding facts from bounded
   producer/consumer dataflow. No model-name/dtype-only proof. Retain provenance,
   unknown and contradictions; declarations cannot override conflicting facts.
   Fake quantization retains float byte size. Runtime request/page/scale values
   remain values/invocation data, not type-equivalence keys.
4. Emit genuine native source .B and side data, independently ir_b2a -st -src
   and gatekeeper-reopen it; retain source at its DST path and compare all
   exposed records. Test stale scale/version, ambiguous producers/aliases,
   undeclared/declared facts, unsupported rounding/conversion and atomic refusal.
   Preserve all existing FP32 public APIs and fallback tests.
5. Lower admitted semantics through VHO to canonical High DO_LOOP/OPR_ARRAY,
   preserving quantization and indirect page effects. Q2 phase facts and Q3
   serving guards must be qualified before optimized paged eligibility. Q5
   adds synchronous selected quantized rules, Q6 optional TMA. Q0 contracts alone
   never enable existing FP32 kernels for raw packed data.

No frontend code constructs WN/ST/TY/DST layouts. The middle end consumes binary
WHIRL without Python. Reuse common descriptor/effect/shape/memory services and
existing owned High snapshot/access/version facts; native inference belongs to
owning passes. All personnel roles remain TBD, so independent transfer is open.

## Reproduce and inspect

Use Python 3.8 / PyTorch 2.4.1+cpu (the pinned Linux AMD64 torch image) with the
tests directory on PYTHONPATH. Select a fresh host bind-mounted destination:

```sh
python3 -m unittest test_mla_q0_capture_optional
python3 mla_q0_capture.py --artifact-dir /out/mla-q0 --native-refusals --native-tools /tools
python3 mla_q0_capture.py --replay /out/mla-q0
```

The native options require the unchanged qualified native frontend package and
ir_b2a/runtime libraries; omit them for source-only exploration and label the
result accordingly. `OPEN64_DSL_TEST_ARTIFACT_DIR` selects an alternative root
with child mla-q0. Existing output destinations refuse. Per-case fx.graph.txt,
aten.graph.txt and census JSON explain the source; values.json and serving.v1.json
bind correctness and invocation facts; ingestion-refusal.json records actual
first blockers. report.json and SHA256SUMS bind the complete receipt.

The final Q0 Linux lane passes all twelve focused tests and 139 torch tests;
the full Python lane has one unchanged FHE authority-digest failure among 341
tests (27 skipped). Keep that qualification with the evidence. Fresh FP32 .B
images are fallback/bridge evidence only; neither native quantized correctness
nor GPU performance is certified. Capturing/referring programs is development
outside candidate exploration. Two-phase search still estimates without native
mutation and lowers/checks only its eligible selected plan.
