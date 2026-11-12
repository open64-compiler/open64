# Automatic explanations carried by WHIRL COMMENT

Version v0.2, November 5 2026. This extends the preserved
[readable selected-schedule view](AI-COMPILER-TENSOR-SCHEDULE-SOURCE-v0.1.md)
and implements the generated-source explanation rule in AGENTS.md.

## Result and ownership

The explanation is generated once from the selected VHO schedule. The
lowering pipeline inserts that text into ordinary WHIRL `COMMENT` statements.
Normal `whirl2c -cuda` emits those nodes through its existing `WN2C_comment`
handler. The readable tensor-level source printer consumes the same detached
records and prints identical comment payloads. Neither emitter chooses an
optimization or independently reconstructs the explanation.

`be/vho/tensor_kernel_explain.cxx/.h` owns deterministic descriptions of
already-selected facts. It uses no model names, hardcoded context lengths,
chosen chunk sizes, runtime execution evidence, or live PU traversal. Guard
descriptions recognize exact source predicates/constants solely to explain
their existing meaning; they do not select or rewrite a guard.

`VHO_tensor_annotate_kernel` changes only the driver-selected active kernel PU.
The driver creates/materializes the PUs, writes checkpoints, invokes annotation,
and owns all scope changes. No common/com optimization policy or frontend
dependency is added. The focused standalone tool links the explanation service;
the backend shared library and normal CUDA emitter remain unchanged.

## Generated content

Comments are emitted automatically and describe:

- Runtime input/result names, logical tensor shapes, selected stage order,
  materialized buffers and their byte counts.
- Fused producer semantics, lazy reshape/transpose, exact F32 constants,
  masking/select predicates, finite-value and safe-divisor guards, and
  recomputation across stages.
- Contraction/reduction widths and retained materialization boundaries.
- Split intervals, chunk counts, tail extents, unique work-item ownership,
  chunk-major scratch indexing, neutral values, ordered merges, and serial
  inner loops.
- Dependencies on completed global maximum/sum merges. Each current chunk
  consumes the same finalized dependency, preserving the selected global
  normalization.
- The selected launch dimensions, explicit reassociation policy, and the
  distinction between serial review execution and mapped device kernels.

Only facts present in the source recipe and selected plan are described.
Compilation/host checking is not reported as GPU execution or speedup. The
normalization and masking arithmetic remains exactly the selected source
arithmetic. The user-facing readable files retain their nine short or twelve
long stages, now with comments emitted directly during regeneration.

## Native representation and compatibility

WHIRL.pdf page 23 defines COMMENT as a nonexecuting statement valid from VH
through VL. Its `st_idx` identifies a CLASS_NAME symbol whose name contains
the text. The implementation uses the existing `WN_CreateComment`, standard
string/symbol tables, and normal mapped-image writer/reader. No opcode, kid
layout, type kind, ELF section, or binary format version changes.

The overview and allocation descriptions belong to the first selected kernel;
each stage and its fused producers belong to that stage's kernel. Their
ordered COMMENT statements precede the existing executable body. Statements
and comment symbols receive source positions from the kernel body. No
executable statement, loop bound, argument binding, or launch decision changes.

The optional kernel metadata `tensor.kernel.explanations_version=1` and
`tensor.kernel.explanations_count` identify annotated products. A reopened
annotated kernel must contain the exact ordered canonical explanation records.
The body fingerprint includes COMMENT text as well as structure, so changed or
missing explanations fail auditing. Legacy images without this metadata retain
their existing audit path. Unknown explanation versions are rejected. An old
annotated receipt should be reopened with its retained producer/auditor when
the canonical explanation contract changes.

Preflight validates the existing kernel and complete comment list before
allocating nodes/symbols. Repeated annotation is rejected without changing
WN/ST/TY state. Text cannot close a C comment or enter the legacy pragma
passthrough path. COMMENT nodes are not used for executable semantics or DSL
markers. Attach explanations after transformations that could discard or
invalidate them; the focused pipeline annotates after executable materialization
and immediately before final publication.

## Commands and evidence boundaries

Existing commands now retain explanations automatically:

```sh
tensor_kernel_lower schedule-source selected.json optimized.cu
tensor_kernel_lower device selected.json device.before.B device.after.B
whirl2c -cuda -TARG:abi=n64 -O0 \
  -CLIST:dotc_file=kernels.cu -CLIST:doth_file=kernels.h device.after.B
```

The `device` command preserves its empty-PU before checkpoint and publishes
annotated executable PUs afterward. For isolated annotation evidence:

```sh
tensor_kernel_lower device-explain selected.json comments.before.B comments.after.B
ir_b2a -st -src comments.before.B comments.before.T
ir_b2a -st -src comments.after.B comments.after.T
```

Here both images already contain the same executable lowering; the second adds
only COMMENT statements, their CLASS_NAME/string entries, source positions, and
annotation audit metadata. Retain their raw unified diff. Emit both with normal
whirl2c to prove that executable CUDA tokens are identical after comments are
removed. This is actual mapped native WHIRL evidence, not a textual projection
substituted for an IR artifact.

## Validation and invalidation

The extended review harness verifies exact payload/order equality between the
readable `.cu` and normal whirl2c output from the annotated `.B`. Native trace
COMMENT counts must equal generated explanation counts. Short and long fixtures
contain 84 and 90 explanation nodes respectively. It also verifies:

- Stripping only the new explanation blocks restores the old readable v0.6
  source exactly; normal mapped CUDA executable tokens are unchanged.
- All seven saved scenarios per context match the independent expanded FP64
  oracle and prior lowered host results. Annotated mapped CUDA host execution
  matches the same output bytes.
- Both readable host/device files and both mapped GPU sources compile with
  NVCC for sm_90. GPU execution remains a separate, unclosed gate.
- Altered/missing COMMENT nodes fail audit without publishing a launch file.
- Generic non-MLA operations, different split widths/uneven tails, numerical
  guards, stale recipes, deterministic printing, and no-overwrite behavior
  remain covered by focused native tests.

Comment/schema changes invalidate explanation records, annotated image hashes,
their audit metadata and derived source/annotation evidence. Regenerate from
the selected recipe instead of editing a retained receipt. No executable
dependence/alias/SSA facts change when only COMMENT nodes are added; the focused
annotation runs before any such analyses are retained, and its body hash and
artifact identities are deliberately refreshed. Owner/successor/reviewer/lead
roles remain TBD under the existing MLA transfer requirements.
