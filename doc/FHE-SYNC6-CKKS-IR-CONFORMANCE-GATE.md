# SYNC-6 CKKS Semantic IR Conformance Gate

Status: design/implementation handoff; no new opcode or binary row allocated.

The provider-independent CKKS semantic IR must be created and certified before
the Open64 compiler or generated program interacts with ACE `FHErt_ant`.
`doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md` records the pinned ACE import/export
gap, but that gap blocks provider execution only. It does not block this IR
work. This sequencing refines the SYNC-6 handoff without weakening the final
secretless client/server acceptance requirement in
`doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md`.

## Existing Boundaries

The certified SYNC-4 `secure_resnet20.ckks.B` contains context-bound refresh,
normalization, three polynomial stages, reconstruction, and CKKS state
*planning records*. It is not a complete executable CKKS primitive graph.
The SYNC-5 `secure_resnet20.mid.B` contains standard WHIRL calls to the stable
FHE C ABI. Its historical `.mid.B` stem does not establish a formal WHIRL
level, and those calls are already past the proposed CKKS optimization
boundary. Both artifacts remain retained regression references; neither is
renamed or misrepresented as the new conformance checkpoint.

The new checkpoint is provisionally named `secure_resnet20.ckks_ops.B` and
`secure_resnet20.ckks_ops.T`. It sits after FHE-CNN/layout/materialization and
before terminal standard-call lowering:

```text
source/CNN and FHE conversion
  -> encrypted layout and context-sensitive materialization
  -> scheme-independent HE, then explicit CKKS semantic operations
  -> CKKS state/key/rotation/precision verifier and .ckks_ops.B checkpoint
  -> optional FHE VHO/IPA, then re-verify and freeze schedule
  -> standard WHIRL FHE C ABI calls, whirl2c, provider adapter
  -> ACE ANT only after separate provider admission
```

## Conformance Contract

1. Represent every executable encrypted step with a logical, provider-neutral
   operation and explicit direct operands/results. At minimum, the reviewed
   census must cover ciphertext/plaintext add and multiply, encode/plaintext
   materialization, signed rotation, rescale, level/modulus alignment,
   relinearization, bootstrap, and the arithmetic needed for the accepted
   Chebyshev evaluation and reconstruction. Query-only scale/level facts may
   remain typed state rather than executable operations. A CNN operation may
   remain as provenance but may not be the sole executable representation.
2. Each result has a stable DSL value identity and separately associated
   encryption descriptor, canonical tensor TY/layout, value-specific CKKS
   level/scale/components/precision/slots, exact key and signed-rotation
   requirements, source/context identity, and transformation provenance.
   A level or scale change creates a new value/state; it never mutates the
   canonical tensor TY. The eleven reusable ReLU definitions retain all
   nineteen distinct call contexts and their approved bounds and target
   post-refresh levels.
   A shared callee body cannot hardcode one context's bound or target level.
   The S6-0a review must choose an explicit context-parameter or deterministic
   context-specialization mechanism for executable operations, with exact
   caller/callee identity and ABI proof. Do not clone merely to store planning
   metadata or silently conflate the nineteen uses.
3. The mandatory pre-ReLU bootstrap is an explicit CKKS operation with
   `PRE_RELU_REFRESH` reason. Bootstrap does not compute ReLU. Normalize,
   ordered degree-7/15/13 stages, and reconstruction are explicit downstream
   operations. No implicit provider refresh, hidden rescale, or automatic
   level repair may substitute for a missing IR operation at `-O0`.
4. Lower every one of the 147 certified dynamic high-level evaluation events
   into one or more CKKS steps while retaining a complete event-to-step
   provenance map. The CKKS step count is not presumed to equal 147.
   Recompute operation, signed-rotation, key, depth, and output-state counts
   from the materialized graph, not from planner estimates or ACE behavior.
5. The verifier proves operand/result representation and TY compatibility;
   per-context call/formal/actual ownership; layout and slot legality;
   scale/level/precision transitions and available modulus depth; exact
   bootstrap target levels 15/17/18 where required; key and rotation
   availability; no unresolved pending action; no unlowered executable CNN,
   SIHE, or `common.relu` node at the CKKS checkpoint; and complete source
   provenance. A rejected PU or context leaves no valid-looking final `.B`.
6. The checkpoint is mapped binary WHIRL, reopened in another process with
   `ir_b2a -st -src`. It prints logical CKKS operators, direct operands,
   context-specific states, key/rotation requirements, and source positions.
   Existing readers, non-FHE artifacts, and the SYNC-5 mock path retain their
   established compatibility behavior.

## Ownership And Review Sequence

| Slice | Owner | Reviewable result |
| --- | --- | --- |
| S6-0a semantic census/physical contract | Main/common owns any shared logical operator registry, WN representation, mapped-image/API, printer, and compatibility additions; FHE task supplies semantic operands, state rules, and tests. | Accepted handoff table; no enum or binary change before review. |
| S6-0b opaque producer and per-value state | FHE task consumes only reviewed common APIs; main supplies missing generic value/operation construction hooks. | Focused add/mul/rotate/rescale/relin/bootstrap `.B`/`.T` fixtures with source and state evidence. |
| S6-0c full ResNet expansion | FHE task expands the certified high-level schedule and 19 context-specific ReLU sequences. | Six-PU `.ckks_ops.B`/`.T`, event-to-step map, key/rotation/depth census, independent numerical checks. |
| S6-0d gate and terminal lowering | FHE task verifies the complete IR and lowers it through the existing stable C ABI; main reviews standard-WHIRL boundary. | Negative malformed-state/ownership/depth/key tests and generated-C/mock equivalence to SYNC-5. |
| S6-1 and later | ACE provider and runtime owners, after S6-0 certification. | New exact ACE pin/capability admission, then broker/worker/client-server execution. |

`osprey/common/com/dsl_opcode.h` currently has no published CKKS logical
operator family. This document requests a reviewed shared/common handoff;
the FHE task must not assign enum values, edit physical `OPR_DSL`, or change
the binary format independently. The private physical escape remains private.

### Proposed Shared Contract Census

All rows below are proposed v1 *logical* names, not allocated enum values.
Main/common owns any shared registry, physical WN, mapped-image, and builder
contract. The FHE task owns CKKS legality, state propagation, schedule
expansion, and terminal ABI lowering. Canonical TY remains the tensor identity;
cipher/plain class and mutable level/scale live in encryption/value-state
records, never in a new binary type kind. An operation is pure with respect to
source-language state but may consume keys and allocate provider values after
terminal lowering; the verifier must not infer numerical algebra laws from
that source-level purity.

| Logical operation | Operand/result contract and required attributes | Verifier and lowering handoff |
| --- | --- | --- |
| `common.tensor_const.v1` (reuse) | Tensor payload -> plain constant/encoded operand; exact TY, encoding, payload digest. | Reuse existing constant identity; FHE proves source bytes and CKKS encoding state before `ckks.encode` or terminal plaintext construction. |
| `common.add.v1` (reuse candidate) | Two cipher/plain-typed values -> one value; exact operand class, layout, scale/level-match policy, no broadcast. | Main decides whether its existing broadcast/marker contract can express this restricted CKKS use or needs a CKKS logical wrapper. FHE verifies alignment and lowers to exact add primitive. |
| `common.mul.v1` (reuse candidate) | Cipher-cipher or cipher-plain -> one value; operation class, output component/scale policy. | Main decides reuse versus wrapper; FHE accounts for multiplicative depth and explicit downstream rescale/relin. No implicit CKKS repair inside `common.mul`. |
| `ckks.encode.v1` (new candidate) | Plain tensor plus encoding descriptor -> packed plain; slots, layout, scale, level, source payload digest. | FHE proves capacity/shape and byte identity; common owns logical node contract if accepted. |
| `ckks.rotate.v1` (new candidate) | Cipher and signed nonzero rotation -> cipher; signed index, layout, key identity. | FHE checks slots, exact rotation key, state preservation or declared transition. |
| `ckks.rescale.v1` (new candidate) | Cipher -> cipher; exact consumed level count and target scale. | FHE checks modulus-chain availability, precision floor, and result state. |
| `ckks.modswitch.v1` (new candidate) | Cipher -> cipher; exact target level. | FHE checks direction, scale/precision, and absence of undeclared repair. |
| `ckks.relin.v1` (new candidate) | Widened cipher -> two-component cipher; key identity. | FHE checks input components, key class, and output components. |
| `ckks.bootstrap.v1` (new candidate) | Cipher -> refreshed cipher; target level, slots, reason, bootstrap key identity. | FHE checks all 19 pre-ReLU boundaries and per-context 15/17/18 targets; bootstrap has no ReLU result semantics. |

The accepted polynomial stages, normalization, and reconstruction lower to
ordered uses of these arithmetic operations with exact coefficient/asset
references and stage provenance. The census is intentionally minimal for the
first ResNet path; subtraction, conjugation, generic ciphertext-ciphertext
matrix operations, and POLY/RNS operators require a separate demonstrated
need. Main/common must resolve the reuse candidates, direct-kid shapes,
attribute schema, value construction API, printer names, and compatibility
strategy before the FHE implementation writes any executable CKKS node.

## Test Ladder And Exit

- Focused deterministic fixtures for each CKKS operation and invalid
  operand/state/key/rotation case, including input preservation on rejection.
- Two-PU same-definition/different-context tests proving values and levels
  remain context-sensitive without arbitrary PU cloning.
- Full six-PU ResNet census: 147 high-level events completely mapped to
  executable CKKS steps; 19 explicit pre-ReLU bootstraps with targets
  15 x 16, 17 x 1, 18 x 2; no standalone executable ReLU.
- Separate-process `.ckks_ops.B` mapped reopen and `ir_b2a -st -src` with
  nonzero source positions; old/non-FHE compatibility and malformed-image
  tests; no tabs and `git diff --check`.
- Terminal lowering and mock-provider comparison uses unchanged ABI v1.
  This proves compiler lowering, not ACE ciphertext execution.

The CKKS IR gate closes only when the complete artifact and verifier pass.
The ACE source finding in `doc/FHE-SYNC6-ACE-ANT-ADMISSION-AUDIT.md` remains an
explicit later provider gate. It must not be used to skip CKKS IR work, and
CKKS IR success must not be reported as SYNC-6 client/server completion.
