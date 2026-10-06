# ACE CKKS-to-C Staging for SYNC-6

Status: proposed S6-0d provider-private code-generation contract. It does not
change the frozen Open64 public C ABI v1, certify a complete `.ckks_ops.B`, or
claim that the pinned ACE ANT provider already executes CKKS-level generated C.
The ACE source authority is commit
`fb76131171b9f82aa6387f84dd73684fba5277e8` in the local ACE checkout.

## What ACE Actually Implements

| Boundary | Pinned ACE source | Consequence |
| --- | --- | --- |
| Pass order | `fhe-cmplr/include/fhe/driver/fhe_pipeline.h` and `fhe-cmplr/poly/src/poly2c_pass.cxx` | The pipeline contains CKKS, POLY, then C emission. `POLY2C_PASS::Check_provider_supported` disables POLY for providers other than ANT. |
| Direct CKKS-to-C | `fhe-cmplr/include/fhe/poly/poly2c_driver.h` and `fhe-cmplr/include/fhe/ckks/ir2c_handler.h` | Non-ANT providers select `CKKS2C_VISITOR`; handlers emit `Add_ciph`, `Mul_plain`, `Rotate_ciph`, `Rescale_ciph`, `Mod_switch`, `Relin`, and `Bootstrap` calls with a result address and direct operands. |
| ANT-to-C | `fhe-cmplr/poly/src/poly2c_pass.cxx` | ANT selects `POLY2C_VISITOR` after POLY lowering. The pinned tree does **not** provide an ANT CKKS-to-C switch. |
| Provider header and metadata | `fhe-cmplr/include/fhe/poly/ir2c_ctx.h`, `fhe-cmplr/poly/src/poly2c_driver.cxx` | Generated C includes the selected rtlib header and emits context parameters plus input/output data schemes. |
| Plaintext assets | `fhe-cmplr/include/fhe/ckks/ir2c_ctx.h` | The C emitter can write a runtime data file and reference plaintext data through `Pt_from_msg`-style calls, carrying count, scale, and level. Open64 must use authenticated external-row identities rather than copying large arrays into `.B`. |
| Runtime surface | `fhe-cmplr/rtlib/include/rt_ant/{rt_api.h,ant_api.h}`, `rt_openfhe/rt_openfhe.h`, and `rt_seal/rt_seal.h` | `rt_ant/rt_api.h` exposes input/output, while `ant_api.h` exposes lower-level cipher/poly headers. The high-level CKKS arithmetic wrappers are in the other provider headers; the OpenFHE wrapper lacks `Bootstrap`, and the SEAL wrapper gates it behind `SEAL_BTS_MACRO`. |

ACE's repository test script enables `-P2C:lib=ant` and comments out its SEAL
and OpenFHE invocations as not fully implemented. A generated C file alone is
therefore not evidence that the pinned ANT runtime accepts a pre-POLY CKKS
circuit, especially the 19 required refreshes.

## Open64 Stage Boundary

The first input is the complete, separately reopened and gatekeeper-approved
`secure_resnet20.ckks_ops.B` with explicit direct-kid CKKS operations,
value-specific state, required keys, authenticated F32 row assets, 19
pre-ReLU bootstraps, and no live source Conv/BN/ReLU. This is the S6-0c gate.

S6-0d may then emit a **provider-private** C evaluator from that CKKS graph,
before any POLY/RNS lowering. It is compiled into the ACE adapter/worker, not
exposed as a new application ABI. The frozen generated-C application still
issues its nine public semantic operation kinds with the certified 87 static
and 147 execution-weighted visits. An exact source-event/group/descriptor
join binds each public call to one private CKKS evaluator entry. A new public
primitive call, changed signature, or reordered visit is forbidden.

The private evaluator and public ABI-v1 C are **sibling outputs from the same
immutable certified CKKS image**, not sequential rewrites of one in-memory
tree. Generate the private evaluator from the CKKS graph before its primitive
definitions are retired in the separate public-call lowering path. Join the
two outputs by authenticated source-event/group/descriptor identities and
publish them together only after both pass their gates.

This follows ACE's `CKKS2C_VISITOR` *staging pattern*, not its ANT provider
implementation. The private evaluator needs an ACE-shaped CKKS facade whose
typed operations match the emitted calls but whose implementation uses the
pinned ANT runtime under the existing secretless worker boundary. The facade
is new, versioned adapter work; it may not be represented as an existing ACE
`rt_ant` API. Keep the existing Open64 ABI mock as the sole runtime test
double through code generation. Compile-only facade declarations or a
focused call stub may check C syntax and linkage but must not become a second
required runtime mock or ciphertext-execution claim. POLY/RNS remains a later
optional provider path, not an implicit step in this `-O0` output.

## Emitter and Adapter Contract

1. Emit private C with an explicit context/asset table, exact function and
   call topology, direct typed CKKS operands, deterministic value IDs, and
   source/event provenance. The private module has no secret-key API and no
   direct file I/O. Broker import/export and public-handle validation remain
   in the existing runtime boundary; ACE's `Get_input_data` and
   `Set_output_data` are architectural analogues, not calls to copy blindly.
2. Map each `ckks.encode` to one authenticated external plaintext asset with
   exact dtype, shape, byte range, checksum, scale, and level. A
   `Pt_from_msg`-like adapter may encode or retrieve it, but cannot derive
   masks from untracked rank-4 weights or store all row bytes in `.B`.
3. Map arithmetic, rotation, rescale, modswitch, and relinearization to
   checked private facade calls. Each returns a typed result and verifies
   the planned input/output level, scale, components, slots, precision, and
   key class. No hidden repair or context-wide state substitution is legal.
4. Map each `ckks.bootstrap` to a facade call with its exact context-specific
   target level (15, 17, or 18), slot count, reason `PRE_RELU_REFRESH`, and
   post-state. Bootstrap does not compute ReLU. Fail closed if the pinned
   ANT build cannot support this operation and evaluation-key import.
5. Make allocation, ownership, aliasing, failure cleanup, and status
   propagation explicit. The facade cannot let an ACE assertion or exception
   escape the supervised worker or expose a secret key in server process
   state. Public ABI v1's single-event atomicity and cursor rules still hold.
6. Prove that each private group is reachable from exactly one public
   semantic descriptor and produces its one externally visible result. No
   internal CKKS value becomes a public ABI call or leaks across the group
   except through a reviewed group-output edge.

## Reviewable Implementation Sequence

1. Finish S6-0c and freeze `.ckks_ops.B`/`.T`, side assets, and the complete
   source-to-step/state/key census. No code generation from a partial graph.
2. Publish the main/common grouped terminal/image-disposition transaction
   requested by `FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md`; FHE supplies the
   descriptor join and a two-output publication plan. Keep the public ABI v1
   untouched.
3. Add a bounded read-only private CKKS-to-C emitter and a header-only or linkable
   ACE-shaped facade contract. A focused add/mul-plain/rotate/encode/bootstrap
   fixture must compile as C against checked declarations. Static inspection
   must preserve exact state and operation order and reject a missing
   primitive; any call-only stub used for a link check is not a runtime
   certification lane. In a separate copy/reopen path, lower the same input
   CKKS groups to the frozen public ABI C; cross-check both outputs before
   atomic publication.
4. Implement the ANT facade against a reviewed immutable ACE pin. Certify
   evaluation-only context, ciphertext and key import/export, all required
   primitives and bootstrap, failure containment, resource release, and
   absence of server-side secret material. Any missing function or key
   capability is a provider blocker, not a compiler rewrite opportunity.
5. Compile the complete private ResNet evaluator and link the unchanged
   six-PU ABI-v1 application C. Compare its 87/147 public schedule and
   ciphertext result identities to the ABI mock; then run real encrypted
   client/server acceptance separately. Retain C, compile/link commands,
   `.B`/`ir_b2a -st -src` `.T`, side data, provenance report, and hashes.

Negative tests must reject a CKKS node without a selected facade operation,
wrong direct operand/result type, missing asset or checksum, missing rotation
or bootstrap key, wrong target level, state mismatch, duplicate or escaping
group output, altered public descriptor, and any partial output after failure.
The generated C's syntax and link success are separate from numerical and
encrypted-runtime certification.

## Coordination Decision

The FHE task owns the private CKKS operation selection, emitter semantics,
ACE facade consumption, and certification. Main/common owns the grouped
native mutation, mapped-image status policy, and any shared codegen hook.
Because this stage differs from the present ANT POLY-to-C implementation,
it requires an explicit ACE facade review before promising an ANT-linked
pre-POLY executable. Until then, S6-0d remains blocked even if S6-0c emits
a complete CKKS graph.
