# FHE ACE RTLIB Runtime Decision

Status: selected first executable backend; implementation pending SYNC-5 and
SYNC-6 review

## Decision

The first Open64 FHE executable target uses the ACE ANT CKKS runtime
(`FHErt_ant`) instead of the ACE OpenFHE adapter or a new direct OpenFHE
provider. The reference source is pinned to ACE commit
`fb76131171b9f82aa6387f84dd73684fba5277e8`.

This decision changes the first runtime provider, not the frontend, binary
WHIRL, logical operator, tensor, FHE image, approximation-profile, or CKKS
planning contracts. Open64 continues to preserve provider-independent
`common.relu`, FHE/SIHE/CKKS planning evidence, and standard-WHIRL runtime-call
lowering.

## Rationale

The pinned ACE ResNet-20 path is already generated against
`rt_ant/rt_ant.h`, links `FHErt_ant`, executes 19 bootstrap boundaries, and
uses the empirically approved `7 -> 15 -> 13` composite ReLU profile. Its
bootstrap implementation performs slot-specific precomputation as needed and
accepts the compiler-selected post-bootstrap level. The same generated model
contains 16 boundaries targeting level 15, one targeting level 17, and two
targeting level 18.

ACE's separate `rtlib/openfhe` adapter is not the runtime used by that
certified ResNet path and is explicitly incomplete. Requiring it as the first
Open64 executable backend would recreate bootstrap and polynomial integration
that already exists in `FHErt_ant`.

## Integration Boundary

The selected link path is:

```text
Open64 binary WHIRL
  -> FHE/SIHE/CKKS materialization
  -> standard middle-WHIRL calls
  -> whirl2c generated C
  -> versioned Open64 FHE C ABI
  -> libopen64_fhe_ace_ant provider adapter
  -> pinned FHErt_ant
  -> local CKKS ResNet-20 execution
```

Generated C must not include ACE internal headers or expose ACE ciphertext,
plaintext, evaluator, polynomial, or key structures. The adapter owns all ACE
types and maps opaque Open64 handles to ACE runtime objects. This preserves the
mock-provider test boundary and permits a later OpenFHE provider without
changing WHIRL or generated C.

The initial provider manifest records:

- Open64 FHE C ABI version;
- provider name `ace-ant` and provider-adapter version;
- exact ACE commit and source-tree hash;
- `FHErt_ant` build options, compiler ABI, and transitive libraries;
- ring degree, modulus and scale policy, security setting, slot count, and
  secret-key hamming weight;
- supported operations and bootstrap capability;
- required rotation keys and supported post-bootstrap levels; and
- licenses and notices required for the linked distribution.

## Required Runtime Mapping

| Open64 operation | ACE ANT service | Required check |
| --- | --- | --- |
| Context lifecycle | `Prepare_context`, `Finalize_context` | Resolved CKKS parameters exactly match the provider manifest. |
| Input preparation | `Prepare_input` / ACE input data services | Tensor shape, packing, name, and slot count match the entry contract. |
| Output handling | ACE output data services and `Handle_output` in the local harness | Output shape and result identity match; decryption remains outside compiler passes. |
| Add | `Add_ciph`, `Add_plain`, scalar form when required | Operand class, level, scale, and alias policy match the planned state. |
| Multiply | `Mul_ciph`, `Mul_plain`, scalar form when required | Component growth, relinearization need, scale, and depth match the plan. |
| Rotate | `Rotate_ciph` | Signed rotation is present in the key-requirement manifest. |
| Relinearize | `Relin` | Input component count and output component count are verified. |
| Rescale/level change | `Rescale_ciph` and reviewed ACE level services | Result level and scale match the persisted CKKS state; names alone are not treated as semantic proof. |
| Bootstrap | `Bootstrap(res, input, level_after_bts)` | Slot-specific precomputation exists or is created; target level is the exact context level 15, 17, or 18. |
| Composite ReLU | Compiler-materialized normalization, ordered `7 -> 15 -> 13` stages, and reconstruction using ACE primitives | Coefficient, range, stage order, depth, bootstrap reason, and source `common.relu` provenance remain inspectable. |
| Conv/linear/pool | Compiler-selected rotate/multiply/add metakernels | Layout, masks, rotations, parameter payload, and result state match the certified plan. |

## Security And Deployment Scope

ACE's working dataset executable is an embedded/local evaluation model. Its
runtime constructs context and key material in process and its harness can
decrypt results. Consequently, SYNC-6 is now a functional and numerical
acceptance gate for the pinned ACE ANT runtime; it is not evidence of a
production server that never possesses a secret key.

Secret keys remain prohibited from WHIRL, mapped images, compiler diagnostics,
generated model C, retained public compiler artifacts, and Git. The local
SYNC-6 harness may own ephemeral test key material in an access-controlled
artifact directory. Production client/server separation, imported evaluation
keys, remote ciphertext transport, and a server-without-secret-key proof are a
separate post-SYNC-6 deployment/security milestone.

## Milestone Effect

| Milestone | Revised runtime objective |
| --- | --- |
| SYNC-4 | Materialize the mandatory bootstrap and approved composite ReLU while preserving provider-independent logical evidence. Validate that every materialized operation is supported by the pinned `ace-ant` capability manifest. |
| SYNC-5 | Lower all FHE/SIHE/CKKS operations to the versioned Open64 C ABI and pass the mock provider executable gate. Add the reviewed ACE adapter mapping without exposing ACE types in generated C. |
| SYNC-6 | Link the complete ResNet-20 generated program with the ACE provider adapter and pinned `FHErt_ant`; execute and compare against the certified clear/polynomial baselines. |
| Post-SYNC-6 | Certify production key separation and optional providers, including a direct OpenFHE adapter, without changing the source or binary-WHIRL contracts. |

## Acceptance Evidence

SYNC-6 must retain:

- original, FHE, CKKS, and middle-WHIRL `.B`/`.T` pairs;
- generated C, object, executable, and exact compile/link commands;
- ACE source revision, provider manifest, build log, dependency inspection,
  and license record;
- coefficient, calibration-range, CKKS-state, layout, key-requirement, and
  conversion reports;
- 19 bootstrap calls with the approved 16/1/2 distribution across levels
  15/17/18;
- runtime operation counts, memory, latency, and output precision;
- clear baseline, clear composite-polynomial emulation, and ACE ciphertext
  result comparison; and
- negative evidence for provider mismatch, unsupported operation, missing
  rotation/bootstrap capability, wrong level/scale, bad payload checksum, and
  partial-artifact cleanup.

No completion claim may describe this as OpenFHE execution or as a
server-without-secret-key deployment. A future provider must pass the same
provider-independent semantic, numerical, state, and artifact gates.
