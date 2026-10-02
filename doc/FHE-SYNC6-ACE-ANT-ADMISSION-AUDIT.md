# SYNC-6 ACE ANT Provider Admission Audit

Status: S6-1 source audit started; exact pinned provider **not admitted**.

This finding is deferred until after the provider-independent CKKS semantic IR
gate in `doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md`. It blocks ACE adapter and
client/server execution, not CKKS operator/state IR design, creation, or
mock-based terminal-lowering tests. No compiler or generated-C interaction
with ACE is required for that IR gate.

This audit implements the first check in
`doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md`. The public C ABI remains
`doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md`, and the provider/privacy decision is
`doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md`. It does not change the SYNC-5
generated C, schedule, or mock-provider acceptance.

## Reproducible Source Check

Run from the Open64 tree with the ACE checkout at the approved pin:

```sh
bash osprey/libopen64fhe/tests/fhe_ace_ant_admission_test.sh \
  /path/to/ace-compiler /private/tmp/open64-fhe-sync6-admission
python3 osprey/libopen64fhe/tests/fhe_ace_ant_admission.py \
  --ace-root /path/to/ace-compiler --require-admitted \
  --output /private/tmp/open64-fhe-sync6-admission/admission.json
```

The second command must return 3 at the current pin. The probe checks the
exact commit, rejects rtlib working-tree changes, hashes the sorted tracked
rtlib path/content inventory and cited files, and emits deterministic JSON.
It proves source identity and the four cited source observations only. It is
not an ABI implementation, complete operation-capability comparison,
reproducible provider build, or binary no-secret certification.

## Exact-Pin Findings

ACE revision: `fb76131171b9f82aa6387f84dd73684fba5277e8`.
The source-inventory SHA-256 produced by the probe is
`d9dfdcc6d78ee6e89acb2f21fb65c3be8bf1b0799b3c462f9831b0f57310a3d6`.
The inventory covers 619 tracked paths under `fhe-cmplr/rtlib` and is
distinct from the Git tree object ID. Recompute it after a reviewed patch;
never reuse it as a new pin's attestation.

| Requirement | Exact pinned evidence | S6-1 disposition |
| --- | --- | --- |
| Evaluation-only context | `fhe-cmplr/rtlib/ant/context/src/ckks_context.c` `Prepare_context()` calls `Alloc_ckks_key_generator`, then constructs an encryptor with the secret key and a decryptor. | Missing for the server. The dataset context path is not evaluation-only. |
| Public/evaluation keyset import | `fhe-cmplr/rtlib/ant/include/ckks/key_gen.h` exposes generation and in-memory key access. No reviewed import path exists in the pinned ANT aggregate/runtime API headers. | Not demonstrated; fail closed. |
| Ciphertext input import | `fhe-cmplr/rtlib/ant/ckks/src/rtlib.c` `Prepare_input()` encodes and encrypts locally; `Get_input_data()` retrieves an in-process registered object. | Not a client-produced ciphertext envelope import. |
| Ciphertext output export | The same source's `Handle_output()` decrypts and decodes locally; `Set_output_data()` copies into an in-process registry. | Not a server-side ciphertext envelope export. |
| Arithmetic/bootstrapping | `ant/include/ckks/cipher.h` and pinned generated ResNet code expose arithmetic and bootstrap services. | Declared/static evidence only; not the full frozen SYNC-5 capability, key, rotation, level, or execution admission. |

`CAPABILITY_MISSING` is the only valid admission result at this pin. An
embedded ACE dataset run can still be useful diagnostic evidence but cannot
prove the v0.10 client/server boundary. In particular, do not implement
S6-2 by invoking `Prepare_context()` inside a server worker and then ignoring
the secret key. That would still generate and retain secret material.

## Required Reviewed ACE Patch

After the CKKS IR gate and before S6-2, the ACE owner and Open64 FHE owner
must review a new immutable ACE pin with all of these distinct services and
tests:

1. Construct an evaluator-only context from authenticated public CKKS
   parameters and non-secret public, relinearization, signed-rotation, and
   bootstrap key material. Its constructor, live object graph, destructor,
   worker binary, and dependency closure must not call key generation,
   encryption with a secret key, or decryption.
2. Export those public assets in the separate client/provisioner, import them
   in a fresh worker, and reject wrong config, key class, pin, digest, level,
   slot count, or signed-rotation set before evaluation.
3. Import client-created ciphertext bytes into a worker-owned ACE ciphertext
   without decrypting, then export worker-produced ciphertext bytes to the
   client without decrypting. Specify versioned encoding, bounds, ownership,
   object lifetime, and cross-process roundtrip tests.
4. Preserve ABI v1 borrowed inputs and distinct owning results, including
   recoverable-error rollback and fatal-worker containment. Prove that
   `Get_input_data`, `Set_output_data`, and `Handle_output` are not silently
   reused across incompatible ownership or trust boundaries.
5. Pin source/patch/build/compiler/dependency hashes and licenses. Compare
   every frozen SYNC-5 operation descriptor, signed rotation, key class,
   target level, coefficient/range asset, and call count field-for-field.
   Resolve the ACE generated program's provider-internal 51/46 modulus
   fields against Open64's logical 60/56 contract explicitly; neither value
   may silently replace the other.

The S6-1 admission report must be regenerated from that new pin and must
include executable positive and negative import/export tests. Only after it
passes may the broker/worker implementation begin. This is a concrete
provider-contract gap, not permission to weaken the stable C ABI or the
secretless-server exit gate.
