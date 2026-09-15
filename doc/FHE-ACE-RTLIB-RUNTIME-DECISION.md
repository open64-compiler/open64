# FHE ACE RTLIB Runtime Decision

Status: selected first executable provider; SYNC-6 capability admission pending

## Authority And Decision

The first Open64 FHE executable provider is the ACE ANT CKKS runtime
`FHErt_ant`, pinned to ACE commit
`fb76131171b9f82aa6387f84dd73684fba5277e8`. It replaces the earlier direct
OpenFHE API plan for the first backend.

This approval changes only provider selection and the provider-private API
mapping. `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx`
remains the sole highest security, semantic, and milestone authority. The
public generated-C interface is defined only by
`doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md`. This decision does not relax the
client/server privacy boundary, change WHIRL or generated-C semantics, or
create another public ABI.

The pinned ACE dataset path establishes a useful implementation reference: it
links `FHErt_ant`, executes 19 bootstrap boundaries with the 16/1/2 target-level
distribution across levels 15/17/18, and uses the approved `7 -> 15 -> 13`
composite ReLU profile. It does not by itself establish that the exact pin can
import a public evaluation context, all required evaluation keys, and
ciphertexts without creating or retaining a secret key. SYNC-6 remains blocked
until those capabilities are proved against the exact source and build hashes
or supplied by a separately reviewed patch and immutable pin.

## Private Provider Boundary

The selected server path is:

```text
unchanged SYNC-5 generated C
  -> Open64 FHE C ABI v1 broker
  -> one supervised worker per imported public context
  -> one ACE singleton context inside that worker
  -> pinned FHErt_ant
  -> ciphertext envelope returned through the broker
```

Generated C and the broker do not include ACE headers or expose ACE objects.
The worker alone links `FHErt_ant` and owns ACE contexts, plaintexts,
ciphertexts, evaluators, polynomials, bootstrap state, and key objects. Multiple
public Open64 contexts create multiple workers; they never create multiple ACE
singleton contexts in one process. Calls within one context are serialized.

The provider manifest records the public ABI/profile hashes, provider and
worker protocol versions, exact ACE commit and source-tree hash, patch hash if
any, `FHErt_ant` build options, compiler ABI, worker executable and dependency
hashes, resolved CKKS/security configuration, supported import/export and
operation capabilities, complete signed-rotation and key requirements,
post-bootstrap levels, and required licenses and notices.

## Provider Private Mapping

This table constrains the ACE adapter implementation. The public signatures,
descriptors, statuses, and ownership rules remain in the ABI v1 contract.

| ABI v1 behavior | Provider-private ACE mapping and proof obligation |
| --- | --- |
| Public context import | Reconstruct or import only the authenticated evaluation context for the exact config identity. No key generation or secret-key-bearing context is permitted. |
| Evaluation keyset import | Treat ACE blobs as opaque. Import only objects whose exact digest, non-secret key class, config, and provider pin match the privileged ABI provisioning registry and the outer canonical directory; reject unknown, unregistered, mismatched, or secret-class objects before worker dispatch. |
| Plain model tensor import | Validate envelope, tensor identity, shape, encoding, and checksum; create only provider-private plaintext/encoded objects. |
| Ciphertext import/export | Convert ciphertext envelopes admitted through the ABI broker's authenticated request-session binding without decrypting, changing config/provider identity, or publishing partial bytes. Envelope hashes alone are only integrity evidence. |
| Plain-weight convolution | Expand the frozen high-level descriptor through reviewed ACE rotate, multiply-plaintext, add, mask, and scale services; verify output layout and CKKS state. |
| Residual add | Map through reviewed ACE addition/normalization services; reject broadcasting, layout mismatch, or unplanned level/scale repair. |
| Bootstrap | Map to `Bootstrap(result, input, level_after_bts)` with slot-specific precomputation and the exact descriptor target level 15, 17, or 18. |
| ReLU normalization | Apply the exact approved normalization assets and range identity from the descriptor. |
| ReLU polynomial stage | Evaluate exactly one stage through ACE arithmetic with stage order 0/1/2 and degrees 7/15/13; preserve coefficient digest, evaluation schedule, and CKKS state. |
| ReLU reconstruction | Apply the approved reconstruction to the refreshed input and stage-2 result without hiding another bootstrap or approximation. |
| Average pool | Expand the frozen high-level pool descriptor through reviewed addition, rotation, mask, and plaintext-scale services. |
| Flatten/layout conversion | Execute the one mandatory scheduled ABI call and exact slot/layout descriptor; ACE may optimize it to metadata-only private work but cannot omit the public call or create a layout choice. |
| Plain-weight linear | Expand the frozen matrix/layout descriptor through reviewed rotate, multiply-plaintext, and add services and produce ciphertext logits. |
| Status and cleanup | Translate all ACE results and failures to ABI v1 status/diagnostic records; release provider-private objects in deterministic reverse order. |

The high-level schedule, coefficient bytes, layouts, rotations, keys, and state
transitions come from the retained SYNC-5 manifests. In particular, the ABI
contract's existing operation-descriptor-directory entry carries the canonical
ReLU profile/evaluation-schedule identities, ordered 7/15/13 coefficient tensor
identities, and CKKS state transitions. The adapter preserves those identities
and executes that schedule; it does not redesign it from ACE capabilities
during SYNC-6.

## Client And Server Boundary

A separate client or provisioner owns key generation, the secret key, input
encryption, result import, and decryption. A validation helper may privately use
ACE `Prepare_input`, key-generation services, and decrypting `Handle_output` to
create client-side test envelopes and compare results. That helper and its
secret-key dependency are not linked into, loaded by, or invoked from the
server broker or worker.

The server accepts only public-context, public/evaluation-key, model-package,
plaintext-model, and ciphertext envelopes admitted by the ABI v1 privileged
launcher, immutable provisioning-registry, and HMAC request-binding rules.
Generated C cannot initialize a broker or replace its registry. Envelope
SHA-256 fields provide integrity, not authenticity. The broker rejects
the secret-key class with `OPEN64_FHE_STATUS_SECRET_KEY_FORBIDDEN` before worker
dispatch. The server binary, worker binary, dependency closure, runtime state,
temporary files, logs, generated C, WHIRL, and retained public evidence contain
no secret key or secret-key API dependency.

The ABI contract alone defines the host-only one-shot launcher capability,
broker identity/generation, closed-world value/descriptor joins, HMAC verifier
retention, and broker/context diagnostic ownership. The ACE worker receives
only an already admitted broker request and does not acquire launcher privilege
or redefine any of those public states.

If the exact ACE pin cannot satisfy this boundary, an embedded dataset harness
may remain diagnostic evidence but cannot close SYNC-6. The project must review
the required ACE import/export patch, change the exact pin and hashes, and
rerun capability admission.

## Consuming Ownership Adaptation

ACE raw pointers and dataset helpers do not define the public ownership model.
While a worker remains active, the adapter treats every public arithmetic input
as borrowed and every result as a distinct owning handle. Success and
recoverable errors preserve the semantic input value. Fatal worker termination
destroys its ACE objects and preserves only poisoned public token bits and
reference counts for release; it does not preserve a usable input value. The
following rules are implementation obligations pending confirmation against
the exact pinned headers and source; that audit is part of capability admission.

| ACE surface | Ownership hazard | Required adapter behavior |
| --- | --- | --- |
| `Get_input_data` | The returned wrapper and its underlying polynomial/ciphertext storage can have different owners and lifetimes. | Separate wrapper ownership from payload ownership explicitly. Copy or move into a worker-private temporary before use; never retain a pointer into a temporary wrapper or expose it as a public handle. |
| `Set_output_data` | The callee may consume or register ownership of the supplied ACE object. | Pass only a newly allocated private temporary or an object recorded as explicitly moved. Never pass the object backing a borrowed public ciphertext handle. |
| `Handle_output` | Dataset handling may allocate result buffers, decrypt, and consume or release a registered ciphertext. | Use only in the client/provisioner validation helper. Record every allocation and consumption; never invoke it in the server worker or on a server public handle. |
| ACE arithmetic and bootstrap calls | A raw result/input convention may alias, mutate, or consume provider objects. | Adapt through private temporaries or deep copies and commit a new provider object only after success. On recoverable failure, discard temporaries and leave public inputs/refcounts and provider values unchanged. On fatal worker loss, preserve only poisoned tokens/refcounts for release. |

The implementation audit must cite the exact function declarations and bodies,
record whether each argument is borrowed, owned, or consumed, and add success,
failure, alias, and cleanup tests. Similar names or a passing happy path are not
ownership evidence.

## Failure Containment

Normal close follows `ACTIVE -> CLOSED`; a fatal worker path follows
`ACTIVE -> POISONED -> CLOSED`.

- The broker publishes an output handle or envelope only after a complete
  successful worker response.
- A returned, recoverable provider error discards private temporaries and
  leaves inputs, public tokens/references, provider objects, and the evaluation
  cursor at their pre-call values.
- An ACE assertion, abort, unhandled exception, signal, nonzero exit, or broken
  IPC channel returns `OPEN64_FHE_STATUS_PROVIDER_TERMINATED`, commits no
  output, removes private temporaries, destroys all provider objects, preserves
  public token/reference counts for cleanup, marks every live child token
  `POISONED`, leaves the evaluation cursor unchanged, and poisons the context.
- Later non-cleanup calls on that context or a recognized poisoned child return
  `OPEN64_FHE_STATUS_CONTEXT_POISONED` without dispatch. Release calls may
  discard poisoned handle tokens; the diagnostic query remains available.
- Context destroy reaps the worker and reaches `CLOSED`. A separately imported
  fresh context starts a new worker; the runtime never silently replays the
  failed call.
- Double destroy, stale handle, and use after destroy return
  `OPEN64_FHE_STATUS_INVALID_HANDLE`. Destroy during live work or with live
  children in an active context returns `OPEN64_FHE_STATUS_BUSY`.

No ACE assertion, provider-private status, raw pointer, or C++ exception crosses
the process or C ABI boundary. Patching ACE assertions into returned errors is
permitted only with review and a new exact source/patch hash.

## Milestone Effect

| Milestone | Provider responsibility |
| --- | --- |
| SYNC-4 | Certify provider-independent bootstrap plus composite-ReLU materialization. Check only the ReLU/bootstrap subset needed for that exact evidence; do not claim the complete ACE capability gate. |
| SYNC-5 | Freeze the complete high-level ResNet generated-C surface, execution-expanded semantic schedule, successful static/dynamic evaluation census, separate lifecycle/failure transcripts, signed rotations, key requirements, ABI header, envelopes, ownership, and full mock behavior. ACE is not required to build or pass this gate. |
| SYNC-6 admission | Compare the exact ACE pin/build with every frozen SYNC-5 capability, including evaluation-only context/key/ciphertext import and ciphertext export. Missing capability returns `CAPABILITY_MISSING` and blocks execution. |
| SYNC-6 execution | Run the unchanged complete ResNet generated C through the broker/worker provider with separate client/provisioner key custody and retain full functional, numerical, failure, dependency, and no-secret-key evidence. |
| Later work | Add direct OpenFHE or GPU providers, service orchestration, or a new ABI version without changing accepted v1 semantics. |

## Acceptance Evidence

SYNC-6 retains the original, FHE, CKKS, and middle-WHIRL `.B`/`.T` pairs;
unchanged SYNC-5 generated C and ABI header; expanded semantic schedule,
evaluation census, and separate lifecycle/failure transcripts;
descriptors and hashes; server, worker, and client-helper build/link/dependency
reports; exact ACE source/build/provider manifests; import/export envelopes;
19 ReLU bootstrap/normalize/three-stage/reconstruction traces; runtime operation
counts, memory, latency, and precision; client-side clear, polynomial, and
ciphertext-result comparison; and negative results for secret-key import,
provider/config mismatch, missing capability/key, corrupt envelope, wrong call
order, ownership/alias violations, worker termination, partial publication,
double destroy, and destroy/recreate.

No completion claim may describe this as OpenFHE execution. No completion claim
may rely on an embedded secret-key ACE harness as evidence for the v0.10
client/server acceptance boundary.
