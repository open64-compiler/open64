# FHE SYNC-5 Native Infrastructure Ownership Audit

Status: main-owned phase, projection, and descriptor-selection infrastructure
merged; full-model entry/resource binding contract remains pending

## Purpose

SYNC-5 lowers the accepted FHE conversion and materialization plans to normal
WHIRL calls, symbols, result stores, status checks, and control flow.  The
resulting `application.mid.B` must be consumable by the existing binary WHIRL
reader and by an otherwise unchanged `whirl2c`.

This audit records the shared ownership boundary and the first implemented
main-owned infrastructure batch.  It does not
define the public FHE runtime ABI, generate the 87-static/147-dynamic call
schedule, implement the mock runtime, or select provider operations.  Those
remain FHE-owned work under `FHE-RUNTIME-C-ABI-V1-CONTRACT.md` and S5-1 through
S5-6 in `FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md`.

## Compilation Scope

Runtime-call lowering is PU-local.  The backend driver selects one PU, restores
its local symbol table and map table, invokes the lowering callback, verifies
the resulting tree, and writes that PU while the scope is active.  The
program-finalization callback may verify aggregate counts and managed-image
coverage, but it must not revisit or mutate an inactive PU.

Cross-PU execution expansion belongs to the persisted semantic schedule and
FHE-owned planning logic.  It does not authorize the main framework to retain
borrowed WN, ST, or local-symbol-table pointers across PU transitions.
Program finalization may aggregate stable IDs and counters and may register
auxiliary artifacts through the binary-last checkpoint lifecycle.  It retains
no PU-local pointers and performs no late PU mutation.

## Existing Open64 Mechanisms To Preserve

The first SYNC-5 slice needs no new WHIRL operator, opcode, TY kind, or binary
section merely to construct standard calls.  The implementation must use the
existing Open64 mechanisms:

- `New_TY`, `New_TYLIST`, `Set_TYLIST_type`, and `TY_is_unique` for exact,
  interned C function prototypes;
- `New_ST`, `ST_Init`, `Gen_Intrinsic_Function`, and `Set_ST_Srcpos` for
  external runtime symbols and source-bearing temporaries;
- `WN_Call`, `WN_CreateParm`, `WN_Lda`, `WN_CreateLdid`, `WN_CreateStid`, and
  the normal `Return_Val_Preg` convention for calls and scalar status results;
- `WN_CreateBlock`, `WN_INSERT_Block*`, `WN_CreateIf`, comparisons, and
  `WN_CreateReturn_Val` for checked failure propagation;
- the existing mapped-image reader/writer and generic FHE checkpoint service
  for an all-PU `.mid.B` checkpoint; and
- standard `OPR_CALL` handling already present in `whirl2c`.

The FHE lowerer must not use `DSL_Builder_*`, frontend handles, private WN
encodings, or a special in-memory path.

## Ownership Matrix

| Surface | Main/shared owner | FHE owner |
| --- | --- | --- |
| Per-PU phase lifecycle | Register callbacks, invoke under active PU scope, accumulate results, fail closed | Register semantic gate and lowering callback |
| Standard WHIRL construction | Provide checked helpers for exact function TY/ST, typed parameters, return-status capture, output-handle slots, source positions, and detached block construction | Supply ABI symbol, exact ordered argument descriptions, effect flags, output role, and cleanup/failure policy |
| Lowering decision | No operator-to-runtime mapping in shared code | Map accepted CNN/FHE/materialization records to ABI v1 calls and the frozen semantic schedule |
| Tree commit | Provide preflight plus atomic insertion/replacement service within the active PU | Supply the complete replacement request and provenance association |
| Final unlowered gate | Provide structural tree scan, registered semantic hook, stable diagnostics, and driver invocation | Prove all required FHE/SIHE/CKKS operations were consumed and schedule counts agree |
| All-PU checkpoint | Reuse binary-last checkpoint publication and active-PU `Write_PU_Info` discipline | Produce deterministic schedule/report auxiliaries through checkpoint registration |
| `whirl2c` | Gate before invocation; do not add FHE syntax | Prove generated C contains only ABI v1 C names and ordinary C types |
| Runtime ABI and mock | No ownership | Header, descriptors, manifests, deterministic mock, failure injection, compile/link/run tests |

### Common/AI Reuse Criterion

API ownership follows semantic applicability rather than the domain that first
needed the service. A transaction, query, verifier, or lowering helper stays
in `osprey/common/com` when its contract is domain-neutral or naturally usable
by AI and tensor compiler domains. This includes external tensor references,
shape/type refinement, stable logical-value lookup, pure value retirement,
runtime-handle projection, program-interface reconstruction, and native DSL to
standard-WHIRL lowering. FHE may be their first production consumer without
becoming their architectural owner.

An API belongs in an FHE-owned module only when correct interpretation requires
FHE-specific facts such as an encryption scheme or descriptor, CKKS level and
scale state, ciphertext component or precision state, key requirements,
bootstrap reasons, polynomial/composite approximation policy, encrypted
runtime ABI, or FHE-specific diagnostics and publication gates. File names,
current caller locations, and milestone provenance do not override this test.

Code review must apply the following decision order:

1. If an API can express an AI/tensor transformation without cryptographic
   vocabulary or policy, keep it in `common/com`.
2. If generic mechanics and FHE policy are mixed, keep the mechanics in
   `common/com` and move only the policy adapter and FHE records to the
   FHE-owned module.
3. If the API cannot be specified or verified without FHE semantics, place it
   in the FHE-owned module and expose only a narrow generic registration hook
   where shared infrastructure must invoke it.

The persisted FHE source and planning images therefore live in
`osprey/common/fhe`. That module owns `fhe_image.{h,cxx}`,
`fhe_plan.{h,cxx}`, `fhe_image_print.cxx`, and `fhe_plan_print.cxx`. The
FHE-specific basenames make the ownership boundary visible; fixed rows,
section identifiers, mapped-image services, and public `DSL_FHE_*` symbols
remain unchanged. Generic DSL image, rewrite, retype, lowering,
runtime-interface, and program-interface services remain in
`osprey/common/com` under the Common/AI reuse criterion.

## Reserved Main-Owned Files

The main infrastructure workstream exclusively owns edits to these shared
files for the coordinated SYNC-5 batch:

- `osprey/be/vho/fhe_runtime_lower.h`
- `osprey/be/vho/fhe_runtime_lower.cxx`
- `osprey/be/vho/fhe_standard_whirl.h`
- `osprey/be/vho/fhe_standard_whirl.cxx`
- `osprey/be/vho/fhe_unlowered_gate.h`
- `osprey/be/vho/fhe_unlowered_gate.cxx`
- `osprey/common/com/config_fhe.h`
- `osprey/common/com/config_fhe.cxx`
- `osprey/be/be/driver.cxx`
- `osprey/be/be/cleanup.cxx`
- `osprey/be/be/Makefile.gbase`
- `osprey/ir_tools/Makefile.gbase`
- `osprey/common/com/tests/dsl_native_syntax_test.sh`

The first slice reserves no edit in `osprey/be/whirl2c`.  A proposed
`whirl2c` source change is a contract-review stop: standard `OPR_CALL`, normal
symbols/types, ordinary control flow, and source positions must already print
as C.  Driver or test orchestration changes do not justify teaching
`whirl2c` about FHE.

The first slice also reserves no new `osprey/common/com` IR-construction file.
The established `symtab`, `wn`, and type APIs above are sufficient.  If
implementation discovers a missing generic constructor or verifier query, the
main owner must publish that exact gap before adding a common/com API.  Such an
API must remain domain-neutral and may construct or inspect IR, but must not
capture FHE content or make a lowering decision.

## Reserved FHE-Owned Files

The FHE workstream owns files that encode ABI v1 policy and deterministic
schedule content, including:

- the checked-in public ABI v1 header and its C/C++ conformance tests;
- `osprey/be/vho/fhe_semantic_runtime_lower.h` and
  `osprey/be/vho/fhe_semantic_runtime_lower.cxx`;
- descriptor, semantic-event, rotation, key-requirement, census, and lifecycle
  artifact producers;
- the deterministic mock provider and its failure-injection tests; and
- generated-C compile, link, execute, and transcript certification scripts.

FHE-owned code consumes the main callback and standard-WHIRL construction
interfaces.  It must not edit the shared driver, checkpoint lifecycle,
`whirl2c`, mapped-image reader/writer, or generic WN/ST construction code.

## Implemented Main Interfaces

### Native Transaction Source Ownership

The generic common/com implementation is divided by transaction authority:

- `dsl_ir_rewrite.cxx` owns typed external tensor lookup/materialization,
  identity-preserving native logical rewrite, and pure value redirect/retire;
- `dsl_runtime_interface.cxx` owns runtime handle projection plans, projected
  FUNC_ENTRY/call/return reconstruction, and runtime projection verification;
- `dsl_program_interface.cxx` owns program-wide input promotion, verified-dead
  formal/call retirement, deterministic ABI reconstruction, and final
  interface verification;
- `dsl_ir_lower.cxx` owns native DSL to standard-WHIRL replacement; and
- `dsl_ir_retype.cxx` owns atomic value/TY refinement.

The private headers `dsl_ir_transaction_internal.h` and
`dsl_runtime_interface_internal.h` expose only active-PU predicates and
cross-transaction journals/helpers. They are not frontend APIs, mapped-image
contracts, or permission to bypass the public atomic transactions. Every
extracted unit retains complete preflight-before-commit behavior and stable
diagnostics; the split changes no WN encoding, ELF section, row layout, or
binary WHIRL revision.

The generic call-ABI and PU-interface validators remain beside the generic IR
transaction preflight because they join mapped call, formal, value,
runtime-projection, and retired-interface records. They neither choose runtime
projection policy nor perform program reconstruction. Moving them into either
mutation unit would make that unit own the other's mapped-image invariants;
they remain read-only shared validation until a dedicated image-validation
module is justified by broader reuse.

Active-PU mutation and definition-lookup APIs accept `PU_Info *` as their sole
physical authority. They derive the global owner ST, PU tree, and containing
BLOCKs from that object and the named definitions or insertion anchors.
Runtime-only requests therefore do not repeat `owner_pu_st`, `pu_root`,
`containing_block`, or `insertion_block`. This prevents disagreement among
multiple representations of the selected PU while preserving stable logical
value IDs and mapped-image provenance as the semantic authority.

### Per-PU Runtime-Lowering Driver

`fhe_runtime_lower.h` publishes opaque callback types and a result record
following the existing conversion and materialization drivers:

```text
VHO_FHE_Runtime_Lower_Register_Semantic_Gatekeeper(...)
VHO_FHE_Runtime_Lower_Register_Pass(...)
VHO_FHE_Runtime_Lower_Register_Checkpoint_Lifecycle(...)
VHO_FHE_Runtime_Lower_Driver_Try(PU_Info *, WN **, result *)
VHO_FHE_Runtime_Lower_Result_Init(...)
VHO_FHE_Runtime_Lower_Result_Accumulate(...)
VHO_FHE_Runtime_Lower_Checkpoint_Validate(...)
```

The options passed to callbacks contain only reviewed phase controls,
authenticated manifest paths/digests, and the stable final checkpoint output
path used to derive same-directory schedule/census/rotation/key artifacts.
They do not expose mutable checkpoint driver state or frontend handles.  The
same option strings remain valid and identical across every per-PU callback.

The phase is controlled by `-FHE:runtime_lower` and the all-PU checkpoint by
`-FHE:runtime_checkpoint=<application.mid.B>`.  Conversion, materialization,
and runtime-lowering checkpoints are mutually exclusive.  Selecting the
runtime checkpoint enables the runtime-lowering phase and uses the existing
binary-last transaction for the final `.mid.B` and any registered auxiliary
artifacts.

### Standard-Call Construction

`fhe_standard_whirl.h` describes, without FHE operator enums:

- an external C symbol name and exact return/parameter TY list;
- ordered parameters with by-value, read-only borrowed pointer, mutable
  borrowed pointer, or output-slot policy;
- source position and standard call-effect flags;
- an optional local output-handle symbol;
- scalar status capture from `Return_Val_Preg`; and
- a caller-selected failure builder that receives the created status and
  output ST identities and returns ordinary WHIRL.

The builder returns a detached standard-WHIRL block plus created ST identities.
It preflights every fallible semantic, type, argument, output, and
failure-block condition before tree insertion.  A failed construction or
commit leaves the executable tree and managed provenance associations
unchanged.  Append-only interned TY/ST entries may remain only where the normal
Open64 interning service cannot roll them back; the implementation and focused
rollback test must state that behavior explicitly.  It sets source positions
on every created symbol and statement.  It does not know ABI symbol meanings,
FHE operation kinds, descriptor contents, schedule order, or cleanup order.

The initial result pattern is:

```text
OPR_CALL open64_fhe_*_v1(typed OPR_PARM ...)
STID status <- LDID Return_Val_Preg
IF status != OPEN64_FHE_STATUS_OK
  <caller-supplied ordinary-WHIRL failure block>
```

Opaque output handles are caller-owned local symbols passed by address.  Input
handles are ordinary pointer-width loads passed according to the ABI's
borrow/ownership contract.  A call is not committed until its exact prototype,
arguments, output slot, status capture, failure block, and source positions
all validate.

Opaque handles use real WHIRL pointer TYs.  An input handle is a typed pointer
value; an output handle is a caller-owned pointer local passed through a typed
pointer-to-pointer output parameter.  Every input supplies an explicit
`actual_ty` and construction requires exact TY identity with the formal, not
only equal machine types.  Every output local is initialized to NULL before
its call.  The failure builder may load the captured status and output local,
which still contains NULL if the provider returns failure without publishing
an output.

Repeated use of the same exact function prototype goes through `TY_is_unique`,
allowing the same external function symbol to be reused.  A pre-existing
same-name function with a conflicting prototype is rejected rather than
creating a second external declaration.  The initial API permits at most one
output slot per call; expansion requires a reviewed API revision and focused
ABI tests.

### Dynamic Descriptor Selection

Shared class-centric PUs cannot embed one context-specific operation
descriptor. SYNC-5 therefore uses the session-scoped reservation contract in
`FHE-SYNC5-DESCRIPTOR-SELECTION-CONTRACT.md`. Generated WHIRL calls
`open64_fhe_operation_desc_select_v1(model, anchor, static_ordinal,
operation_kind, &descriptor)`, checks its status, and immediately passes the
borrowed descriptor to the evaluation call. The anchor is ABI operand zero
and identifies the current inference session and schedule cursor.

The runtime owns dynamic event selection and reservation. The shared PU owns
only its static ordinal and operation kind. Successful selection does not
advance the cursor; successful evaluation output publication advances it once.
Recoverable provider failure clears the reservation without advancing, and a
retry selects again. A wrong or second selection/evaluation returns
`CALL_ORDER_MISMATCH` under the detailed ABI priority rules.

This is represented entirely with the existing standard-call builder and
ordinary local symbols. It adds no hidden PU formal, global counter, DSL
opcode, mapped-image section, or common/com API. Selection is a control call
outside the 87-static/147-dynamic evaluation census and has a separate exact
87/147 selector census.

### Canonical Tensor To Runtime-Handle Projection

SYNC-5 requires an owner-safe transition from canonical tensor-valued PU and
call interfaces to standard WHIRL interfaces carrying opaque runtime handles.
The optional append-only `.WHIRL.dsl_runtime_interface` image records that
transition without changing any canonical tensor, DSL value, PU formal, call
ABI, source, REGION, or FHE planning row.  Those earlier rows remain immutable
provenance.

The generic plan/apply service is split deliberately:

```text
DSL_Runtime_Interface_Plan_Validate(...)
DSL_Runtime_Interface_Apply_PU(active_pu, ...)
DSL_Runtime_Interface_Validate_PU(active_pu, ...)
```

The plan covers the complete program with stable IDs, but `Apply_PU` touches
only the currently selected PU and its active local symbol and map tables.  It
creates one exact handle TY/ST projection for each selected source value,
rebuilds that PU's `FUNC_ENTRY`, function TYLIST, call parameters, and return
stores, and appends fixed projection rows.  The backend driver remains
responsible for selecting every PU and invoking the service under the normal
per-PU compilation scope.  No WN, local ST, or borrowed query result survives
a PU transition.

Each value row records the canonical source value/ST/TY, the projected handle
ST/TY, and one of these structural roles:

- local runtime value;
- by-value input formal; or
- hidden caller-owned result formal carrying a pointer to the exact handle TY.

Each call row joins a stable callsite and actual ordinal to the selected value
projection and callee formal ordinal.  Inputs become by-value, read-only,
passed-not-saved handle loads.  Results use a caller-owned handle local,
initialized to NULL before the call and passed as a typed pointer-to-handle
output parameter.  Exact TY identity is required: ciphertext and plaintext
handles remain different TYs even when both use the same pointer-width machine
type.

Canonical DSL call metadata may associate an actual and formal with different
ordinals.  Standard WHIRL and the C ABI are positional, so runtime-interface
v1 deliberately requires `actual_ordinal == callee_formal_ordinal`.  A
non-positional canonical call must be normalized or adapted by a separately
reviewed semantic transformation before projection; v1 never silently swaps
arguments.

The service does not infer encryption roles, choose provider operations, lower
FHE semantics, or select handle types.  FHE-owned propagation must first
certify shape and context-sensitive encryption state, then select the exact
runtime role and handle TY for every participating value.  Only after those
decisions are complete may the generic projection run.  Standard-call
construction follows projection.

Preflight verifies complete formal and call coverage, exact owner/value/ST/TY
identity, legal role/sentinel combinations, physical source ABI agreement,
cross-PU formal agreement, and duplicate absence before mutating the active
PU.  A later failure is terminal for the checkpoint transaction; the driver
must not retry or continue conversion in the same process.  Final PU
validation proves the rebuilt prototype and calls match the fixed rows and
that executable WHIRL no longer references canonical tensor STs.  Lifecycle
locals created solely by standard-call lowering do not require projection
rows.

The v1 mapped image has a 32-byte header, 48-byte value rows, 40-byte call
rows, and 8-byte ELF section alignment.  Unknown or malformed rows fail
closed.  Older readers may ignore the optional section, but they cannot safely
process the projected physical ABI as the original tensor interface; the
standard-WHIRL checkpoint and current gatekeeper are therefore the supported
consumer boundary.  `ir_b2a -st -src` prints logical value and call projection
tables while ordinary WHIRL printing continues to show the resulting
`FUNC_ENTRY`, `OPR_PARM`, `OPR_CALL`/`OPR_VCALL`, loads, stores, and control
flow.

### Final Unlowered Gate

`fhe_unlowered_gate.h` provides a PU-local structural verifier and a
registered FHE semantic verifier.  Before `.mid.B` publication and immediately
before `W2C_Outfile_Translate_Pu`, it must reject:

- any executable native `OPR_DSL` carrier, including a surviving common or CNN
  operation that the FHE schedule should have consumed;
- any executable transitional `OPR_XPRAGMA` or `OPR_EVAL` DSL carrier;
- any live FHE, SIHE, CKKS, HPOLY, or materialization logical operation;
- any unconsumed managed REGION that still represents FHE execution;
- malformed call prototypes, parameters, status capture, output ownership, or
  source positions; and
- any FHE semantic event lacking exactly one matching standard call.

Historical comments and accepted provenance rows may remain inspectable.  They
must not be executable and must not cause `whirl2c` to decode FHE semantics.
The semantic callback owns the exact 87-static/147-dynamic census and ABI
symbol mapping; the shared gate owns tree integrity and invocation timing.

Stable diagnostics should reserve `CFHELOWER-*` for lowering failures and
`CFHEMID-*` for the final standard-WHIRL boundary.

## Driver Placement

For the normal `-O0` FHE path, the reviewed order is:

```text
FHE conversion
  -> shape refinement when invalidated
  -> FHE materialization
  -> FHE-owned CKKS schedule/planning checkpoint (.ckks.B/.T)
  -> FHE runtime-call lowering
  -> final unlowered-FHE gate
  -> normal language VHO/lowering only as applicable to standard WHIRL
  -> final unlowered-FHE gate immediately before whirl2c
```

The `.mid.B` checkpoint uses the same per-PU traversal and binary-last
publication protocol as the conversion and materialization checkpoints.  It
writes each lowered PU while its local scope is active, performs complete
program and schedule validation after all PUs, and publishes the binary last.
Checkpoint mode does not continue into code generation.

The second gate before `whirl2c` protects non-checkpoint flows and catches any
later phase that reintroduces or fails to consume an unsupported node.  It is
not a second lowering pass.

The shared framework must preserve the required
`secure_resnet20.ckks.B`/`secure_resnet20.ckks.T` review boundary.  The
FHE-owned schedule and CKKS planner produces it from the certified conversion
and materialization records before standard-call lowering.  Shared code does
not infer, synthesize, rename, or bypass that artifact family.

## `whirl2c` And Generated-C Boundary

`whirl2c` remains a consumer of standard WHIRL.  Acceptance requires:

1. `application.mid.B` reopens through the ordinary mapped-image reader;
2. `ir_b2a -st -src application.mid.B application.mid.T` shows only standard
   executable WHIRL plus nonexecuting provenance;
3. the final gate passes before `whirl2c` is called;
4. generated C uses the public ABI v1 header and opaque C handles, with no ACE,
   OpenFHE, DSL-builder, WN, or private provider type;
5. an unlowered-node negative emits no generated C; and
6. ordinary non-FHE WHIRL produces unchanged C.

The same gate applies to the backend `w2c_only` path immediately before
`W2C_Outfile_Translate_Pu`.  A rejected input must publish no final generated
C file; removal of an unpublished temporary output is acceptable.  Standalone
`whirl2c` remains unchanged and relies on the admitted standard-WHIRL input
contract.

Generated-C compilation/link orchestration belongs to the FHE/mock test lane,
not to `whirl2c` syntax handling.

## Dependency State

PR #152 completed the certified SYNC-4 materialization prerequisite before
this implementation batch began.  The main and FHE SYNC-5 branches therefore
share the same merged materialization-image and semantic baseline.  SYNC-5
keeps its runtime-lowering infrastructure and FHE semantic implementation in
coordinated but independently reviewable commits.

### Propagation and runtime-interface dependency

The semantic runtime lowerer must not repair tensor/runtime type differences by
changing canonical tensor `TY_IDX` values. Before it emits a standard call, it
consumes successful generic shape and FHE encryption-state propagation and
selects one exact runtime role for every live value. Generic runtime-interface
infrastructure then records and applies the owner-qualified projection from the
unchanged DSL value and tensor TY to an exact ciphertext- or plaintext-handle
TY/ST.

The required order is:

```text
shape certification
  -> context-sensitive FHE state certification
  -> exact runtime-role selection
  -> program-level PU/call interface projection
  -> standard-call emission
  -> final unlowered-node and semantic census gate
```

Input handles cross a PU boundary by borrowed value. Hidden result handles are
caller-owned null-initialized locals passed through exact pointer-to-handle
formals. Canonical tensor values, PU-interface rows, call-ABI rows, source
positions, TensorDescriptorIR, and FHE planning records remain inspectable
provenance and are not retyped. A migration failure is terminal for the
checkpoint and publishes no `.mid.B` or auxiliary artifact.

The detailed analysis, role mapping, transaction contract, diagnostics, and
negative-test matrix are in
`doc/FHE-SHAPE-AND-ENCRYPTION-STATE-PROPAGATION.md`.

## First Main-Owned Batch

The first main-owned coding batch now provides:

1. the callback-only per-PU runtime-lowering framework;
2. detached standard-call/status/output/control-flow construction with a
   two-call focused test;
3. the structural unlowered-node gate and a native-carrier negative;
4. an all-PU `.mid.B` checkpoint using the existing generic checkpoint;
5. gate invocation before the existing `whirl2c` translation call; and
6. proof by build and source isolation that `whirl2c` itself needs no source
   change.

The focused linked test also proves exact prototype construction, source
positions, return-status capture, checked failure control flow, pointer output
slots initialized to NULL, exact actual/formal TY checks, wrong-pointee and
same-name/prototype-conflict rejection, failure-path access to captured locals,
function TY/ST reuse, stable checkpoint-path delivery across PUs,
disabled-phase behavior, callback registration, result aggregation, and
rejection of an executable native DSL carrier. It also constructs the exact
selector-then-evaluation sequence and proves descriptor-local flow, immediate
status-checked ordering, exact opaque TY identities, unchanged PU formals, and
absence of a global cursor variable.

The FHE workstream may implement ABI v1 headers, descriptor selection,
reservation/mock behavior, and schedule producers concurrently. Its next
coordinated step is to bind the semantic runtime lowerer to these interfaces,
register the final semantic verifier, and certify the exact evaluation and
selector 87-static/147-dynamic censuses through `.mid.B`, `ir_b2a -st -src`,
unchanged `whirl2c`, and mock-runtime execution.

PR #156 closed the post-PR #155 program-interface gaps. The merged generic
transaction now prunes verified-dead BatchNorm formals and caller actuals,
promotes root-owned live plaintext values to launcher-supplied runtime inputs,
and threads model and composite-coefficient resources through reused PUs. The
FHE consumer can therefore resolve a source value to its exact
owner-qualified projected handle and resolve a semantic resource role to the
exact program input handle. A focused fixture also constructs and verifies a
detached descriptor-select plus bootstrap call sequence using those resolved
handles and ordinary standard WHIRL.

PR #157 closed the next boundary with the reviewed atomic transaction. The FHE
consumer now supplies a detached complete ReLU standard-WHIRL block whose
final `STID` defines the exact projected runtime output handle. The successful
focused transaction emits six selector calls plus refresh, normalization,
three polynomial stages, and reconstruction; it removes the executable
`common.relu` definition and retains the logical node/value as lowered
provenance. The root model and three coefficient handles are explicit program
inputs. Promoted external tensors use the transaction's source-elision mode;
runtime-only resources have no source DSL definition and remain excluded.

S5-2c is no longer blocked on shared infrastructure. It remains fail-closed
until the FHE pass expands this pattern to every admitted full-model operation,
collects complete per-PU request arrays, registers the production semantic
callback, proves the exact 87-static/147-dynamic census, and atomically
publishes the complete `.mid.B` artifact family.

The original evidence, rejected workarounds, PR #156/#157 resolution, and current
consumer sequence are recorded in
`doc/FHE-SYNC5-RUNTIME-ENTRY-BINDING-GAP.md`.

The focused consumer checkpoint is reproduced by
`osprey/be/vho/tests/fhe_semantic_runtime_lower_test.sh`. It runs the linked
contract producer, writes `runtime_call_resolution.B`, reopens that binary in a
separate `ir_b2a -st -src` process, extracts the target PU trace, and checks
the exact six-selector/one-refresh/one-normalization/three-stage/one-
reconstruction census. It also checks the four root resource roles, lowered
`common.relu` provenance, absence of an executable ReLU in the target PU, and
retains commands, diagnostics, and SHA-256 hashes under the selected artifact
directory.

The same FHE-owned semantic builder also admits the five remaining evaluation
classes in ABI v1: `conv2d_plain`, `residual_add`, `average_pool`,
`layout_convert`, and `linear_plain`. Each request resolves exact
owner-qualified projected operands, enforces its closed operand count and
ciphertext/plain-handle role constraints, and builds one descriptor selector
followed by one checked evaluation. Composite ReLU stages remain on their
specialized path and are rejected by this generic operation entry point. The
linked fixture exercises every admitted kind and its exact selector kind,
static ordinal, function symbol, output type, status check, and unsupported-
kind rejection. Production callback registration and source-node-to-static-
ordinal mapping remain the next S5-2c boundary.

`common.output_logits` is deliberately outside the evaluation census. Its
lowering uses a zero-call identity block whose sole final store copies the
input ciphertext handle to the exact projected output handle. It does not
invent an ABI entry point, descriptor selector, output allocation, or status
check. The same atomic native-value transaction removes its executable DSL
definition while retaining lowered logical provenance.

The schedule preflight orders physical evaluation definitions by stable PU
source-definition identity and DSL node identity, then propagates execution
multiplicity through the canonical callsite graph. A ReLU definition occupies
six consecutive static ordinals; Conv2D, residual add, average pool, layout
conversion, and linear occupy one; tensor sources and output logits occupy
none. Cyclic calls, unknown live executable operators, ambiguous ownership,
and count overflow fail before mutation. Applied to the retained six-PU
SecureResNet SYNC-4 binary, the independent mapped-image test finds 32
physical evaluation definitions and proves exactly 87 static evaluations and
147 execution-expanded events. The optional
`OPEN64_FHE_RUNTIME_SCHEDULE_INPUT` lane in the focused script retains this
result as `schedule-census.log`.

The same mapped-image preflight now derives the complete program-interface
request census from stable semantic tables rather than source symbol names.
It uses call-ABI semantic roles to identify 48 unique dead BatchNorm formals
and all 80 matching caller actuals. It joins live Conv call arguments, the
root Conv fold, and the classifier node to prove 44 distinct external
plaintext sources: 42 folded Conv weight/bias tensors plus classifier weight
and bias. The resulting flow requires four runtime-only resources, 44 root
source bindings, 24 shared-callee plaintext slots, 24 model/coefficient
resource bindings across six PUs, and 76 rooted call edges. This census is a
no-mutation prerequisite to constructing and validating the complete
`DSL_PROGRAM_INTERFACE_PLAN` and `DSL_RUNTIME_INTERFACE_PLAN`.

## Reviewable Commit Sequence And Acceptance Gates

This section is the canonical detailed execution sequence for the remaining
SYNC-5 work. The consolidated and integration plans summarize the milestone;
they do not replace the commit-level gates below. A commit may be split for
review size, but it must not combine a later gate with an incomplete earlier
gate or claim full-model certification from a focused fixture.

### S5-A: Checked Runtime Operation Construction

**Status:** complete in local commits `7388b98e`, `93ed75fb`, and `17476faf`.

**Purpose:** establish the standard-WHIRL construction pattern independently
of whole-program ABI mutation.

**Implementation scope:**

1. Resolve canonical DSL values to exact owner-qualified runtime handles.
2. Resolve `fhe.model` and three coefficient roles through program-input
   bindings rather than globals or source names.
3. Build descriptor selection followed by checked evaluation for
   Conv2D/plain, residual add, mandatory-refresh composite ReLU, average pool,
   layout conversion, and linear/plain.
4. Lower output logits as a zero-call identity assignment.
5. Capture every status result, initialize every output handle, preserve
   source positions, and finish each computed block with one store to the
   exact projected output.
6. Submit focused definitions through
   `DSL_IR_Lower_Native_Values_To_Standard_Blocks` and retain logical
   `status=lowered` provenance.

**Primary owned files:**

- `osprey/be/vho/fhe_semantic_runtime_lower.{h,cxx}`
- `osprey/be/vho/tests/fhe_runtime_lower_contract_test.cxx`
- `osprey/be/vho/tests/fhe_semantic_runtime_lower_test.sh`

**Required review evidence:** exact ABI symbol, operation kind, operand count,
handle TY, selector-before-evaluation order, source position, output store,
status check, unsupported-kind rejection, and separate-process
`ir_b2a -st -src` reopen.

**Exit criterion:** every evaluation class needed by SecureResNet has an exact
detached standard-WHIRL builder; this does not yet imply complete model
lowering.

### S5-B: Deterministic Full-Program Runtime Schedule

**Status:** complete in local commit `90ec2afd`.

**Purpose:** freeze descriptor/evaluation ordinals before operation mutation.

**Implementation scope:**

1. Order physical evaluation definitions by stable PU source identity and DSL
   node identity.
2. Assign one static ordinal to each one-call operation and six consecutive
   ordinals to each composite ReLU.
3. Propagate PU execution multiplicity through the canonical call graph.
4. Reject recursive/cyclic calls, unknown executable operators, ambiguous
   value ownership, and count overflow.
5. Exclude tensor sources and output-logits identity from the evaluation
   census.

**Positive full-model expectation:** six PUs, 32 physical scheduled
definitions, 87 static evaluations, and 147 context-expanded evaluations.

**Negative tests:** unknown live operator, cyclic call graph, malformed owner,
and overflow must fail before mutation.

**Retained evidence:** `schedule-census.log` beside the focused runtime
lowering artifact family.

**Exit criterion:** repeated runs over the same mapped input produce the same
source-node-to-static-ordinal mapping and exact 87/147 totals.

### S5-C: Whole-Program Interface Request Census

**Status:** complete in local commit `f43dfb4e`.

**Purpose:** prove the exact population of the later program/runtime interface
plans before creating handle symbols or rewriting any PU.

**Implementation scope:**

1. Use call-ABI semantic roles to identify dead BatchNorm inputs and live Conv
   plaintext inputs. Source ST names are not classification keys.
2. Deduplicate dead callee formals by `(callee PU, canonical formal ordinal)`.
3. Join root BatchNorm-fold provenance to the folded stem Conv operands.
4. Join the live linear node to classifier weight and bias.
5. Prove each selected plaintext source is a live root-owned tensor constant.
6. Derive rooted model/coefficient flow across all canonical callsites.

**Positive full-model expectation:**

| Population | Exact count |
| --- | ---: |
| Dead BatchNorm formals | 48 |
| Matching dead caller actuals | 80 |
| Folded Conv plaintext tensors | 42 |
| Classifier plaintext tensors | 2 |
| Runtime-only resources | 4 |
| Root source bindings | 44 |
| Shared-callee plaintext bindings | 24 |
| Model/coefficient bindings across six PUs | 24 |
| Total runtime bindings | 92 |
| Runtime-input call edges | 76 |

**Exit criterion:** the census is deterministic, owner-qualified, and derived
only from persisted semantic identity tables and logical operands.

### S5-D: Construct And Prevalidate Complete Interface Plans

**Status:** implementation started; the typed TCON lookup prerequisite is
implemented and focused validation is in progress before request construction.

**Purpose:** materialize the S5-C identities into complete immutable
`DSL_PROGRAM_INTERFACE_PLAN` and `DSL_RUNTIME_INTERFACE_PLAN` arrays before
the first PU is changed.

**Implementation checklist:**

1. Create retirement requests for all 48 dead formals and 80 corresponding
   call arguments, retaining canonical IDs and semantic roles.
2. Create 44 `SOURCE_EXTERNAL_TENSOR` inputs with exact source owner, value,
   tensor TY, TCON, external reference, and plaintext handle TY.
3. Create `fhe.model` as an opaque resource and the three coefficient inputs
   as TCON-backed resources with exact coefficient tensors.
4. Create all 92 root/threaded bindings in canonical contract order.
5. Create all 76 runtime-input calls, joining caller and callee bindings by
   stable callsite and semantic role.
6. Create projections for every effective live formal, hidden result,
   operation operand/result, promoted source, and retained call value. Exclude
   redirected values and verified-dead BN inputs.
7. Assign ciphertext handle TY only to encrypted activations/results and
   plaintext handle TY only to folded weights, biases, and coefficients.
   Canonical tensor `TY_IDX` values remain unchanged.
8. Create call projections for every surviving input and hidden result edge
   across all nine callsites.
9. Reject duplicate, missing, cross-owner, wrong-direction, wrong-handle-TY,
   unrooted-resource, and incomplete-plan cases before mutation.
10. Call `DSL_Program_Interface_Plan_Validate` on the complete arrays and keep
    those arrays byte-stable for every later PU callback.

**Tests:** request permutation determinism, missing final request, wrong source
owner, wrong plaintext/ciphertext TY, unrooted resource, duplicate retirement,
missing hidden result, and all-PU plan fingerprint mismatch.

**Native contract resolution:** every
`DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR` request requires the exact
`source_tcon`. The backend-safe
`DSL_IR_Image_Get_External_Tensor_Reference()` view currently exposes the
external side file, key, byte range, checksum, descriptor type, element type,
shape, and layout, but not the tensor's attached `TCON_IDX`. Recovering that
index by parsing the owner-local ST metadata key `tensor_tcon_idx` would
violate the reviewed structured-access boundary and is not permitted in the
FHE pass. BatchNorm provenance also cannot supply a complete substitute
because the classifier weight and bias are not BatchNorm-fold outputs.

`DSL_IR_EXTERNAL_TENSOR_REFERENCE` now includes `TCON_IDX tensor_tcon`, and
`DSL_IR_Image_Get_External_Tensor_Reference()` populates it without changing
mapped-image rows or binary WHIRL. The service requires the active owner PU
and validates the exact value,
ST, and TY. A present TCON must be nonzero and side-file-dense, with exact
descriptor type, path, range, dtype, and shape agreement; absence maps to zero
for legacy source tensors. Focused common tests must reject malformed metadata
and path/range/type disagreement without mutation and preserve mapped-reopen
behavior. S5-D requires a nonzero result while constructing all 44 external
plaintext input requests exclusively through this typed service.

**Exit criterion:** one complete plan validates against the real six-PU input
without modifying WN, ST, TY, or managed image tables.

### S5-E: Apply Program And Runtime Interfaces Across Six PUs

**Status:** pending S5-D.

**Purpose:** perform the reviewed ABI transition while each PU's local symbol
table and map table are active.

**Implementation checklist:**

1. Apply exactly one owner-qualified slice through
   `DSL_Program_Interface_Apply_PU` during normal preorder PU traversal.
2. Compact dead BN formals and matching call actuals while retaining immutable
   provenance rows.
3. Promote 44 root external tensors to launcher-supplied plaintext handles.
4. Thread model, three coefficients, and context-specific plaintext handles
   through five shared callee PUs without cloning them.
5. Rebuild each `FUNC_ENTRY`, function TYLIST, call actual list, and hidden
   result path once.
6. Prove input handles are borrowed by value, hidden results are caller-owned
   null-initialized pointer-to-handle outputs, and exact TY identity agrees on
   both sides of every call.
7. Run `DSL_Program_Interface_Validate_PU` after each apply.

**Negative tests:** wrong active PU, colliding local ST indexes, later-PU
failure, missing incoming threaded edge, call-to-root edge, and cycle. Any
post-commit failure is checkpoint-terminal and publishes no artifact.

**Exit criterion:** all six PU/call interfaces match persistent interface rows
and every runtime handle has a rooted initialization path.

### S5-F: Atomic Full-Model Operation Lowering

**Status:** pending S5-E.

**Purpose:** replace every admitted executable DSL definition with standard
WHIRL using the frozen schedule and projected handles.

**Implementation checklist:**

1. Collect each PU's complete lowering request array before changing its first
   definition.
2. Use S5-B records for exact static ordinals and ABI operation kinds.
3. Build 13 Conv2D/plain, five residual-add, 11 physical composite-ReLU,
   average-pool, flatten/layout-convert, and linear/plain sequences.
4. Build output logits as a zero-call identity outside the 87-call census.
5. Elide promoted external sources through `PROMOTED_SOURCE_ELISION`.
6. Submit computed and elision requests atomically through
   `DSL_IR_Lower_Native_Values_To_Standard_Blocks`.
7. Preserve source node, result value, owner, position, attributes, lineage,
   and lowered relation.
8. Reject operands without exactly one projected handle and final stores that
   do not define the exact projected result.

**Tests:** invalid last request rollback, wrong ordinal, missing coefficient,
wrong Conv plaintext TY, wrong result ST, duplicate source definition, and
surviving promoted source.

**Exit criterion:** every executable definition is replaced once and per-PU
counts sum to exactly 87 evaluations and 87 descriptor selections statically.

### S5-G: Production Callback, Final Gates, And Atomic Checkpoint

**Status:** pending S5-F.

**Purpose:** integrate S5-D through S5-F into the all-PU runtime-lowering phase
and publish only a complete middle-WHIRL checkpoint.

**Implementation checklist:**

1. Register the FHE gatekeeper and production lowering callback.
2. Prepare and authenticate immutable schedule/interface plans on the first
   callback; require the same fingerprint for every later PU.
3. Accumulate exact PU, definition, selector, evaluation, output, status, and
   elision counters.
4. Run structural and semantic verification after each PU.
5. Require six converted PUs, 32 scheduled definitions, 87 static/147 dynamic
   evaluations, matching selector events, complete lowered relations, and
   valid program/runtime images in the finalizer.
6. Run lowered-interface and final unlowered-FHE gates.
7. Register schedule/report auxiliaries and publish
   `secure_resnet20.mid.B` last as the commit marker.
8. Clear retained process state on success and every failure path.

**Negative tests:** failure at each PU position, counter mismatch, plan change,
finalizer failure, stale destination, and signal cleanup. No final or `.tmp`
artifact may survive rejection.

**Exit criterion:** the six-PU `.mid.B` family publishes atomically and
contains no executable DSL/FHE carrier.

### S5-H: Inspection, Generated C, And Mock Executable Certification

**Status:** pending S5-G.

**Purpose:** close SYNC-5 without depending on an ACE installation.

**Implementation checklist:**

1. Reopen `secure_resnet20.mid.B` separately with `ir_b2a -st -src` and retain
   `secure_resnet20.mid.T`.
2. Verify standard functions, calls, parameters, status paths, output handles,
   source positions, interface rows, and lowered provenance.
3. Run unchanged `whirl2c`; generated C may include only the public FHE C ABI
   and ordinary C/Open64 support headers.
4. Compile and link generated C against the standalone mock runtime.
5. Execute the mock with the deterministic schedule manifest and verify exact
   ordering, kinds, ordinals, resource roles, output ownership, and failures.
6. Retain source, input `.B/.T`, `.mid.B/.mid.T`, phase trace, generated C,
   commands, executable log, manifests, diagnostics, and `SHA256SUMS` in a
   host-visible directory.

**Negative tests:** unlowered carrier, malformed manifest, wrong ordinal,
missing resource, wrong handle TY, mock status failure, and generated-C
publication failure.

**Final SYNC-5 exit criterion:** generated C from the complete six-PU
SecureResNet middle-WHIRL artifact compiles, links, and passes the deterministic
mock runtime. Evidence proves exact 87/147 evaluation and selector accounting,
no executable DSL/FHE nodes, and no partial output on failure. ACE ANT provider
execution remains SYNC-6.
