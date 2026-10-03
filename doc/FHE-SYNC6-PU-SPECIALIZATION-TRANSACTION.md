# SYNC-6 Whole-PU Specialization And Typed Call Transaction

Status: detailed design for review, not an implementation or certification
claim. The FHE consumer requirements are in
`FHE-SYNC6-CONTEXT-SPECIALIZATION-CONTRACT.md` and
`FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md` on the FHE task branch. Linked helper
tests exist, but no physical specialized PU or `.ckks_ops.B` has been produced.

## 1. Purpose And Compilation Scope

The input is the SYNC-4 context-bound planning image, conventionally
`secure_resnet20.ckks.B`. The SYNC-5 `secure_resnet20.mid.B` contains terminal
C ABI calls and is a regression reference, not an input to reverse into CKKS
operators. The new checkpoint is `secure_resnet20.ckks_ops.B`, inspected as
`secure_resnet20.ckks_ops.T` with `ir_b2a -st -src`. It sits after
FHE-CNN/layout/materialization planning and before terminal call lowering.

This is an explicit **program-scope correctness preparation** at `-O0`.
Ordinary backend processing writes one PU at a time, so it cannot revisit a
caller after writing it. The new path must inspect every affected definition
and caller before writing any PU. It does not enable ordinary cross-PU
optimization without `-ipa`; normal non-FHE traversal is unchanged.
The physical PU and typed-call transaction is intended to serve other AI
domains too. Only the FHE policy that requests this particular transaction
is specific to CKKS, ReLU, and its approved bounds.

```text
openpy/capture -> context-bound planning .ckks.B
  -> separate backend checkpoint invocation
  -> read globals and affected PU trees/local symtabs/maps/REGIONs
  -> derive complete FHE executable signatures and approved bound routes
  -> generic program transaction: clone/reuse, formals, caller actuals
  -> expand CKKS operators and bind states/events on executable variants
  -> whole-program generic and FHE verification
  -> write all PUs and globals into a private .ckks_ops.B candidate
  -> close and reopen candidate through the mapped WHIRL reader
  -> atomically publish .ckks_ops.B
  -> ir_b2a -st -src .ckks_ops.B .ckks_ops.T
  -> later lower verified CKKS IR to the stable FHE C ABI
```

The checkpoint is its own process. Failure leaves the input image and final
artifact family untouched and terminates that process. This is
**process/artifact atomicity**, not a claim that Open64's global tables can
all be restored and compilation retried in-process. Section 8 records the
stricter alternative and its required review decision.

## 2. Ownership

| Component | Responsibility |
| --- | --- |
| FHE VHO producer | Derive complete CKKS signatures, approved context-to-bound TCONs, state/key/layout legality, root binding, and final semantic checks. No raw WN or private `OPR_DSL` decoding. |
| `osprey/be/vho/dsl_pu_specialize.{h,cxx}` (current) | Read-only program-plan preflight. It currently mixes generic routing with FHE ReLU/range checks; split those responsibilities before making it an apply API. |
| `osprey/be/com/dsl_pu_transaction.{h,cxx}` (initial home) | Domain-neutral program transaction: complete preflight, physical/logical clone, typed formal/call rewrite, staged verification, and owner-qualified result lookup. |
| `osprey/be/com/clone.{h,cxx}` | Reuse `IPO_CLONE` for physical WN, local symtab, and map cloning. |
| `osprey/common/com` | Generic logical rows, typed formals/calls, REGION store, origin image, mapped validation, and printing. No specialization or FHE profitability decisions. |
| Backend driver | Explicit program-scope checkpoint before the ordinary per-PU loop, PU activation/lifetime, all-PU writing, cleanup, and atomic publication. |
| FHE verifier | Prove every executable CKKS state and bound use agrees with routed contexts; reject live unexpanded ReLU/BN, missing event, key, or state. |

`be.so` and `lw_inline` must remain link-closed with no `DSL_Builder_*` or
unapproved library symbols. Python and `torch2whirl` retain opaque handles.

### Reusable AI Specialization Boundary

Treat specialization as a service with separate **policy**, **transaction**,
and **checkpoint** owners. The policy constructs a canonical executable
signature, chooses source-PU variants and complete call routes, and supplies
typed additional formal/actual requests. It also proves domain legality.
For FHE, this policy belongs in `osprey/be/vho/fhe_pu_specialize.{h,cxx}`
(proposed): it checks `OPR_DSLRELU`, authenticated context ranges, positive
`F8` TCONs, and CKKS state/key rules. An AI policy may instead specialize a
shared module for a reviewed tensor shape, layout, precision, or kernel
interface; the existing VHO AI variant-planning phase is a natural caller.
Neither policy edits physical WN or mapped rows directly.

The backend-neutral `dsl_pu_transaction` service in `osprey/be/com` checks
complete program ownership, signature identity, clone/reuse, typed formals,
callsites, actuals, hidden results, source positions, REGION interfaces, and managed-image
consistency. It reuses `IPO_CLONE` and returns owner-qualified IDs, not
borrowed WN or local-table pointers. `osprey/common/com` owns only the
generic IR records, their construction/validation services, and the private
image mutation support needed by this transaction. The backend driver owns
PU activation and the separate-process checkpoint lifecycle. Keep all
domain profitability and semantic choices out of `common/com` and the
generic transaction.

`be/com` is the initial implementation location because the transaction
reuses its existing clone machinery and serves backend program processing.
Revisit the directory if a later IPA or other program-scope consumer needs a
different shared linkage boundary. Such a move must preserve the generic
transaction contract and domain/driver ownership split; location alone does
not justify moving policy or mapped-image definitions into `be/com`.

The generic request must not have a `positive_bound_tcon`, ReLU value, or
FHE context-range field. Describe each added argument by a stable role,
exact `TY_IDX`, pass convention, insertion slot, source kind (such as a
caller-owned value or exact TCON), and owner-qualified source identity. The
FHE adapter maps its approved scalar `B` to this request; it remains
responsible for proving positivity and the context-range join. An AI
adapter can use the same transaction with a tensor TY and a different
semantic proof. New formals still precede hidden results; no implicit
tensor-to-scalar or runtime-handle conversion is permitted.

Maintain the current `VHO_DSL_PU_Specialization_Plan_Validate()` as a
read-only compatibility entry point while callers migrate. Move its
FHE-specific checks into the FHE adapter, then let both FHE and AI clients
invoke the same generic preflight/stage/commit service. Do not rename or
repurpose a published mapped row or alter the existing WHIRL image merely
to perform this source-code split.

The `-O0` invocation here is an explicit program-scope **correctness**
checkpoint required to produce executable, type-correct FHE IR. It does not
license automatic cross-PU AI optimization in ordinary per-PU VHO.
Profitability-driven cloning across PUs requires an explicit program-scope
optimization pipeline such as `-ipa`; the same transaction can execute an
approved plan there. A single-PU pass must not infer or apply routes for
other PUs on its own.

### Proposed Native Surface

Keep request arrays and temporary maps in process memory, not mapped rows.
The existing `VHO_DSL_PU_Specialization_Plan_Validate()` remains a read-only
compatibility entry point. The generic transaction performs the complete P0
preflight and should have these semantics (names are proposed, not published
API):

```c++
BOOL DSL_PU_Transaction_Preflight(
    PU_Info *program, const DSL_PU_TRANSACTION_PLAN *plan,
    DSL_PU_TRANSACTION_DIAGNOSTIC *diagnostic);
BOOL DSL_PU_Transaction_Stage(
    PU_Info *program, const DSL_PU_TRANSACTION_PLAN *plan,
    DSL_PU_TRANSACTION **transaction,
    DSL_PU_TRANSACTION_DIAGNOSTIC *diagnostic);
BOOL DSL_PU_Transaction_Commit(
    DSL_PU_TRANSACTION *transaction, DSL_PU_TRANSACTION_RESULT *result,
    DSL_PU_TRANSACTION_DIAGNOSTIC *diagnostic);
void DSL_PU_Transaction_Abort(DSL_PU_TRANSACTION *transaction);
```

The transaction object is opaque outside the owner module. A domain policy
callback supplies the plan; the driver invokes the generic transaction once
before its PU traversal. The result exposes owner-qualified lookup, not raw
cloned WN pointers: `(source_pu_st, signature) -> executable_pu_st`,
`(source_pu_st, source_value_id, executable_pu_st) -> executable_value_id`,
`callsite_id -> final callee`, and `(executable_pu_st, added_formal_slot) ->
formal value/ST/ordinal`. Domain expansion uses these queries only while the matching
PU is active. Do not retain borrowed local-table or WN pointers across PU
switches. A process-terminal failure discards the transaction and forbids
retry; `Abort` still releases staged REGION and temporary artifacts for
ordinary pre-commit rejection.

## 3. Complete Input Plan

The existing FHE-shaped `DSL_PU_SPECIALIZATION_PLAN` is a read-only start,
not the reusable apply contract. The generic apply plan has source groups,
executable signatures, routes, and typed argument requests; the FHE adapter
supplies the fourth group's approved bounds as one kind of argument request:

1. **Source groups:** global source function ST, source PU identity, the
   selected signature for the existing PU, and additional signatures needing
   clones. A local ST_IDX never identifies a value without its owning PU.
2. **Executable signatures:** domain/schema version, source function
   identity, canonical byte sequence, and SHA-256. The generic transaction
   compares complete canonical bytes; a digest alone is not semantic
   equality. In the first generic service, `signature_sha256` is a
   producer-supplied diagnostic fingerprint: its lowercase syntax and
   cross-variant consistency are checked, but the service does not recompute
   it. A domain policy using that digest as provenance must authenticate it
   before submitting the plan. Each policy specifies which executable properties
   enter the versioned encoding. FHE includes ordered logical CKKS
   op/version, operand/result TY and representation, static circuit
   constants, effects, layout/slots, state/level/scale/components,
   keys/rotations, and schedule. Call ordinal and `F8` bound bytes are
   excluded only when `B` is a genuine input formal and all circuit behavior
   is otherwise equal. An AI policy must similarly justify which shape,
   layout, precision, and interface properties distinguish variants.
3. **Routes:** every physical managed callsite ID, caller PU ST, original
   callee PU ST, selected variant, and source/context identity. Cover every
   call to a specialized source. An untracked, indirect, recursive, or
   unsupported nested call cannot silently remain on an incompatible body.
4. **Bounds:** exact source ReLU value/static ordinal, approved context-range
   ID, positive finite `MTYPE_F8` TCON, bound slot, and callsite ID. Called
   contexts have nonzero callsite IDs. An entry/root context is a separate
   request with callsite zero and exact entry owner; it creates an entry-owned
   constant, never a fictitious formal or call.

The authenticated FHE context-range row, not metadata text, is the authority
for `B`. Compare exact TCON identity/bytes and type, not rounded host text.
The observed ResNet profile has 19 ReLU contexts: 18 called bound actuals
and one root-owned bound. Each called block variant has two independent ReLU
bound slots. These counts are certification evidence, not generic limits.

Choose the canonical existing-PU signature deterministically; clone the
other signatures. This matches the observed lower bound of nine reachable
executable PUs while retaining all six source FUNC_ENTRYs. The assignment
must appear in the plan; ReLU-only levels cannot fix the final variant count
before all CKKS operators are known.

## 4. Typed Formal And Call ABI

The v1 `B` interface is scalar `MTYPE_F8` by value with exact formal/actual
`TY_IDX` agreement. No tensor or runtime-handle conversion is implicit. A
required packed plaintext is created by an explicit `ckks.encode` (or
reviewed equivalent) consuming the scalar formal and producing a new typed
value/state.

For `I` existing inputs, `R` hidden results, and `B` new bounds:

```text
old entry/call: input[0..I) | hidden_result[0..R)
new entry/call: input[0..I) | bound[0..B) | hidden_result[0..R)
```

Original inputs/results retain relative order and exact TY. Hidden results
remain trailing, as in `DSL_Builder_Rebuild_PU_Entry` and
`DSL_Builder_Create_PU_Call`; their ordinals shift by `B`. Appending bounds
after hidden results is rejected. Build a new function TY/TYLIST per
variant, without mutating the old function or canonical tensor TY. Preserve
call flags, source position, result guards, and return convention.

Create one `SCLASS_FORMAL`, DSL value, and PU-interface row per bound slot;
mark the ST as a value parameter and call `Set_ST_Srcpos`. For each routed
call, initialize a caller-owned `F8` temporary from the exact approved TCON,
then pass its `LDID` in a read-only, passed-not-saved, by-value `OPR_PARM`.
Create a caller-owned value and exact ordinal call-ABI row. The temporary
uses the callsite source position. The callee verifier must prove each bound
formal feeds its intended normalization operand, not merely that ABI rows
exist. The root bound follows the same TCON/value discipline but has no
formal or call-ABI row.

## 5. Read-Only Preflight

Complete preflight before `New_ST`, `New_PU`, `Save_Str`, output open, or
`IPO_CLONE::New_Clone`. A rejection leaves process state unchanged.

1. Validate existing DSL, callsite, call-ABI, PU-interface, REGION, tensor,
   FHE-plan/range/state, and CKKS-event images. Reject terminal runtime-call
   WHIRL as an input to this phase.
2. Enumerate PU and call trees. Prove each source/caller has a valid global
   function ST, matching physical PU, local symtab, map table, source
   identity, and ordinary FUNC_ENTRY.
3. Reject recursion, alternate entries, unsupported nested PUs/calls, and
   PU-local statics until `IPO_CLONE::Promote_Statics` side effects are
   reviewed. Do not permit an unplanned global mutation.
4. Match every physical call to callsite/ABI rows, callee formals, return
   convention, TYs, flags, and ownership. Reject unregistered calls,
   unresolved indirect calls, or address-taken uses of a targeted function.
5. Check complete canonical signature bytes, deterministic original-PU
   assignment, clone-name uniqueness, route coverage, and compatibility of
   all contexts reusing a variant. Bound slots are dense and correspond to
   the same source ReLU roles across reused contexts.
6. Join exact context range, callee/caller identity, callsite, source ReLU,
   TCON type/bytes, positive finite `B`, and source position. Prove new
   formal/call counts and function prototype are representable.
7. Construct an immutable manifest of affected PUs, old/new ordinals,
   planned clone/value/REGION/call rows, and expected counts. Hash it for
   deterministic replay. Do not retain borrowed local WN/ST pointers after
   switching the active PU.

The current `VHO_DSL_PU_Specialization_Plan_Validate` covers only part of
this list; it must not authorize mutation yet.

## 6. Program-Scope Stage And Commit

The driver retains every affected PU tree, local table, map, and managed
REGION store for program lifetime before `Preorder_Process_PUs` writes any
caller. Only one local symtab is active at a time. Each operation activates
its owner, then restores the prior PU, map tab, and scope.

The insertion point is after global-image setup and `Phase_Init()` but
before the first `Preorder_Process_PUs()` call in `driver.cxx`. Refactor a
small load/activate service around `Read_Local_Info()` and
`Save_Local_Symtab()`; do not call all of `Preprocess_PU()` as a discovery
pass because that routine also initializes optimizer/REGION state and PU
pools. Its already-in-memory branch must subsequently recognize the prepared
symtab/map for both original and clone PUs. Recompute traversal and progress
counts after linking variants. The specialized checkpoint opens output only
after preflight/staging succeeds; the existing conversion checkpoint's
early-open sequence is not reused unchanged.

Stage in deterministic `(source PU, signature bytes)` order:

1. Create each variant's global function ST and PU entry. Reuse
   `IPO_CLONE` for WN/local-symtab/map copying and DRA's precedent for DST
   source identity. Do not link a staged clone into the PU traversal yet.
2. Clone and reown executable DSL nodes, values, references, attributes,
   formals, and supported nested call rows with fresh IDs and a typed
   source-to-clone map. Copy the managed REGION store to corresponding cloned
   WNs, retaining interface roles and owner-local ST ordinals. Do not copy a
   live RID pointer; normal REGION initialization builds clone-local RID
   state later.
3. Specialize each existing or cloned PU to its assigned signature. Insert
   bound formals before hidden results and build a new entry/prototype.
   Stage the entry/root B constant independently. Expand CKKS operations
   using executable-owner values, never source-owner local STs.
4. For each caller, stage bound STIDs/values and a replacement OPR_CALL with
   final callee ST, F8 PARMs, shifted hidden-result PARMs, source position,
   and original flags. Stage corresponding callsite and ABI updates without
   extracting the old call.
5. Verify the whole staged graph: exact route-to-variant, one actual per
   formal, original results in order, valid REGION interfaces, no cross-PU
   local ST, and complete generic/FHE image semantics.
6. Commit only preflighted operations: link variant PU_Info objects in
   deterministic order, install entries/prototypes, replace old calls in
   their owning BLOCKs, update runtime call associations and managed rows,
   and record origins. Reverify. Any unexpected late failure is terminal;
   it cannot proceed to ordinary per-PU writing or publish output.

`IPO_CLONE` alone is not transactional: it may promote statics, allocate
global entries, and change scope/map state. The existing private logical
row savepoint and REGION discard helpers cover only their own stores.
`strtab.h` exposes insertion/deduplication but no public rollback. The
recommended first implementation therefore runs the entire stage and commit
in the dedicated checkpoint process, not a reusable in-process transaction.

## 7. Origin, Mapped Image, And Output

Keep a typed process-local result map while staging and expanding:

```text
(source_pu_st, source_value_id, source_static_ordinal,
 executable_variant_st, context_identity, callsite_id)
  -> (executable_value_id, CKKS event IDs, final result value ID)
```

`DSL_CKKS_EVENT_RECORD` already has origin owner/value/static-ordinal
fields. Bind those during variant expansion; do not infer them from names,
metadata strings, or colliding local ST indices. Recount the 147 certified
source-context high-level events through one or more CKKS steps, including
19 pre-ReLU refreshes. Step count need not equal 147.

If final `.T` inspection must show source-to-clone relations for formals or
values that create no CKKS event, add a separately reviewed optional
fixed-row clone-origin image. It needs owner-qualified source/variant PU,
value/node IDs, signature schema/identity, and original call-route identity.
Do not overload reserved fields or metadata strings. Audit the section
registry (currently through `WT_DSL_CKKS_EVENT`) before allocating a tag;
land fixed widths/alignment, writer, reader, printer, validator, malformed
mapped-load tests, and old-reader fail-closed evidence together. No existing
section, TY kind, or physical WN encoding changes silently.

No PU is written before all affected PUs are final. Write every PU with its
own active local symtab/map through `Write_PU_Info`, then globals, and close
the ELF mapped image. Reopen the closed private candidate in another process
before an atomic no-clobber publish. Existing side payloads remain immutable;
any new auxiliary output joins the checkpoint's registered-artifact
transaction. On writer, reopen, or final gate failure, remove the candidate
and publish no final binary.

## 8. Failure Contract And Decision Gate

| Contract | Guarantee | Additional work |
| --- | --- | --- |
| Dedicated checkpoint process, recommended v1 | Failure terminates the worker; original input `.B`, caller process, and final artifact family remain unchanged. No retry in the mutated worker. | Explicit separate-process phase and all-PU retention in that worker. |
| Reusable in-process transaction | The same process may retry from exactly the old WN, ST/PU/TY/TYLIST/TCON/STR/DST/map, REGION, and managed-image state. | New audited savepoint/restore services for every global table, including deduplicating STR and DST. Current helpers are insufficient. |

The FHE consumer contract asks for complete rollback. **Review gate:** the
FHE owner must explicitly accept process-isolated failure as equivalent for
SYNC-6 certification, or main/common must implement the stricter in-process
service before enabling the pass. Do not silently conflate them. Assertions
and handled signals must run checkpoint cleanup and leave neither final nor
temporary valid-looking output.

## 9. Validation And Retained Evidence

1. Negative plan cases: malformed signature/collision, duplicate name,
   wrong owner, missing route, bad range/TCON, nonfinite or nonpositive B,
   wrong TY/flags, hidden-result displacement, recursion, alternate entry,
   local static, unsupported nested call, and untracked function use.
2. Inject failures after physical clone, REGION copy, formal construction,
   first and last call staging, image update, CKKS expansion, writer close,
   and mapped reopen. Prove the selected failure contract and no published
   final/temporary artifact at every boundary.
3. Small positive: at least three real FUNC_ENTRYs (source, clone, and caller
   with two callsites). The callsites share the clone with distinct B actuals;
   test two bound slots in one callee, a shifted hidden
   result, source positions, REGION interface, typed origin; reopen `.B` in
   a separate process.
4. Full positive: retain source `.ckks.B`, specialized `.ckks_ops.B`, both
   `ir_b2a -st -src` `.T` traces, FHE report, source/clone/call census,
   signature hashes, and exact commands in a host-mounted artifact family.
   Show a focused before/after WHIRL diff for review.
5. Count reachable executable PUs separately from total FUNC_ENTRYs; prove
   six source PUs remain inspectable, all routes agree, 18 called B actuals
   plus one root binding, exact 15/17/18 target states, 147 source-context
   events, and no live executable ReLU/BN. Recompute CKKS operation,
   rotation, key, depth, and result-state counts from materialized IR.
6. Rebuild `be.so`, `be`, and `lw_inline`; inspect symbols for builder or
   new library dependencies. Run native syntax/target-layout and old-image
   compatibility tests. Any optional origin section needs previous-reader
   fail-closed proof.
7. Add a non-FHE AI fixture with one shared callee, two distinct reviewed
   tensor signatures, two callsites, and typed actuals. Prove both physical
   variants, correct routes and formal TYs, separate-process `.B` reopen,
   and `ir_b2a -st -src` evidence. Passing only FHE tests does not certify
   that the transaction is domain-neutral.

## 10. Implementation Checkpoints

| Point | Main/common deliverable | FHE synchronization |
| --- | --- | --- |
| P0: plan | Canonical signature schema, original-PU assignment, complete routes/root bounds, chosen failure contract, supported PU subset. | FHE signs off signature and context/B join. |
| P1: clone | `IPO_CLONE` physical PU, local tables/maps/DST, logical origins, REGION copy, deterministic placement, small binary roundtrip. | FHE checks source-to-clone owner/value mapping. |
| P2: interface | F8 formals before hidden results, exact TCON actuals, prototype/callsite/ABI batch, root constant, no stale calls, failure tests. | FHE checks two slots and distinct B values on reused variants. |
| P3: checkpoint | Pre-traversal driver hook, all-PU retention, cleanup, complete gates, mapped reopen, atomic publish, compatibility. | FHE registers producer/semantic callbacks. |
| P4: CKKS gate | Clone-aware `.ckks_ops.B`/`.T`, exact origin/event census and before/after traces. | FHE owns legality, numerical checks, and later terminal C ABI lowering. |

The generic `be/com` transaction now implements P0-P2 for the supported
non-nested, non-address-taken PU subset. Its producer test retains a physical
three-PU `.B` and `ir_b2a -st -src` trace, with two distinct F8 bound actuals,
a root-owned constant, logical origins, call/comment/ABI evidence, and REGION
evidence. The dedicated `-DSL:pu_specialization_checkpoint=<path>` backend
path retains all PUs, invokes a registered domain policy once, verifies the
result, and atomically publishes the binary image. Without a policy it fails
before opening output. Generic checkpoint wiring is not FHE SYNC-6
certification: the FHE producer must still register its plan and post-apply
semantic callbacks and demonstrate the CKKS-operation `.B`/`.T` gate in P4.

When `OPEN64_WHIRL2C` is provided, the producer test also retains
`pu_transaction_w2c.B`, its `ir_b2a -st -src` trace, and `pu_transaction_w2c.c`.
This standard-WHIRL ABI view preserves the generic PU, the physical specialized
PU with its typed `bound_b` formal, and the two routed calls with distinct
bound constants. Its body returns the input tensor directly; it is not a
translation or lowering of the executable `common.relu` node in
`pu_transaction_apply.B`. That DSL-bearing image remains the semantic proof
and must pass DSL lowering before `whirl2c` can translate its computation.
