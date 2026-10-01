# FHE SYNC-5 Runtime Entry And Resource Binding Gap

Status: closed by PR #156 and PR #157; full-model consumption is FHE-owned

## Purpose

PR #153 supplies owner-safe tensor-value runtime projection, PR #154 supplies
dynamic descriptor selection, and PR #155 supplies the public ABI and mock
provider. Those contracts are sufficient to construct an individual checked
runtime call, but they do not yet supply every runtime handle required by the
complete six-PU SecureResNet program. Full S5-2c lowering must stop at this
boundary rather than emit uninitialized locals, hidden process state, or an
incomplete projected interface.

## Source And IR Evidence

The retained SYNC-4 artifact has these relevant facts:

- the root `SecureResNet20` source PU has one encrypted input formal and one
  hidden result formal;
- the FHE entry contract also identifies 107 source parameter values;
- BatchNorm conversion materializes 42 live folded weight/bias tensors for 21
  call contexts while retaining dead source BN ABI values as provenance;
- three composite-ReLU coefficient tensors are TCON/profile resources rather
  than ordinary source `DSL_IR_VALUE_ID` values; and
- five shared block PUs are reached through nine callsites and 129 original
  call-argument rows.

The ABI requires every evaluation call to receive an
`open64_fhe_model_v1_t`. Convolution, linear, and polynomial-stage calls also
receive explicit plaintext handles. The ABI deliberately does not model
weights, biases, or coefficients as process-global or provider-private
implicit state.

The focused descriptor-selection fixture currently creates `fhe_model` and
`fhe_anchor` as local symbols solely to certify call construction. It does not
define their full-program origin. Copying that fixture into the semantic
lowerer would therefore emit an uninitialized model handle.

The generic runtime-interface validator also requires every source formal and
every call-ABI argument to appear in the projection plan. That rule conflicts
with the accepted FHE requirement that verified-dead BatchNorm-only formals
remain provenance but disappear from the executable runtime interface.

## Required Contracts

### 1. Dead Canonical ABI Pruning

Add one generic, transactional program-interface operation that removes a
proved-dead callee formal and its corresponding caller actuals from every
callsite. The request must identify the owner PU, source formal value and
ordinal, every callsite/actual relation, and the proof that no executable WN,
REGION interface, return, DSL operand, or runtime projection uses the value.

Preflight must cover the complete request array before mutation. Commit must
rebuild callee `FUNC_ENTRY`/prototype and caller `OPR_CALL` parameters with a
deterministic old-to-new ordinal map. Existing PU-interface and call-ABI rows
remain inspectable provenance and acquire an explicit retired/nonexecuting
status through a reviewed append-only contract. No name parsing or foreign
local-symbol-table access is allowed.

### 2. Entry Parameter Promotion

Add a generic projection binding for a root-PU local semantic value that is
supplied by the launcher as an exact by-value runtime handle formal. This is
needed for the live converted Conv weights/biases and classifier parameters.
It must not mutate the canonical tensor `TY_IDX` or pretend the original local
tensor constant was already a physical formal.

The transaction must:

1. prove the root owner and exact source value/ST/TY;
2. prove the value is a live admitted external tensor parameter;
3. append a source-linked exact handle formal in deterministic role order;
4. record the source-value-to-handle projection for mapped reopen and
   `ir_b2a -st -src`;
5. let semantic lowering consume the handle and retire the executable tensor
   constant definition; and
6. reject missing, duplicate, dead, wrong-class, or wrong-owner parameters
   without mutation.

### 3. Runtime Resource Threading

The model handle and three ReLU coefficient handles have no canonical tensor
formal path that the current projection service can reuse. Introduce a
generic runtime-resource threading transaction with stable role strings, exact
opaque handle TYs, root entry formals, callee formals, and caller actuals.

Required initial roles are:

```text
fhe.model
fhe.relu.coefficient.stage0
fhe.relu.coefficient.stage1
fhe.relu.coefficient.stage2
```

These are explicit generated-program inputs, not hidden invocation-context
IDs. Threading `fhe.model` through each PU preserves the frozen ABI signatures
without a process-global current model. Threading coefficient handles keeps
the explicit-plaintext ABI rule and avoids reconstructing runtime resources
from TCON bytes inside generated C.

The preferred generic contract records each role and exact owner/call
relationship in inspectable mapped evidence. If extending
`.WHIRL.dsl_runtime_interface` v1 cannot remain backward compatible, use a new
optional append-only section rather than changing the existing exact-sized
header or rows.

## Rejected Workarounds

- uninitialized owner-local model or plaintext handle symbols;
- process-global or thread-local current-model/current-descriptor state;
- parsing source names to rediscover weight, bias, or coefficient roles;
- retaining and projecting dead BN inputs merely to satisfy complete-coverage
  validation;
- embedding ACE ANT types or provider objects in WHIRL or generated C;
- changing the frozen evaluation-call signatures; and
- inventing DSL values for runtime-only resources without a reviewed identity
  contract.

## Required Tests

The main/common contract must include:

- two-PU pruning with multiple callers and ordinal compaction;
- rejection when a purported dead formal has an executable, REGION, return,
  or DSL-reference use;
- root entry promotion of distinct ciphertext and plaintext handle TYs;
- model plus three coefficient roles threaded through a shared callee reused
  by multiple callsites;
- exact source positions and owner-safe local `ST_IDX` collision coverage;
- no-mutation rollback for an invalid final request in a multi-request batch;
- mapped reopen and `ir_b2a -st -src` evidence;
- immediately previous reader compatibility for any new optional section; and
- proof that the resulting standard WHIRL contains no executable canonical
  tensor use for a projected or pruned value.

## FHE Consumer Sequence After Publication

After the contracts merge, the FHE semantic lowerer will:

1. certify shape and context-sensitive CKKS state;
2. identify the live 42 folded Conv tensors, classifier parameters, model, and
   three coefficient resources;
3. prune verified-dead BN ABI inputs;
4. promote/thread runtime resources and apply owner-safe projection;
5. lower each logical operation to descriptor selection plus its ABI call;
6. retire executable tensor constants and logical operators;
7. run the final unlowered-node and exact 87-static/147-dynamic census gates;
   and
8. publish `.mid.B` and its auxiliary schedule artifacts atomically.

## Post-PR #156 Resolution And Current Boundary

PR #156 published the generic program-interface transaction required by the
first three steps above:

- verified-dead formals and all matching caller actuals can be pruned as one
  owner-safe program edit;
- live root external tensors can be promoted to exact launcher-supplied
  runtime input handles while retaining their canonical tensor/value rows as
  provenance; and
- role-qualified runtime resources can be threaded through the root and
  shared callee PUs without globals, name parsing, or invented tensor values.

The FHE consumer has exercised those contracts with exact owner-qualified
value lookup and role lookup. It also constructs a detached checked
`open64_fhe_operation_desc_select_v1` followed by
`open64_fhe_bootstrap_v1`, preserving source position and using the merged
`fhe.model` program input plus the exact projected ciphertext handle. This
proves handle origin and standard-call construction, but deliberately leaves
the logical source node and physical DSL definition unchanged.

PR #157 supplies the remaining atomic native-value lowering transaction.
Its relation is a closed tagged union:

1. `COMPUTED_STANDARD_BLOCK` replaces one executable native DSL definition
   with a detached standard-WHIRL block whose final store defines that source
   value's exact projected runtime output handle.
2. `PROMOTED_SOURCE_ELISION` removes the executable external tensor-constant
   definition without adding a store because the root launcher supplies the
   corresponding promoted runtime input.

Both modes retain logical node/value rows and source evidence, mark them as
lowered/nonexecuting provenance, reject incomplete or inconsistent arrays
before mutation, and commit in deterministic physical definition order.
Runtime-only model and coefficient resources have no source DSL definition and
therefore need no request. The FHE consumer has certified computed mode with a
complete source-ReLU block containing six selector/evaluation pairs and one
final projected-result assignment. Full-model operation coverage, all-PU
callback registration, census verification, and `.mid.B` publication remain
FHE-owned; no further common/com API gap is known at this checkpoint.
