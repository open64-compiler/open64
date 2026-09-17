# IPA DSL Tensor Shape Propagation TODO

## Status

Incubating research and action list. This document does not authorize a new
binary section, IPA summary record, command-line option, cross-PU mutation, or
released API. It collects work that becomes relevant only when Open64 is
invoked through the `-ipa` path and IPA establishes call-graph compilation
scope.

The ordinary VHO shape-refinement pass remains per-PU. Driver iteration over
multiple PUs is coverage, not interprocedural propagation. Nothing in this
document may be implemented by making `VHO_DSL_Shape_Refine_Driver()` traverse
or reactivate another PU.

## Objective

Investigate an IPA-owned tensor-shape analysis that can propagate reviewed
TensorDescriptorIR facts across caller/callee boundaries, specialize PUs when
different calling contexts require different signatures, and leave each
resulting PU suitable for the existing per-PU VHO refinement pass.

## Existing Open64 Model To Preserve

1. IPL analyzes one active PU and emits procedure summaries.
2. IPA builds the program call graph from IPL summaries.
3. IPA traverses `IPA_NODE` and `IPA_EDGE` objects and uses
   `IPA_NODE_CONTEXT` when a transformation must activate a PU's symbol,
   WHIRL, map, REGION, DST, feedback, and memory-pool context.
4. WOPT, LNO, VHO, and CG remain intraprocedural consumers even when their
   results inform an IPA summary.
5. Existing IPA symbol/type remapping and merged-global-table services remain
   authoritative. DSL code must not retain unmapped file-local `TY_IDX` or
   `ST_IDX` values across files.

## Research Queue

### IPA-S0: Pipeline and summary inventory

- Trace the exact IPL-to-IPA phase order for `-ipa`, including local Preopt,
  summary production, call-graph construction, constant propagation, cloning,
  inlining, dead-function elimination, and output rewriting.
- Identify existing `SUMMARY_PROCEDURE`, formal, actual, callsite, value,
  symbol, and type records that can carry or reference tensor boundary facts.
- Study IPA global symbol/type merging and index remapping before choosing any
  TensorDescriptorIR identity in a summary.
- Identify when IPA transformations invalidate call edges, actual/formal
  correspondence, summaries, and inferred tensor descriptors.
- Record which existing tracing, skip-list, and option conventions the new
  pass must follow.

Exit question: can the first analysis use existing summaries plus managed DSL
images, or is an append-only versioned IPA summary extension required?

### IPA-S1: Interprocedural semantic contract

- Define the exact facts exported by each PU: formal and hidden-result tensor
  descriptors, return relationships, pending or symbolic dimensions,
  representation constraints, and shape-generation state.
- Separate canonical tensor type facts from source metadata, runtime state,
  profile data, and FHE value state.
- Define actual-to-formal and result-to-caller constraint rules by logical DSL
  operator and call ABI version.
- Specify conservative behavior for external calls, indirect calls, varargs,
  incomplete summaries, mixed-language calls, recursion, and unknown callees.
- Decide whether inference is forward, backward, or bidirectional at each call
  edge. No direction should be assumed merely because a descriptor is present.
- Define stable diagnostics for incompatible call contexts and unresolved
  public interfaces.

Exit gate: one reviewed, monotonic lattice and boundary-transfer contract.

### IPA-S2: Summary representation and compatibility

- Prefer references to canonical semantic facts over duplicating full tensor
  descriptors in every call edge.
- Define file-local versus merged-global identities and all required remapping.
- If new summary rows are required, specify fixed-width layout, revision,
  alignment, ownership, reader/writer behavior, malformed-input validation,
  previous-reader behavior, and `ir_b2a` or IPA-summary inspection evidence.
- Ensure absence of the optional tensor summary preserves legacy IPA behavior.
- Keep Python and frontend-builder APIs out of IPA and `be.so`.

Exit gate: binary and mapped-image compatibility review before implementation.

### IPA-S3: Call-graph solver

- Attach tensor boundary facts to `IPA_NODE` and `IPA_EDGE` through an
  IPA-owned analysis object.
- Solve acyclic call-graph portions in a deterministic order.
- Define strongly connected component handling for recursion and mutually
  recursive PUs, including convergence and widening or fail-closed policy.
- Track dependencies so a refined formal, actual, or return fact reschedules
  only affected nodes and edges.
- Preserve context sensitivity explicitly. Repeated calls to one definition
  are calling contexts, not operator versions.
- Produce analysis-only evidence before permitting WHIRL or signature changes.

Exit gate: deterministic check-only propagation over multi-file call graphs.

### IPA-S4: Specialization and mutation policy

- Define when all calling contexts may share one refined PU signature.
- Define when incompatible contexts require IPA cloning or specialization
  rather than weakening a descriptor or silently selecting one caller.
- Reuse IPA cloning, call-edge update, symbol-table context, and output rewrite
  mechanisms. Do not create a parallel cross-PU mutation framework in VHO or
  common/com.
- Specify atomicity across clone creation, formal and return retyping, caller
  actual/result updates, call graph updates, summaries, managed DSL rows, and
  rollback or terminal failure.
- Preserve canonical tensor-type immutability and lookup-before-create
  uniquing after IPA symbol/type remapping.
- Define ownership for source position, class/instance identity, lineage, and
  specialization provenance.

Exit gate: reviewed mutation contract and check-only evidence demonstrating a
real need for specialization.

### IPA-S5: Interaction with existing IPA transformations

- Decide the pass placement relative to IPA constant propagation, inlining,
  cloning, dead-function elimination, and partitioning.
- Define which IPA transformations preserve shape facts and which invalidate
  or require recomputation.
- Ensure newly inlined or cloned PUs re-enter the existing per-PU VHO shape
  refinement when later compiled by the backend.
- Do not make WOPT, LNO, or VHO interprocedural as a side effect of this work.
- Define behavior when IPA chooses not to materialize or optimize a callee.

Exit gate: a fixed, option-controlled IPA pipeline with explicit invalidation.

### IPA-S6: Option, diagnostics, and inspection

- Propose an IPA-owned option following existing `config_ipa` conventions;
  exact spelling and default remain TBD.
- Preserve full driver option propagation: IPL and IPA consume their own
  options and silently ignore unrelated options.
- Add phase tracing for summaries, edge transfers, SCC iterations, conflicts,
  clone decisions, and final interfaces.
- Define review artifacts for pre-IPA summaries, post-propagation call graph,
  transformed `.B`, and `ir_b2a -st -src` output.

Exit gate: a developer can disable only IPA tensor-shape propagation and
compare retained artifacts without changing ordinary per-PU refinement.

### IPA-S7: Scoped validation

Cross-PU tests begin only after IPA-S3 introduces cross-PU behavior. Required
eventual cases include:

- one caller and one callee with compatible static refinement;
- multiple callers sharing one compatible callee interface;
- incompatible calling contexts requiring deterministic specialization;
- caller/callee result propagation and hidden-result conventions;
- recursive and mutually recursive SCCs;
- indirect, external, mixed-language, and missing-summary calls;
- multi-file type and symbol remapping;
- inlining and cloning invalidation;
- malformed or previous-version summaries;
- `-ipa` disabled, proving ordinary per-PU behavior is unchanged;
- failed interprocedural refinement publishing no misleading final artifact.

The existing per-PU shape suite remains responsible for local fixed points,
REGION handling, atomic local retyping, and VHO pipeline legality. Do not
duplicate those tests merely because an IPA fixture contains several PUs.

## Open Decisions

1. Whether tensor boundary facts fit existing IPA summaries or need an
   append-only summary capability.
2. Whether symbolic dimension expressions can be remapped without a new
   persisted identity table.
3. Whether the first IPA pass is analysis-only or may refine compatible public
   signatures without cloning.
4. The context key used to decide specialization and clone reuse.
5. The interaction between shape specialization and existing IPA constant
   propagation annotations.
6. The required whole-program atomicity and recovery model when several input
   files are involved.
7. Whether profile-guided context selection is ever legal for shape, which is
   primarily a semantic legality property rather than a profitability fact.
8. The first motivating model and smallest reproducible cross-PU case.

## Coordination Rule

Before coding begins, review IPA-S0 and IPA-S1 with both IPA and DSL owners.
The first implementation PR should be check-only and must not add cloning,
signature mutation, or a binary summary extension unless those contracts have
already passed separate review.

