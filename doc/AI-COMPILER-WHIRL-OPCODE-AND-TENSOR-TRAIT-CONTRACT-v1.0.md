# AI compiler WHIRL opcode and tensor trait contract

Version v1.0 | 2026-11-12 | Governing documentation clarification

AI compiler optimization uses native WHIRL operation contracts, semantic tensor
descriptors and tensor traits, and existing Open64 analyses. This contract
governs the interpretation of the design and implementation-plan series,
including embedded historical revisions. It clarifies representation and API
ownership; it adds no compiler implementation, registered API, option, binary
format or backend dependency. Historical milestone and hardware acceptance
claims retain their original scope. The accompanying
[audit](AI-COMPILER-WHIRL-ARCHITECTURE-AUDIT-v1.0.md) identifies corrected files
and retained originals.

## Representation authority

| Information | Native authority | Required interpretation |
| --- | --- | --- |
| Operation identity and semantics | WHIRL opcode and, for logical DSL operations, exact logical operator/version contracts | Operand/result rules, attributes, effect declarations and allowed levels are checked through the existing native registry and gatekeepers |
| Semantic tensor properties | TY/TensorDescriptorIR and tensor traits | Element type, rank, shape, layout, strides and admitted semantic/encoding roles follow the existing canonical descriptor rules |
| Static fusibility properties | Exact opcode/version property records | Iteration/indexing class and registered eligibility describe an operation; they do not establish legality of a particular fusion site |
| Proven facts about a program site | Phase-owned access, DU, alias, dependence, numerical and effect analyses | Facts have a compilation scope, source binding, lifetime and validity generation |
| Schedule and resource alternatives | Owned detached planning records | Tiles, ownership, staged-buffer generations, events, resource estimates and costs are alternatives until selected and committed |
| Provenance and empirical evidence | Compiler metadata, FEEDBACK and bound profile records | Source names, pass ownership, measurements and model estimates remain separate from canonical tensor-type equivalence |

Two tensors with equal types and cache-role traits can name different storage.
A cache trait does not prove alias exclusion, residency, readiness, live length,
scale generation or reuse safety. Such obligations need owned program facts
and, where applicable, Q3 serving bindings and runtime guards. An opcode effect
declaration similarly needs concrete operand/storage/alias analysis at a site.

The existing `DSL_FUSIBILITY_INFO` and
`DSL_Operator_Get_Fusibility_Info()` in
[dsl_opcode.h](../osprey/common/com/dsl_opcode.h) are opcode/version property
records and lookup services. Their source may use the word `trait`; that
spelling does not create a second tensor-trait system. VHO consumes the
properties and performs site-specific checks in
[dsl_fusion_candidate_opt.cxx](../osprey/be/vho/dsl_fusion_candidate_opt.cxx).
This clarification neither moves these static properties into tensor types
nor renames the existing APIs or public enums.

## Native phase legality

The lowering legality service is a set of native phase admission and exit
checks. Its inputs are the active driver-established scope, current WHIRL
level, exact opcode/version and type contracts, tensor traits, required
canonical forms, numerical permissions, valid analysis generations, target
capabilities and any selected-plan obligations.

Each check classifies a construct as admitted, requiring a supported semantic
lowering, conditionally admitted with explicit verified guards, or unsupported.
These are conceptual outcomes, not newly registered enum values. Unknown
alias, dependence, shape, lifetime, numerical or target obligations cannot
authorize a transformation. Use a representation-compatible admitted fallback
or diagnose before emission. Profitability cannot override a legality failure;
representation legality alone does not prove semantic equivalence.

Structural checks and policy-free record lookups remain with their established
owners. The phase performing the transformation owns semantic legality and
its analysis requirements. PREOPT canonicalizes for its downstream consumers;
VHO owns domain/common semantic preparation, LNO owns canonical-loop reasoning,
WOPT owns CFG/SSA reasoning, and IPA owns explicitly enabled cross-PU work.
The driver controls scope, options, lifetime and output publication. This
contract preserves the
[phase ownership audit](AI-COMPILER-OPTIMIZATION-PHASE-OWNERSHIP-AUDIT.md).

## Scheduling service contract

Scheduling is an owning-phase service over immutable owned facts. The existing
[High snapshot records](../osprey/be/lno/dsl_high_snapshot.h) and
[detached MLA search](../osprey/be/vho/tensor_attention_search.h) are bounded
implementation anchors; generalization to other algorithms or schedule
providers requires its own admission and implementation evidence.

1. Capture stable loop, access and storage identities, canonical bounds and
   indexing, source/activation/mutation epochs, dependence/alias/effect facts,
   numerical permissions and target/resource/model identities. Copy facts
   needed beyond native manager lifetime into owned records.
2. Propose and estimate detached complete plans under global budgets and
   declared convergence rules. Exploration creates no candidate WN/ST/TY,
   emitted source, compiled kernel, device measurement or correctness run.
   A family without an implemented, qualified materializer remains analysis-only.
3. Revalidate the selected plan against live source, scope, analysis and
   target bindings. The owning phase preflights, atomically applies and
   verifies one selected realization, or restores the prior valid program
   and analyses on recoverable failure.
4. Return loop/access/buffer correspondence, selected semantics and COMMENT
   explanations, diagnostics and a preserved/invalidated analysis record.
   Invalidate and rebuild affected parent, DU, alias, dependence, access,
   resource and selected-plan facts. Rebind facts after subsequent PREOPT.
5. Perform selected correctness/resource qualification outside exploration.
   Keep timing and counter profiling separate. Failure retains an admitted
   fallback; it does not reopen an unbounded candidate-lowering loop.

Wolf unimodular transforms consume valid dependence facts. A wider Cohen-style
polyhedral or hybrid provider is an optional separately qualified proposal
source. Its mathematical schedule representation does not replace WHIRL
opcode semantics, tensor traits or the native application/verifier contract.

## Continuous lowering and exit obligations

| Boundary | Required ownership and exit evidence |
| --- | --- |
| Very High to common High | Domain gatekeeper discharges domain obligations; semantic expansion preserves numerical, storage and effect contracts |
| Canonical High and LNO | DO_LOOP/OPR_ARRAY forms expose bounds, tails and indexing; valid access/dependence facts support estimation and selected loop transformations |
| Selected High exit | Explicit multilevel loops, buffers, staging/prefetch DO_LOOPs, fixed ownership constraints and numerical/mask semantics; readiness, visibility, consumer completion and reuse joins; all Q3 and target obligations bound |
| Mid | Structured memory/address/control lowering realizes selected transfers, conversions/dequantization, guards and synchronization |
| Low | GPU CTA/warp/lane mapping, layouts, address spaces, target operations and completion mechanisms realize the selected contract |
| PTX and whirl2c | Emit and validate the selected native semantics and COMMENT explanations; target qualification checks actual resources and execution separately |

Later lowering and emitters preserve selected decisions. They cannot invent a
missing schedule, choose a new group/chunk/depth, or repair an unproved reuse
claim. Existing fallback, AOT baseline and deferred AOT-selection policy remain
unchanged. O0 performs required straightforward semantic lowering; O1/O2/O3
and IPA retain their established scope contracts.

## Implementation and review obligations

Future implementation work must reuse native registry, descriptor, analysis,
transaction and verifier services before adding a new abstraction. Specify
exact scope, minimum optimization level, controls, inputs, outputs, ownership,
mutation and failure behavior. Extend native opcode or descriptor schemas only
through their normal compatibility review and mapped-image/tooling gates.

Focused implementation tests should distinguish opcode/version properties from
site legality, equal tensor types from storage identity, and semantic traits
from stale planning facts. Exercise rejected/unknown obligations, guards,
fallback, epoch invalidation, selected rollback and phase exits. Native
transformation changes retain before/after B/T/diff evidence under the existing
artifact policy. This documentation correction requires no new native test
run and certifies no GPU correctness or speedup.
