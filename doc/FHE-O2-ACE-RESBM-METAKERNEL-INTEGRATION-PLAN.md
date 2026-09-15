# Open64 FHE `-O2` Integration Plan

Status: discussion-aligned revision proposal; master-plan reconciliation, O0
qualification, and O2 implementation remain open

Plan version: 1.6

Date: 2026-09-15

Reviewed working-tree baseline: `develop@eca97d4843aef03c50e5cb866f2f0e6c82e6b064`

Governing architecture commit: `ee1dc6382246c58f49a3097157a8c4e8ff2440c8`

Detailed delivery target: `O2 Stage 1` (`ACE mapping + MetaKernel + ReSBM`)

Retained final capability: `O2 Stage 2` (`FHEFusion + reviewed HPOLY/HPAO`)

Revision 1.6 retires the separate detailed-execution-plan outline and moves its
remaining normative content into this plan. Sections 12.3-12.5 now define the
three distinct dependency graphs, the engineering-stage-to-milestone mapping,
the detailed stage-plan contract, the plan-set document structure, and the
end-to-end traceability rule. The English detailed execution index and the 13
English stage plans remain subordinate execution documents. This revision does
not authorize implementation and does not modify the master plan or pending ADR.

Revision 1.5 retains the user-selected mature Fhelipe O0 baseline from v1.4 and
records two additional execution decisions. First, **no O2 phase, including
provider-free algorithm or fixture work, starts until the complete external O0
baseline has passed an independent O2-consumer qualification gate**. This is a
deliberate sequencing policy: it sacrifices possible schedule overlap so a
failure can be attributed to either the qualified O0 handoff or one O2 phase.
Second, the runtime provider is fixed to ANT through the public C surface of the
ACE compiler repository's rtlib (`FHErt_common` plus `FHErt_ant`, provider ID
`LIB_ANT`). Open64 remains authoritative for CKKS-and-above semantic IR,
planning, verification, and optimization; finalized CKKS and lower-level
execution is projected to ACE rtlib/ANT.

Revision 1.4 adopted the user-selected planning direction: the frozen mature
Fhelipe layout/lowering pipeline and its default DP bootstrap placement form
the proposed O0 baseline. O2 owns enhancements beyond that baseline. This
supersedes v1.3's attempt to move Fhelipe's existing cost-guided passes into O2.
The DP baseline conflicts with the current v0.10 O0 policy and is not yet an
accepted architecture or executable baseline. F0 records the required explicit
reconciliation; this revision edits only this O2 plan, not architecture or O0
source trackers. This v1.5 edit likewise does not modify the master plan. The
master-plan wording for the O0 baseline and runtime boundary remains pending
collaborator agreement and must be reconciled before `F0`/O0 qualification can
close. HPAO-MU/FM/LM and the MD design gate remain unchanged.

## 0. Proposed Baseline Policy and Cross-Plan Ownership

### 0.1 Selected planning direction and existing architecture conflict

The user has selected a mature Fhelipe baseline rather than a new reduced
packing/bootstrap implementation. The adoption unit is the fixed source
revision plus an accepted pass/configuration/support manifest, not every future
feature carrying the Fhelipe name. Its existing layout assignment, compaction,
conversion hoisting/decomposition, lowering and default DP placement remain
baseline capabilities even when upstream calls them optimizations.

| Owner | Proposed scope after F0 reconciliation | Required evidence |
| --- | --- | --- |
| O0/Fhelipe execution owner | Frozen mature Fhelipe layout/lowering and default DP initial bootstrap placement, with accepted rescale/state handling; required canonicalization, immediate Relin, ACE rtlib/ANT execution contract and executable acceptance | Full pass/config/source lock, policy-compatible DP adaptation, complete agreed support domain, independent reopen, ANT execution and ResNet-20 acceptance |
| O1 execution owner | Any separately accepted local incremental policy above the same baseline | Preserve the baseline manifest and protected semantics; F0 reconciles the old local-policy mapping without redefining O1 here |
| O2 integration owner | Additional MetaKernel layout/kernel optimization, ReSBM replacement of permitted baseline placement, FHEFusion, reviewed HPOLY/HPAO and individually reviewed further enhancements | Start only after complete O0 consumer qualification; compare incremental effects against the frozen Fhelipe baseline; retain baseline-off controls and semantic/placement provenance |
| Future O3 owner | Physical parallel scheduling and memory/data optimization of the finalized selected semantic plan | Preserve approximation, layout semantics, state, protected bootstrap boundaries, keys and source provenance |

This is a proposed change to the existing DSC O0 definition, not a claim that
v0.10 already permits it. The conflict is precise:

| Current v0.10 requirement | Conflict with proposed baseline | Required reconciliation |
| --- | --- | --- |
| Section 11.4, line 812: O0 is no profitability optimization | Mature Fhelipe includes cost-guided layout passes and DP | Architecture must define the frozen baseline package as baseline policy; names or cost-guided implementation alone no longer assign it to O2 |
| Section 11.4, lines 814, 826: pre-ReLU boundaries and greedy JIT refresh elsewhere | Default Fhelipe DP supplies a different initial placement rule | Explicitly replace or scope the automatic JIT rule; decide protected boundaries and DP domain before O0 acceptance |
| Section 11.4, line 830: no removal/motion/merging/global replacement at O0 | Upstream DP removes bootstrap nodes before planning | Adapt the producer to preserve hard boundaries; do not import unrestricted removal |
| Section 11.5, line 838: O0 disables DP/min-cut planning | Default DP is now the selected baseline direction | Accept the specific DP/related boundary options in the manifest and synchronize architecture/options/source trackers |

Until reconciliation, the existing architecture remains the implementation
contract for any claim of v0.10 conformance. It is not the target comparison
baseline of this revision. A legacy greedy/JIT implementation may remain a
diagnostic profile, but cannot close the proposed Fhelipe baseline gate.
Only governance discussion and this plan revision may proceed; by project
choice, O2 source/oracle implementation also waits for complete O0 qualification.

### 0.2 Paper/code evidence and limits of the baseline claim

MetaKernel Section 5 (PDF page 19) implements the method in ANT-ACE's Vector
IR and uses Fhelipe as a comparison baseline. It is not a code dependency or
patch to Fhelipe. Its Section 1 (PDF page 2) excludes multicore implementation;
its algebraic horizontal/vertical batching remains an O2 increment, distinct
from physical O3 thread scheduling.

Fhelipe Sections 5-6 describe mature layout/lowering and placement passes. The
new project choice retains the frozen pipeline as O0; cost guidance, an
`Optimizer` class name or upstream default is not by itself a reason to strip
a capability from that baseline. Keeping separate layout and CKKS interfaces
allows O2 replacements without making the baseline an indivisible algorithm.

Section 6.2 (PDF pages 15-16) limits the DP search to depth boundaries, with a
fixed bootstrap result budget `l0` and basic complexity `O(d * l0)`. Section
6.3 describes boundary/shortcut refinements. The paper's latency recurrence
and the inspected implementation's bootstrap-count objective are different
evidence claims; neither establishes unrestricted global latency optimality.
Section 8.2/Table 6 reports a 3.5x geometric-mean DP-versus-Lazy speedup in the
paper's evaluation. It is not an Open64 performance prediction, and must not
be attributed to a protected-boundary adaptation before measurement.

| Fixed source | Direct evidence | Adoption consequence |
| --- | --- | --- |
| `../../ace-compiler`, `origin/metakernel-proof@d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be` | `nn-addon/vector/src/vector_utils.cxx:48-83,538-595` and `nn-addon/include/nn/vector/tensor2vector_handler.h:611-620,633-655,694-712`: cost search, IMRA, batching and Ke2Col | O2 candidate above the frozen baseline, with isolated comparison and fallback |
| `../../fhelipe/fhelipe`, `891b3086bf6a144deebac79290801253b9cc510c` | `backend/src/compiler.cc:65-71`, `backend/src/targets/compile.cc:142-154` separate layout, optimization, rescale and bootstrap components | Freeze mature components/configuration as one baseline package while retaining independent semantic owners |
| Same Fhelipe snapshot | `backend/src/layout_hoisting_pass.cc:47-68,94-100` uses legality and cost | Existing baseline pass; no duplicate O2 implementation solely because it is cost-guided |
| Same Fhelipe snapshot | `backend/src/targets/compile.cc:70,82-86` selects `dp` by default; `backend/src/dp_bootstrapping_pass.cc:451-487` minimizes bootstrap count using frontier and shortcut counts | DP is the selected baseline initial-placement policy; lock the actual objective/domain/options separately from paper latency claims |
| Same Fhelipe snapshot | `backend/src/dp_bootstrapping_pass.cc:502-506,554-556` removes all `TBootstrapC` before DP; `:540-546` restores usable levels | Upstream cannot be copied unchanged across DSC hard boundaries; O0 owns protected-boundary adaptation and acceptance |

### 0.3 Migration ledger relative to the frozen baseline

Source sections refer to the previously reviewed tracker snapshot. This revision
does not edit those sources. F0 must synchronize their ownership links and the
architecture conflict explicitly, so pending old text cannot create a second
implementation queue or an accepted-baseline claim.

| ID | Source activity | Proposed owner/disposition | Destination / acceptance |
| --- | --- | --- | --- |
| MIG-01 | FHE-DSL-INTEGRATION-PLAN.md conversion (436-448); architecture 11.4-11.5 | O0 keeps canonicalization, legality/runtime; its automatic placement policy must change through F0 to the selected Fhelipe baseline | External F1/P0/P1 contracts; no O2 dependency for baseline bring-up |
| MIG-02 | FHE-DSL-INTEGRATION-PLAN.md layout/SYNC-7A-E (450-528); consolidated C7/SYNC-7 (177,571-609) | Frozen Fhelipe layout/lowering, including existing optimizers, belongs to O0; common records to common/com; only MetaKernel and additional reviewed layout enhancements belong to O2 | Import baseline evidence; S1.3/S1.4 and S1.LAYOUT compare increments and recompute census; no second Fhelipe implementation |
| MIG-03 | Deferred Work (924-929); consolidated SYNC-7 (573-580); FHE-WHIRL-INTEGRATION-PLAN.md (529,564) | Baseline DP initial placement belongs to O0; ReSBM and permitted placement replacement/fusion, plus FHEFusion beyond baseline, belong to O2 | F1 placement-policy evidence then S1.6-S1.9/S2.0-S2.2/S2.5; hard boundaries preserved |
| MIG-04 | FHE-DSL-INTEGRATION-PLAN.md (440-448); consolidated (606-609); architecture 12.4-12.8 | HPOLY, dedicated SSAPRE-model MU and FM/LM remain O2; MD remains design-gated | S2.3/S2.4 and MD-DESIGN; legacy WOPT SSAPRE unchanged |
| MIG-05 | Architecture 8.8-8.9,11.5-11.7,12.6; consolidated C8/SYNC-8 | New target-aware ReSBM/HPOLY profitability remains O2; baseline's own frozen costs do not become an O2 prerequisite | O2 extension records and source/cost/oracle gates; provider availability remains external |
| MIG-06 | Architecture O3 rows (654,841,844) and GPU/native deferrals | Physical scheduling, NUMA/GPU memory placement, transfer overlap and working-set management remain reserved to third plan | Section 0.6 handoff, no third-plan implementation here |
| MIG-07 | Earlier F1-PREP/F1-IMPL/F1-ACCEPT and P0/P1 queues; v1.3 S1.LAYOUT Fhelipe implementation | O0/source/runtime owners implement the frozen mature baseline and protected DP; O2 imports it and deletes duplicate baseline implementation work | Section 13 handoffs; S1.LAYOUT is incremental verification, with new algorithm work only when defined |

### 0.4 Required architecture and baseline reconciliation gate

The selected direction is settled for this plan; its detailed manifest,
hard-boundary adaptation and architecture acceptance are not yet settled.

| Decision | Remaining contract | Owner / evidence | Blocks |
| --- | --- | --- | --- |
| FRZ-01 | Freeze mature Fhelipe revision, complete layout/lowering/rescale/DP pass order, defaults and supported domain; distinguish future increments | O0/Fhelipe owner; source-to-pass/config manifest, disabled experimental options and delta review | Baseline implementation/acceptance; S1 comparisons |
| FRZ-02 | Reproducible complete source/build/fixtures/oracles | O0/Fhelipe owner; fixed bundle/file/build hashes, license and supported/unsupported corpus | F1-IMPL/F1-ACCEPT |
| FRZ-03 | Resolve v0.10 O0 no-profitability/JIT/no-DP conflicts and protected-boundary policy | Architecture/O0 owners; explicit amendment/ADR, Section 11.3 truth table and constrained-DP feasibility/oracle evidence | Production defaults, F1 acceptance, S1.9/S2.6 |
| FRZ-04 | Auto/manual/off, baseline DP, O1 and O2 increment controls, advanced-off and fallback mappings | Driver/O0/O2 owners; four-level truth table; O2 off preserves baseline passes and DP | F0/default integration |
| FRZ-05 | Baseline layout/CKKS/placement/provenance records first; separate O2 candidate/replanning extensions | O0/common-com then O2 owners; versioned crosswalk and independent reopen | Baseline consumers before P2; O2 schema after P2 |
| FRZ-06 | Frozen-baseline versus increment comparisons, identical DP algorithm/constraints for layout ablations | Performance/O2 owners; profile/support matrix, objective labels, fixed artifacts and controls | S1.LAYOUT/S1.9/S2.6 |
| FRZ-07 | Synchronize architecture and source-tracker links for MIG-01..07 | Architecture/tracker editors; explicit changed/unchanged policy inventory and accepted document diffs | F0 closure |
| FRZ-08 | Freeze ACE rtlib as the runtime interface and ANT as the only required provider; define the Open64 CKKS semantic-plan to rtlib projection | Runtime/Open64 owners; ACE rtlib revision/header/build hashes, generated-C include/symbol allowlist, parameter and operation mapping | P1a/P1b, O0 qualification, all executable O2 gates |
| FRZ-09 | Decide compiler-side rtlib dependency topology and client/server key/context lifecycle | Build/runtime/security owners; explicit `be.so` dependency review or isolated resolver design, plus no-server-secret lifecycle tests | P1b release acceptance; does not reject ANT as the selected provider |

Do not infer that the architecture has accepted DP or the revised rtlib boundary
merely because this execution
plan chooses it. F0 architecture reconciliation is not an O2 implementation gate
for O0: it produces the baseline contract, after which O0 can build and accept
the complete baseline without P2 or any MetaKernel/ReSBM implementation.

### 0.5 Proposed option and fallback contract

Exact syntax for baseline policy/configuration remains an F0 contract; do not
invent an accepted public DP flag. The target semantics after reconciliation are:

- Bare O0 uses the frozen Fhelipe baseline, including its accepted existing
  layout optimizers and DP initial placement when automatic bootstrap is allowed.
- O1 inherits that baseline; any additional local policy is separately defined.
  The old local-only mapping cannot silently disable baseline DP.
- `advanced=off` or an O2 layout/replanning enhancement set to off disables only
  increments beyond the frozen baseline. It preserves baseline layout passes,
  default DP and all required legality/protected-boundary checks.
- `scale-policy=auto` at O2 may select reviewed ReSBM as an incremental
  replacement; with it off or unsupported, use the accepted baseline DP policy.
  `fusion=auto` similarly controls additional FHEFusion, not baseline passes.
- `manual` permits no compiler-created bootstrap and preserves explicit sites;
  `off` forbids bootstrap. These modes bypass automatic DP/ReSBM placement and
  fail if protected-boundary or depth requirements cannot be met. `auto/on`
  uses the reconciled Section 11.3 policy, never unrestricted bootstrap removal.
- O3 inherits the selected result; its parallel/memory controls remain separate.
  Phases receive the full option set and ignore unrelated driver options.

Fallback means the accepted mature baseline, not an invented weaker local/JIT
plan. Failed incremental planning must not publish mixed/stale placement or
layout data. If baseline DP cannot consume a changed layout, reject that
increment or fall back to the whole baseline profile. Retain manifests, protected
sites, source/cost/state provenance and the explicit reason for every fallback.

### 0.6 O2-to-O3 handoff for the reserved third plan

The third plan is not created in this revision. The shared DSC parallel/data
owner consumes independently reopenable finalized layout, iteration-space,
CKKS/HPOLY and placement records, config/target hashes, effect/dependence facts,
key requirements and protected/manual boundaries. It verifies completeness and
staleness before physical scheduling or placement.

Baseline layout optimization and DP belong to the frozen O0 package; new
MetaKernel algebraic packing/batching, ReSBM replanning and HPOLY profitability
belong to O2. O3 maps the selected finalized operations/data to physical threads
and memory hierarchies. It may not rerun baseline DP, extend ReSBM search, or
change logical packing, approximation, scale/level or protected boundaries.
Any required semantic change returns to the owning baseline/O2 planner and
produces a new verified handoff. Parallel/memory-off evidence must preserve the
exact selected semantic plan; O3 execution acceptance remains the third plan's.

## 1. Executive Delivery Decision

| Delivery | O2-owned work | Completion rule |
| --- | --- | --- |
| Stage 1 | Open64/ACE mapping, O2 record/transaction extensions, additional MetaKernel MVM/Conv optimization, ReSBM core/approved replacement planning, incremental comparison and ACE rtlib/ANT materialization | F0 reconciliation accepted; complete external mature Fhelipe/ANT baseline passes `O2-O0Q-001`; P2/S1.0-S1.9 and S1.LAYOUT verification pass |
| Stage 2 | Additional FHEFusion, HPOLY and independently controlled HPAO-MU/FM/LM, MD-DESIGN, recosting/ablations/default certification | All Stage 1 and Stage 2 gates; MD implementation only after separate design acceptance and plan amendment |

```text
accepted baseline source/config/protected-boundary contract
  -> mandatory canonical input
  -> frozen Fhelipe layout/lowering + state/rescale handling
  -> baseline DP initial placement within accepted boundary policy
  -> accepted baseline plan and executable evidence
O2 increment path from the same inputs:
  -> optional additional FHEFusion                     [Stage 2]
  -> baseline layout or additional MetaKernel           [Stage 1]
  -> authoritative state analysis + baseline DP policy
  -> optional permitted ReSBM replacement               [Stage 1]
  -> HPOLY + reviewed HPAO                              [Stage 2]
  -> finalized semantic handoff / standard WHIRL / provider
```

Separate interfaces do not require two planners to own one decision. O0 owns
initial placement; enabled ReSBM explicitly replaces only permitted automatic
placement and records its provenance. Required state transfer, immediate Relin
and hard boundaries remain authoritative throughout. Stage numbers are delivery
milestones, not new public O levels. No new baseline is accepted by this edit.

## 2. Authority, Evidence, and Status Labels

### 2.1 Authority order

1. Explicit user requirements and applicable `AGENTS.md` repository invariants.
2. `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx` and its
   Markdown companion at commit `ee1dc6382246c58f49a3097157a8c4e8ff2440c8`.
3. Accepted versioned contracts and an ADR only for its explicit amendment.
4. This O2 execution plan and the O0/consolidated trackers in their assigned
   scopes; current source and tests prove implementation status, not semantics.
5. Original papers for intent/claims within their assumptions, and fixed
   artifacts for implementation evidence. Source-locked MetaKernel/ReSBM
   production rows retain their explicit artifact-backed deviations below.
6. `../../ace-paper-guide/` for navigation, never independent authority.

The PR suggestion and the user's interpretation were reviewed against both
architecture and research evidence in Section 0. Neither paper names nor an
existing implementation silently determine Open64 O-level ownership. This plan
does not amend architecture or claim paper-exact behavior for artifact-only
extensions. The selected baseline is proposed policy until F0 explicitly reconciles the
identified v0.10 conflict; unchanged architecture is not evidence of acceptance.

The v0.10 master plan remains the current recorded architecture authority while
its successor is being negotiated. Where this v1.6 proposal differs - most
notably the mature O0 baseline sequencing and use of ACE rtlib's public C surface
instead of the former proposed opaque `dsc_fhe_*` facade - the difference is
explicitly `Proposed` and blocks `F0`/`O2-O0Q-001`. This document and the detailed
execution plan set may be revised now, but neither may be used to claim that the
master plan has already changed.

### 2.2 Status labels

- **Verified**: directly observed in a reviewed document, current tree, fixed
  revision, or executed check.
- **Frozen**: accepted behavior that an implementation must preserve.
- **Proposed**: a reviewable choice in this plan, not yet architecture authority.
- **Unknown**: missing evidence or decision that blocks a dependent exit gate.
- **Extension**: Open64 behavior beyond the cited paper, requiring its own
  semantics, oracle, and acceptance gate.

### 2.3 Locked research inputs

| Work | Paper and reviewed SHA-256 | Fixed code evidence | Role |
| --- | --- | --- | --- |
| ACE | `../../ace-paper/ACE_paper.pdf`, `BC9B1AAFB56B179F9D507F071C3A0706AAD706A454912D163CC584EEEAE8ACE5` | `cgo2025-artifacts@15c95a7346d89355d68d5bf4fe8ab3b952740e1a` | Stage 1 architecture/lowering evidence |
| ReSBM | `../../ace-paper/RESBM_paper.pdf`, `C6D46834C37CF534D1D13050E7CB946962E3D7FD810539C347BE21EBEA6C3A00` | `origin/20250524@323e8bb02a0e036fe2369eb48badfe6692398c2b` | Stage 1 production algorithm authority inside the locked support domain; paper-strict behavior is differential evidence |
| MetaKernel | `../../ace-paper/MetaKernel_paper.pdf`, `E2AB5C3C89EBC7B5B6CEA7E79EAA00BC98975EF66EC69104EA458570DDBE2FEF` | `origin/metakernel-proof@d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be` | Stage 1 production algorithm authority inside the locked support domain; paper-strict behavior is differential evidence |
| FHEFusion | `../../ace-paper/FHEFusion_paper.pdf`, `415E2ACFE8E7505263B369BF3B550A190612E48E8E92D66A321C0D8098A24A7E` | `origin/fhefusion@f18f971710b7270aec2c3963878ac3493d9f8e06` | Stage 2 pre-layout work |
| HPAO | `../../ace-paper/HPAO_AE.pdf`, `FAFCEE50E3978FECE2D847DD7B2623C71DB5B8E7CD79226B43917F87188D02FC` | `origin/hpao@990e2289a866397e92c69ebe251adedba46cd44a` | Stage 2 post-CKKS work |
| Fhelipe | `../../fhelipe/fhelipe-paper.pdf`, `6E7A58F934AC64B4A043166980BFA81BFE162EFDFBA371A47D649A5A77DA0D65`; [DOI 10.1145/3656382](https://doi.org/10.1145/3656382) | local `../../fhelipe/fhelipe@891b3086bf6a144deebac79290801253b9cc510c`; inspected component files below | activity-boundary evidence verified; full source bundle/build and implementation acceptance pending |
| ACE rtlib / ANT | no paper claim; public runtime/API source is authoritative | local `../../ace-compiler@929e9b621f11bebbaa9ec1e215f4a52e3d07109b` | Required runtime interface and only release provider; `FHErt_common` + `FHErt_ant`, `LIB_ANT` |

The inspected Fhelipe evidence files at that commit have SHA-256:

| File relative to `../../fhelipe/fhelipe/` | SHA-256 |
| --- | --- |
| `backend/src/compiler.cc` | `DA1732396725CBE16D06E70CC1344F0CE2FF48B73F35086CA25C162686346BEA` |
| `backend/src/targets/compile.cc` | `5A3D61E24A41B29223357F080B3B109ABF4FC471F6B953CF9353912A922FB090` |
| `backend/src/layout_hoisting_pass.cc` | `DE71CB7FEE5F4844C6DFF27DFD8D08CF4D859AE1A0571F5CC1992603EB8FDA72` |
| `backend/src/dp_bootstrapping_pass.cc` | `BD057964E7260F72D604C115B434C859A05162DAE0359C5AB49A5D26A64D0A16` |

These four hashes support this revision's ownership analysis, not a claim of
complete reproducible source-bundle/build acceptance. FRZ-02/F1-ACCEPT must
lock the complete adopted implementation and test dependencies before release.

The inspected ACE rtlib interface files at the selected revision have SHA-256:

| File relative to `../../ace-compiler/` | SHA-256 |
| --- | --- |
| `fhe-cmplr/rtlib/include/common/rt_api.h` | `0025B6FB981F9578B15C9F05435DB0951E4B6C5E7547B398496F39CD0C5BC5C8` |
| `fhe-cmplr/rtlib/include/common/common.h` | `1DF878C051ADACCEABD3ECB57A88C5566E19F446AAE9C974FF9430FBD64E4AAB` |
| `fhe-cmplr/rtlib/include/rt_ant/rt_ant.h` | `AB4189F3970C2CEB80EC70018A31650E8ECC9C8102AB7BB55C3DB92A67E50C6E` |
| `fhe-cmplr/rtlib/include/rt_ant/ant_api.h` | `B9AE0016F701DE2FBE6D12BE59604DDF5C04B7666A90AFA44B8E1B671B20577A` |
| `fhe-cmplr/rtlib/ant/include/ckks/cipher.h` | `D5166E651AE61828841AFE8991A6AB0E73DAA36D161DE8A2C19344EBF4684469` |
| `fhe-cmplr/include/fhe/core/lib_provider.h` | `E1C3D250F62AC7E72FDB2E24E75C150412743D39FFBE745BC5144D9E26EC40EF` |

These hashes establish the current planning input, not a permanent upstream ABI
promise. Any rtlib revision/header/build change invalidates P1a/P1b, provider
manifests, parameter projections, operation mappings, cost models, and every
dependent accepted plan until targeted requalification passes.

The v0.10 SHA-256 is
`0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9`.
The v0.10 Markdown SHA-256 is
`7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25`.
The guide snapshot remains locked to:

| Guide | SHA-256 |
| --- | --- |
| `../../ace-paper-guide/README.md` | `7DE53FA87D3DDE2F0AC6311B86F9BB587CB129CC35927F8E12069F47DD2B7691` |
| `../../ace-paper-guide/01-ACE.md` | `DCC298ADE236E44C47BE1545B33E12C3B9D2E1ED241D45EDF030278883EA462D` |
| `../../ace-paper-guide/02-ReSBM.md` | `ED6FC8371FA397189B71D701EE136AFAC6CE1EDD51A498E3673BBDF70C89C5F5` |
| `../../ace-paper-guide/03-MetaKernel.md` | `0A1CC7760F6BACD18E5C7D42A9DE19B5A3E3585121E1A55A3DDAA76E1ED30AAA` |
| `../../ace-paper-guide/04-FHEFusion.md` | `E306A0A92A3CF0488AEACBBC24BF79FEA2E15CC83D188D7B297AE033B3022C26` |
| `../../ace-paper-guide/05-HPAO.md` | `3F95DA6683C3C3D6E138C2C89E7CCD8481300FEBE743FF3529E1C67BD2A7BF5A` |

Any source-lock change requires a recorded delta review. A branch name or a
remote URL without a content hash is not a fixed implementation input.

The Stage 1 source delta is itself acceptance evidence:

| Work | Material paper/artifact delta | Stage 1 production choice |
| --- | --- | --- |
| MetaKernel | paper Eq. 11 uses a `gamma` replication term and `kd*(Ps+1)<=S`; the fixed artifact uses `ceil(log2(rep))`, `kd*Ps+nd<=S`, and a `kd==S, Ps==1` full-slot special case | reproduce the fixed artifact cost, candidate legality, and larger-`Ps` tie rule; run a separate paper-strict differential oracle |
| ReSBM | the paper describes broader loop/candidate behavior; the fixed artifact rejects a multi-node SCC containing multiplication and schedules with integer scale-degree plus logical level under uniform one-level transitions | accept only compile-time-known zero-level-consuming retained loops, freeze `q_w=q`, and reject the broader cases before ReSBM |

## 3. Current Repository State

### 3.1 Frozen repository invariants

- The SYNC-3 Commit 16 issue and the two decision PDFs are frozen and are not
  modified by this plan.
- Binary WHIRL is the process boundary; retained `.B` files and
  `ir_b2a -st -src` output are required evidence.
- Python remains a source frontend; production planners operate on persisted
  Open64 semantic identities, not Python objects.
- Existing `TY_TENSOR`, DSL registries, mapped images, standard WHIRL, and
  `osprey/common/com/wn_simp_code.h` mechanisms are reused. ACE AIR classes are
  not imported as a second physical IR universe.
- The semantic gatekeeper runs before lowering. No FHE, SIHE, CKKS, HPOLY, or
  private planner node may reach an unmodified `whirl2c` path.
- `-O0` is the external correctness/comparison baseline; its frozen mature Fhelipe
  manifest, protected DP adaptation and acceptance come from the O0 owner.
  This baseline remains proposed until F0 reconciliation and F1 acceptance.

### 3.2 Carried-forward implementation inventory

The following inventory was established for v1.2 at `e349477270e6f8b09fdf0509c5726d191233efeb`.
This document revision does not recertify those implementations at the current
HEAD. F0/P2 revalidate relevant source deltas before consuming a claimed gate.

| Area | Previously observed state | Planning consequence |
| --- | --- | --- |
| FHE source image | `osprey/common/com/dsl_fhe.h/.cxx` persists compilation configuration, entry contracts, encrypted tensor bindings, packing policy, and logical key requirements | extend accepted owners; do not duplicate them |
| SYNC-3 plan image | `osprey/common/com/dsl_fhe_plan.h/.cxx` version 1 has conversion disposition, approximation, a small CKKS state record, and BN-fold provenance | insufficient for O2; do not overload it without a versioned compatibility review |
| Driver | `osprey/be/be/driver.cxx` processes PUs sequentially and has an all-PU atomic checkpoint | use a file-wide planning transaction and independent reopen |
| Conversion | `osprey/be/vho/fhe_convert.cxx` fails closed if the production gatekeeper/pass is absent | integrated O0/O2 execution is not currently accepted |
| Generic DSL optimizer | `osprey/be/vho/dsl_opt.cxx` has ordered stages, with only canonicalization/algebraic defaults implemented | add FHE work through reviewed stage contracts |
| WOPT | semantic-info and bridge tests exist | admit only operators with explicit alias/effect rules |
| Optional images | `osprey/include/sys/elf_whirl.h` allocates optional sections through current FHE/PU interface images | a new identity requires common/com compatibility review; O2 allocation awaits P2 |

`O2 Stage 1` is planned, not implemented. The recorded Open64 baseline has no
production MetaKernel planner, complete CKKS contract, ReSBM implementation,
accepted ACE rtlib/ANT integration contract, or accepted Stage 1 executable
path.

## 4. Scope and Frozen Support Matrices

### 4.1 Stage scope

Stage 1 consumes external prerequisites and implements O2 work as follows.
Items 1-3 and mandatory portions of 6, 9 and lowering belong to O0/shared
infrastructure; their presence here is an interface requirement, not ownership
of an O0 implementation queue. Items 4-8 include O2-specific extensions.

The integrated capability includes:

1. a pre-canonicalization `FHECompilationConfigIR` with fixed `N`, security,
   bootstrap policy, and precision/scale constraints;
2. mandatory file-wide FHE canonicalization;
3. a stable runtime/provider boundary;
4. a common normalized planner input and final-plan semantic contract;
5. MetaKernel MVM and the accepted Conv subset;
6. Cipher/Plain classification and CKKS parameter/value-state propagation;
7. the conservative artifact-backed ReSBM region, cut, and endpoint-DP subset;
8. separately identified Open64 control-flow/call extensions;
9. logical and provider-expanded key requirements;
10. independent reopen and standard-call materialization; and
11. objective Stage 1 acceptance against the accepted O0 baseline.

Stage 2 retains full FHEFusion and reviewed HPAO-MU/FM/LM plus the
HPAO-MD design gate; these activities are not removed from `-O2`.

### 4.2 Stage 1 algorithm support matrix

This matrix prevents circular definitions such as "supported graphs are the
graphs that the implementation accepts."

| Dimension | Scope class | Source-locked Stage 1 production core | Open64 extension gate | Unsupported Stage 1 behavior |
| --- | --- | --- | --- | --- |
| graph | `shared-o0-o2-comparison-required` | static typed tensor/CKKS DFG accepted by the locked artifacts | file-wide identities and accepted direct-call summaries | indirect calls, unresolved externals, recursion, mutually recursive calls, irreducible encrypted control flow |
| shape | `o0-handoff-required` | fully static ranks/dimensions | deterministic padding recorded with lineage | dynamic rank/dimension or overflow |
| fixed `N` | `o0-handoff-required` | `FHECompilationConfigIR.N` exists before canonicalization; all planners use `S=N/2` capacity | minimum acceptable/recommended `N` may be reported as a diagnostic | `N=auto`, silent post-layout `N` change, or rewrite-selected larger `N` |
| runtime/provider | `o0-handoff-required` | source-locked ACE rtlib public C surface, `FHErt_common` + `FHErt_ant`, `LIB_ANT` | Open64 plan is checked before projection; runtime-created Q/P/CRT/profile must match exactly | another release provider, rtlib drift without requalification, private ACE header leakage, or runtime override of the Open64 plan |
| MVM | `shared-o0-o2-comparison-required` | one ciphertext input, plaintext matrix, locked AE/source shape domain, fixed `S=N/2` | deterministic external-weight cache and approved whole-planner fallback | ciphertext weights, unknown weights, or unapproved shape fallback |
| Conv | `shared-o0-o2-comparison-required` | one ciphertext input/output, plaintext kernel, square odd kernel, stride 1, symmetric same padding, channel-divisible or explicitly zero-padded | every broader case below needs a dedicated oracle | unapproved fallback that silently changes shape/layout |
| broader Conv | `o2-extension-only` | none assumed | stride >1, general/valid/asymmetric padding, depthwise/grouped Conv, multi-ciphertext tensors, height sharding, halo exchange, output compaction | rejected until its extension gate passes |
| CKKS chain | `o0-handoff-required` | complete named chain is represented; ReSBM projects it to integer scale-degree plus logical level with `q_w=q` | Stage 1 ReSBM accepts uniform one-level-consuming regions only | `q_w!=q`, non-uniform or multi-level-consuming ReSBM schedules |
| relin | `o0-handoff-required` | canonical `MulCC` has an instantaneous three-component result followed immediately by mandatory `Relin`; ordinary SSA values have two components | ANT maps this explicitly as `Mul_ciph3` followed by `Relin`; the richer fused `Mul_ciph` entry point remains available but is not the canonical O2 mapping | non-immediate relin or a provider that cannot implement immediate relin |
| loops/SCC | `o2-core-only` | acyclic graph or compile-time-known retained loop whose multi-node SCC consumes zero levels | multiplication loops may be legally unrolled when trip <=64 and expanded encrypted nodes <=10000 | retained SCC with multiplication, unknown trip, scale-changing recurrence, or exceeded bound |
| calls | `o2-core-only` | intragraph artifact algorithm | nonrecursive direct calls with persisted formal/actual contracts and one resolved body | recursion, scale-changing recursive call, indirect call, missing effect or frequency summary |
| frequency | `o2-core-only` | known positive static count; unreachable is exactly zero | profile count only with provenance/hash | unknown treated as zero or silently guessed |

The scope vocabulary is normative for detailed work plans:

- `shared-o0-o2-comparison-required`: both the accepted O0 baseline and the O2
  increment must execute the row under identical comparison inputs;
- `o0-handoff-required`: O0 must produce the record/API/evidence, while O2 only
  validates and consumes it;
- `o2-core-only`: required O2 behavior that does not broaden O0 acceptance;
- `o2-extension-only`: separately reviewed optional extension; and
- `fallback-required`: unsupported O2 increment whose whole-profile fallback
  must reproduce the qualified O0 baseline.

Unsupported input must produce a stable diagnostic before any partial O2 plan is
published. A whole-profile fallback to the accepted O0 path is allowed only if
the future ADR explicitly defines it; mixing O0 and O2 ownership inside one
unverified plan is forbidden.

## 5. Paper-to-Open64 Responsibility Mapping

### 5.1 ACE

| ACE layer | Open64 Stage 1 responsibility | Representation rule |
| --- | --- | --- |
| ONNX/NN | existing Python capture and common/CNN FHE WHIRL | reuse logical operator registry and images |
| VECTOR | MetaKernel layouts, packing, rotations, masks, reductions | virtual plan records keyed to persisted source values |
| SIHE | Cipher/Plain flow, encode obligations, approximation semantics | one transfer-function implementation and verifier |
| CKKS compiler semantics | resolved parameters, value state, immediate relin/rescale/mod-switch/bootstrap, ReSBM | Open64 immutable semantic plan plus materialization proof; never replaced by an ACE AIR image |
| CKKS/POLY execution | project finalized CKKS actions and, where selected, HPOLY/POLY actions to ACE rtlib | public ACE rtlib C headers and `FHErt_common` + `FHErt_ant`; provider fixed to `LIB_ANT` |
| HPOLY/HPAO | Stage 2 Open64 compiler-side contract and reviewed optimizations over finalized CKKS regions | bootstrap stays opaque; legal non-bootstrap lowering targets the ANT LPOLY/POLY surface and does not create a second CKKS truth |
| C/runtime | generated calls, descriptors, lifetimes, context parameters and provider manifests | source-locked ACE rtlib public C interface; private ACE compiler/AIR headers are not a generated-C contract |

Useful fixed ACE anchors include
`fhe-cmplr/ckks/src/ckks.cxx:36-51`,
`fhe-cmplr/include/fhe/core/ctx_param_ana.h:614-635`,
`fhe-cmplr/poly/src/poly2c_driver.cxx:121-157`, and
`fhe-cmplr/include/fhe/core/rt_data_mgr.h:19-79`. They are evidence, not code to
copy blindly.

ACE rtlib is suitable as the O2 runtime interface because its `common/rt_api.h`
defines compiler-generated/runtime entry points, `common/common.h` defines the
shared C records, `lib_provider.h` defines the provider set, and the build
exports `FHErt_common` and `FHErt_ant`. Provider selection is principally a
compiler/generated-code choice, not a promise of opaque runtime polymorphism.
That distinction is acceptable because this plan fixes `LIB_ANT`; it must still
be recorded in the provider manifest and generated-C include/symbol allowlist.

### 5.2 MetaKernel

| Responsibility | Paper/fixed artifact evidence | Proposed Open64 owner |
| --- | --- | --- |
| artifact-equivalent `(Pb, Ps)` search and rotation objective | fixed `vector_utils.cxx:44-84`, with paper Algorithm 2/Eq. 11 retained as a differential reference | pure `fhe_metakernel_search` library |
| MVM IMRA packing/materialization | fixed `tensor2vector_handler.h:590-712`; paper Algorithms 1-2 provide intent and teaching cases | MVM plan builder and external-weight transformer |
| Ke2Col Conv core | fixed artifact `tensor2vector_util.cxx` and `vector_utils.cxx` anchors; paper Algorithm 3 is not a claim for arbitrary shapes | Conv plan builder |
| broader Conv/sharding | implementation evidence only where present | separate Open64 extension validators; never inferred from paper core |

### 5.3 ReSBM

| Responsibility | Paper/fixed artifact evidence | Proposed Open64 owner |
| --- | --- | --- |
| artifact intragraph remove/build/plan/insert sequence | `ace-compiler/fhe-cmplr/ckks/src/resbm.cxx:118-138` at the fixed ReSBM ref | pure ReSBM driver |
| endpoint and minimum-result-level DP | `ace-compiler/fhe-cmplr/ckks/src/resbm.cxx:58-103` at the fixed ReSBM ref | endpoint DP library |
| region construction | fixed `ace-compiler/fhe-cmplr/ckks/src/dfg_region_builder.cxx`; paper Algorithm 1 is a differential reference | intragraph region builder |
| rescale/bootstrap cuts | fixed `ace-compiler/fhe-cmplr/ckks/src/min_cut_region.cxx`; paper Algorithms 4-5 provide intent | immutable cut solver |
| scale selection/costing | fixed `ace-compiler/fhe-cmplr/ckks/src/resbm_scale_mgr.cxx`; not a claim of general paper Algorithm 3 coverage | state simulator and cost model |
| calls, formal/actuals, SCCs, recursion, file-wide identities | not attributed to the paper | explicit Open64 extension layer |

Reports may say `region-optimal under artifact contract and cost model <id>`
only when the locked support assumptions and oracle gates pass. They must never
say globally optimal or general paper-Algorithm-3 exact.

## 6. Imported Mandatory File-Wide FHE Canonicalization Contract

Canonicalization is a prerequisite for both O0 and O2 and is not disabled with
FHEFusion. It is distinct from generic DSL simplification and from Stage 2
profitability search. Its immutable input includes `FHECompilationConfigIR`
with fixed `N`; canonicalization may diagnose a capacity problem but may not
change `N`.

### 6.1 Canonical form

The canonicalizer shall produce one deterministic normal form across every PU:

- canonical tensor dimensions, layouts, signed rotation offsets, and stable
  source/value lineage;
- explicit `valid`, `zero`, `junk`, and `gap` slot classifications;
- explicit masks, strided slices, padding, compaction, and layout conversions;
- canonical polynomial/approximation identity without erasing `common.relu` or
  its v0.10 bootstrap policy provenance;
- canonical direct-call formal/actual links and one result identity per return;
- canonical external plaintext payload identity and content hash; and
- deterministic node ordering independent of address, traversal accident, PU
  read order, thread scheduling, or whether fusion search is enabled.

All PUs are canonicalized even when `fusion=off`. Generic DSL simplification may
run before or after only under its admitted semantic contract; it may not infer
that encrypted junk or gaps are ordinary dead scalar values.

### 6.2 Required properties

The canonicalizer must be:

- idempotent in memory and after binary reopen;
- stable across two clean compiler invocations;
- file-wide and call-consistent;
- fail-closed for unknown shapes, ambiguous lineage, overlapping non-equivalent
  writes, integer/range overflow, or unsupported slot semantics; and
- independently verified per logical output slot and per physical slot class.

Negative tests must make junk/gap errors observable: fill non-valid input slots
with nonzero sentinels, execute the canonical and lowered plan, and prove that
valid outputs are unchanged and required-zero outputs are zero. Tests that seed
all gaps with zero cannot establish this property.

## 7. ACE rtlib and ANT Provider Boundary Contract

The selected runtime boundary is the ACE compiler repository's public C rtlib
surface with ANT fixed as the provider. In this document, "ACE rtlib/ANT" means
`FHErt_common` plus `FHErt_ant`, provider ID `LIB_ANT`; it does not mean
`LIB_ACE`, the distinct GPU provider in the same provider list.

### 7.1 Interface contract

Generated C may use only the reviewed installed/public header and symbol
allowlist, initially:

```text
common/rtlib.h
common/rt_api.h
common/common.h
rt_ant/rt_ant.h
rt_ant/rt_api.h
reviewed ANT CKKS/LPOLY/POLY declarations reachable from rt_ant/rt_ant.h
```

It links the source-locked `FHErt_common` and `FHErt_ant` targets. It must not
include ACE compiler AIR/IR headers, private implementation headers, C++
provider classes, STL types, or exceptions. `CKKS_PARAMS`, `CIPHER`, `CIPHER3`,
`PLAIN`, and related ANT C records are accepted as the source-locked rtlib ABI
for this plan. Because those records do not currently carry an independent ABI
version, compatibility is controlled by the ACE rtlib revision, public-header
hash set, build configuration, symbol inventory, and compile/link/run probes.

Open64 records remain authoritative. `FHECompilationConfigIR`,
`CKKSResolvedParameterIR`, `CKKSValueStateIR`, layout, placement and logical key
records are not replaced by `CKKS_PARAMS` or by provider memory. Materialization
performs a checked one-way projection to rtlib parameters/calls, then verifies
that the runtime-created `Q/P` chain, CRT data, levels, scales, slots, bootstrap
profile and key capabilities agree with the persisted plan. rtlib may reject an
unsupported projection but may not silently select a new `N`, chain, layout,
placement or bootstrap policy.

The canonical multiply mapping is explicit even though ANT provides a richer
surface:

```text
Open64 MulCC(two,two)
  -> ANT Mul_ciph3
  -> Open64 mandatory adjacent Relin
  -> ANT Relin(two-component result)
```

ANT's fused `Mul_ciph` remains available for other accepted use cases, but the
O2 materializer does not map canonical `MulCC` to `Mul_ciph` and then emit a
second `Relin`. The availability of both operations is a provider capability,
not a provider defect.

### 7.2 Build and compiler-query boundary

No direct rtlib linkage or source-level dependency is added to `be.so` by this
plan. If compiler-side exact parameter/CRT queries require
`common/cmplr_api.h`, the implementation must first choose and review either an
isolated resolver/tool or an explicitly approved backend dependency. A direct
`be.so` dependency requires consumer link-closure, static/shared, platform,
license and distribution evidence under `AGENTS.md`.

### 7.3 Lifecycle and key-separation TODO

The inspected ANT path currently uses a process-global `Context`, and
`Prepare_context()` constructs key generation, encryptor, decryptor and
evaluator objects together. This does not disqualify ANT as the selected
provider, but `ANT-RT-TODO-CLIENT-SERVER-LIFECYCLE` must be resolved before P1b
release acceptance: define test/client and server-evaluation modes, prove that
the server path receives no secret key or decryptor, specify context
reentrancy/concurrency, and close allocation/status/cleanup behavior. Until
then, encrypted execution is qualification work, not accepted deployment
evidence.

No secret key or secret-key-derived private material is persisted in WHIRL,
planner records, provider manifests, retained test artifacts, or generated C.
Records may persist only the secret-key distribution identifier, logical key
requirements, and hashes/provenance for public or evaluation-key setup. Test
decryption keys remain ephemeral inside the isolated client/test process.

### 7.4 Provider manifest

Each execution records the ACE rtlib source revision and public-header hashes,
build flags, installed target/library hashes, `LIB_ANT`, generated-C include and
symbol inventory, target, resolved-parameter fingerprint, supported fixed ring
dimensions, level/scale semantics, exact `Mul_ciph3 -> Relin` mapping, bootstrap
profiles, key-expansion rules, lifecycle mode, thread mode, and runtime library
hash. The manifest is evidence/cache data, not an alternative source of
compilation intent.

## 8. Semantic Records and Physical Crosswalk

### 8.1 No duplicate truth

The names below are semantic master-plan concepts. They do not by themselves
allocate new WHIRL sections or tables. Existing authoritative records stay
authoritative; proposed records either extend them under a reviewed version or
reference them by stable ID. Derived costs, provider expansion, and reports are
never allowed to overwrite user intent or resolved parameters.

| Semantic record | Existing image/table | Proposed physical record or gap | Authority versus derived/cache | Verifier | Serialization owner |
| --- | --- | --- | --- | --- | --- |
| `FHECompilationConfigIR` | `WT_DSL_FHE_IMAGE` / `DSL_FHE_COMPILATION_CONFIG_RECORD` | versioned extension for accepted O-level/planner options after `F0`; fixed `N`, security target, bootstrap policy, and precision/scale constraints exist before canonicalization; no unreviewed public option allocation | authoritative user/project intent; normalized aliases derived; `N` is never resolver-selected | FHE config verifier plus option truth-table and fixed-`N` tests | `osprey/common/com/dsl_fhe.*` |
| `CKKSResolvedParameterIR` | no complete table; current value-state rows are insufficient | O0/shared baseline parameter record referencing one config/key domain; P2 may add reviewed O2-only provenance or extension fields | authoritative resolved compilation contract; provider import blob is cache/evidence | parameter/security/provider verifier | O0 and `osprey/common/com` accept baseline before P1b/F1/P2; P2 owns only O2 extensions |
| `CKKSValueStateIR` | `DSL_FHE_CKKS_VALUE_STATE_RECORD` v1 | O0/shared compatible state successor with parameter ID, scale identity, bounds, chain position and mandatory action IDs; P2 adds only reviewed O2 references | derived from source plus resolved parameters; never a second parameter truth; three components exist only in the transient `MulCC` result before immediate `Relin` | independent CKKS transfer verifier | O0/common-com baseline owner before P1b/F1/P2; same editor reviews P2 extension |
| `CKKSScaleBootstrapPlanIR` | pending-action fields only | After FRZ-03 policy reconciliation, O0/common-com first accept baseline DP/manual/protected-boundary action, profile and objective records; ReSBM candidate/replanning extensions require P2 | selected placement is authoritative for materialization; O2 candidates/cost tables are derived | O0 independent baseline-DP and hard-boundary verifier; ReSBM oracle for O2 extensions; common materializer verifier | O0/common-com baseline before P1b/F1/P2; O2 planner and same serializer own later global extensions |
| `EncryptedTensorLayoutIR` | packing policy plus layout-name string | proposed explicit logical-to-physical slot map, class map, shards, masks, and lineage | selected layout authoritative; candidate costs/cache derived | slot-map oracle and canonicalization verifier | planner producer plus `osprey/common/com` serializer |
| `MetaKernelPlanIR` | none | proposed input, artifact-equivalent `(Pb,Ps)` reparameterization, derived values, cost components, operations, extension flags | selected candidate authoritative within selected planner; transformed weights derived/cache | independent artifact-contract and slot oracle plus separate paper-strict differential oracle | MetaKernel planner plus `osprey/common/com` serializer |
| `FHEBackendCapabilityIR` | none | proposed normalized ACE rtlib/ANT capability and source/build manifest reference | provider evidence, not compilation intent; hash-bound to resolved parameters | public-header/symbol/build probe and capability-consumption verifier | rtlib integration owner; compiler stores immutable reference |
| `KeyMaterialContractIR` | `DSL_FHE_KEY_REQUIREMENT_RECORD` | retain logical rows; add separate ANT-expansion manifest reference | logical requirements authoritative; ANT-expanded key list derived/evidence | logical collector and ANT expansion verifier | `osprey/common/com/dsl_fhe.*` for logical; rtlib integration for expansion |
| normalized final execution plan | no accepted top-level record | **Frozen:** semantic interface may be prototyped, but no `WT_*`, section, table, or planner-family field is named before `F0` | one selected plan authoritative; alternatives/reports derived | independent reopen and full-plan verifier | decided by ADR/common-com review |

The former proposal to immediately name `.WHIRL.dsl_fhe_o2_plan` is withdrawn.
The common/com review may later choose a new optional image, a versioned existing
image, or another established mapped-image pattern.

### 8.2 Serialization invariants

Every eventual physical record uses fixed-width fields, invalid-zero IDs,
checked `first/count` ranges, no pointers, no host-sized enums, compile-time size
assertions, explicit capability/version checks, and deterministic ordering.

Each record family must pass:

1. public builder construction and validation;
2. binary WHIRL write;
3. producer process exit and memory destruction;
4. reopen in an independent process;
5. ID/range/hash/cross-record validation;
6. stable `ir_b2a -st -src` output;
7. normalized-text and binary determinism across two clean runs; and
8. header/count/range/ID/hash corruption tests that fail closed.

Producer-memory inspection is not serialization evidence.

## 9. CKKS Parameter, State, and Action Contract

### 9.1 Requested configuration and resolved parameters

The pipeline starts with this distinction:

```text
requested FHECompilationConfigIR:
  fixed N
  security target
  bootstrap policy
  precision/scale constraints

compiler-resolved CKKSResolvedParameterIR:
  active slots
  required depth
  Q/P chain
  bootstrap profiles
  logical/provider key requirements
  achieved security validation
```

`N` is mandatory before canonicalization and layout planning. MetaKernel and
Fhelipe consume the same fixed `N` and CKKS capacity `S=N/2`. The later resolver
must **finalize remaining CKKS parameters and validate the fixed `N`**; it may
not silently replace it. Layout overflow, an insecure resolved `Q/P` envelope,
or an unsupported provider ring dimension fails closed and reports the minimum
acceptable/recommended `N` for a user-requested recompilation.

`CKKSResolvedParameterIR` must contain or reference, without ambiguity:

- compilation-config ID, encryption/key domain ID, provider/capability ID, and
  resolution provenance;
- the unchanged requested ring dimension `N`, derived usable/active slots,
  secret-key distribution, security inputs, estimator/version, target bits, and
  achieved result;
- complete ordered coefficient-modulus chain identity, each prime ID/bit length,
  special `P` primes, decomposition base/count, and complete `Q`/`QP` basis
  fingerprints;
- scaling technique, first-prime bits, per-level prime bits/IDs, level numbering
  direction, entry level, terminal level, and provider mapping;
- symbolic scale identity, provider-native scale identity, exact/log2 bit length,
  rounding policy, plaintext encode-scale compatibility, and allowed add/mul
  alignment rules;
- two-component canonical input/output states, a three-component transient
  `MulCC` result, immediate-relin capability, and the mandatory relin-key
  requirement;
- input/output value-range and absolute/relative error budgets; and
- complete bootstrap profiles.

Each bootstrap profile records profile ID/hash, supported slot count, input and
output levels, level budget and consumed-prime sequence, BSGS dimensions,
secret-key distribution, correction factor, iteration count, input range,
output error bound, input/output scale identities, provider setup parameters,
required capabilities, and provenance.

### 9.2 Value state

`CKKSValueStateIR` contains parameter ID, value/version ID, chain position and
remaining prime IDs, symbolic/provider scale IDs and bit lengths, component
count, slots/layout ID, value range, accumulated absolute/relative error bounds,
plaintext compatibility class, and pending non-relin provider/consumer
obligations.

Formal actions are explicit typed transitions:

- `Encode`, `Encrypt`, `AddAlign`, `MulCC`, `MulCP`, `Relinearize`, `Rescale`,
  `ModSwitch`, `Bootstrap`, `KeySwitch`, `Rotate`, and `Decode`;
- each action records input state IDs, consumed prime IDs, output state ID,
  preconditions, error/range transfer, reason, source lineage, and selected
  provider capability;
- canonical ct-by-ct lowering is exactly
  `MulCC(two,two) -> transient three-component state -> Relin(two-component)`,
  with no intervening ordinary SSA consumer or movable scheduling gap;
- joins require identical parameter/key domain and compatible level, scale,
  components, layout, and error/range budgets after explicit actions.

### 9.3 Stage 1 chain restriction

The common record can represent non-uniform chains and actions that consume
multiple named primes. The Stage 1 ReSBM algorithm accepts only non-bootstrap
level-consuming actions that consume exactly one logical level with a uniform
ReSBM latency/state transition. Bootstrap is a separate profile transition from
its legal input state to one explicit result level. A double-prime rescale,
fused multi-level action, or non-uniform level effect is rejected with
`FHE-O2-CKKS-UNSUPPORTED-LEVEL-CONSUMPTION`.

Supporting such actions later is an extension: region depth becomes weighted
level consumption, `l_bts` is a sum rather than a region count, every ReSBM cut
and DP proof obligation must be restated, and the exhaustive oracle must be
extended before enabling it.

Relinearization is mandatory immediately after every canonical `MulCC`, as
required by v0.10. The three-component multiply result may be represented for the
single transition edge so its state is auditable, but it is not a normal SSA
value available to subsequent operations. A provider without the immediate
relin operation/capability is incompatible with current O2. Any non-immediate
relinearization policy is outside Stage 1, Stage 2, and O2 completion; Section 19
is the sole research-backlog record for it.

## 10. MetaKernel and ReSBM Algorithm Contracts

### 10.1 MetaKernel search variables and derivations

Production behavior is source-locked to
`origin/metakernel-proof@d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be`,
especially `nn-addon/vector/src/vector_utils.cxx:44-84`. Within the accepted
AE/source domain this is an **Artifact-backed behavior/extension** contract, not
a claim that arbitrary inputs reproduce paper Eq. 11 exactly.

The semantic search variables remain `(Pb, Ps)`. The artifact enumerates a
divisor named `curr_bs` and derives `curr_pb=nd/curr_bs`; this is the equivalent
reparameterization `bsopt=curr_bs`, `Pb=curr_pb`, not a third independent search
variable. For fixed slot capacity `S=N/2`:

```text
enumerate:  bsopt divides nd, in ascending artifact order
derive:     Pb       = nd / bsopt
enumerate:  Ps in [1,Pb], with Pb % Ps == 0
derive:     rep      = ceil((kd*Ps + nd) / kd)
            gs       = Pb / Ps
            cost     = ceil(log2(rep))
                       + (bsopt - 1) + (gs - 1) + (Ps - 1)
            f        = bsopt / bs_input
            Shiftopt = f * Shift_input
            sf       = gs * Shiftopt
```

`bs_input` and `Shift_input` remain separate plan inputs. An accepted candidate
also requires their integral mapping constraints. Candidate capacity is exactly
the artifact predicate:

```text
if kd != S: kd*Ps + nd <= S
if kd == S: Ps == 1                 # artifact full-slot special case
```

The production ordering is minimum artifact `Get_num_rot` cost, then larger
`Ps`. If both are equal, the first candidate in the fixed ascending
`bsopt`/`Ps` enumeration remains selected. Larger `Ps` is recorded as the
artifact tie rule; this plan does not assert that it globally maximizes
plaintext-weight slot utilization.

The paper-strict differential oracle separately computes its Eq. 11 `gamma`
term and `kd*(Ps+1)<=S` predicate. `gamma` and artifact
`ceil(log2(rep))` are not generally equivalent, and neither are the two capacity
predicates. Differential cases must preserve and explain the mismatch rather
than force equality.

The independent production oracle recomputes from raw inputs every candidate,
`rep`, cost component, legality predicate, derived field, complete slot map,
`valid/zero/junk/gap` class, transformed plaintext, signed rotation, winner, and
tie reason. It may not call the production cost/search helper. Accepted layouts
must have no out-of-range slot and must match cleartext and decrypted results.

Locked teaching/evaluation cases include paper Figure 4, artifact MVM1, artifact
MVM2, and the AE/golden neural-network shapes. Figure 4 and MVM1 agree in the
common power-of-two `nd==kd` domain. MVM2 is retained specifically to expose the
paper-capacity versus artifact/full-slot behavior. Additional boundary cases
cover non-power-of-two `rep` and `Ps`, `nd<kd`, `kd==S`, the exact capacity
boundary, and one-over-capacity rejection. Any shape outside the locked AE/source
support matrix is rejected or sent to an F0-approved whole-planner fallback; it
is never accepted merely because one candidate happens to fit.

### 10.2 MetaKernel materialization and extension boundary

For accepted source-locked/AE MVM and Conv, the planner records the
logical-to-physical slot map, transformed plaintext weights,
input replication/alignment rotations,
MetaKernel rotations, shifts, reduction rotations, masks, output slot classes,
and exact signed logical rotation requirements.

The following are Open64 extensions unless a fixed paper/source review proves a
narrower case belongs to the locked core: stride greater than one,
general/valid/asymmetric
padding, depthwise or grouped convolution, multiple ciphertexts for one tensor,
height sharding, halo exchange, and output compaction. Each extension must have:

- a frozen input subset and rejection boundary;
- an exact cleartext tensor oracle and an exact physical slot-map oracle;
- sentinel-filled junk/gap tests;
- exact rotation-component recomputation;
- independent decryption/decoding comparison; and
- its own capability bit and diagnostic.

An unsupported extension is rejected or uses an ADR-approved whole-planner
fallback. It is never silently approximated by the closest locked-core case.

### 10.3 ReSBM source-locked production core

Production behavior is source-locked to
`origin/20250524@323e8bb02a0e036fe2369eb48badfe6692398c2b`.
Stage 1 adopts a **conservative artifact-backed subset of the paper loop
domain**. It does not claim the paper's general retained depth-one loop behavior
or a general implementation of paper Algorithm 3.

ReSBM produces regions `R0...RD`:

- `R0` contains input ciphertexts and has multiplicative depth exactly zero;
- every non-input computation region `R1...RD` has multiplicative depth exactly
  one under the locked artifact's uniform logical-level projection;
- critical multiplications at depth `i` belong to `Ri`; forward/backward
  placement must preserve the paper's entry-multiply and boundary invariants;
- the number of regions is maximum multiplication depth plus one; and
- `R0` must never be accepted by a verifier that blindly requires every region
  to have depth one.

Before region construction, every multi-node SCC is inspected. The artifact
accepts it only when every retained element has zero multiplication depth. An
SCC containing a scale/level-consuming multiplication must be legally unrolled
within the frozen bounds or rejected with
`FHE-O2-RESBM-MUL-SCC-UNSUPPORTED`. A retained loop therefore has a
compile-time-known trip count and consumes zero logical levels per iteration.

Stage 1 freezes `q_w=q`. Its ReSBM scheduling state is the artifact's integer
`(scale_degree, logical_level)` abstraction with uniform one-level-consuming
regions. That pair is only a projection from the complete
`CKKSResolvedParameterIR`/`CKKSValueStateIR`; it never becomes global CKKS truth
and cannot erase prime IDs, provider scale identities, basis, components,
precision, range, or error.

For a proposed bootstrap endpoint segment `[src,dst]`, ScaleMgr determines the
rescaling regions, `l_bts` is the number of uniform one-level rescaling regions
excluding `src`, artifact SMOPLC/BTSPLC solve the supported cuts, and the
endpoint DP selects the minimum modeled latency. All ties use the exact locked
artifact behavior followed by a stable documented order where the artifact is
silent.

The optimality claim is limited to this accepted artifact region construction,
`q_w=q`, uniform one-level action model, fixed latency table, and enumerated
endpoints. Independent `q_w`, non-uniform chains, multi-level consumption, the
paper's more general Algorithm 3 candidate scan, and retained depth-one
multiplication loops are deferred extensions. Constant folding, FHE-aware graph
rewrites, layout changes, or parameter changes invalidate the selected result.

### 10.4 Open64 ReSBM extensions

Calls, formal/actual propagation, file-wide identity, SCC handling, recursion,
and general control flow are not attributed to the ReSBM paper.

The Stage 1 extension policy is:

- direct nonrecursive calls are accepted only when the callee body, formal and
  actual encryption/key domains, layout/state contracts, effects, and execution
  frequency are all resolved;
- recursion, mutually recursive SCCs, scale-changing recursive calls, indirect
  calls, unresolved externals, and incompatible formal/actual states fail with
  a stable diagnostic;
- a constant-trip multiplication loop is unrolled within Section 4.2 bounds or
  rejected before ReSBM; it is never retained as a Stage 1 depth-one SCC;
- a retained loop must consume zero multiplicative levels per iteration and
  have an exact compile-time trip count/frequency; otherwise it is rejected;
- backedges are never deleted to force a DAG;
- unreachable nodes have frequency zero with an `unreachable-proof` provenance;
  unknown frequency is a distinct state and cannot be costed as zero; and
- profile frequencies include counter schema, run-set hash, merge rule, and
  saturation behavior. Overflow saturates at `UINT64_MAX`, emits
  `FHE-O2-FREQ-SATURATED`, and disqualifies performance optimality claims.

The artifact-backed intragraph implementation must be accepted before the
call/control-flow extension is enabled.

### 10.5 Independent ReSBM semantic oracle

The acceptance oracle takes a raw CKKS graph and raw latency table; it may not
call a production region builder, state-transfer helper, min-cut wrapper,
endpoint enumerator, or tie-break helper.

For exhaustive graphs with at most 8 nodes, 12 edges, 6 logical levels, and 10
candidate action sites, the oracle shall:

1. validate the fixed artifact preconditions, including `q_w=q`, SCC/loop
   restrictions, uniform one-level consumption, known frequency, and supported
   call depth;
2. enumerate every legal artifact-contract region assignment;
3. enumerate every legal rescale, mod-switch, and bootstrap placement; immediate
   mandatory relin is already part of canonical `MulCC`, not a placement choice;
4. project full CKKS state to integer scale-degree/logical-level, then propagate
   and cross-check the full before/after state, range, error, and components;
5. reject illegal joins, provider/profile mismatches, non-uniform chains,
   multi-level consumption, and unsupported SCCs before planning;
6. recompute per-node frequency-weighted costs from the raw table;
7. sum total cost with checked saturating integer arithmetic; and
8. select by frozen tuple order
   `(total_cost, bootstrap_count, rescale_count, lexicographic_action_edges)`.

The production result and oracle must match legality, regions, placements,
before/after states, frequencies/provenance, result levels, total cost, and the
first stable diagnostic for invalid input. Randomized coverage uses 2000 graphs,
seed `0x5253424d`, with generator bounds checked into the fixture manifest.

The frozen oracle suite includes: a zero-level-consuming retained loop; a
multiplication/depth-one SCC that is explicitly unrolled or rejected; rejection
of unknown-trip loops and scale-changing recursive calls; `q_w=q` positives and
structured `q_w!=q` rejection; below/equal/above scale-rescale thresholds;
uniform one-level transitions; pre-ReSBM rejection of non-uniform chains and
multi-level actions; and bypass-edge, formal/callsite-state, frequency,
result-level, cut, bootstrap, and cost validation. The oracle imports no
production helper.

## 11. Optimizer Effects, Keys, and Bootstrap Policy

### 11.1 Virtual operation effects and WOPT admission

Ordinary WOPT and FHE-aware optimizers have different admission boundaries.
Before FHE conversion, ordinary DSL WOPT may admit only common/DSL operations
with complete semantic identity and an explicit pure effect summary. FHEFusion
runs later on the canonical high-level FHE graph. HPAO runs on HPOLY WHIRL and
reuses Open64 SSA/HSSA, value numbering, CSE/DCE, and dominance infrastructure;
it does not build an incompatible optimizer outside Open64.

The effect contract is:

| Operation family | Virtual semantic effect | Runtime/provider effect | WOPT rule |
| --- | --- | --- | --- |
| pure pre-conversion common/DSL expression | deterministic value with complete semantic identity | none yet | ordinary WOPT CSE/PRE only when operands, descriptors, shape, lineage, and all semantic attributes are identical |
| layout view/lineage | pure metadata | none after erasure | FHE-aware rewrite only with identical slot classes and lineage |
| high-level FHE add/mul/rotate | deterministic mathematical value | future context/key read, allocation, and failure | optimized by a dedicated FHE-aware pass on canonical FHE IR; not admitted to ordinary WOPT as an unqualified scalar expression |
| in-place provider arithmetic | mutation | reads/writes aliased ciphertext and may allocate/fail | not CSE/PRE/hoistable; requires explicit noalias/lifetime proof |
| encode/transformed weight | deterministic for fixed payload/config | reads external payload/cache, allocates, can fail | cache by complete fingerprint; no motion across payload/config change |
| bootstrap/key switch/immediate Relin/Rescale/ModSwitch | explicit ordered CKKS state transition | reads context/keys, allocates or mutates, can fail | never ordinary-WOPT CSE/PRE/hoist/speculation; canonical `MulCC->Relin` adjacency is invariant |
| HPOLY operation | explicit basis/level/scale semantics | may later allocate/fail | HPAO rule only, using Open64 analysis infrastructure and the HPOLY legality/profitability contract |
| context/key import or generation | establishes cryptographic state | I/O/allocation/provider global state may occur | ordered, effectful, never duplicated |
| retain/release/destroy | lifetime effect | reference count/free | ordered; removal only by verified balanced lifetime rewrite |
| materialized ACE rtlib/ANT call | ordered runtime action | context/key reads, allocation/lifetime, status and failure ordering | ordinary WOPT may optimize surrounding scalar/control flow only; the rtlib call is ordered/effectful by default |

The ACE rtlib/ANT integration contract must state immutable-result versus in-place behavior,
alias/noalias guarantees, escape rules, ownership, and destruction for every
entry point. Context and keys are read effects even when immutable. Allocation,
bootstrap, and failure are observable effects.

CKKS states merge only through an explicit phi/join contract. Fanout preserves
one immutable input state or records distinct versioned mutations. Critical-edge
actions are attached to a stable edge identity. Loops and calls use the policies
in Section 10.4. A dedicated FHE-aware graph-changing pass must reuse Open64
analysis infrastructure and invalidate layout, CKKS resolution, ReSBM, logical
and provider keys, and downstream costs before replanning. It may not delegate
those proofs to ordinary WOPT.

Negative/non-interference tests must prove that ordinary WOPT does not CSE, PRE,
hoist, speculate, duplicate, or reorder bootstrap, key switch, immediate Relin,
Rescale, ModSwitch, context/key import, provider allocation/lifetime, or
ACE rtlib/ANT calls. They also cover distinct key domains/scales, may-alias
operands, escaping ciphertexts, provider failures/context changes, calls, phis,
fanout, critical edges, and loops.

### 11.2 Logical and provider-expanded key requirements

Logical requirements and provider-expanded material are separate:

- logical signed rotations are normalized to `[-S/2,S/2)`, zero is removed,
  duplicates are canonicalized, and each requirement records its first consumer;
- every logical `MulCC` records its adjacent mandatory immediate relin and the
  associated relin-key requirement;
- logical bootstrap requirements reference complete bootstrap profile IDs;
- provider expansion maps logical rotations to automorphism/Galois elements and
  may add conjugation, decomposition, auxiliary, relin, or bootstrap keys; and
- both layers record provenance and fingerprints.

Exact equality of logical signed rotation sets is required between independent
collection and the selected plan. That equality does **not** imply equality of
relinearization, conjugation, bootstrap, decomposition, or other provider key
material. Provider-expanded equality is tested separately under the same
provider manifest.

### 11.3 Bootstrap policy: protected baseline DP and O2 replacement

The current v0.10 first-release policy requires a pre-ReLU bootstrap and
greedy JIT refresh at O0; it forbids DP and global replacement. The proposed
baseline changes automatic initial placement to frozen Fhelipe DP. FRZ-03 must
explicitly reconcile these provisions before production or acceptance; current
architecture text has not been edited by this plan.

The default protection for the pending design is:

| Boundary/policy | Proposed O0 producer behavior | O2 replacement constraint / evidence |
| --- | --- | --- |
| User-authored manual site | Preserve exact site, identity and semantic state; no compiler-created bootstrap in manual mode | Never remove, move, merge or reclassify the site; byte/source identity and state oracle checks |
| Required pre-ReLU boundary | Preserve as a hard boundary unless an explicit architecture decision changes it | Neither baseline DP nor ReSBM may silently erase/move it; every surviving ReLU has boundary/approximation provenance |
| Automatic depth refresh in auto/on | Initial placement by the accepted DP policy within admissible regions and hard-boundary constraints | ReSBM may replace only permitted automatic placement after state/legality/provenance checks |
| Bootstrap off | No bootstrap; retain existing rejection of surviving ReLU or insufficient-depth paths unless separately amended | No planner may create a forbidden refresh or hide failure behind fallback |

F0 must decide whether pre-ReLU remains a hard fixed barrier or becomes
planner-selectable. This revision adopts preservation as the safe pending-design
default, not permission to remove it. Changing that rule requires an explicit
architecture decision covering approximation, range/error, scale/level and
source-visible semantics, followed by matching acceptance cases.

The inspected Fhelipe DP calls `RemoveBootstraps` before planning and removes
all `TBootstrapC`; it cannot be imported unchanged. The O0 adaptation must
distinguish hard/manual boundaries from DP-owned automatic sites, partition or
constrain candidate regions, and preserve site identities. Define the actual
depth-boundary domain, result budget, count objective, shortcut behavior,
tie-breaking and infeasible cases in the baseline manifest. A constrained
variant is an Open64 adaptation, not proven equivalent to unrestricted upstream
output, the paper's latency objective or its reported speedup.

F1 evidence must include positive and negative manual/off/auto/on cases,
pre-ReLU barriers, deep chains, joins/shortcuts, no feasible placement and stale
site provenance. Compare against an independent enumerator on bounded graphs;
reopen persisted boundary and placement records; prove hard sites survive and
no partial plan is published on failure. An unavailable feasible constrained-DP
design keeps F1 acceptance open; it does not authorize a weak JIT substitute.
Retain measured bootstrap count, compile time and execution latency under the
actual protected-boundary policy. DP by name alone does not certify baseline
quality; record constraint-induced limitations and any proposed relaxation as
a separate architecture decision, without promising the paper speedup.

O0's initial DP placement is baseline construction. O2 ReSBM replacement is an
additional transformation with separate controls and records. Invalidate only
replaceable automatic placement; do not run both as competing materializers.
Reports distinguish `manual`, `policy-mandated`, `baseline-DP-selected` and
`O2-ReSBM-selected` sites, and always verify level, scale, precision, security,
approximation and provider capability.

## 12. Program-Wide Planning and Materialization Flow

### 12.1 Required transaction

After the required imported and O2 contracts are accepted, the target flow is:

```text
requested configuration: fixed N + security + bootstrap + precision/scale
  -> persist FHECompilationConfigIR before graph transformation
accepted converted application.fhe.B + fixed configuration
  -> independent reopen and complete source/image validation
  -> canonicalize every PU and persist canonical evidence
  -> build immutable common planner input
  -> optional Stage 2 FHEFusion and reverified canonical evidence
  -> select frozen Fhelipe layout or an additional reviewed MetaKernel candidate
       fixed N and S=N/2; common iteration/rotation/gap/key contract
  -> construct and verify canonical pre-ReSBM CKKS state for the selected layout
       provisional state/cost facts are distinct from canonical state
  -> import baseline DP placement under protected-boundary policy
  -> optional ReSBM replacement of permitted automatic placement
  -> verify selected actions and post-ReSBM final state
  -> finalize remaining CKKS parameters and validate the fixed N
  -> resolve logical/ANT-expanded keys and ACE rtlib/ANT capabilities
  -> independently verify normalized final plan
  -> atomically write binary plan plus reports
  -> exit producer process
  -> independent materializer reopens source and plan
  -> validate fingerprints and materialize each PU
  -> Stage 2 HPOLY/HPAO hook or Stage 1 bypass
  -> lower all remaining virtual operations to standard WHIRL calls
  -> run WHIRL/DSL/FHE verifiers
  -> retain application.o2.mid.B and ir_b2a -st -src output
  -> whirl2c -> generated C -> compile/link/load/run ACE rtlib/ANT
```

The current per-PU backend schedule is not allowed to run ReSBM independently
per PU. File-wide planning is needed for stable identities and the approved call
extension; it does not turn calls/SCCs into a paper claim.

### 12.2 Publication and failure rules

- planning reads immutable source images and publishes only after every PU and
  cross-record invariant passes;
- publication uses the existing all-PU atomic checkpoint pattern;
- a failure leaves no apparently complete plan, transformed weights, or key
  manifest;
- capacity, resolved-Q/P security, or provider incompatibility rejects the plan,
  preserves requested `N`, and reports a minimum acceptable/recommended `N` for
  an explicit recompilation;
- the materializer rejects stale source/config/provider hashes;
- a second materialization of the same plan is structurally identical; and
- final `whirl2c` input contains only supported standard WHIRL.

### 12.3 Implementation dependencies versus compiler pass order

External governance, implementation sequencing, and the compiler's execution
schedule are three different graphs. None may be inferred from section order or
used as a substitute for another.

```text
external governance and O0 activation
  accepted successor master and ownership ADR
      -> PRE-O2-LOCK-EXIT
      -> execute O2-E0 only
      -> complete independent O0 qualification
      -> O2-O0Q-001=Qualified
      -> O2-E1 may start

O2 implementation and build dependency
  PRE-O2-LOCK -> O2-E0 -> O2-E1 -> O2-E2 -> O2-E3 -> O2-E4
      -> O2-E5A -> O2-E5B -> O2-E5C -> O2-E6
      -> O2-E7A ----\
                       -> O2-E8
      -> O2-E7B ----/

compiler pass order for a complete accepted O2 plan
  canonical Open64 FHE graph
      -> optional FHEFusion
      -> recanonicalization and derived-fact invalidation
      -> frozen Fhelipe layout or selected MetaKernel layout
      -> canonical selected-layout pre-ReSBM CKKS state
      -> baseline DP import and protected-site provenance
      -> optional ReSBM replacement of replaceable automatic sites
      -> post-ReSBM state verification
      -> final CKKS parameters, security, capabilities, and keys
      -> optional HPOLY/HPAO on legal non-bootstrap regions
      -> standard WHIRL and generated C
      -> ACE rtlib/ANT execution
```

`PRE-O2-LOCK` is external contract closure, not O2 implementation. O2-E0 is an
O2-consumer qualification stage, not an O2 identity-plan or algorithm stage. A
failed O0 row returns to O0 remediation and complete requalification; no O2 work
may overlap that loop.

A provider-free MetaKernel search/plan can be implemented and verified before
the final materializer, but O2-E4 cannot claim generated-C or ANT execution.
Pre-ReSBM state is a distinct gate: provisional candidate facts are discarded,
canonical state is recomputed for the selected layout in O2-E5A, ReSBM records
the selected actions in O2-E5B, and post-ReSBM state is reverified before final
parameter/key resolution and ANT projection in O2-E5C. O2-E7A and O2-E7B may be
implemented independently after Stage 1 acceptance; O2-E8 alone owns their
composition, full invalidation/replanning, and final certification.

### 12.4 Engineering stages, milestone mapping, and exits

The engineering stage names below are normative for the detailed execution plan
set. They do not create new semantic milestones; they package the milestones in
Sections 13-14 into reviewable implementation and certification units.

| Engineering stage | Purpose | Governing milestones | Required entry | Exit gate |
| --- | --- | --- | --- | --- |
| `PRE-O2-LOCK` | Close master, baseline, record, runtime, and lifecycle contracts | `F0`, `FRZ-01..09` | Planning authorities available for review | `PRE-O2-LOCK-EXIT` |
| `O2-E0` | Independently qualify the complete O0 baseline and handoff | consume `F1-PREP/P0/P1a/P1b/F1-IMPL/F1-ACCEPT` | `PRE-O2-LOCK-EXIT` | `O2-E0-EXIT` and `O2-O0Q-001=Qualified` |
| `O2-E1` | Lock claims, support, fixtures, diagnostics, costs, and oracles | `S1.0` | current `O2-O0Q-001=Qualified` | `O2-E1-EXIT` |
| `O2-E2` | Implement records, stable identities, and atomic plan publication | `P2`, `S1.1` | accepted E1 | `O2-E2-EXIT` |
| `O2-E3` | Establish the Open64-owned CKKS semantic substrate | `S1.2`, substrate portion of `S1.5A` | accepted E2 | `O2-E3-EXIT` |
| `O2-E4` | Implement provider-free MetaKernel layout increments | `S1.3`, `S1.4`, early `S1.LAYOUT` | accepted E3 | `O2-E4-EXIT` |
| `O2-E5A` | Publish the selected-layout canonical pre-ReSBM state | `S1.5A` integration | accepted E4 | `O2-E5A-EXIT` |
| `O2-E5B` | Implement the ReSBM artifact core and protected adapter | `S1.6`, `S1.7` | accepted E5A | `O2-E5B-EXIT` |
| `O2-E5C` | Finalize CKKS, project to ANT, and materialize standard calls | `S1.5B`, `S1.8` | accepted E5B | `O2-E5C-EXIT` |
| `O2-E6` | Integrate and certify Stage 1 | final `S1.LAYOUT`, `S1.9` | accepted E5C | `O2-E6-EXIT` |
| `O2-E7A` | Implement and certify FHEFusion | `S2.0`, `S2.1`, `S2.2` | accepted E6 | `O2-E7A-EXIT` |
| `O2-E7B` | Decide HPOLY representation; implement and certify HPOLY/HPAO | `S2.3`, `S2.4` | accepted E6 and representation ADR | `O2-E7B-EXIT` |
| `O2-E8` | Replan across Stage 2, certify final O2, and publish the O3 handoff | `S2.5`, `S2.6` | accepted E7A and E7B | `O2-E8-EXIT` |

`S1.5A` and `S1.5B` are an implementation split, not two conflicting semantic
owners. S1.5A defines transfer, effect, immediate-Relin, and pre-ReSBM state.
S1.5B resolves post-ReSBM parameters, security, capabilities, provider mapping,
and key requirements after layout and placement are fixed.

Every exit decision is exactly one of `Accepted`, `Rejected`, or `Unverified`.
Only a current `Accepted` decision unlocks the next stage. A stale authority,
source, schema, provider, build, lifecycle, support, or qualification fingerprint
invalidates the exit and every dependent downstream result.

### 12.5 Detailed stage-plan and traceability contract

The detailed execution index is
`doc/FHE-O2-DETAILED-EXECUTION-PLAN.md`. The stage plans are the 13 English files
under `doc/fhe-o2-stages/`, using the names in the index. The index is navigation,
not an additional architecture authority. Each stage plan is subordinate to this
plan and must contain all of the following:

1. metadata: stage ID, status, governing plan version, milestones, entry and exit
   gates, owners, independent reviewers, and locked source identities;
2. objectives, in-scope work, explicit non-goals, and support-scope labels;
3. implementation/build dependencies and compiler pass-order relationships as
   separate sections;
4. records, APIs, versions, header and symbol allowlists, invariants, ownership,
   serialization, and compatibility rules applicable to the stage;
5. a WBS whose rows name the work ID, concrete change, proposed files,
   dependencies, tests, retained artifacts, owner, reviewer, and row exit;
6. a proposed commit and review split that keeps common/com, algorithm,
   materialization, compatibility, security, and evidence reviews separable;
7. a verification matrix with an exact proposed command, inputs and hashes,
   expected result and tolerance, seed and bounds, platform/provider, run
   protocol, artifacts, owner, reviewer, and pass rule;
8. positive, negative, failure-atomicity, compatibility, security, determinism,
   fallback, and performance cases appropriate to its claim;
9. evidence layout, retention, independent reopen, exit wording, invalidation,
   rollback, and architecture stop rules; and
10. traceability from authority to implementation and evidence.

Future scripts and interfaces must be labeled `Proposed target command` or
`Proposed path`; a plan cannot claim that they already exist. A meaningful WHIRL
artifact is a `.B` file plus its same-stem `.T` from
`ir_b2a -st -src input.B input.T`. A renamed internal dump is supplementary and
cannot replace the same-stem trace.

The mandatory traceability chain is:

```text
accepted master section/version/hash
  -> accepted ADR decision and hash, when applicable
  -> this plan section and milestone
  -> engineering stage and WBS item
  -> verification row and exact command
  -> retained artifact and content hash
  -> exit decision and downstream consumer
```

If implementation requires a change to an invariant, support scope, runtime
boundary, pass order, record owner, security lifecycle, or completion definition,
the governing master/ADR or this plan must be updated first as required by the
authority boundary. A stage plan cannot approve such a change by itself.

## 13. Prerequisites and O2 Stage 1 Milestones

All commands in this section are **Proposed target commands** to be added by the
named milestone. They are exact interfaces, not claims that the scripts already
exist. A milestone cannot close while its fixture `SHA256SUMS` contains a
placeholder, while a required test is skipped, or while the command differs from
the checked-in contract without a reviewed plan update.

The old prerequisite IDs are retained as external handoff names, not O2-owned
implementation milestones. The existing O0 execution-plan owner chooses its
internal queue. O2 must not require O0 to implement optional MetaKernel,
FHEFusion, ReSBM or HPAO to close these handoffs. However, **all** external O0
handoffs must be complete and must pass `O2-O0Q-001` before any `P2`, `S1.*` or
`S2.*` work starts.

```text
External governance: F0/master-policy reconciliation
O0 owner: frozen Fhelipe manifest -> canonicalization + rtlib/state records
         -> mature layout/lowering + protected DP + ACE rtlib/ANT
         -> complete O0 producer acceptance
O2 consumer gate: independently qualify the complete O0 bundle (O2-O0Q-001)
O2 owner, only after O2-O0Q-001:
         S1.0 -> P2/S1.1 -> S1.2..S1.8 + S1.LAYOUT -> S1.9
         -> Stage 2 -> final acceptance
```

This sequencing intentionally rejects the earlier parallel-start proposal.
Source review, fixture construction, provider-free algorithms, and independent
oracles are O2 work and therefore do not begin early. Planning notes may be
recorded in this document, but no implementation branch, acceptance fixture,
cost model, or O2 production artifact is started before O0 qualification. This
hard boundary provides fault attribution: a failure in `O2-O0Q-001` returns to
the O0 producer; a later failure belongs to a named O2 milestone unless its
bound qualification fingerprint is stale.
Each shared record has one common/com editor. O0 record changes merge first;
P2 rebases and extends them; O2 producers then consume the reviewed APIs.

### F0: Reconcile the selected mature baseline with architecture and trackers

**Owner:** architecture/O0/common-com/driver owners, plus O2 interface reviewers;
one editor per source document. This is contract reconciliation, not an O2
algorithm implementation prerequisite for the O0 producer.

**Implementation:** record FRZ-01..09 in the proposed
`doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md`. Freeze the mature Fhelipe source/pass/
config/support manifest, count-based DP objective and protected-site adaptation.
Explicitly amend the conflicting O0 no-profitability/JIT/no-DP provisions and
their option mappings, then synchronize architecture and source-tracker links.
Until those changes are accepted, label the new baseline proposed. This edit
does not itself perform the architecture or source-tracker synchronization.

**Verification:** proposed `python3 osprey/common/com/tests/verify_fhe_o2_architecture_lock.py
--adr doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md
--design doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md
--plan doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md
--source-lock testdata/fhe_o2/source-lock.json` (`O2-F0-001`; shown wrapped).
Inspect actual accepted amendment/source diffs, not only a plan checklist.
Enumerate four O levels and automatic/manual/off/on modes, protected ReLU/manual
sites, explicit choices and unrelated driver options. O2 advanced/layout/ReSBM
off must retain the frozen baseline optimizers/DP. Reject contradictory accepted
documents, missing manifest fields, uncontrolled `RemoveBootstraps`, duplicate
owners or O0 dependence on O2 records with `FHE-O2-ARCH-FREEZE-UNRESOLVED`.
Deterministic, one run, 120 s on a supported host; retain truth table, source/
manifest hashes, conflict disposition and accepted diffs for repository lifetime.

**Exit:** the specific architecture conflicts and baseline contract are accepted
and synchronized. This does not certify the protected-DP implementation; O0
then closes F1 independently of P2/MetaKernel/ReSBM. S1 release acceptance waits
for the imported executable baseline bundle.

### External O0 prerequisite handoffs (F1-PREP/P0/P1a/P1b/F1-IMPL/F1-ACCEPT)

Sections 6-7 and mandatory parts of Sections 8-9 specify what O2 consumes; they
do not transfer implementation ownership from FHE-DSL-INTEGRATION-PLAN.md,
FHE-WHIRL-INTEGRATION-PLAN.md and consolidated SYNC-4..6.

| Handoff | Producer / prerequisites | Required imported evidence and rejection checks | O2 consumer / exit |
| --- | --- | --- | --- |
| F1-PREP | O0 source/test owner; FRZ-03 baseline scope | Frozen Fhelipe source/pass/config/support manifest, build/license hashes, DP objective/domain/result-budget/shortcut options, protected-boundary specification, layout/state/placement oracles and locked focused/ResNet fixtures; reject moving refs or undocumented stripped baseline passes | F1-IMPL support/input agreement; preparation never claims O0 execution |
| P0 | O0 canonicalization owner; config fixed before transformation | Canonical `.B`/`ir_b2a`, stable source/slot/gap identities, fixed N, two-pass and reopen idempotence, nonzero sentinels, malformed/capacity/lineage negatives; Section 6 contract | P2, S1.1, FHEFusion; no fusion-dependent canonicalization |
| P1a | O0/ACE-rtlib interface owner; common-com capability contract | Source-locked public C header/symbol allowlist for `FHErt_common` + `FHErt_ant`, `LIB_ANT` selection, compile/link/load/run probe, `CKKS_PARAMS` projection, lifecycle/effect/ownership tests, revision/header/build mismatch negatives, and exact `Mul_ciph3 -> Relin`/bootstrap contract; no ACE AIR/private compiler header leakage | O0 qualification, P2 and optimized materializer; rtlib cannot override persisted Open64 configuration |
| P1b | O0/ANT provider owner; accepted baseline CKKS records and P1a | Pinned ACE rtlib/ANT build and provider manifest, exact fixed-N/Q/P/CRT/profile mapping, generated-C execution, rotate/mul3/relin/rescale/bootstrap tests, unsupported-profile failures, cleanup reports, and disposition of `ANT-RT-TODO-CLIENT-SERVER-LIFECYCLE` | F1-ACCEPT and O2 provider support; no server secret/decryptor in the accepted evaluation mode |
| F1-IMPL | O0 layout/CKKS owner; F1-PREP, P0/P1a and baseline records | Complete frozen Fhelipe layout/lowering and existing layout optimizers, state/rescale handling and constrained DP initial placement after FRZ-03; hard/manual/pre-ReLU checks, independent layout/state/count-objective oracle, infeasibility/failure atomicity and reopen | F1-ACCEPT; O2 increments disabled while baseline optimizers/DP remain active; no O2 implementation or P2 prerequisite |
| F1-ACCEPT | O0 acceptance owner; accepted preceding handoffs and P1b | Signed reconciled-baseline SYNC-6-equivalent whole ResNet-20 binary-WHIRL -> generated-C -> ACE rtlib/ANT bundle; matching `.B`/`.T`, code, manifests, logical and ANT-expanded key requirements, decoded/oracle outputs, build/run logs and hashes; no server secret key/decryptor; frozen baseline manifest, protected-DP provenance, mandatory Relin, fixed N and negative policy evidence | `O2-O0Q-001`; Section 15 numeric/support/measurement requirements must be met or explicitly remain unverified |

An O2 reviewer imports the producer's exact accepted bundle and records source
commit, environment, commands, seeds, support rows, tolerances, counts and
signoff. Baseline acceptance evidence is retained for repository lifetime.
Provider availability or a missing required check keeps the dependent gate
unverified. The O2 plan must not silently rename a synthetic fixture as full O0
acceptance. Earlier `O2-F1P-001`, `O2-P0-001`, `O2-P1A-001`, `O2-P1B-001`,
`O2-F1I-001`, and `O2-F1A-001` are retired O2 implementation test proposals;
F0 maps them to producer tests/evidence without requiring duplicate scripts.

### O2-O0Q: Complete O0 baseline qualification and O2 handoff gate

**Objective:** independently establish that every foundation consumed by O2 is
implemented, reproducible, complete, and executable before O2 work begins. This
is an O2 consumer qualification, not an O0 implementation phase and not a
second O0 feature queue.

**Prerequisites/owner:** accepted F0/master-policy reconciliation and complete
producer bundles for F1-PREP, P0, P1a, P1b, F1-IMPL and F1-ACCEPT. The O2
qualification owner and reviewers must be independent of the producer signoff
for the evidence they check.

**Verification:** proposed `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py
--build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS
--artifacts test-artifacts/o2/O2-O0Q-001` (`O2-O0Q-001`; shown wrapped). The
qualification must cover:

1. exact source/pass/config/support/license/toolchain and ACE rtlib/ANT hashes;
2. all-PU and call-consistent canonicalization with `fusion=off`, exact
   `valid/zero/junk/gap` slots, nonzero sentinels, fixed `N`, two-clean-run and
   independent-reopen determinism;
3. malformed shape, ambiguous lineage, overlapping-write, capacity, security,
   stale/corrupt ID/range/hash and unsupported-provider/profile negatives, with
   the gatekeeper rejecting before lowering, WOPT, LNO or CG;
4. unique authoritative configuration/layout/CKKS state/placement/protected-site
   and logical-key records, including old/feature-absent compatibility;
5. P1a public-header/symbol audit, P1b exact Open64-to-`CKKS_PARAMS` and
   runtime-created Q/P/CRT/profile agreement, explicit `Mul_ciph3 -> Relin`,
   compile/link/load/run/cleanup, and rtlib-drift invalidation;
6. separate client/test and server-evaluation key census, no server secret key
   or decryptor, and no secret material in `.B`, `.T`, generated C, manifests,
   logs or retained artifacts;
7. complete mature Fhelipe layout/lowering/state/rescale/protected-DP execution
   over the locked focused MVM/Conv/ReLU/residual cases and full ResNet-20; and
8. an O2 consumer process that reopens the accepted O0 `.B` and baseline
   records, validates schema/owners/fingerprints/protected sites, and emits only
   a normalized immutable planner-input digest. It does **not** create, reopen
   or materialize an O2 identity selected plan; that capability belongs to
   P2/S1.1.

The retained qualification family uses matching stems, including
`canonical.fhe.B`/`canonical.fhe.T`, `baseline-plan.B`/`baseline-plan.T`, and
`materialized.o0.mid.B`/`materialized.o0.mid.T`. A qualification ID binds the
master/ADR decision, O0 source/pass/config, record schema, ACE rtlib revision and
headers, ANT build, support matrix, test protocol and environment. A change to
any bound input stales the qualification.

**Exit:** the only results are `Qualified`, `Rejected`, or `Unverified`.
`Qualified` is the sole unlock for every P2/S1/S2 task. `Rejected` returns the
defect and evidence to its O0 owner. `Unverified` does not permit provisional O2
work. O2 must not patch, substitute or silently narrow the baseline inside this
gate.

### S1.0: Lock Stage 1 algorithms, fixtures, diagnostics, and cost model

**Objective:** translate paper/fixed-source assumptions into frozen executable
contracts before production algorithms are written.

**Prerequisite:** `O2-O0Q-001=Qualified`. The source/fixture/oracle lock is the
first O2 work item and is not prepared in parallel with O0 implementation.

**Implementation:** check in source-lock manifests, paper teaching cases,
paper/artifact delta cases, support matrices, diagnostics, tie orders, raw
latency tables, fixture generators, and independent-oracle interfaces. Record
fixed-resolved and auto-resolved test families separately; `N` is the same fixed
requested input in both, while only remaining CKKS parameters are auto-resolved.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S10-001`; `python3 osprey/be/vho/tests/verify_o2_stage1_source_lock.py --manifest testdata/fhe_o2/stage1/source-lock.json --diagnostics testdata/fhe_o2/stage1/diagnostics.json --support testdata/fhe_o2/stage1/support-matrix.json --artifacts test-artifacts/o2/O2-S10-001` | all hashes in Section 2.3, MetaKernel/ReSBM paper-artifact delta corpus, and generated fixture manifests | exact hashes, refs, test IDs, support rows, fixed-N rule, diagnostics, tolerances, cost units and artifact/paper claim labels; `FHE-O2-SOURCE-LOCK-MISMATCH` on change | generators declare fixed seeds/bounds; no unbounded generator | provider-free; manifests name ACE rtlib/ANT requirements | 0 warmups, 1 run, 120 s; retain normalized lock report, differential cases, and source delta indefinitely | Stage 1 owner / paper-code and test reviewers; exit 0 and no placeholder, moving ref, duplicate ID, unowned diagnostic, or paper-exact label on artifact-only behavior |

**Exit:** every Stage 1 claim maps to a locked source and an independent test.

### P2: Shared-record import and O2 semantic record extensions

**Owner:** common/com serialization editor; O0 supplies baseline state/rtlib
contracts, O2 planner owners supply extension requirements. **Prerequisites:**
`O2-O0Q-001=Qualified`, accepted S1.0 extension requirements, and FRZ-05
decisions. P2 does not introduce the baseline DP representation.

**Implementation:** audit Section 8 crosswalk; reuse baseline identities and
transfer functions; add only approved common iteration-space/census and O2
candidate/selected-plan records, provenance and version/capability gates. Keep
layout selection separate from CKKS scale/bootstrap placement. Do not make the
O0 provider depend on O2-only records or allocate a new image before review.

**Verification:** proposed `python3 osprey/common/com/tests/run_fhe_o2_record_contract.py
--build-dir build --manifest testdata/fhe_o2/records/SHA256SUMS
--artifacts test-artifacts/o2/O2-P2-001` (`O2-P2-001`; shown wrapped). Cover old
artifacts, new feature-absent artifacts and O2 records, every count/range/ID/hash
corruption, fixed N, layout/iteration/census ownership and immediate Relin.
Use seed `0x52454344`, chain sizes 1..16, component counts 2/3, two clean
serializations and independent producer-free reopen on supported readers; 300 s,
no tolerance. Retain images, `ir_b2a`, hashes and corruptions for 180 days,
accepted goldens indefinitely. Missing reader or compatibility evidence blocks
the gate; no new legacy-WOPT or provider-library dependency is authorized.

**Exit:** common-com and compatibility reviewers accept one authoritative owner
per record and a verified independent reopen; S1 producers can use the contract.

### S1.1: File-wide plan transaction and independent reopen

**Objective:** implement atomic program-wide plan production and source-bound
materialization without yet selecting MetaKernel/ReSBM results.

**Implementation:** use the accepted P2 representation, stable PU/node/value/edge
IDs, source/config/capability hashes, atomic publication, stale-plan rejection,
and `ir_b2a -st -src` sections. An identity plan exercises the whole boundary.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S11-001`; `python3 osprey/be/driver/tests/run_fhe_o2_transaction_contract.py --build-dir build --manifest testdata/fhe_o2/transaction/SHA256SUMS --artifacts test-artifacts/o2/O2-S11-001` | multi-PU `.B`, identity plan, stale/corrupt variants, injected publication failure | independent reopen exact; no partial artifact after failure; stale hash gives `FHE-O2-PLAN-STALE`; no tolerance | seed `0x54584e31`; 1..16 PUs; failure at every publication step | supported host; provider manifest hash treated as input | 0 warmups, 2 clean productions, 300 s; retain source/plan/mid `.B`, dumps, hashes, failure directories 180 days | backend transaction owner / common-com and reliability reviewers; byte determinism, all-or-nothing publication, exact diagnostics |

**Exit:** an identity plan survives two processes and materializes every PU
atomically.

### S1.2: ACE virtual operation graph and transfer substrate

**Objective:** build immutable VECTOR/SIHE/CKKS planning values and operations
from canonical Open64 identities without introducing private physical WHIRL
operators.

**Implementation:** define Cipher/Plain classes, operands/results, layouts,
states, effects, call/edge identities, and transfer functions. The local legal
schedule is an oracle/fallback candidate only; its production ownership follows
F0.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S12-001`; `python3 osprey/be/vho/tests/run_fhe_ace_graph_contract.py --build-dir build --manifest testdata/fhe_o2/ace-graph/SHA256SUMS --artifacts test-artifacts/o2/O2-S12-001` | locked `cgo2025-artifacts@15c95a7346d89355d68d5bf4fe8ab3b952740e1a` and fixture hashes; canonical add/mul/rotate/mask/MVM/Conv/ReLU/call/phi/fanout/loop graphs | virtual graph and raw transfer trace exact; unsafe effect/merge emits listed diagnostic; no numeric tolerance | seed `0x41434531`; <=8 PUs, <=2000 ops | provider-free plus capability fixtures | 0 warmups, 1 run, 300 s; retain graph/state/effect dumps and `.B` 180 days | ACE substrate owner / WOPT, FHE, CKKS reviewers; every accepted source op has one owner and transfer, every rejection is stable |

**Exit:** no virtual operation is stored as an unregistered WHIRL opcode and all
state/effect merges are explicit.

### S1.3: MetaKernel MVM search and plan

**Objective:** reproduce the source-locked artifact search and selected-plan
contract for the frozen MVM input class and separately measure its differences
from paper Eq. 11. Provider execution belongs to S1.8.

**Implementation:** reproduce artifact-equivalent `(Pb,Ps)` enumeration,
`ceil(log2(rep))`, artifact capacity/full-slot predicate, `Get_num_rot` cost, and
larger-`Ps` tie; independently derive every field, transform plaintext weights,
and construct exact slot maps/rotations. A separate paper-strict oracle records
Eq. 11/capacity differences. Do not call ReSBM.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S13-001`; `python3 osprey/be/vho/tests/run_metakernel_mvm_contract.py --build-dir build --source-ref d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be --manifest testdata/fhe_o2/metakernel-mvm/SHA256SUMS --artifacts test-artifacts/o2/O2-S13-001` | locked `origin/metakernel-proof@d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be`; AE/golden neural shapes, Figure 4, MVM1, MVM2, non-power-of-two rep/Ps, `nd<kd`, `kd==S`, exact/one-over capacity, zero/ill-conditioned weights | production candidate legality/cost components/tie/winner, full slot classes, no out-of-range access, transformed weights and signed rotations exact against independent artifact oracle; paper-strict differential equals recorded delta; clear `1e-12/1e-12`; unsupported shape or no candidate emits manifest code | seed `0x4d4b5231`; `n,k=1..32`, fixed `N` with `S=N/2 in {8,16,32,64}`, all divisor pairs plus named AE bounds | provider-free pure search and clear slot/tensor oracle | 0 warmups/1 run, 10 min; retain source delta, artifact/paper candidate tables, selected-plan records, complete slot traces, transformed-plaintext hashes and matching `.B`/`.T` 180 days | MetaKernel owner / independent artifact-oracle, paper-differential and tensor reviewers; zero production-oracle mismatch, recorded differential exact, no unsupported implicit fallback; no generated-C/ANT execution claim |

**Exit:** every candidate component is independently recomputed, the selected
MVM plan/slot map is exact and independently reopenable, and no runtime claim is
made before S1.8.

### S1.4: MetaKernel Conv core and gated extensions

**Objective:** implement the source-locked/AE Conv core first, then enable only
separately accepted extension rows without broad paper-Algorithm-3 claims.

**Implementation:** implement Ke2Col for the Section 4.2 core; attach an
extension capability/diagnostic to every stride, padding, depthwise, multi-CT,
shard, halo, or compaction case. Each extension commit carries its exact oracle.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S14-001`; `python3 osprey/be/vho/tests/run_metakernel_conv_contract.py --build-dir build --source-ref d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be --manifest testdata/fhe_o2/metakernel-conv/SHA256SUMS --support testdata/fhe_o2/stage1/support-matrix.json --artifacts test-artifacts/o2/O2-S14-001` | locked artifact/AE golden 1x1/3x3/5x5 stride-1 same-pad shapes plus one manifest row per approved extension and negative boundary | artifact plan, logical tensor, complete physical slot classes, masks, padding, output layout and rotation components exact; no out-of-range slot; clear `1e-12/1e-12`; unsupported source-domain row emits exact code or approved whole-planner fallback | seed `0x4d4b5243`; fixed `N`; C in/out 1..16, H/W 3..32, kernel 1/3/5; extension bounds in manifest | provider-free clear tensor/slot oracle; capability rows fixed per fixture | 0 warmups/1 run, 20 min; retain source ref/delta, selected-plan records, transformed-weight hashes, full slot/sentinel traces and matching `.B`/`.T` 180 days | MetaKernel Conv owner / artifact and tensor-semantics reviewers; all locked core rows pass; no generated-C/ANT execution claim; an extension is enabled only if its entire row passes |

**Exit:** unsupported Conv inputs never enter the core path; supported outputs and
slot classifications are exact.

### S1.5A: Pre-ReSBM CKKS state/effect contracts and WOPT non-interference

**Objective:** implement the layout-independent Section 9 state/action schema
and Section 11 effects, validate fixed `N`, and prove that ordinary WOPT does not
transform ordered FHE actions. Final parameters/provider projection belong to
S1.5B after layout and ReSBM decisions.

**Implementation:** define transfer/effect/alias/order rules, propagate
provisional range/error and chain requirements, emit mandatory immediate
`MulCC->Relin`, preserve fixed `N`, and add optimizer negative/non-interference
tests. Candidate/provisional state must be visibly distinct from the canonical
pre-ReSBM state recomputed for the selected layout. No new FHE-specific WOPT
profitability pass is promised here.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S15A-001`; `python3 osprey/be/vho/tests/run_ckks_state_contract.py --mode pre-resbm --build-dir build --manifest testdata/fhe_o2/ckks/SHA256SUMS --artifacts test-artifacts/o2/O2-S15A-001` | fixed-N requested records, layout-independent transfer cases, uniform/non-uniform requirements, bootstrap barriers, `MulCC->Relin`, and PRE/CSE/DCE/hoist/speculation cases for state actions/context/key/lifetime | requested `N` unchanged; schema/transfer/effect traces and immediate-relin adjacency exact; bounds conservative; provisional/canonical state labels cannot alias; WOPT dump shows non-interference | seed `0x434b4b53`; fixed `N in {2^12..2^16}`, chain 1..16, canonical components 2 with transient 3, fanout 1..8 | provider-free capability fixtures | 0 warmups/1 transfer run, 20 min; retain requested config, traces, optimizer dumps, diagnostics and matching `.B`/`.T` 180 days | CKKS owner / cryptography and optimizer reviewers; zero N mutation, state-label ambiguity, illegal action movement or ordinary-WOPT FHE transform |

**Exit:** every selected-layout value can receive one canonical pre-ReSBM state,
provisional facts cannot be published as final state, and no optimizer rewrite
violates effects or error budgets.

### S1.6: ReSBM artifact-core regions and SCC boundary

**Objective:** reproduce the locked artifact region contract with `R0` depth
zero, uniform depth-one computation regions, and conservative SCC/loop support.

**Implementation:** implement the fixed artifact builder, deterministic
placement, region verifier and bypass edges. Retain only compile-time-known
zero-level-consuming loops. A multiplication SCC must be legally unrolled first
or rejected. Open64 call/control-flow extensions stay disabled.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S16-001`; `python3 osprey/be/vho/tests/run_resbm_region_contract.py --build-dir build --source-ref 323e8bb02a0e036fe2369eb48badfe6692398c2b --manifest testdata/fhe_o2/resbm-region/SHA256SUMS --artifacts test-artifacts/o2/O2-S16-001` | locked `origin/20250524@323e8bb02a0e036fe2369eb48badfe6692398c2b`; artifact examples, chains, diamonds, bypasses, zero-level retained loop, unrollable and retained multiplication SCC, disconnected graphs | regions/entry/exit/depth exact against independent artifact enumerator; `R0=0`, all `Ri>0=1`; zero-level loop retained with exact frequency; multiplication SCC explicitly unrolled or `FHE-O2-RESBM-MUL-SCC-UNSUPPORTED`; no tolerance | exhaustive <=8 nodes/12 edges; trip <=64, expanded <=10000; random 2000 seed `0x5253424d` | provider-free raw cost-independent graph tests | 0 warmups, 1 exhaustive run, 300 s; retain source delta, raw/unrolled graphs, production/oracle partitions, loop/SCC decisions and minimized mismatch 180 days | ReSBM region owner / artifact and independent algorithm reviewers; exact equality, no retained level-consuming SCC, zero call-extension code in core |

**Exit:** the artifact-core partition and SCC boundary are independently
reproduced, including the special input region.

### S1.7: ReSBM ScaleMgr, cuts, endpoint DP, and independent oracle

**Objective:** produce the deterministic artifact-contract schedule under the
frozen `q_w=q`, integer-scale-degree, uniform one-level model.

**Implementation:** implement the locked ScaleMgr/cuts/endpoint-minimum-level DP,
checked frequency costs, immutable candidates, artifact tie behavior, full-state
projection checks, and the raw-graph oracle in Section 10.5. Do not enable
independent `q_w`, general paper Algorithm 3 scanning, non-uniform chains, or
multi-level consumption.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S17-001`; `python3 osprey/be/vho/tests/run_resbm_semantic_oracle.py --build-dir build --source-ref 323e8bb02a0e036fe2369eb48badfe6692398c2b --manifest testdata/fhe_o2/resbm-oracle/SHA256SUMS --artifacts test-artifacts/o2/O2-S17-001` | locked artifact; raw graphs/tables; `q_w=q` and `q_w!=q`; below/equal/above scale thresholds; uniform/non-uniform/multi-level profiles; known/zero/unknown/saturating frequency; bypass/formal/callsite/result-level cases | artifact legality, projection, regions, cuts, bootstrap, before/after full state, frequency/provenance, result levels, integer cost/tie exact; `q_w!=q`, non-uniform and multi-level reject before ReSBM; first diagnostic exact | Section 10.5 exhaustive bounds; random 2000 seed `0x5253424d`; checked saturation at `UINT64_MAX` | provider-free oracle; accepted fixed capability/profile records | 0 warmups, 1 exhaustive run, 600 s; retain source ref/delta, threshold cases, production/oracle complete plans, summary and minimized mismatches 180 days | ReSBM solver owner / artifact, independent oracle and CKKS reviewers; oracle imports no production helper, zero field mismatch, no general-Algorithm-3 claim |

**Protected-baseline integration gate (`O2-S17-PROTECTED`):** alongside the
unchanged artifact-core cases, add a separately labeled Open64 adapter test to
the same proposed oracle runner with
`--protected-sites testdata/fhe_o2/resbm-protected/sites.json` and
`--manifest testdata/fhe_o2/resbm-protected/SHA256SUMS`. Import F1's immutable
protected-site manifest and baseline-DP provenance; remove only explicitly
replaceable DP-owned automatic sites. Protected/manual/pre-ReLU boundaries
constrain region construction, endpoint windows and admissible cuts. If the
artifact core cannot represent those constraints, split at verified boundary
contracts or reject the increment and retain the whole accepted baseline.
Manual/off modes bypass automatic replanning; they never erase or add forbidden
sites. Do not claim the constrained adapter is the original full-graph artifact.

Use an independent constraint-aware enumerator on graphs of at most 8 nodes/12
edges, fixed seed `0x52534250`, protected barriers/joins/shortcuts, replaceable
DP sites, manual/off modes, infeasible cuts and stale/corrupt site IDs. Verify
exact protected-site identity/order/state, permitted-site replacement and
failure-atomic baseline fallback; reopen the selected plan and manifest in an
independent process. Retain source/constraint hashes, original baseline,
production/oracle constrained domains, selected sites and fallback diagnostics
for 180 days; provider-free, 0 warmups, one exhaustive run, 600 s. The independent
CKKS/boundary reviewer must accept both core equality and this distinct adapter
gate before integrated ReSBM acceptance. S1.8 also imports these cases to verify
protected identities survive generated-code materialization and execution.

**Exit:** production and independent enumeration agree completely; a cost-only
or placement-only match is insufficient.

### S1.5B: Final CKKS parameters, ANT projection, and post-ReSBM state gate

**Objective:** after the selected layout and placement actions are fixed,
recompute authoritative state, finalize all remaining parameters, validate the
fixed `N`, and prove exact agreement with ACE rtlib/ANT before materialization.

**Implementation:** discard provisional candidate state; recompute canonical
pre-ReSBM state on the selected layout; apply the selected baseline-DP/ReSBM
actions; verify post-ReSBM state; finalize Q/P chain, CRT/profile, scales, levels,
range/error/security and logical keys; project to `CKKS_PARAMS` and ANT-expanded
keys/capabilities; query the created runtime context through the reviewed
interface and compare exact Q/P/CRT/profile identities. The runtime may reject
but may not mutate the Open64 plan.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S15B-001`; `python3 osprey/be/vho/tests/run_ckks_state_contract.py --mode final-ant --build-dir build --manifest testdata/fhe_o2/ckks/SHA256SUMS --artifacts test-artifacts/o2/O2-S15B-001` | selected baseline/MetaKernel layouts, baseline-DP/ReSBM actions, fixed/auto-remaining suites, unsupported N/security/depth/bootstrap cases, pinned ACE rtlib/ANT manifest | pre/post state, Q/P/CRT, scale/level, security, `CKKS_PARAMS`, logical/ANT key manifests and `Mul_ciph3 -> Relin` mapping exact; mismatch or unsupported profile rejects without plan mutation | seed `0x434b4b42`; fixed `N in {2^12..2^16}`, chain 1..16 | pinned ACE rtlib/ANT revision/build; reviewed resolver topology | 0/1 resolver runs per case, 20 min; retain resolved records, rtlib projection/query, state/key traces, diagnostics and hashes 180 days | CKKS/runtime owners / cryptography, ReSBM and build reviewers; zero field mismatch, N mutation, stale state or unreviewed `be.so` dependency |

**Exit:** the selected plan has one verified final CKKS state and one exact ANT
projection; S1.8 may materialize it.

### S1.8: Open64 call/control-flow extension and standard-call materialization

**Objective:** add the bounded Open64 extension, materialize the verified plan,
and execute through the accepted P1b ACE rtlib/ANT path without private nodes.

**Implementation:** add direct-call summaries, compile-time-known zero-level
retained loops, stable edge actions, exact logical/ANT-expanded key manifests,
mandatory adjacent `Mul_ciph3 -> Relin` actions, transformed data descriptors,
lifetime/error code, ACE rtlib/ANT calls, and final verifier. Unsupported multiplication SCC,
unknown trip, recursion/scale-changing recursive call, indirect call, and unknown
frequency fail before publication.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S18-001`; `python3 osprey/be/vho/tests/run_o2_materialization_contract.py --build-dir build --manifest testdata/fhe_o2/materialization/SHA256SUMS --artifacts test-artifacts/o2/O2-S18-001` | locked MetaKernel/ReSBM refs; direct-call, phi/fanout/critical-edge, zero-level loop, unrollable/rejected multiplication SCC, residual, MVM/Conv/ReLU/bootstrap, unknown-trip/recursive-scale-change/indirect/unknown-frequency fixtures | reopened materialization structurally exact; every `MulCC` maps to adjacent ANT `Mul_ciph3` then `Relin`; no private op in `ir_b2a`; logical/ANT keys exact separately; decoded `1e-4/1e-6`; exact diagnostics before partial publication | seed `0x4d415431`; call depth <=8, trip <=64, expanded nodes <=10000, fixed N | pinned Ubuntu 22.04 x86_64; accepted P1a interface/P1b ACE rtlib/ANT manifests; generated-C capability required | 2 warmups/10 samples, 30 min/sample; retain source/plan/mid matching `.B`/`.T`, C, binaries, manifests, keys, outputs, diagnostics and logs 180 days | materializer owner / backend, WOPT, ReSBM, runtime reviewers; compile/link/load/run succeeds, no server secret/decryptor, zero leak, exact state/keys, no unsupported WHIRL or retained multiplication SCC |

**Exit:** the independently reopened plan is the sole source of materialization
decisions and the generated program executes correctly.

### S1.LAYOUT: Verify layout increments against the frozen baseline

**Objective:** import Fhelipe's baseline layout evidence once and verify
additional MetaKernel changes against it using the common SYNC-7A-E interface.
This is an incremental comparison gate, not another Fhelipe implementation stage.

**Prerequisites/owner:** current O2 qualification, accepted FRZ-01..09/F1
baseline/runtime manifests, and P2 records
and S1.3/S1.4 MetaKernel evidence. O0 produces the frozen baseline; O2 integration
owns the delta/comparison and common/com owns shared census records. Independent
layout/census reviewers accept the result.

**Work:** reuse the imported mature layout/lowering, compaction, conversion
hoisting/decomposition and schedule materialization. Implement no duplicate
baseline pass. MetaKernel's new algorithm work is already in S1.3/S1.4; this gate
adds its normalized comparison. A further Fhelipe-derived enhancement is optional
future work only when an explicit delta algorithm, support domain, control and
oracle are defined in a plan amendment. An empty future delta creates no required
implementation milestone or completion claim.

Retain the frozen baseline as the selection/fallback candidate. Auto selection
accepts a legal increment only under the reviewed cost/tie model. Unprofitable or
unsupported increments retain the whole baseline or the documented composable
baseline component. No fallback disables baseline layout optimizers or DP.
For layout-only execution comparisons, run the same baseline DP algorithm,
manifest and hard-boundary policy on each resulting layout; placement output may
differ because the graph differs. This regenerates DP-owned sites for the changed graph; it does not reuse stale
baseline site locations or alter protected sites. ReSBM remains off in that cell.

**Verification:** proposed `python3 osprey/be/vho/tests/run_fhe_layout_ab_comparison.py
--build-dir build --source-lock testdata/fhe_o2/source-lock.json
--manifest testdata/fhe_o2/layout-ab/SHA256SUMS
--artifacts test-artifacts/o2/O2-LAYOUT-001` (`O2-LAYOUT-001`; shown wrapped).
Use identical pre-layout input, N/parameters, provider, protected sites and all
non-layout controls. Retain the materialized layout checkpoint before automatic
bootstrap placement, plus end-to-end baseline-DP outputs with full provenance.
Cover MVM/Conv/residual, single/multiple ciphertexts, gaps, padding/replication,
capacity/unsupported/no-profitable-candidate boundaries, increment off, stale
census and whole-baseline fallback. Prove increment-off uses the same frozen
Fhelipe pass/config manifest rather than a reduced layout.

Recompute per-value/op/PU/program static and known-frequency weighted rotations,
signed key offsets, total/active/gap slots, gap ratio and peak/introduced/compacted
gaps, masks, ciphertext count, packing density, conversions, permutations and
rotate-add reductions. `gap_slots = total_slots - active_slots`; padding and
replicas are not active logical values. Unknown frequency remains explicit.
Independent slot/census oracles verify both outputs against source semantics.

Seed `0x4c41594f`, focused dimensions 1..32 and locked full ResNet-20; use Section
15 tolerances/protocol. Structure tests run twice within 300 s; execution uses
accepted P1a interface fixture/P1b ACE rtlib/ANT. Retain baseline and candidate manifests, pre-layout,
decision/materialized `.B`/`ir_b2a`, census, protected sites, DP outputs, fallback
reason and decoded/oracle results for 180 days; accepted bundles indefinitely.

**Exit:** baseline evidence is imported, defined increments have independently
verified census/equivalence, increment-off preserves the mature baseline, and
selected-plan provenance survives handoff. No extra baseline implementation or
undefined future enhancement is claimed complete.

### S1.9: O2 Stage 1 acceptance

**Objective:** certify MetaKernel+ReSBM against the accepted F1-ACCEPT bare-O0
baseline using the F0 comparison protocol.

**Implementation:** run correctness, structural, key, cost-model, performance,
determinism, failure and Section 15 single-factor matrices. S1.LAYOUT isolates
layout changes before ReSBM; S1-MKR uses the same accepted baseline DP
algorithm and hard-boundary
policy on the changed layout; S1-RESBM holds baseline layout fixed and replaces
only permitted automatic placement. These required cells must
not be excused by the superseded indivisible-family interpretation. Publish one
immutable acceptance bundle and review decision.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S19-001`; `python3 osprey/be/vho/tests/run_o2_stage1_acceptance.py --build-dir build --manifest testdata/fhe_o2/acceptance-stage1/SHA256SUMS --protocol testdata/fhe_o2/acceptance-stage1/protocol.json --artifacts test-artifacts/o2/O2-S19-001` | accepted F0/O2-O0Q/P2/S1.0-S1.8/S1.LAYOUT hashes; fixed-resolved and auto-remaining suites with the same fixed N; frozen focused/full workloads | all Section 15 hard gates; artifact-oracle equality and recorded paper differentials; focused decoded `1e-4/1e-6`, application max-abs `<=1e-3`, top-1 exact; immediate relin and no N mutation; only F0-defined comparison cells are hard | workload seeds in protocol; resampling seed `0x4f325331`; support matrix and F0 profile-status table fixed | one pinned Ubuntu x86_64 machine, accepted ACE rtlib/ANT manifest; no cross-machine ratio | focused 5 warmups/30 paired/20 min; full 1/5 paired/3 h; no outlier deletion; retain complete accepted bundle for repository lifetime | Stage 1 owner / architecture, independent test, artifact, crypto, runtime reviewers; O0 qualification fingerprint remains valid, every prerequisite and hard quantitative gate passes, all required baseline and single-factor cells pass, signed review says only `O2 Stage 1 accepted` |

**Exit:** Stage 1 is accepted. The report must not say `O2 complete`, `full O2
accepted`, or `v0.10 O2 complete`.

## 14. O2 Stage 2 Milestones

Stage 2 has the same evidence standard as Stage 1. It consumes the canonical
input and normalized final-plan contracts; it may not change their meaning
silently. Final bare-`-O2` completion remains blocked by Section 0 until F0 and
F1-ACCEPT are accepted.

### S2.0: Freeze the full FHEFusion catalog and Stage 2 support matrix

**Objective:** enumerate the complete fixed-paper CF, MF, and SF rules, pass
ordering, FHE-aware operators, legal graph contexts, and rejected cases.

**Implementation:** map every paper rule and artifact implementation to a stable
rule ID; specify constant/shape/layout/gap/lineage preconditions and exact
postconditions; separate mandatory canonicalization from profitable fusion.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S20-001`; `python3 osprey/be/vho/tests/verify_fhefusion_catalog.py --source-lock testdata/fhe_o2/stage2/source-lock.json --catalog testdata/fhe_o2/fhefusion/rules.json --support testdata/fhe_o2/fhefusion/support-matrix.json --artifacts test-artifacts/o2/O2-S20-001` | FHEFusion paper/ref hashes in Section 2.3; one positive and one negative graph per rule/context | exact source mapping, pre/postconditions, order, capability, diagnostic, oracle ID; no tolerance | deterministic catalog; all rule permutations enumerated where legal | provider-free | 0 warmups, 1 run, 120 s; retain normalized catalog/source delta indefinitely | FHEFusion owner / paper-code, canonicalization, MetaKernel reviewers; no unmapped rule, overlapping ambiguous owner, placeholder, or circular support row |

**Exit:** every fusion transformation is named, bounded, and testable before the
production search is enabled.

### S2.1: Implement CF/MF/SF semantics with exact mask/gap lineage

**Objective:** implement the full accepted catalog while preserving logical
outputs and physical valid/zero/junk/gap semantics.

**Implementation:** add algebraic matching, constant and mask folding, strided
slice/compaction folding, stable lineage, rewrite verification, and complete
  pass-order ablations. Reuse `osprey/common/com/wn_simp_code.h` only for
  equivalent scalar algebra.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S21-001`; `python3 osprey/be/vho/tests/run_fhefusion_semantic_contract.py --build-dir build --catalog testdata/fhe_o2/fhefusion/rules.json --manifest testdata/fhe_o2/fhefusion/SHA256SUMS --artifacts test-artifacts/o2/O2-S21-001` | locked `origin/fhefusion@f18f971710b7270aec2c3963878ac3493d9f8e06` and fixture hashes; rule graphs, overlaps, pass permutations, sentinel junk/gaps, no-fusion controls | rule match/reject and slot-map output exact; clear `abs<=1e-12`, `rel<=1e-12`; decoded `1e-4/1e-6`; exact rule diagnostic | seed `0x4655534e`; <=12 nodes exhaustive local overlaps, 2000 random legal contexts | ACE rtlib/ANT; same parameters for before/after pair | 2 warmups/10 paired samples, 20 min/sample; retain matching pre/post `.B`/`.T`, slot traces, outputs, rule log 180 days | FHEFusion owner / independent tensor oracle and CKKS reviewers; zero semantic mismatch, idempotent fixed point, deterministic rule order |

**Exit:** every enabled rule preserves visible output and required slot classes
under all accepted overlaps.

### S2.2: Fixed-`N` gap profitability and minimum-required-`N` diagnostic

**Objective:** prevent SF or mask propagation from lowering local operation count
while increasing ring dimension, shards, keys, memory, or runtime unprofitably.

**Implementation:** compute pre/post active slots, gaps, ciphertext count,
minimum required `N` as a diagnostic, rotations, depth, key bytes,
transformed-weight bytes, peak live bytes, bootstrap effects, and backend cost.
The requested `N` stays fixed. If a rewrite requires a larger `N`, reject the
rewrite or report that the user must recompile with the recommended `N`; never
mutate the current compilation input.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S22-001`; `python3 osprey/be/vho/tests/run_fhefusion_profitability_contract.py --build-dir build --manifest testdata/fhe_o2/fhefusion-profit/SHA256SUMS --model testdata/fhe_o2/cost/ant-v1.json --artifacts test-artifacts/o2/O2-S22-001` | locked `origin/fhefusion@f18f971710b7270aec2c3963878ac3493d9f8e06`, ANT cost-model/fixture hashes; gap densities 0..15/16, boundary shapes around fixed `N/2`, fixed and auto-remaining suites with identical N | capacity/security decision and minimum-required-N diagnostic exact; predicted components exact; legal output within numeric tolerance; a larger-N rewrite is rejected or requests explicit recompilation; unprofitable/unsafe reason exact | seed `0x4741504e`; fixed `N in {2^12..2^16}`, shards 1..8 | pinned ACE rtlib/ANT host/manifest; provider capability and memory counters required | 5 warmups/30 paired focused samples, 20 min/sample; no outlier deletion; retain requested config, decisions, recommendations, predictions, measurements, C and matching `.B`/`.T` 180 days | profitability owner / CKKS security, MetaKernel, performance reviewers; zero N mutation, no infeasible rewrite, quantitative model gates pass |

**Exit:** every selected fusion has a complete capacity/security/cost proof and
uses the post-fusion graph for downstream planning.

### S2.3: HPOLY contract and bootstrap-opaque lowering

**Objective:** introduce the HPAO middle-level representation without exposing
bootstrap internals or changing accepted CKKS plan semantics.

**Implementation:** first review the semantic contract, logical operator set,
physical carrier, allowed WHIRL level, and common/com ownership. Only then decide
whether HPOLY is persisted. If persisted, define versioned records/operators,
binary compatibility, `ir_b2a -st -src`, independent reopen and corruption
tests. If transient, require reviewable pre-HPOLY and post-HPOLY matching
`.B`/`.T` artifacts plus an HPOLY trace, but do not invent an independent-reopen
requirement for a representation that never crosses a process boundary. Then
implement basis/level/scale/static attributes, effect/alias rules,
CKKS-to-HPOLY and HPOLY-to-ANT LPOLY/POLY/runtime lowering.
HPAO reuses stable, non-invasive Open64 analysis services where suitable.
HPAO-MU is a new HPOLY phase following the SSAPRE algorithmic model; it must
not route HPOLY through, or modify, the existing WOPT SSAPRE implementation.
Bootstrap remains an opaque ACE rtlib/ANT call with a complete input/output
contract. Lowering may expose the
polynomial substructure of the already adjacent Relin, but may not relocate the
canonical `MulCC->Relin` boundary or make its result available before Relin.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S23-001`; `python3 osprey/be/vho/tests/run_hpoly_contract.py --build-dir build --manifest testdata/fhe_o2/hpoly/SHA256SUMS --artifacts test-artifacts/o2/O2-S23-001` | fixed HPAO ref/hash, Rotate/KeySwitch/Mul/adjacent-Relin/ModUp/ModDown/DotProd graphs and bootstrap barriers | basis, level, scale, static attributes, effects and lowering exact; `MulCC->Relin` boundary unchanged; decoded `1e-4/1e-6`; crossing bootstrap/Relin boundary or unknown basis emits exact diagnostic | seed `0x48504f4c`; chain 1..16, fanout 1..8 | accepted ACE rtlib/ANT CKKS/LPOLY/POLY capability; bootstrap treated only by profile | 2 warmups/10 samples, 30 min/sample; retain CKKS/HPOLY/POLY dumps, matching `.B`/`.T`, C, output 180 days | HPOLY owner / common-com, CKKS, ANT runtime reviewers; zero bootstrap expansion or Relin relocation, no custom node reaches whirl2c, all state identities preserved |

**Exit:** HPOLY is a verified middle level, not a second CKKS parameter truth or
a leak of provider internals.

### S2.4: Implement reviewed HPAO-MU/FM/LM; close MD-DESIGN

**Objective:** implement the v0.10 adopted MU/FM/LM rules and preserve its
explicit HPAO-MD implementation gate. A paper rule catalog is an inventory,
not permission to implement every transformation.

**Prerequisites/owner:** S2.3 and fixed HPAO source/target model; HPOLY owner,
independent polynomial/basis, crypto, WOPT-continuity and performance reviewers.

**Implementation:** MU uses an independently controlled dedicated HPOLY phase
following SSAPRE's redundancy discovery, equivalence, placement and elimination
model. Include level, scale, basis, decomposition and effects in equivalence.
Reuse stable services where non-invasive; leave legacy WOPT SSAPRE unchanged.
FM pre-encodes eligible static weights with parameter-keyed package/cache
identity and explicit storage/compile-time tradeoffs, then specializes eligible
ciphertext-times-plaintext multiplication as HPOLY `poly.fast_mul` under the
approved level/scale/basis/static-operand and provider-capability predicates.
Keep ordinary ct x pt multiplication when specialization is unsafe or unprofitable.
LM uses forward/backward
bit-width bounds and inserts reduction whenever word safety is not proven.
Each family has stable rule IDs, fixed integer-ns weights, legality/effect
checks, invalidation after provider/parameter/graph change, and independent off
controls. Bootstrap remains opaque and immediate Relin remains mandatory.

**MD-DESIGN:** retain ModDown sinking/merging as a design item. The design owner
must specify analysis, legal extended-basis operations, control-flow/placement,
lifetime, transform order, cost interface and positive/negative oracles.
Until a separate review accepts these and amends the support/acceptance matrix,
MD cannot execute. Record `design-pending, implementation-disabled`; do not
fabricate an MD ablation or claim full HPAO-paper equivalence. Closing this
delivery's MD-DESIGN disposition means the blocked design and owner are explicit,
not that its technical design or implementation has been accepted.

**Verification:** proposed `python3 osprey/be/vho/tests/run_hpao_rule_contract.py
--build-dir build --catalog testdata/fhe_o2/hpao/rules.json
--weights testdata/fhe_o2/hpao/ant-v1.json
--manifest testdata/fhe_o2/hpao/SHA256SUMS
--artifacts test-artifacts/o2/O2-S24-001` (`O2-S24-001`; shown wrapped).
MU covers equivalent and distinct basis/scale/decomposition/effect inputs,
dominance and illegal motion; verify the legacy WOPT SSAPRE source/control
behavior is unchanged. FM compares `poly.fast_mul` against independent ordinary
ct x pt multiplication for eligible static weights; cover dynamic/plaintext and
ciphertext operands, mismatched level/scale/basis, absent capability, unprofitable
selection, parameter/payload/cache mismatch, and ordinary-operation fallback.
Require exact modular/state agreement with the ordinary reference and exact
operation counts against the independent expected specialization schedule,
decoded error within the stated tolerance, and predicted/measured specialization
cost evidence including offline encoding/storage. LM covers exact overflow
boundaries and required-reduction fallback.
Verify MD remains disabled without accepted design and emits its declared
unsupported diagnostic. Rule/state/modular/operation-count comparisons are exact;
decoded tolerances are `1e-4/1e-6`. Seed `0x4850414f`; primes 1..16, fanout
1..8, dot length 1..64. Pinned ACE rtlib/ANT target model, 5 warmups/30 paired focused
samples, 20 min/sample, no outlier removal. Retain rules, oracle traces,
bit-width/basis proofs, package/cost hashes, counters, `.B`/dumps/C/output and
MD disposition for 180 days; accepted design/disposition indefinitely.

**Exit:** every enabled MU/FM/LM rule passes semantics, safety, controls and
cost-model gates, legacy WOPT is unchanged, and MD is explicitly design-gated.
Any later MD implementation requires a new plan amendment and acceptance rows.

### S2.5: Cross-pass recosting and F0-defined ablation matrix

**Objective:** measure and control interactions among FHEFusion, MetaKernel,
ReSBM, and HPAO instead of assuming their paper speedups compose.

**Implementation:** after each graph-changing pass, invalidate/recompute affected
layouts, CKKS states, ReSBM plans, keys, minimum-required-`N` diagnostics, and
HPAO weights while preserving requested `N`. Run every F0-defined single pass,
legal pair, Stage 1/full, and all-on profile with fixed parameters; repeat
auto-resolution of remaining parameters with the same fixed N.
Required `S1-MKR`/`S1-RESBM` cells use the accepted separate baseline owners.
Only design-gated MD or an explicitly out-of-support optional extension may be
labeled unavailable, with a reason and owner; this cannot waive a required cell.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S25-001`; `python3 osprey/be/vho/tests/run_o2_factorial_ablations.py --build-dir build --manifest testdata/fhe_o2/ablations/SHA256SUMS --protocol testdata/fhe_o2/ablations/protocol.json --artifacts test-artifacts/o2/O2-S25-001` | F0-defined legal profile matrix, workload/data/fixed-N/parameter/ACE-rtlib-ANT hashes; auto-remaining suite separate with same N | all hard correctness gates; complete metrics for required cells; design-gated/out-of-support optional cells carry exact reason; stale downstream plan/key or N mutation gives exact diagnostic | workload seeds locked; statistical seed `0x41424c32` | one pinned host/ACE rtlib/ANT manifest; no cross-machine speed ratios | focused 5/30, full 1/5 paired, timeouts 20 min/3 h; no outlier deletion; retain matrix, unavailable-cell reasons, and plan deltas for repository lifetime | integration owner / all pass owners and independent performance reviewer; no missing F0-required cell, no fabricated blocked cell, stale plan, or N mutation; quantitative gates pass |

**Exit:** the report identifies positive and negative interactions and the final
profile is replanned from the actual all-on graph.

### S2.6: Final bare-`-O2` certification

**Objective:** certify the complete user-visible `-O2` default selected by F0.

**Implementation:** run the accepted O0 baseline, Stage 1 profile, each Stage 2
ablation, and bare O2 on the locked suite. Verify options/defaults, serialization,
generated C, provider execution, numerical quality, security, keys, cost models,
performance, deterministic evidence, and unsupported diagnostics.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S26-001`; `python3 osprey/be/vho/tests/run_o2_final_acceptance.py --build-dir build --manifest testdata/fhe_o2/acceptance-final/SHA256SUMS --protocol testdata/fhe_o2/acceptance-final/protocol.json --artifacts test-artifacts/o2/O2-S26-001` | F0 option/profile truth table; accepted O2-O0Q/Stage1/Stage2 hashes; fixed and auto-remaining suites with identical fixed N | all Section 15 hard gates; focused decoded `1e-4/1e-6`, application max-abs `<=1e-3`, top-1 exact; immediate relin, exact diagnostics, zero N mutation; bare O2 maps to accepted profile | locked workload/statistical seeds | pinned host, accepted ACE rtlib/ANT manifest and security estimator | focused 5/30, full 1/5 paired, 20 min/3 h; no outlier deletion; retain signed release bundle for repository lifetime | release owner / architecture, crypto, runtime, all pass owners, independent reviewer; O0 qualification and S1/S2 complete, every required gate passes |

**Exit:** only this milestone may declare `O2 complete` or `v0.10 O2 complete`,
and only if the accepted ADR/v0.10 text authorizes those exact words.

## 15. Acceptance and Measurement Contract

### 15.1 Compared profiles

Profile names below identify evidence rows, not yet accepted command-line syntax:

| Profile ID | Planner/transforms | Purpose |
| --- | --- | --- |
| `BASE-O0` | Frozen mature Fhelipe layout/lowering and baseline DP under the accepted protected-boundary policy | Required after F0/F1 acceptance; not the legacy greedy/JIT or stripped-layout profile |
| `BASE-O1` | Same frozen baseline plus any separately accepted O1 increment | Conditional on supported O1 increment; its controls must not strip baseline passes/DP |
| `S1-MKR` | Additional MetaKernel plus the same baseline DP algorithm/config/protected-boundary policy | Required layout increment comparison; DP output may differ on the changed graph |
| `S1-RESBM` | Frozen Fhelipe layout plus ReSBM replacement of permitted baseline automatic placement | Required placement increment comparison; hard/manual sites unchanged |
| `S1-FULL` | MetaKernel plus ReSBM | Stage 1 combined candidate versus BASE-O0 and each component |
| `LAYOUT-FHELIPE`, `LAYOUT-MKR` | Frozen Fhelipe versus additional MetaKernel with same pre-layout input and baseline DP policy | Required defined-increment SYNC-7 comparison; import Fhelipe evidence rather than implement it again |
| `S2-CF`, `S2-MF`, `S2-SF` | One FHEFusion family at a time | Required Stage 2 rule ablation |
| `S2-HPAO-MU`, `S2-HPAO-FM`, `S2-HPAO-LM` | One approved HPAO family at a time | Required Stage 2 polynomial ablation |
| `S2-HPAO-MD` | Design-pending, implementation disabled | Not runnable until separate accepted design and plan amendment |
| `O2-ALL`, `O2-ADVANCED-OFF` | Accepted O2 increments and frozen-baseline fallback under advanced=off | Required final controls; baseline layout optimizers and DP retained |
| `O3-HANDOFF` | Reopened finalized plan with O3 scheduling/memory controls disabled | O2 handoff integrity only; future O3 execution acceptance belongs to the third plan |

Every comparison has two experiment families:

- **fixed resolved parameters:** identical `CKKSResolvedParameterIR`, provider,
  keys/profile domain, input, and transformed-constant source between profiles;
- **automatic resolution:** each profile resolves parameters from the same user
  constraints while holding the requested `N` fixed. Only active slots, depth,
  Q/P chain, scale/bootstrap profiles, keys, and achieved-security validation
  are resolved. Results are labeled auto and never substitute for a defined
  fixed-parameter comparison.

F0 records exact control settings and supported fixtures for every required row.
An unsupported optional extension or MD design gate is reported separately;
it does not waive the required baseline, component, combined, default or
advanced-off cells. Unmodified upstream or legacy Lazy/JIT profiles may be
informational only.
They cannot replace BASE-O0, imply the constrained adaptation attains the paper
speedup, or change both layout and placement algorithms in a single-factor cell.

### 15.2 Hard correctness and integration gates

A profile fails if any item fails:

1. input/model/weight/dataset/provider/source-lock hashes match the manifest;
2. requested configuration contains fixed `N` before canonicalization; every
   later record preserves it, and capacity/security/provider failure is closed
   with a minimum acceptable/recommended-N diagnostic;
3. canonicalization is file-wide, deterministic, idempotent, and slot-exact;
4. source, plan, and materialized `.B` independently reopen and pass all
   WHIRL/DSL/FHE/CKKS record verifiers;
5. stable node, value, PU, call, edge, config, parameter, layout, action, and key
   identities have no dangling or duplicate owner;
6. every MetaKernel production decision matches the independent artifact oracle;
   every paper-strict differential matches its recorded difference; and every
   logical-to-physical slot map, `valid/zero/junk/gap` classification,
   mask, padding, shard/halo, and output map matches its independent oracle;
7. every CKKS action satisfies the complete chain, scale, component, plaintext,
   range/error, security, provider, and bootstrap-profile contract;
8. every `MulCC` has only a transient three-component result and an immediately
   adjacent mandatory `Relin`; every subsequent ordinary SSA value has two
   components;
9. production and independent ReSBM artifact-contract results match all fields
   in Section 10.5, including `q_w=q`, SCC/loop decisions, projections and cuts;
10. independent logical key collection equals logical requirements exactly, and
   independent provider expansion equals the provider manifest exactly;
11. no ciphertext-byte comparison is used as a correctness oracle;
12. the generated C uses only the allowlisted ACE rtlib/ANT headers and symbols,
    compiles, links, executes through `FHErt_common` + `FHErt_ant`, and cleans up
    without sanitizer/lifetime failure or server secret/decryptor;
13. `ir_b2a -st -src` contains no unlowered private FHE/O2/HPOLY operation at the
     final `whirl2c` boundary;
14. two clean runs produce identical normalized plans, manifests, transformed
     plaintext hashes, logical keys, and materialized WHIRL; and
15. each unsupported support-matrix row emits its frozen first diagnostic before
     a complete plan is published.

### 15.3 Numerical gates

Correctness compares decrypted and decoded values with the clear tensor oracle.
Ciphertexts are randomized and their bytes are not expected to match.

| Test class | Exact gate |
| --- | --- |
| pure double layout/MVM/Conv oracle | `abs_error <= 1e-12` and `rel_error <= 1e-12`; NaN/Inf always fail |
| focused encrypted primitive/kernel | elementwise `abs_error <= 1e-4` and `rel_error <= 1e-6`; signed-zero ignored after decode; NaN/Inf fail |
| application logits | maximum absolute error `<=1e-3`, maximum relative error `<=1e-4` for reference magnitude `>=1e-2`, and identical top-1 index; ties use lowest class index |
| zero/required-zero slots | absolute value `<=1e-8` after decode |
| predicted error bound | every observed absolute/relative error is `<=` its persisted bound; a non-conservative bound fails even if the global tolerance passes |

Input generation includes zeros, signs, boundary magnitudes, ill-conditioned
matrices, residual cancellation, sentinel junk/gaps, and values at bootstrap
profile input-range boundaries. Each random input is retained or reproducible by
manifest seed and generator version.

### 15.4 Structural and optimization-quality gates

- MetaKernel artifact candidate legality, `ceil(log2(rep))` and other cost
  components, capacity/full-slot predicate, derived fields, winner, and tie
  reason equal the independent production oracle exactly; paper Eq. 11 is
  checked by the separate differential oracle.
- At least one locked MVM and one locked Conv reduce total logical rotations
  versus `BASE-O0` under fixed parameters; the exact fixture IDs are frozen in
  the S1.0 manifest before measurement.
- ReSBM modeled total latency is no greater than its independently evaluated
  legal local schedule for every accepted graph under the same raw table.
  This is an independent solver/oracle bound, not the project performance
  baseline; performance comparisons use the accepted Fhelipe-DP profile.
- No profile or rewrite changes requested `N`. A larger minimum-required `N`
  rejects the rewrite or requests a user recompilation. Changes to ciphertext
  shards, bootstrap count, logical key classes, provider key bytes,
  transformed-weight bytes, or peak live bytes record the responsible decision
  and measured tradeoff.
- Stage 2 replans layout, state, scale/bootstrap, keys, and HPAO after every
  upstream graph change; stale-plan reuse is a hard failure.
- `unreachable` zero frequency and `unknown` frequency remain distinct in plan,
  cost report, and diagnostic. Saturation disqualifies a profitability claim.

### 15.5 Cost-model validation

Cost models use integer nanoseconds and checked/saturating sums. Calibration and
holdout fixture IDs are split by a checked-in hash before fitting; holdout rows
cannot tune weights.

For operations with measured median `>=1 us`:

- holdout median absolute percentage error must be `<=20%`;
- holdout 95th-percentile absolute percentage error must be `<=35%`.

For smaller operations, absolute prediction error must be `<=2 us`. Candidate
ordering is evaluated on all holdout pairs whose measured medians differ by more
than both `5%` and twice the larger profile's median absolute deviation. The
wrong-order rate must be `<=10%`; ties outside that separation band are reported
but not counted as ordered pairs.

Every selected candidate also passes a local explanation gate: each predicted
component corresponds to an independently counted operation/state transition,
and residual error is reported. A correct total produced by wrong components
fails.

### 15.6 Pre-registered performance protocol

The protocol JSON freezes before measurements:

- machine/OS/kernel/CPU/memory/governor/thread/NUMA state;
- compiler/linker flags, ACE rtlib revision/public-header/build/library hashes,
  `LIB_ANT` capability manifest,
  security estimator, resolved parameters, keys/profiles, model/data/weights;
- profile order, pairing, seeds, warmups, samples, timeouts, counters, and
  statistics.

Focused tests use 5 unmeasured warmups and 30 paired measured samples. Full
application tests use 1 warmup and 5 paired measured samples. Pair order
alternates `AB/BA` from the protocol seed. A timeout is a failed sample and is
not removed. No outlier is deleted; raw and summarized samples are both kept.

Reports include paired median ratio and a 95% confidence interval computed by
10,000 fixed-seed paired resamples. Stage 1 performance passes only if:

- the upper 95% interval bound for `S1-FULL / BASE-O0` is `<1.05` on every
  mandatory focused and application workload; and
- the geometric mean of workload median ratios is `<=1.00`.

Stage 2/final acceptance applies the same non-regression limits to `O2-ALL` and
must report every F0-defined legal ablation cell plus the exact blocked reason
for unavailable cells. Paper headline speedups are prior results, not acceptance
thresholds.

Compilation time, runtime, peak memory, external plaintext bytes, ciphertext
count, active-slot utilization, rotations by component, logical/provider key
counts and bytes, adds, MulCP/MulCC, relins, rescales, mod-switches, bootstrap
profiles/result levels, graph/region sizes, and model prediction error are all
mandatory metrics.

### 15.7 Retained acceptance bundle

Each accepted run retains:

```text
source-lock.json
protocol.json
environment.json
support-matrix.json
diagnostics.json
requested-configuration.json
input.fhe.B
input.fhe.T
canonical.fhe.B
canonical.fhe.T
normalized-planner-input.json
resolved-parameters.json
fixed-n-capacity-security-diagnostics.json
provider-capabilities.json
planner-candidates.json
selected-plan.B
selected-plan.T
layout-and-slot-map.json
metakernel-artifact-oracle.json
metakernel-paper-differential.json
ckks-state-trace.json
resbm-regions.json
resbm-actions.json
logical-key-requirements.json
provider-key-manifest.json
transformed-plaintext-manifest.json
materialized.o2.mid.B
materialized.o2.mid.T
generated.c
build-and-link.log
decoded-output.json
oracle-output.json
raw-samples.csv
metrics.json
cost-model-validation.json
diagnostics.log
acceptance-decision.md
SHA256SUMS
```

Failed development runs are retained at least 30 days; milestone evidence is
retained at least 180 days; accepted baseline/release bundles are retained for
the repository lifetime.

## 16. Commit and Review Plan

Each row is independently buildable/reviewable and may split further. No commit
may combine a semantic change with unrelated formatting or generated artifacts.

| Commit group | Change | Required evidence before merge |
| --- | --- | --- |
| `ARCH-1` | F0 ownership/options/runtime lock and source-tracker relinks against the accepted master | `O2-F0-001`; this revision does not edit master/trackers |
| `IMPORT-O0` | Import accepted producer evidence for F1/P0/P1; no O0 implementation commit is owned here | Section 13 external handoffs and immutable bundle hashes |
| `QUAL-O0` | Independently qualify the complete O0 bundle before O2 work | `O2-O0Q-001=Qualified`; matching `.B`/`.T`, ANT and signed decision |
| `S1-1` | first O2 work: source/support/diagnostic/cost locks | `O2-S10-001` |
| `REC-O2` | P2 shared-contract reuse and O2 record extensions after S1.0 requirements | `O2-P2-001`; independent reopen and legacy compatibility |
| `S1-2` | atomic identity-plan transaction | `O2-S11-001` |
| `S1-3` | ACE virtual graph/effects/transfers | `O2-S12-001` |
| `S1-4` | MetaKernel artifact-equivalent `(Pb,Ps)` search plus paper differential | Figure 4, MVM1/MVM2 and `O2-S13-001` |
| `S1-5` | MVM selected-plan/slot-map/clear-oracle slice | `O2-S13-001`; ANT execution deferred to `S1-13` |
| `S1-6` | Ke2Col locked artifact/AE core | core rows of `O2-S14-001` |
| `S1-7+` | one Conv extension per commit | complete extension row of `O2-S14-001` |
| `S1-8A` | fixed-N pre-ReSBM CKKS state/effect and WOPT non-interference | `O2-S15A-001` |
| `S1-9` | ReSBM artifact-core region/SCC builder | `O2-S16-001` |
| `S1-10` | artifact ScaleMgr, cuts, endpoint DP with `q_w=q` | teaching cases and unit tests |
| `S1-11` | independent raw-graph oracle | `O2-S17-001` and dependency audit |
| `S1-11B` | post-ReSBM final CKKS/security/ANT projection | `O2-S15B-001` |
| `S1-12` | bounded call/control-flow extension | positive/negative extension tests |
| `S1-13` | key manifests and standard-call materializer | `O2-S18-001` |
| `S1-LAYOUT` | Imported baseline evidence, defined-increment census and comparison only; no duplicate Fhelipe pass implementation | `O2-LAYOUT-001` |
| `S1-14` | acceptance bundle only | `O2-S19-001` and signed review |
| `S2-1` | full FHEFusion catalog | `O2-S20-001` |
| `S2-2` | CF/MF/SF semantic rewrites | `O2-S21-001` |
| `S2-3` | gap/minimum-required-N diagnostics with immutable requested N | `O2-S22-001` |
| `S2-4` | HPOLY contract | `O2-S23-001` |
| `S2-5` | one approved MU/FM/LM family per commit; MD design/disposition separate | applicable `O2-S24-001` rows; MD implementation disabled |
| `S2-6` | cross-pass invalidation and ablations | `O2-S25-001` |
| `S2-7` | final acceptance bundle only | `O2-S26-001` and signed review |

## 17. Definition of Done

### 17.1 O2 Stage 1

Stage 1 is done only when:

- F0 explicitly reconciles the selected baseline and ACE rtlib boundary with
  architecture/options and source links; `O2-O0Q-001=Qualified` before any O2
  work, and its complete bound fingerprint remains valid;
- S1.0-S1.9 and S1.LAYOUT pass their exact contracts;

- artifact-backed production behavior, paper-strict differential behavior, and
  Open64 extensions are separated in code, reports, and capability bits;
- every accepted input belongs to the frozen support matrix and every other
  input fails or uses the explicit whole-profile fallback;
- complete CKKS parameters, states, errors, security, bootstrap profiles, and
  key layers are verified;
- requested `N` exists before canonicalization and is unchanged; every `MulCC`
  has mandatory immediate Relin and ordinary SSA ciphertext values have two
  components;
- MetaKernel and ReSBM independent oracles have zero structural mismatch;
- plan production and materialization cross an independent binary reopen;
- generated C executes through the source-locked ACE rtlib public C interface
  with `LIB_ANT` and the exact accepted parameter/action/key mapping;
- all hard, numeric, model, performance, required ablation and advanced-off
  gates pass; no superseded indivisible-family waiver hides missing cells; and
- the signed report uses `O2 Stage 1 accepted`, not a full-O2 claim.

### 17.2 Complete O2

Complete O2 for this v0.10 delivery requires S2.0-S2.6, every required legal
ablation, actual post-pass replanning with fixed N, accepted default/advanced-off
behavior, the O3 handoff contract, and release-lifetime evidence. Stage 1 cannot
waive Stage 2. The release states precisely: MetaKernel/ReSBM/FHEFusion and
reviewed HPOLY/HPAO-MU/FM/LM accepted; HPAO-MD design pending and disabled unless
separately approved. It must not claim the full HPAO paper or all future v0.10
research is implemented. Future O3 parallel/data execution, MD implementation
and Section 19 research are not concealed completion requirements.

## 18. Decision Register, Risks, and Stop Rules

### 18.1 Explicit decision register

| Decision | Status | Owner | Evidence required | Blocks |
| --- | --- | --- | --- | --- |
| Mature Fhelipe O0 package and DP | User-selected proposed baseline; architecture conflict open | O0/architecture owners | Frozen pass/config/support manifest, FRZ-03 amendment and protected-DP acceptance | F1 acceptance, S1.9/S2.6 |
| Fhelipe fixed research snapshot | Source-inspected; full baseline acceptance pending | Fhelipe/O0 owner | `891b3086bf6a144deebac79290801253b9cc510c`, complete hashes/build/pass/objective/oracle evidence | F1 acceptance before S1 comparisons |
| Separate layout and CKKS owners | v0.10 governing decision | common/com and FHE owners | shared interfaces and source-component crosswalk | F0/P2 |
| Bare levels, DP and increment-off mapping | Proposed; current v0.10 O0 conflicts require reconciliation | driver/config/architecture owners | Four O levels, auto/on/manual/off, protected sites, baseline-preserving O2 off/fallback | F0/default acceptance |
| Complete O0 before O2 | User-selected frozen sequencing policy | O2 qualification owner | `O2-O0Q-001` verifies every F1/P0/P1 record, executable path and negative row | Every P2/S1/S2 task |
| Runtime provider | User-selected: ACE rtlib public C surface with `LIB_ANT`; master reconciliation pending | runtime/Open64 owners | pinned revision/header/build/symbol hashes, exact Open64-plan projection and P1a/P1b/O0Q execution | F0, P1a/P1b, O0Q and executable O2 gates |
| ANT context/key separation | TODO; ANT remains selected | runtime/security owners | client/test versus server-evaluation lifecycle, no server secret/decryptor, reentrancy/cleanup/status tests | P1b and release execution acceptance |
| Compiler-side rtlib dependency | TODO; no `be.so` dependency authorized | build/runtime owners | isolated resolver or explicit `be.so` dependency/link-closure review | S1.5B implementation |
| O2 physical record storage | Proposed, requires acceptance | common/com owner | versioned crosswalk/reopen/compatibility review after shared baseline records | P2/S1.1 |
| HPAO-MD implementation | Design pending, disabled | HPOLY design owner | separate analysis/legality/lifetime/order/cost review and amended tests | MD implementation only |
| Stage 1 `q_w=q`, integer scale-degree/logical-level, uniform one-level ReSBM restriction | Frozen artifact-backed plan contract | CKKS/ReSBM owner | fixed artifact, projection and rejection tests | S1.5A/S1.5B, S1.6-S1.9 |
| MetaKernel minimum artifact cost, then larger `Ps`, then first enumeration order | Frozen artifact-backed plan contract | MetaKernel owner | fixed artifact plus independent artifact/paper-differential cases | S1.3 |
| broader Conv rows | Proposed one-by-one extensions | MetaKernel Conv owner | exact oracle and capability row | S1.4 |
| direct-call/zero-level retained-loop ReSBM extension | Proposed conservative subset | ReSBM/Open64 owner | semantics, frequency/effect proof, multiplication-SCC and recursion rejections | S1.8 |
| Stage 1 and final quantitative thresholds | Proposed acceptance contract | performance owner | pre-registration review before first measurement | S1.9/S2.6 |

Every accepted decision records date, approving reviewers, replaced text, source
hashes, and affected milestone/test IDs. A decision is not accepted merely
because implementation code exists.

### 18.2 Principal risks and mitigations

| Risk | Stop rule or mitigation |
| --- | --- |
| duplicate planner ownership | one selected layout producer and one authoritative CKKS plan owner; stop only affected integration pending F0 |
| paper and artifact semantics silently conflated | source-locked production oracle plus separate paper-strict differential oracle; use `Artifact-backed behavior/extension`, never paper-exact language |
| baseline weakened or gated by O2 | O0 supplies frozen mature Fhelipe pipeline and protected DP independently; P2/MetaKernel/ReSBM are never baseline prerequisites |
| unrestricted RemoveBootstraps erases hard sites | FRZ-03 reconciles no-DP/JIT conflict explicitly; O0 adaptation preserves manual/pre-ReLU sites by default and proves constrained placement |
| ablation or advanced-off strips baseline optimization | freeze pass/config manifest; layout cell keeps DP algorithm/constraints, placement cell keeps layout, O2 off retains both |
| duplicate O0 queue or lost migration item | external producer owns F1/P0/P1; source-tracker relinks and MIG ledger close in F0, O2 acceptance consumes signed producer evidence |
| duplicate on-disk truth | enforce Section 8 crosswalk and one authoritative owner per field |
| WHIRL compatibility regression | common-com review, version/capability gates, old/new reader tests, binary reopen |
| canonicalization hides junk/gap bugs | nonzero sentinel negatives and per-slot verification |
| ACE compiler/private ABI leaks into generated C | enforce the source-locked rtlib public-header/symbol allowlist and compile generated C without ACE AIR/private compiler headers |
| ACE rtlib header or build drift silently changes the interface | revision/header/library hashes stale P1a/P1b, O0 qualification, projections, costs and dependent plans |
| ANT global context mixes client secret and server evaluation | close `ANT-RT-TODO-CLIENT-SERVER-LIFECYCLE` before P1b; retain no secret/decryptor in server artifacts/process |
| backend gains an unreviewed rtlib dependency | default to an isolated resolver; stop any direct `be.so` linkage until the repository dependency review passes |
| resolver or rewrite silently changes `N` | fixed N is persisted before canonicalization; fail closed and report recommended N for explicit recompilation |
| CKKS plan assumes uniform chain incorrectly | represent full chain, reject unsupported ReSBM consumption, stable diagnostic |
| non-immediate relin enters current O2 | enforce canonical `MulCC->immediate Relin` and adjacent action/key tests; Section 19 is the sole deferred research record |
| ordinary WOPT moves effectful FHE calls | Section 11 boundary plus PRE/CSE/DCE/hoist/speculation non-interference tests; FHEFusion/HPAO reuse Open64 infrastructure at their own IR levels |
| ReSBM oracle shares production logic | dependency audit; oracle accepts only raw graph/table and imports no production helper |
| ReSBM paper loop scope is overstated | retain only compile-time-known zero-level SCCs; unroll/reject multiplication SCCs and reject `q_w!=q`, non-uniform, multi-level and recursive scale changes |
| unknown frequency becomes free | distinct unknown state; fail/fallback, provenance and saturation rules |
| cost model selects wrong plan | holdout error and wrong-order gates; fixed parameters and measured counters |
| aggregate speedup hides regression | per-workload CI and component reports; no outlier deletion |
| Stage 2 invalidates Stage 1 plan | mandatory invalidation/replanning and complete F0-defined legal ablation matrix |

Implementation stops and returns to architecture review if any of the following
occurs:

- a required planner decision has two owners or no owner;
- the known v0.10 O0 conflict is treated as resolved without an accepted
  FRZ-03 amendment, or another architecture conflict lacks an accepted ADR;
- a moving/unhashed source is needed for an algorithm claim;
- any P2/S1/S2 work begins without a current `O2-O0Q-001=Qualified` fingerprint;
- the ACE rtlib revision/public headers/build change without targeted
  requalification, or generated C includes a non-allowlisted ACE header/symbol;
- any stage changes requested `N`, silently accepts a larger-N rewrite, or
  materializes a non-adjacent Relin after `MulCC`;
- the materializer maps canonical `MulCC` to fused `Mul_ciph` and also emits an
  explicit `Relin`, or otherwise cannot prove the exact `Mul_ciph3 -> Relin`
  transition;
- a provider cannot express the complete resolved parameter/bootstrap profile;
- runtime-created ANT Q/P/CRT/profile state differs from the persisted Open64
  plan, a server evaluation path contains a secret/decryptor, or a direct
  `be.so` rtlib dependency appears without explicit approval;
- a record cannot be reopened independently without producer memory;
- an extension lacks an exact oracle/rejection boundary;
- a cost or security calculation saturates or relies on unknown frequency;
- an unsupported private operation reaches the final WHIRL/C boundary; or
- any acceptance fixture requires relaxing a tolerance after observing its
  result without a fresh pre-registered review.

## 19. Deferred Work and Research Backlog

HPAO-MD implementation remains behind MD-DESIGN (Section 14); O3 parallel/data
optimization belongs to the reserved third plan (Section 0.6). Target-sensitive
ReSBM extensions remain O2 research with separate support/oracle gates, not O3.
Bootstrap-internal HPOLY/native GPU lowering requires the separate architecture
and provider reviews already identified by v0.10 and consolidated SYNC-8.

Lazy relinearization is outside both delivery stages and this O2 completion
claim. A future proposal needs a new ADR, explicit provider capability,
component-count/alias/state/effect/lifetime/serialization/bootstrap proofs,
independent placement oracle, complete keys and pre-registered performance
evidence against mandatory immediate Relin. This revision makes no commitment
to implement or accept these research items.

## 20. Immediate Dependency-Ready Queue

1. Close F0's pending master/ADR reconciliation for the selected mature Fhelipe
   baseline/DP and ACE rtlib/ANT boundary. Freeze pass/config/support/runtime and
   objective manifests, hard/manual/pre-ReLU semantics, protected-DP adaptation,
   baseline-preserving controls, and public rtlib header/symbol locks.
2. O0/runtime owners implement and accept the complete baseline through
   F1/P0/P1 using ACE rtlib/ANT, including constrained-DP feasibility/oracle,
   boundary preservation, exact parameter projection and the client/server
   lifecycle disposition. O0 does not wait for P2.
3. Run `O2-O0Q-001` independently over the complete accepted O0 bundle. Do not
   begin source-lock/oracle/fixture or other O2 work while the result is
   `Rejected` or `Unverified`.
4. After `O2-O0Q-001=Qualified`, lock S1.0 claims, sources and oracles; then P2
   reviews only O2 record extensions and S1.1 owns the first O2 identity-plan
   roundtrip.
5. Implement the defined S1 increments; S1.LAYOUT imports baseline evidence and
   verifies the MetaKernel delta without reimplementing existing Fhelipe passes.
6. Reconfirm the O0 qualification fingerprint before S1.9; compare
   baseline/component/combined profiles with manifest/algorithm-controlled
   ablations and baseline-preserving off tests.
7. Complete Stage 2 with the existing MU/FM/LM contracts and explicit disabled
   MD disposition pending separate design acceptance.
8. Publish the finalized O2-to-O3 handoff; third-plan physical parallel/data
   work does not change baseline DP or ReSBM semantic decisions.

This plan does not unfreeze SYNC-3 Commit 16 and does not modify either PDF in
`doc/` associated with that issue.
