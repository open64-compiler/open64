# Open64 FHE `-O2` Integration Plan

Status: revision proposal with an active architecture freeze

Plan version: 1.2

Date: 2026-09-10

Repository baseline: `develop@e349477270e6f8b09fdf0509c5726d191233efeb`

Detailed delivery target: `O2 Stage 1` (`ACE + MetaKernel + ReSBM`)

Deferred but retained final capability: `O2 Stage 2` (`FHEFusion + HPAO`)

Revision 1.2: makes fixed `N` an immutable pipeline input; restores mandatory
immediate relinearization after `MulCC`; makes the source-locked MetaKernel and
ReSBM artifacts authoritative inside their accepted Stage 1 production domains;
splits F1 and P1 so their dependencies are executable; narrows ordinary WOPT to
non-interference; and records the measurable cost of the unresolved Fhelipe/O0
architecture freeze. Non-immediate relinearization research is outside current
O2 scope.

## 0. Implementation Freeze: `-O0` Layout and Fhelipe Baseline

### 0.1 Why this plan is frozen

The v0.9 design document currently describes Fhelipe as a peer layout planner
whose output feeds the selected CKKS scale/bootstrap policy. That description is
not sufficient to assign production ownership.

The reviewed Fhelipe paper describes one end-to-end compiler path: it assigns
tensor layouts and inserts conversions, applies EVA waterline rescaling, places
bootstraps automatically, and then lowers to CKKS vector operations. Therefore,
the current project interpretation is that a complete Fhelipe path covers both
the MetaKernel-like layout responsibility and the ReSBM-like scale/bootstrap
responsibility. Under that interpretation, these are alternative end-to-end
planning paths:

```text
common canonical FHE input
  +-> complete Fhelipe path --------------------+
  |                                             +-> normalized final plan
  +-> MetaKernel layout -> ReSBM scale/BTS -----+
```

Composing complete Fhelipe with MetaKernel and ReSBM would give two planners
ownership of the same decisions. The second planner could invalidate the first
planner's costs, layouts, level assumptions, and bootstrap placement.

The v0.9 baseline requires an executable bare-`-O0`/OpenFHE path before the
MetaKernel/Fhelipe planning phase, but it also has three unresolved gaps:

1. it does not specify a concrete non-Fhelipe `-O0` layout planner;
2. it assigns `-O0` scale/bootstrap legality to a local manager without fixing
   its complete algorithm and persisted result; and
3. it does not bind the MetaKernel/Fhelipe choice, automatic/manual/off
   behavior, or planner defaults to optimization levels.

The preferred project direction is to independently implement the complete
Fhelipe paper path as the bare `-O0` baseline. This would provide a meaningful,
non-throwaway baseline instead of spending project time on temporary local
layout and bootstrap planners used only to certify `-O2`. This direction is a
proposal for architecture review. It is not an accepted v0.9 amendment.

### 0.2 Fhelipe evidence and source-lock status

The exact paper inspected for this revision is:

- Aleksandar Krastev, Nikola Samardzic, Simon Langowski, Srinivas Devadas, and
  Daniel Sanchez, *A Tensor Compiler with Automatic Data Packing for Simple and
  Efficient Fully Homomorphic Encryption*, PLDI 2024,
  [DOI 10.1145/3656382](https://doi.org/10.1145/3656382), 25 pages;
- [author PDF](https://people.csail.mit.edu/devadas/pubs/pldi24_fhelipe.pdf);
- [official repository](https://github.com/fhelipe-compiler/fhelipe).

The paper's Figure 4 and Sections 5-6 verify the combined scope described above.
The current upstream repository contains matching implementation families:

- `backend/src/generic_layout_pass.cc`,
  `backend/src/fill_gaps_layout_pass.cc`, and
  `backend/src/layout_hoisting_pass.cc` implement layout selection/conversion
  cleanup;
- `backend/src/level_minimization_pass.cc` implements the level-minimization /
  waterline-rescaling side of the pipeline; and
- `backend/src/dag_depth_info.cc` and
  `backend/src/dp_bootstrapping_pass.cc` provide DAG depth information and
  dynamic-programming bootstrap placement.

However, the intended fixed implementation revision is **Unknown**. The upstream
history shows a paper-era initial commit
`6afbd1cb1630cba896b9ab85f93647413430b48d` and a later observed commit
`a631dae4bbf48a144c1e0a474bf81fc46e257f3e`; the repository page reports a
moving `main`, and intervening commits include a layout bug fix and performance
changes. Neither observed revision has been accepted or copied into the project
source lock. The remote paper and repository also do not have a locally reviewed
SHA-256 snapshot.

Consequently:

- the paper-backed statement that Fhelipe performs layout, rescaling, and
  bootstrap planning is **Verified**;
- treating complete Fhelipe and MetaKernel+ReSBM as interchangeable Open64
  planner families is a **Project interpretation requiring confirmation**;
- all Fhelipe code-level equivalence, test-vector, and performance claims remain
  **Unknown** until a fixed source revision is selected and hashed locally.

### 0.3 What is frozen and what may proceed

The following work is frozen until Section 0.5 is resolved and v0.9 is updated
or accompanied by an accepted ADR:

- production ownership of layout, scale, rescale, and bootstrap placement;
- bare `-O0` and bare `-O2` defaults;
- public planner-family and automatic/manual/off option semantics;
- allocation of persisted planner-family fields or a normalized top-level plan
  to a physical WHIRL image/table;
- integrated Fhelipe, MetaKernel, or ReSBM materialization behind a default
  optimization level; and
- `O2 Stage 1` or complete `-O2` certification.

The following work may proceed because it does not claim production ownership:

- source locking and paper-to-code mapping;
- the mandatory canonicalizer and its verifier;
- the runtime/provider boundary and provider-neutral capability manifest;
- pure MetaKernel and ReSBM algorithm libraries;
- independent semantic oracles and fixed test-vector preparation;
- schema prototypes that do not allocate a frozen planner-family or top-level
  physical record; and
- serialization mechanics for already accepted Open64 record owners.

No proceeding task may write a default planner choice into a user-visible
interface or claim v0.9 conformance while this freeze is active.

### 0.4 Cost/Consequences of the Freeze

The freeze has a concrete delivery cost; it is not a statement that the current
architecture has no bootstrap semantics. v0.9's existing
`bootstrap=auto|on|manual|off` meanings remain authoritative. What is frozen is
whether a complete Fhelipe implementation preserves those meanings or changes
them through the F0 ADR, and which layer realizes them.

Until Fhelipe's final responsibility and bare-`-O0` role are accepted:

- planner ownership, bare-`-O0`/bare-`-O2` defaults, Fhelipe bootstrap behavior,
  and the final public option mapping cannot be closed;
- if Fhelipe is accepted as an indivisible end-to-end baseline, the only
  formally defined Stage 1 comparison is `BASE-O0` versus `S1-FULL`;
- the `S1-MKR` and `S1-RESBM` single-factor ablations are `F0-blocked`, not hard
  Stage 1 gates, until F0 proves component separability or approves a neutral
  layout/CKKS baseline for those experiments;
- source locks, fixtures, golden results, independent oracles, pure algorithms,
  `F1-PREP`, `P0`, `P1a`, and `P2` work within already accepted record
  ownership may proceed; `P1b` may follow an accepted P2 contract; but frozen
  top-level record choices still await F0, and `F1-IMPL`, `F1-ACCEPT`, `S1.9`,
  and final O2 certification cannot close; and
- work allowed to proceed must not publish a production planner/default or
  treat a provisional top-level record as architecture authority.

This dependency is reflected in Sections 13, 15, 17, 18, and 20. It prevents a
nominal ablation matrix from becoming an implicit F0 decision.

### 0.5 Decisions required to lift the freeze

An accepted ADR, followed by a synchronized v0.9 amendment, must decide all of
the following as one coherent contract:

| Decision ID | Required decision | Owner | Required evidence | Blocking milestone |
| --- | --- | --- | --- | --- |
| `FRZ-01` | exact Fhelipe paper scope adopted by Open64 | architecture owner | paper-to-semantics review | `F0` |
| `FRZ-02` | fixed Fhelipe source revision and local SHA-256 source bundle | Fhelipe/O0 owner | revision delta, license, reproducible build/test evidence | `F0` |
| `FRZ-03` | bare `-O0` layout owner and bootstrap/scale owner | architecture and O0 owners | architecture comparison and implementation cost | `F0`, `F1-IMPL`, `F1-ACCEPT` |
| `FRZ-04` | bare `-O2`, explicit planner-family, and `auto/on/manual/off` mappings | driver/config owner | option truth table and compatibility review | `F0` |
| `FRZ-05` | common planner input and normalized final-plan contracts | common-com and FHE planner owners | record crosswalk and independent verifier prototype | `F0`, `P2` |
| `FRZ-06` | comparison protocol for Fhelipe versus MetaKernel+ReSBM | performance and acceptance owners | fixed inputs, resolved parameters, oracles, metrics, and statistical protocol | `S1.9` |
| `FRZ-07` | accepted ADR and synchronized v0.9 text | architecture owner | architecture-owner approval and synchronized document diff | `F0`, `F1-IMPL`, `F1-ACCEPT`, `S1.9`, `S2.6` |

Partial decisions do not lift the freeze. In particular, selecting a Fhelipe
commit without deciding O-level and bootstrap option semantics is insufficient.

## 1. Executive Delivery Decision

`-O2` remains one capability delivered in two stages:

| Delivery stage | Included work | Completion rule |
| --- | --- | --- |
| `O2 Stage 1` | ACE responsibility mapping, full canonicalization prerequisite, provider boundary, MetaKernel MVM/Conv planning, CKKS contract, ReSBM planning, logical/provider key contracts, and standard-call lowering | independently accepted only after `F0`, `F1-PREP`, `P0`, `P1a`, `P2`, `P1b`, `F1-IMPL`, and `F1-ACCEPT` |
| `O2 Stage 2` | complete FHEFusion rule catalog and search, HPOLY/HPAO, backend-aware recosting, full ablations, and bare-`-O2` certification | remains part of final `-O2`; cannot close while the Section 0 freeze is active |

The intended final architecture, subject to `FRZ-01` through `FRZ-07`, is:

```text
Python frontend
  -> common/CNN/FHE WHIRL
  -> mandatory file-wide FHE canonicalization
  -> FHEFusion search                         [O2 Stage 2]
  -> one selected end-to-end planner family
       Fhelipe                                [preferred bare O0 proposal]
       or MetaKernel -> CKKS -> ReSBM         [O2 Stage 1]
  -> HPOLY/HPAO                               [O2 Stage 2]
  -> standard WHIRL calls and stable C ABI
  -> provider runtime
```

Stage names are delivery states, not public optimization levels. No new public
`o2-stage` switch is proposed. Until the freeze is lifted, examples of
`-dsc-fhe-layout-planner=...` and `-dsc-fhe-ckks-scale-policy=...` are design
vocabulary only, not accepted command-line behavior.

## 2. Authority, Evidence, and Status Labels

### 2.1 Authority order

Unless an accepted ADR explicitly amends it, authority is:

1. `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.9.docx` for
   architecture and semantics;
2. an accepted ADR for an explicitly identified v0.9 conflict or omission;
3. `AGENTS.md` and current Open64 code for repository invariants and observed
   implementation behavior;
4. original papers for algorithm intent and claims inside their stated
   assumptions;
5. fixed artifact revisions for implementation evidence and, inside the
   source-locked MetaKernel/ReSBM production support domains, production
   algorithm semantics when paper and artifact differ; and
6. `../../ace-paper-guide/` for navigation and explanation.

This plan owns implementation staging, test contracts, retained evidence, and
handoffs. It does not silently amend v0.9. Code demonstrates current behavior;
it does not override the architecture. Papers do not define Open64 behavior
outside their assumptions. A fixed artifact may explain a paper but may not
expand the paper's theorem, scope, or evaluation claim. Within a source-locked
MetaKernel/ReSBM support row, an artifact/paper difference is named as
`Artifact-backed behavior/extension`; it is never silently reported as a
paper-exact implementation.

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
| Fhelipe | author PDF and DOI above; local SHA-256 **Unknown** | intended fixed revision **Unknown**; two observed candidates are in Section 0.2 | architecture-freeze evidence only |

The v0.9 SHA-256 is
`4B9DAC9927E86518142CA9A9E71AEAE7AEA5C454D01C544311359680639DF4B6`.
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
- `-O0` is the correctness and comparison baseline, but its concrete planning
  owner is currently frozen by Section 0.

### 3.2 Verified implementation baseline

| Area | Verified state | Planning consequence |
| --- | --- | --- |
| FHE source image | `osprey/common/com/dsl_fhe.h/.cxx` persists compilation configuration, entry contracts, encrypted tensor bindings, packing policy, and logical key requirements | extend accepted owners; do not duplicate them |
| SYNC-3 plan image | `osprey/common/com/dsl_fhe_plan.h/.cxx` version 1 has conversion disposition, approximation, a small CKKS state record, and BN-fold provenance | insufficient for O2; do not overload it without a versioned compatibility review |
| Driver | `osprey/be/be/driver.cxx` processes PUs sequentially and has an all-PU atomic checkpoint | use a file-wide planning transaction and independent reopen |
| Conversion | `osprey/be/vho/fhe_convert.cxx` fails closed if the production gatekeeper/pass is absent | integrated O0/O2 execution is not currently accepted |
| Generic DSL optimizer | `osprey/be/vho/dsl_opt.cxx` has ordered stages, with only canonicalization/algebraic defaults implemented | add FHE work through reviewed stage contracts |
| WOPT | semantic-info and bridge tests exist | admit only operators with explicit alias/effect rules |
| Optional images | `osprey/include/sys/elf_whirl.h` allocates optional sections through current FHE/PU interface images | a new identity requires common/com compatibility review; exact O2 allocation is frozen |

`O2 Stage 1` is planned, not implemented. There is no production MetaKernel
planner, complete CKKS contract, ReSBM implementation, stable provider ABI, or
accepted Stage 1 executable path in this baseline.

## 4. Scope and Frozen Support Matrices

### 4.1 Stage scope

Stage 1 includes:

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

Stage 2 retains full FHEFusion and HPAO; it is not removed from `-O2`.

### 4.2 Stage 1 algorithm support matrix

This matrix prevents circular definitions such as "supported graphs are the
graphs that the implementation accepts."

| Dimension | Source-locked Stage 1 production core | Open64 extension gate | Unsupported Stage 1 behavior |
| --- | --- | --- | --- |
| graph | static typed tensor/CKKS DFG accepted by the locked artifacts | file-wide identities and accepted direct-call summaries | indirect calls, unresolved externals, recursion, mutually recursive calls, irreducible encrypted control flow |
| shape | fully static ranks/dimensions | deterministic padding recorded with lineage | dynamic rank/dimension or overflow |
| fixed `N` | `FHECompilationConfigIR.N` exists before canonicalization; all planners use `S=N/2` capacity | minimum acceptable/recommended `N` may be reported as a diagnostic | `N=auto`, silent post-layout `N` change, or rewrite-selected larger `N` |
| MVM | one ciphertext input, plaintext matrix, locked AE/source shape domain, fixed `S=N/2` | deterministic external-weight cache and approved whole-planner fallback | ciphertext weights, unknown weights, or unapproved shape fallback |
| Conv | one ciphertext input/output, plaintext kernel, square odd kernel, stride 1, symmetric same padding, channel-divisible or explicitly zero-padded | every broader case below needs a dedicated oracle | unapproved fallback that silently changes shape/layout |
| broader Conv | none assumed | stride >1, general/valid/asymmetric padding, depthwise/grouped Conv, multi-ciphertext tensors, height sharding, halo exchange, output compaction | rejected until its extension gate passes |
| CKKS chain | complete named chain is represented; ReSBM projects it to integer scale-degree plus logical level with `q_w=q` | Stage 1 ReSBM accepts uniform one-level-consuming regions only | `q_w!=q`, non-uniform or multi-level-consuming ReSBM schedules |
| relin | canonical `MulCC` has an instantaneous three-component result followed immediately by mandatory `Relin`; ordinary SSA values have two components | none in current O2 | non-immediate relin or a provider that cannot implement immediate relin |
| loops/SCC | acyclic graph or compile-time-known retained loop whose multi-node SCC consumes zero levels | multiplication loops may be legally unrolled when trip <=64 and expanded encrypted nodes <=10000 | retained SCC with multiplication, unknown trip, scale-changing recurrence, or exceeded bound |
| calls | intragraph artifact algorithm | nonrecursive direct calls with persisted formal/actual contracts and one resolved body | recursion, scale-changing recursive call, indirect call, missing effect or frequency summary |
| frequency | known positive static count; unreachable is exactly zero | profile count only with provenance/hash | unknown treated as zero or silently guessed |

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
| CKKS | resolved parameters, value state, immediate relin/rescale/mod-switch/bootstrap, ReSBM | immutable semantic plan plus materialization proof |
| POLY | Stage 1 runtime-library implementation | no private POLY nodes; Stage 2 owns HPOLY/HPAO |
| C/runtime | standard calls, descriptors, lifetimes, key/provider manifests | stable provider-neutral C ABI |

Useful fixed ACE anchors include
`fhe-cmplr/ckks/src/ckks.cxx:36-51`,
`fhe-cmplr/include/fhe/core/ctx_param_ana.h:614-635`,
`fhe-cmplr/poly/src/poly2c_driver.cxx:121-157`, and
`fhe-cmplr/include/fhe/core/rt_data_mgr.h:19-79`. They are evidence, not code to
copy blindly.

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

## 6. Mandatory File-Wide FHE Canonicalization

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
  its v0.9 bootstrap policy provenance;
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

## 7. Stable Runtime and Provider Boundary

Stage 1 cannot call a provider through C++ library types, `std::shared_ptr`, STL
containers, exceptions, or provider-owned class layouts and then label that
surface a stable C ABI.

### 7.1 ABI contract

The proposed boundary uses opaque handles and fixed C structures:

```c
typedef struct dsc_fhe_provider dsc_fhe_provider;
typedef struct dsc_fhe_context dsc_fhe_context;
typedef struct dsc_fhe_keyset dsc_fhe_keyset;
typedef struct dsc_fhe_ciphertext dsc_fhe_ciphertext;

typedef struct {
  uint32_t struct_size;
  uint32_t abi_version;
  uint64_t capability_bits;
  uint32_t provider_id;
  uint32_t provider_version;
} dsc_fhe_capability_v1;
```

Every public structure starts with size/version fields and uses fixed-width
integers. Provider memory is accessed only through opaque handles and exported
create/retain/release/destroy functions. The ABI contract fixes:

- ownership, borrowing, aliasing, and destruction for every handle/buffer;
- thread safety and reentrancy per handle type;
- no exception crossing; stable status codes plus thread-local/handle-local
  diagnostic retrieval;
- capability query and manifest fingerprint;
- context setup and resolved-parameter import/export;
- key generation/import/export, including rotation, mandatory immediate relin,
  and bootstrap data;
- bootstrap setup/profile validation and evaluation;
- encode/encrypt/decrypt/decode and Stage 1 arithmetic primitives; and
- deterministic mock-provider behavior for tests.

No secret key or secret-key-derived private material is persisted in WHIRL,
planner records, provider manifests, retained test artifacts, or generated C.
Records may persist only the secret-key distribution identifier, logical key
requirements, and hashes/provenance for public or evaluation-key setup. Test
decryption keys remain ephemeral inside the isolated test provider process.

The OpenFHE adapter is behind this boundary. Generated C must compile, link,
load the adapter, construct the context and keys, execute, decrypt/decode, and
destroy every object under ASan/LSan where supported.

### 7.2 Provider manifest

Each execution records ABI version, provider/build ID, capability bits, target,
resolved-parameter fingerprint, supported fixed ring dimensions, scaling
technique, level semantics, immediate-relin support, bootstrap profiles,
key-expansion rules, thread mode, and runtime library hash.
The manifest is evidence/cache data, not an alternative source of compilation
intent.

## 8. Semantic Records and Physical Crosswalk

### 8.1 No duplicate truth

The names below are semantic master-plan concepts. They do not by themselves
allocate new WHIRL sections or tables. Existing authoritative records stay
authoritative; proposed records either extend them under a reviewed version or
reference them by stable ID. Derived costs, provider expansion, and reports are
never allowed to overwrite user intent or resolved parameters.

| Semantic record | Existing image/table | Proposed physical record or gap | Authority versus derived/cache | Verifier | Serialization owner |
| --- | --- | --- | --- | --- | --- |
| `FHECompilationConfigIR` | `WT_DSL_FHE_IMAGE` / `DSL_FHE_COMPILATION_CONFIG_RECORD` | versioned extension for accepted O-level/planner options after `F0`; fixed `N`, security target, bootstrap policy, and precision/scale constraints exist before canonicalization; no frozen option allocation while frozen | authoritative user/project intent; normalized aliases derived; `N` is never resolver-selected | FHE config verifier plus option truth-table and fixed-`N` tests | `osprey/common/com/dsl_fhe.*` |
| `CKKSResolvedParameterIR` | no complete table; current value-state rows are insufficient | proposed versioned parameter record referencing one config/key domain | authoritative resolved compilation contract; provider import blob is cache/evidence | parameter/security/provider verifier | `osprey/common/com` after `P2` review |
| `CKKSValueStateIR` | `DSL_FHE_CKKS_VALUE_STATE_RECORD` v1 | proposed compatible successor with parameter ID, scale identity, bounds, chain position, and action IDs | derived from source plus resolved parameters; never a second parameter truth; three components exist only in the transient `MulCC` result before immediate `Relin` | independent CKKS transfer verifier | `osprey/common/com` FHE plan owner |
| `CKKSScaleBootstrapPlanIR` | pending-action fields only | proposed region/action records; exact storage frozen until `F0` | selected plan is authoritative for materialization; candidates/cost tables are derived | ReSBM semantic oracle and materializer verifier | planner producer plus `osprey/common/com` serializer |
| `EncryptedTensorLayoutIR` | packing policy plus layout-name string | proposed explicit logical-to-physical slot map, class map, shards, masks, and lineage | selected layout authoritative; candidate costs/cache derived | slot-map oracle and canonicalization verifier | planner producer plus `osprey/common/com` serializer |
| `MetaKernelPlanIR` | none | proposed input, artifact-equivalent `(Pb,Ps)` reparameterization, derived values, cost components, operations, extension flags | selected candidate authoritative within selected planner; transformed weights derived/cache | independent artifact-contract and slot oracle plus separate paper-strict differential oracle | MetaKernel planner plus `osprey/common/com` serializer |
| `FHEBackendCapabilityIR` | none | proposed normalized capability/ABI manifest reference | provider evidence, not compilation intent; hash-bound to resolved parameters | ABI probe and capability-consumption verifier | runtime adapter; compiler stores immutable reference |
| `KeyMaterialContractIR` | `DSL_FHE_KEY_REQUIREMENT_RECORD` | retain logical rows; add separate provider-expansion manifest reference | logical requirements authoritative; provider-expanded key list derived/evidence | logical collector and provider expansion verifier | `osprey/common/com/dsl_fhe.*` for logical; adapter for expansion |
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
required by v0.9. The three-component multiply result may be represented for the
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
| materialized `dsc_fhe_*` call | ordered runtime action | context/key reads, allocation/lifetime, status and failure ordering | ordinary WOPT may optimize surrounding scalar/control flow only; the FHE call is ordered/effectful by default |

The provider adapter must state immutable-result versus in-place behavior,
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
`dsc_fhe_*` calls. They also cover distinct key domains/scales, may-alias
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

### 11.3 Bootstrap language

Neither ReLU nor arbitrary nonlinear computation implies a general theorem that
a bootstrap must occur at a particular point. Mandatory refresh immediately
before each surviving `common.relu` is the frozen first-release DSC v0.9 policy,
not a ReSBM or CKKS theorem.

The current v0.9 meanings remain authoritative during the freeze:

- `auto/on` at bare `-O0` materializes a mandatory refresh before each surviving
  `common.relu` and deterministic greedy JIT refresh elsewhere when permitted;
- `manual` creates no compiler bootstrap, requires every surviving ReLU boundary
  to be explicit, and rejects any path that exhausts the chain; and
- `off` forbids bootstrap, rejects a surviving ReLU under the first-release
  policy, and requires every other path to fit the resolved chain.

`FRZ-03` and `FRZ-04` must decide whether a complete Fhelipe path preserves and
implements these semantics directly or proposes a specific ADR amendment, and
which layer realizes the policy. The freeze does not turn the current modes into
undefined behavior.

An explicit user-authored manual bootstrap site remains authoritative in every
case: a planner may not delete or relocate it. The freeze concerns supplemental
automatic placement and surrounding optimization semantics, not permission to
override the manual site.

Every mode fails closed when the final plan violates level, scale, range,
precision, security, or provider capability. Reports distinguish
`policy-mandated`, `planner-selected`, and `provider-required` bootstraps.

## 12. Program-Wide Planning and Materialization Flow

### 12.1 Required transaction

Subject to the architecture freeze, the target implementation flow is:

```text
requested configuration: fixed N + security + bootstrap + precision/scale
  -> persist FHECompilationConfigIR before graph transformation
accepted converted application.fhe.B + fixed configuration
  -> independent reopen and complete source/image validation
  -> canonicalize every PU and persist canonical evidence
  -> build immutable common planner input
  -> select exactly one ADR-approved planner family
       complete Fhelipe using fixed N and S=N/2
       or MetaKernel using fixed N and S=N/2 -> CKKS -> ReSBM
  -> finalize remaining CKKS parameters and validate the fixed N
  -> resolve logical/provider keys and provider capabilities
  -> independently verify normalized final plan
  -> atomically write binary plan plus reports
  -> exit producer process
  -> independent materializer reopens source and plan
  -> validate fingerprints and materialize each PU
  -> Stage 2 HPOLY/HPAO hook or Stage 1 bypass
  -> lower all remaining virtual operations to standard WHIRL calls
  -> run WHIRL/DSL/FHE verifiers
  -> retain application.o2.mid.B and ir_b2a -st -src output
  -> whirl2c -> generated C -> compile/link/load/run provider
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

## 13. Prerequisites and O2 Stage 1 Milestones

All commands in this section are **Proposed target commands** to be added by the
named milestone. They are exact interfaces, not claims that the scripts already
exist. A milestone cannot close while its fixture `SHA256SUMS` contains a
placeholder, while a required test is skipped, or while the command differs from
the checked-in contract without a reviewed plan update.

Acceptance dependencies are:

```text
F0 -> F1-PREP -> P0 -> P1a -> P2 -> P1b -> F1-IMPL -> F1-ACCEPT
                                                   -> Stage 1 integration
```

F1-PREP and provider-independent preparation for P0/P1a/P2 may overlap in time,
but no arrow may be bypassed at milestone closure. In particular, P1b consumes
P2 parameters/profiles, F1-IMPL consumes all P0/P1a/P2/P1b contracts, and S1.9
requires F1-ACCEPT.

### F0: Resolve planner ownership and synchronize architecture

**Objective:** accept `FRZ-01` through `FRZ-07`, update v0.9, and make the common
planner input/final-plan ownership mechanically unambiguous.

**Implementation:** write one ADR containing the Fhelipe source lock, ownership
matrix, O-level/option truth table, common input/output semantics, comparison
protocol, fallback policy, and physical-record decision. Update v0.9 in the same
review. This milestone makes no compiler code change.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-F0-001`; `python3 osprey/common/com/tests/verify_fhe_o2_architecture_lock.py --adr doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md --design doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.9.docx --plan doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md --source-lock testdata/fhe_o2/source-lock.json` | v0.9 hash in Section 2.3; ADR and Fhelipe bundle hashes must be concrete; architecture-document input | exact presence/consistency of all seven decisions; no tolerance; `FHE-O2-ARCH-FREEZE-UNRESOLVED` on any omission | deterministic; 64 option combinations minimum | any supported host; no provider | 0 warmups, 1 run, 120 s; retain normalized truth table, hash report, and ADR diff in `test-artifacts/o2/O2-F0-001/` for repository lifetime | architecture owner / common-com, FHE, runtime reviewers; exit 0, zero `UNKNOWN`, and accepted ADR+v0.9 in same change |

**Exit:** the Section 0 freeze is lifted only for the exact accepted contract.

### F1-PREP: Prepare the F0-selected bare-`-O0` baseline evidence

**Objective:** prepare reproducible inputs for the baseline selected by F0
without publishing a production planner or closing bare-`-O0` acceptance. This
work may begin in parallel with P0/P1a preparations, but it closes only against
the concrete source lock accepted by F0.

**Implementation:** inspect the source, record paper/code delta and license,
reproduce the fixed build, freeze supported fixtures/golden outputs, and specify
an independent layout/state/bootstrap oracle. If F0 selects complete Fhelipe,
the evidence covers its indivisible layout/rescale/bootstrap path. If F0 selects
another baseline, this milestone requires an amendment of equal specificity.
It may not add a production planner, public default, or O0 acceptance claim.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-F1P-001`; `python3 osprey/be/vho/tests/prepare_fhe_o0_baseline_evidence.py --source-lock testdata/fhe_o2/o0/source-lock.json --manifest testdata/fhe_o2/o0-prep/SHA256SUMS --artifacts test-artifacts/o2/O2-F1P-001` | exact F0-selected source ref/hash and local bundle; paper, license, build recipe, static MVM/Conv/branch/deep-bootstrap fixtures and locked ResNet-20 inputs | source/hash/delta/license/build reproduction exact; golden/oracle schema complete; unsupported rows have exact diagnostics; no production plan/default or acceptance artifact is emitted | seed `0x46315052`; focused shapes 1..32, depth 0..12; full inputs fixed by manifest | pinned Ubuntu 22.04 x86_64; source build plus provider-free golden generator; no production adapter required | 0 warmups, 2 clean builds/golden generations, 60 min; retain source-lock report, delta, license, build logs, fixtures, goldens, oracle design, and hashes for repository lifetime | O0 preparation owner / architecture, legal, independent test reviewers; exact F0 hash, reproducible outputs, zero placeholder, and audit proves no production planner/default was published |

**Exit:** the accepted source and test basis is reproducible and ready for
implementation. Bare `-O0` remains unimplemented/unaccepted at this point.

### P0: Mandatory file-wide FHE canonicalization

**Objective:** provide the common normal form in Section 6 independently of
planner family and FHEFusion enablement.

**Implementation:** add a dedicated FHE canonicalization stage, file-wide
lineage and slot-class verifier, stable dump, corruption diagnostics, and an
all-PU checkpoint. Reuse generic DSL helpers only when their semantics are
identical.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-P0-001`; `python3 osprey/be/vho/tests/run_fhe_canonicalization_contract.py --build-dir build --manifest testdata/fhe_o2/canonicalization/SHA256SUMS --artifacts test-artifacts/o2/O2-P0-001` | checked-in raw/canonical `.B` plus fixed-N `FHECompilationConfigIR`; all-PU calls, mask, slice, valid/zero/junk/gap, padding, lineage, disabled fusion | two applications and reopen reach byte-identical normal form; requested `N` unchanged; per-slot oracle exact; capacity/lineage negatives emit fixture diagnostic; no numeric tolerance | seed `0x43414e4f`; fixed `N in {2^12..2^16}`, <=8 PUs, <=1000 nodes, sentinels `{-7,-1,2,11}` | supported host; provider-free mock evaluator | 0 warmups, 2 clean invocations per fixture, 300 s; retain requested config, input/output `.B`, dumps, hashes, corruptions 180 days and accepted corpus for repository lifetime | FHE canonicalization owner / common-com and optimizer reviewers; exact idempotence/determinism, zero N mutation, no unresolved slot class or skipped fixture |

**Exit:** canonicalization is identical with fusion off/on-bypass and survives an
independent reopen.

### P1a: Provider-neutral C ABI and mock provider

**Objective:** accept the provider-neutral ABI, capability, ownership, error,
immediate-relin, and bootstrap boundary in Section 7 before records or generated
code depend on provider-specific behavior.

**Implementation:** add the provider-neutral C header/library, deterministic mock
adapter, capability manifest schema, ABI conformance tool, and generated C smoke
programs. No OpenFHE adapter is included and no provider C++ type crosses the
public header.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-P1A-001`; `python3 osprey/libfhe/tests/run_provider_cabi_mock_contract.py --build-dir build --manifest testdata/fhe_o2/provider-mock/SHA256SUMS --artifacts test-artifacts/o2/O2-P1A-001` | public C header hash, ABI golden layouts, fixed mock manifest, generated C ownership/error programs | C/C++ compile and C link/load/run exact; fixed-`N` capability, mandatory `MulCC->Relin`, bootstrap setup, status ordering and mock values exact; invalid ownership/version/capability returns manifest code and no leak | seed `0x50314131`; 1..4 threads, 100 create/use/destroy cycles, component counts 2/3 | pinned Ubuntu 22.04 x86_64; deterministic mock; ASan/LSan required for leak subtest | 1 warmup, 10 functional runs, 30 min; retain headers, ABI sizes, mock manifest, generated C, logs, and sanitizer reports for repository lifetime | runtime ABI owner / FHE planner and external C reviewer; zero C++ ABI exposure, zero sanitizer finding, exact call/effect ordering, all mock capabilities consumed consistently |

**Exit:** the public C ABI and mock provider are accepted. No claim about an
OpenFHE adapter or cryptographic execution is made.

### P2: Semantic record and CKKS contract review

**Objective:** accept the crosswalk, CKKS semantics, and physical serialization
decision allowed by F0 without creating duplicate authority.

**Implementation:** add builders/readers/verifiers/dumpers for accepted records;
add fixed-`N` provenance, transfer-rule tables, mandatory immediate-relin state,
security validation, bootstrap profiles, corruption tests, and the independent
reopen harness. P2 consumes P0 identities and the P1a ABI/capability schema.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-P2-001`; `python3 osprey/common/com/tests/run_fhe_o2_record_contract.py --build-dir build --manifest testdata/fhe_o2/records/SHA256SUMS --artifacts test-artifacts/o2/O2-P2-001` | public builders, P0 stable identities, P1a manifest schema, golden/corrupt `.B`, fixed `N`, resolved uniform/non-uniform chains, bootstrap profiles | clean reopen and `ir_b2a` exact; requested `N` unchanged; immediate `MulCC->Relin` exact; every invalid ID/range/hash/profile fails with listed diagnostic; no tolerance | seed `0x52454344`; every header/count/range/enum family, chain lengths 1..16, components 2/3 | all supported readers; fixed mock capabilities; OpenFHE profile examples are data only | 0 warmups, 2 clean serializations, 300 s; retain `.B`, dumps, hashes and corruptions 180 days, golden images indefinitely | common-com serialization owner / architecture, FHE, CKKS, compatibility reviewers; exact sizes/version behavior, old-reader policy, fixed-N provenance, no duplicate truth |

**Exit:** a producer-free independent process proves the accepted physical and
semantic contract. Frozen top-level fields remain absent unless F0 authorized
them.

### P1b: OpenFHE adapter on the accepted P2 parameter/profile contract

**Objective:** implement the OpenFHE provider behind P1a only after P2 fixes the
parameter, bootstrap-profile, state, immediate-relin, and serialization contract.

**Implementation:** pin the OpenFHE revision/build, map every P2 field and
capability explicitly, import the fixed `N`, construct the resolved Q/P chain and
keys, execute generated C smoke programs, and reject unsupported fixed-N/profile
combinations. No adapter default may override the compilation record.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-P1B-001`; `python3 osprey/libfhe/tests/run_openfhe_adapter_contract.py --build-dir build --manifest testdata/fhe_o2/provider-openfhe/SHA256SUMS --records testdata/fhe_o2/records/SHA256SUMS --artifacts test-artifacts/o2/O2-P1B-001` | pinned OpenFHE source/build hashes, accepted P1a header and P2 golden parameter/profile records; add/mul/rotate/immediate-relin/rescale/bootstrap programs | field/capability mapping and `MulCC->Relin` adjacency exact; generated C compiles/links/loads/runs; decoded `abs<=1e-4`, `rel<=1e-6`; unsupported fixed N, profile, ownership, or version returns exact diagnostic without fallback/leak | seed `0x50314231`; fixed `N in {2^12..2^16}`, chain 1..16, 1..4 threads, 100 lifecycle cycles | pinned Ubuntu 22.04 x86_64 and pinned OpenFHE; ASan/LSan where supported | 2 warmups/10 functional runs, 30 min/sample; retain source/build manifests, records, C, binaries, outputs, logs, and sanitizer reports for repository lifetime | OpenFHE adapter owner / P1a ABI, P2 CKKS, crypto, external C reviewers; no C++ ABI leak, no record override, zero sanitizer finding, all fixtures/diagnostics exact |

**Exit:** OpenFHE implements the accepted P1a/P2 contract and generated C
executes without changing requested `N` or any persisted parameter/profile fact.

### F1-IMPL: Implement the F0-approved bare-`-O0` planner

**Objective:** implement the complete baseline selected by F0 on accepted P0,
P1a, P2, and P1b contracts. Complete Fhelipe remains a preferred proposal until
F0 accepts it; this milestone does not pre-decide that choice.

**Implementation:** consume P0 canonical input and fixed `N`, implement the
F1-PREP locked layout/rescale/bootstrap semantics as one production owner, emit
the accepted normalized plan representation, and materialize through P1a calls.
If F0 chooses a non-Fhelipe baseline, the plan is amended with equal algorithmic
and test specificity before implementation.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `O2-F1I-001`; `python3 osprey/be/vho/tests/run_fhe_o0_baseline_plan_contract.py --build-dir build --source-lock testdata/fhe_o2/o0/source-lock.json --manifest testdata/fhe_o2/o0-plan/SHA256SUMS --artifacts test-artifacts/o2/O2-F1I-001` | exact F0/F1-PREP source ref/hash, P0 canonical `.B`, accepted P2 records; static MVM/Conv/branch/deep-bootstrap and locked ResNet-20 plan inputs | normalized plan, layout, state, v0.9 bootstrap policy, immediate relin, and keys match the independent oracle exactly; fixed `N` unchanged; unsupported input gives manifest diagnostic before publication | seed `0x4631494d`; focused shapes 1..32, depth 0..12, PUs 1..16; full input fixed by manifest | supported host; provider-free oracle plus accepted P1a mock | 0 warmups, 2 deterministic productions, 60 min; retain source/canonical/plan `.B`, dumps, hashes, oracle diff, and diagnostics for repository lifetime | O0 planner owner / architecture, P0/P2, independent oracle reviewers; zero structural mismatch, no unexpected fallback, deterministic independent reopen |

**Exit:** the F0-approved planner is implemented and emits an independently
verifiable plan, but bare-`-O0` is not accepted until F1-ACCEPT.

### F1-ACCEPT: Materialize and accept bare `-O0`

**Objective:** accept the F1-IMPL plan through independent reopen,
materialization, generated C, OpenFHE execution, and the exact F0 option policy.

**Implementation:** reopen the normalized plan in a producer-free process,
materialize all PUs with mandatory immediate relin, generate C, execute through
P1b, and retain layout/state/bootstrap/key and numerical evidence.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `O2-F1A-001`; `python3 osprey/be/vho/tests/run_fhe_o0_baseline_acceptance.py --build-dir build --manifest testdata/fhe_o2/o0/SHA256SUMS --artifacts test-artifacts/o2/O2-F1A-001` | F0/F1-PREP/F1-IMPL hashes; P0/P1a/P2/P1b accepted artifacts; static MVM, Conv, branching, deep-bootstrap and locked ResNet-20 application/data/weights | normalized-plan reopen/materialization exact; generated C and OpenFHE run; immediate relin/key/lifetime exact; focused decoded `abs<=1e-4`, `rel<=1e-6`; Section 15.3 application gate; exact unsupported diagnostic | seed `0x4648454c`; focused shapes 1..32, depth 0..12; full application fixed by manifest | pinned Ubuntu 22.04 x86_64; accepted mock/OpenFHE manifests | correctness 0/1; focused performance 5/30 paired; full 1/5 paired; 20 min focused, 3 h full; retain `.B`, `ir_b2a`, C, manifests, keys, decoded/oracle output, sanitizer and run logs for repository lifetime | O0 acceptance owner / architecture, independent O2, crypto, runtime reviewers; all locked fixtures pass, zero unexpected fallback/leak, fixed N unchanged, signed `bare O0 accepted` decision |

**Exit:** bare `-O0` is executable, independently reopened, and accepted under
the exact F0/v0.9 option semantics. Stage 1 remains blocked until this exit.

### S1.0: Lock Stage 1 algorithms, fixtures, diagnostics, and cost model

**Objective:** translate paper/fixed-source assumptions into frozen executable
contracts before production algorithms are written.

**Implementation:** check in source-lock manifests, paper teaching cases,
paper/artifact delta cases, support matrices, diagnostics, tie orders, raw
latency tables, fixture generators, and independent-oracle interfaces. Record
fixed-resolved and auto-resolved test families separately; `N` is the same fixed
requested input in both, while only remaining CKKS parameters are auto-resolved.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S10-001`; `python3 osprey/be/vho/tests/verify_o2_stage1_source_lock.py --manifest testdata/fhe_o2/stage1/source-lock.json --diagnostics testdata/fhe_o2/stage1/diagnostics.json --support testdata/fhe_o2/stage1/support-matrix.json --artifacts test-artifacts/o2/O2-S10-001` | all hashes in Section 2.3, MetaKernel/ReSBM paper-artifact delta corpus, and generated fixture manifests | exact hashes, refs, test IDs, support rows, fixed-N rule, diagnostics, tolerances, cost units and artifact/paper claim labels; `FHE-O2-SOURCE-LOCK-MISMATCH` on change | generators declare fixed seeds/bounds; no unbounded generator | provider-free; manifests name future provider requirements | 0 warmups, 1 run, 120 s; retain normalized lock report, differential cases, and source delta indefinitely | Stage 1 owner / paper-code and test reviewers; exit 0 and no placeholder, moving ref, duplicate ID, unowned diagnostic, or paper-exact label on artifact-only behavior |

**Exit:** every Stage 1 claim maps to a locked source and an independent test.

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

**Objective:** reproduce the source-locked artifact search/materialization
contract for the frozen MVM input class and separately measure its differences
from paper Eq. 11.

**Implementation:** reproduce artifact-equivalent `(Pb,Ps)` enumeration,
`ceil(log2(rep))`, artifact capacity/full-slot predicate, `Get_num_rot` cost, and
larger-`Ps` tie; independently derive every field, transform plaintext weights,
and construct exact slot maps/rotations. A separate paper-strict oracle records
Eq. 11/capacity differences. Do not call ReSBM.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S13-001`; `python3 osprey/be/vho/tests/run_metakernel_mvm_contract.py --build-dir build --source-ref d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be --manifest testdata/fhe_o2/metakernel-mvm/SHA256SUMS --artifacts test-artifacts/o2/O2-S13-001` | locked `origin/metakernel-proof@d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be`; AE/golden neural shapes, Figure 4, MVM1, MVM2, non-power-of-two rep/Ps, `nd<kd`, `kd==S`, exact/one-over capacity, zero/ill-conditioned weights | production candidate legality/cost components/tie/winner, full slot classes, no out-of-range access, transformed weights and signed rotations exact against independent artifact oracle; paper-strict differential equals recorded delta; clear `1e-12/1e-12`, decoded OpenFHE `1e-4/1e-6`; unsupported shape or no candidate emits manifest code | seed `0x4d4b5231`; `n,k=1..32`, fixed `N` with `S=N/2 in {8,16,32,64}`, all divisor pairs plus named AE bounds | pure search any host; accepted mock/OpenFHE for execution with rotate/mul/plain capabilities | pure 0/1; runtime 2 warmups/10 samples, 10 min/sample; retain source delta, artifact/paper candidate tables, complete slot traces, keys, outputs, C and `.B` 180 days | MetaKernel owner / independent artifact-oracle, paper-differential, tensor and runtime reviewers; zero production-oracle mismatch, recorded differential exact, numeric bounds met, no unsupported implicit fallback |

**Exit:** every candidate component is independently recomputed and every
selected MVM executes correctly.

### S1.4: MetaKernel Conv core and gated extensions

**Objective:** implement the source-locked/AE Conv core first, then enable only
separately accepted extension rows without broad paper-Algorithm-3 claims.

**Implementation:** implement Ke2Col for the Section 4.2 core; attach an
extension capability/diagnostic to every stride, padding, depthwise, multi-CT,
shard, halo, or compaction case. Each extension commit carries its exact oracle.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S14-001`; `python3 osprey/be/vho/tests/run_metakernel_conv_contract.py --build-dir build --source-ref d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be --manifest testdata/fhe_o2/metakernel-conv/SHA256SUMS --support testdata/fhe_o2/stage1/support-matrix.json --artifacts test-artifacts/o2/O2-S14-001` | locked artifact/AE golden 1x1/3x3/5x5 stride-1 same-pad shapes plus one manifest row per approved extension and negative boundary | artifact plan, logical tensor, complete physical slot classes, masks, padding, output layout and rotation components exact; no out-of-range slot; clear `1e-12/1e-12`, decoded `1e-4/1e-6`; unsupported source-domain row emits exact code or approved whole-planner fallback | seed `0x4d4b5243`; fixed `N`; C in/out 1..16, H/W 3..32, kernel 1/3/5; extension bounds in manifest | accepted mock/OpenFHE; capability rows fixed per fixture | 2 warmups/10 samples focused, 20 min/sample; retain source ref/delta, transformed weights, full slot/sentinel traces, key manifests, output/C/`.B` 180 days | MetaKernel Conv owner / artifact, tensor-semantics and runtime reviewers; all locked core rows pass; an extension is enabled only if its entire row passes |

**Exit:** unsupported Conv inputs never enter the core path; supported outputs and
slot classifications are exact.

### S1.5: CKKS resolution, state/effect contracts, and WOPT non-interference

**Objective:** finalize the remaining CKKS parameters while validating the fixed
`N`, implement Section 9 state/action semantics and Section 11 effects, and prove
that ordinary WOPT does not transform ordered FHE actions.

**Implementation:** resolve complete parameter/profile records, propagate range
and error, emit mandatory immediate `MulCC->Relin`, validate fixed-N capacity,
resolved-Q/P security and provider support, and add optimizer
negative/non-interference tests. Fixed-parameter and auto-resolution suites
remain separate; the auto suite holds `N` fixed and resolves only slots, depth,
chain, scale/bootstrap profiles, keys, and achieved security. No new
FHE-specific WOPT profitability pass is promised here.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S15-001`; `python3 osprey/be/vho/tests/run_ckks_state_contract.py --build-dir build --manifest testdata/fhe_o2/ckks/SHA256SUMS --artifacts test-artifacts/o2/O2-S15-001` | fixed-N requested records, fixed/auto-remaining uniform chains, explicit non-uniform/multi-consumption negatives, bootstrap profiles, `MulCC->Relin`, and PRE/CSE/DCE/hoist/speculation cases for state actions/context/key/lifetime/`dsc_fhe_*` | requested `N` unchanged; remaining resolution, state/action/security/key traces and immediate-relin adjacency exact; bounds conservative; decoded `abs<=1e-4`, `rel<=1e-6`; capacity/security/provider failure reports minimum acceptable/recommended N without mutation; WOPT dump shows non-interference | seed `0x434b4b53`; fixed `N in {2^12..2^16}`, chain 1..16, canonical components 2 with transient 3, fanout 1..8 | accepted mock/OpenFHE manifests; security estimator/version hash fixed | 0/1 transfer; 2/10 encrypted, 20 min/sample; retain requested config, resolved params, traces, optimizer dumps, keys, diagnostics, output 180 days | CKKS owner / cryptography, optimizer, runtime reviewers; zero N mutation, illegal action movement or ordinary-WOPT FHE transform, all errors within predicted bounds |

**Exit:** every materializable value has one valid resolved state and no optimizer
rewrite violates effects or error/security budgets.

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

**Exit:** production and independent enumeration agree completely; a cost-only
or placement-only match is insufficient.

### S1.8: Open64 call/control-flow extension and standard-call materialization

**Objective:** add the bounded Open64 extension, materialize the verified plan,
and execute through P1b without private nodes.

**Implementation:** add direct-call summaries, compile-time-known zero-level
retained loops, stable edge actions, exact logical/provider key manifests,
mandatory adjacent relin actions, transformed data descriptors, lifetime/error
code, standard calls, and final verifier. Unsupported multiplication SCC,
unknown trip, recursion/scale-changing recursive call, indirect call, and unknown
frequency fail before publication.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S18-001`; `python3 osprey/be/vho/tests/run_o2_materialization_contract.py --build-dir build --manifest testdata/fhe_o2/materialization/SHA256SUMS --artifacts test-artifacts/o2/O2-S18-001` | locked MetaKernel/ReSBM refs; direct-call, phi/fanout/critical-edge, zero-level loop, unrollable/rejected multiplication SCC, residual, MVM/Conv/ReLU/bootstrap, unknown-trip/recursive-scale-change/indirect/unknown-frequency fixtures | reopened materialization structurally exact; every `MulCC` immediately followed by Relin; no private op in `ir_b2a`; logical/provider keys exact separately; decoded `1e-4/1e-6`; exact diagnostics before partial publication | seed `0x4d415431`; call depth <=8, trip <=64, expanded nodes <=10000, fixed N | pinned Ubuntu 22.04 x86_64; accepted P1a mock/P1b OpenFHE manifests; generated-C capability required | 2 warmups/10 samples, 30 min/sample; retain source/plan/mid `.B`, `ir_b2a`, C, binaries, manifests, keys, outputs, diagnostics and logs 180 days | materializer owner / backend, WOPT, ReSBM, runtime reviewers; compile/link/load/run succeeds, zero leak, exact state/keys, no unsupported WHIRL or retained multiplication SCC |

**Exit:** the independently reopened plan is the sole source of materialization
decisions and the generated program executes correctly.

### S1.9: O2 Stage 1 acceptance

**Objective:** certify MetaKernel+ReSBM against the accepted F1-ACCEPT bare-O0
baseline using the F0 comparison protocol.

**Implementation:** run correctness, structural, key, cost-model, performance,
determinism, and failure matrices in Section 15. If F0 treats Fhelipe as an
indivisible end-to-end baseline, only `BASE-O0` versus `S1-FULL` is a hard
comparison. `S1-MKR`/`S1-RESBM` remain marked `F0-blocked` unless F0 approves
component separability or a neutral baseline. Publish one immutable acceptance
bundle and review decision.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S19-001`; `python3 osprey/be/vho/tests/run_o2_stage1_acceptance.py --build-dir build --manifest testdata/fhe_o2/acceptance-stage1/SHA256SUMS --protocol testdata/fhe_o2/acceptance-stage1/protocol.json --artifacts test-artifacts/o2/O2-S19-001` | accepted F0/P0/P1a/P2/P1b/F1-PREP/F1-IMPL/F1-ACCEPT and Stage 1 hashes; fixed-resolved and auto-remaining suites with the same fixed N; frozen focused/full workloads | all Section 15 hard gates; artifact-oracle equality and recorded paper differentials; focused decoded `1e-4/1e-6`, application max-abs `<=1e-3`, top-1 exact; immediate relin and no N mutation; only F0-defined comparison cells are hard | workload seeds in protocol; resampling seed `0x4f325331`; support matrix and F0 profile-status table fixed | one pinned Ubuntu x86_64 machine, accepted mock/OpenFHE manifests; no cross-machine ratio | focused 5 warmups/30 paired/20 min; full 1/5 paired/3 h; no outlier deletion; retain complete accepted bundle for repository lifetime | Stage 1 owner / architecture, independent test, artifact, crypto, runtime reviewers; every prerequisite and hard quantitative gate passes, F0-blocked cells are labeled not fabricated, signed review says only `O2 Stage 1 accepted` |

**Exit:** Stage 1 is accepted. The report must not say `O2 complete`, `full O2
accepted`, or `v0.9 O2 complete`.

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
| `O2-S21-001`; `python3 osprey/be/vho/tests/run_fhefusion_semantic_contract.py --build-dir build --catalog testdata/fhe_o2/fhefusion/rules.json --manifest testdata/fhe_o2/fhefusion/SHA256SUMS --artifacts test-artifacts/o2/O2-S21-001` | locked `origin/fhefusion@f18f971710b7270aec2c3963878ac3493d9f8e06` and fixture hashes; rule graphs, overlaps, pass permutations, sentinel junk/gaps, no-fusion controls | rule match/reject and slot-map output exact; clear `abs<=1e-12`, `rel<=1e-12`; decoded `1e-4/1e-6`; exact rule diagnostic | seed `0x4655534e`; <=12 nodes exhaustive local overlaps, 2000 random legal contexts | mock/OpenFHE; same parameters for before/after pair | 2 warmups/10 paired samples, 20 min/sample; retain pre/post `.B`, slot traces, outputs, rule log 180 days | FHEFusion owner / independent tensor oracle and CKKS reviewers; zero semantic mismatch, idempotent fixed point, deterministic rule order |

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
| `O2-S22-001`; `python3 osprey/be/vho/tests/run_fhefusion_profitability_contract.py --build-dir build --manifest testdata/fhe_o2/fhefusion-profit/SHA256SUMS --model testdata/fhe_o2/cost/openfhe-v1.json --artifacts test-artifacts/o2/O2-S22-001` | locked `origin/fhefusion@f18f971710b7270aec2c3963878ac3493d9f8e06`, cost-model/fixture hashes; gap densities 0..15/16, boundary shapes around fixed `N/2`, fixed and auto-remaining suites with identical N | capacity/security decision and minimum-required-N diagnostic exact; predicted components exact; legal output within numeric tolerance; a larger-N rewrite is rejected or requests explicit recompilation; unprofitable/unsafe reason exact | seed `0x4741504e`; fixed `N in {2^12..2^16}`, shards 1..8 | pinned OpenFHE host/manifest; provider capability and memory counters required | 5 warmups/30 paired focused samples, 20 min/sample; no outlier deletion; retain requested config, decisions, recommendations, predictions, measurements, C/`.B` 180 days | profitability owner / CKKS security, MetaKernel, performance reviewers; zero N mutation, no infeasible rewrite, quantitative model gates pass |

**Exit:** every selected fusion has a complete capacity/security/cost proof and
uses the post-fusion graph for downstream planning.

### S2.3: HPOLY contract and bootstrap-opaque lowering

**Objective:** introduce the HPAO middle-level representation without exposing
bootstrap internals or changing accepted CKKS plan semantics.

**Implementation:** define versioned HPOLY operators, basis/level/scale/static
attributes, effect/alias rules, CKKS-to-HPOLY and HPOLY-to-POLY/runtime lowering,
and independent serialization if the architecture review selects persistence.
HPAO reuses Open64 SSA/HSSA, value numbering, CSE/DCE, and dominance facilities;
it does not introduce an external analysis universe. Bootstrap remains an opaque
runtime call with complete input/output contract. Lowering may expose the
polynomial substructure of the already adjacent Relin, but may not relocate the
canonical `MulCC->Relin` boundary or make its result available before Relin.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S23-001`; `python3 osprey/be/vho/tests/run_hpoly_contract.py --build-dir build --manifest testdata/fhe_o2/hpoly/SHA256SUMS --artifacts test-artifacts/o2/O2-S23-001` | fixed HPAO ref/hash, Rotate/KeySwitch/Mul/adjacent-Relin/ModUp/ModDown/DotProd graphs and bootstrap barriers | basis, level, scale, static attributes, effects and lowering exact; `MulCC->Relin` boundary unchanged; decoded `1e-4/1e-6`; crossing bootstrap/Relin boundary or unknown basis emits exact diagnostic | seed `0x48504f4c`; chain 1..16, fanout 1..8 | accepted mock/OpenFHE capability; bootstrap treated only by profile | 2 warmups/10 samples, 30 min/sample; retain CKKS/HPOLY/POLY dumps, `.B`, C, output 180 days | HPOLY owner / common-com, CKKS, runtime reviewers; zero bootstrap expansion or Relin relocation, no custom node reaches whirl2c, all state identities preserved |

**Exit:** HPOLY is a verified middle level, not a second CKKS parameter truth or
a leak of provider internals.

### S2.4: Implement full HPAO catalog with static weights

**Objective:** implement the fixed-paper ModUp hoisting, ModDown optimization,
fusion/factoring, and lazy modular reduction rules under explicit bounds.

**Implementation:** assign stable rule IDs, static integer-ns weights, overflow
bounds, effect/alias legality, profitability decisions, and fallback. Recompute
weights after provider, parameters, or upstream planner graph changes. Rule
matching and cleanup use the accepted Open64 analysis infrastructure at HPOLY,
not ordinary pre-conversion or post-materialization WOPT.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S24-001`; `python3 osprey/be/vho/tests/run_hpao_rule_contract.py --build-dir build --catalog testdata/fhe_o2/hpao/rules.json --weights testdata/fhe_o2/hpao/openfhe-v1.json --manifest testdata/fhe_o2/hpao/SHA256SUMS --artifacts test-artifacts/o2/O2-S24-001` | HPAO paper/ref hashes; positive/negative MU/MD/FM/LM graphs, alias/effect/overflow cases | rule decisions, basis transitions, modular results, operation counts and predicted integer costs exact; decoded `1e-4/1e-6`; exact diagnostic | seed `0x4850414f`; primes 1..16, fanout 1..8, dot length 1..64, values include overflow boundaries | pinned OpenFHE and CPU manifest; required counter/capability bits | 5 warmups/30 paired focused samples, 20 min/sample; no outlier deletion; retain rule traces, weights, measured counters, dumps/C/`.B` 180 days | HPAO owner / POLY, crypto, performance reviewers; zero basis/overflow/effect violation and every enabled rule passes model gate |

**Exit:** every HPAO rewrite is semantically valid, profitable under its fixed
model, and independently measurable.

### S2.5: Cross-pass recosting and F0-defined ablation matrix

**Objective:** measure and control interactions among FHEFusion, MetaKernel,
ReSBM, and HPAO instead of assuming their paper speedups compose.

**Implementation:** after each graph-changing pass, invalidate/recompute affected
layouts, CKKS states, ReSBM plans, keys, minimum-required-`N` diagnostics, and
HPAO weights while preserving requested `N`. Run every F0-defined single pass,
legal pair, Stage 1/full, and all-on profile with fixed parameters; repeat
auto-resolution of remaining parameters with the same fixed N. A profile such
as `S1-MKR` or `S1-RESBM` that remains F0-blocked is labeled unavailable, not
synthesized with incompatible ownership.

| Test ID and exact command/target | Source/hash and input class | Expected result, tolerance, diagnostic | Seed/bounds | Platform/provider/capability | Warmup/sample/timeout and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-S25-001`; `python3 osprey/be/vho/tests/run_o2_factorial_ablations.py --build-dir build --manifest testdata/fhe_o2/ablations/SHA256SUMS --protocol testdata/fhe_o2/ablations/protocol.json --artifacts test-artifacts/o2/O2-S25-001` | F0-defined legal profile matrix, workload/data/fixed-N/parameter/provider hashes; auto-remaining suite separate with same N | all hard correctness gates; complete metrics for required cells; unavailable cells carry exact F0-blocked reason; stale downstream plan/key or N mutation gives exact diagnostic | workload seeds locked; statistical seed `0x41424c32` | one pinned host/OpenFHE manifest; no cross-machine speed ratios | focused 5/30, full 1/5 paired, timeouts 20 min/3 h; no outlier deletion; retain matrix, unavailable-cell reasons, and plan deltas for repository lifetime | integration owner / all pass owners and independent performance reviewer; no missing F0-required cell, no fabricated blocked cell, stale plan, or N mutation; quantitative gates pass |

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
| `O2-S26-001`; `python3 osprey/be/vho/tests/run_o2_final_acceptance.py --build-dir build --manifest testdata/fhe_o2/acceptance-final/SHA256SUMS --protocol testdata/fhe_o2/acceptance-final/protocol.json --artifacts test-artifacts/o2/O2-S26-001` | F0 option/profile truth table; accepted P0/P1a/P2/P1b/F1-ACCEPT/Stage1/Stage2 hashes; fixed and auto-remaining suites with identical fixed N | all Section 15 hard gates; focused decoded `1e-4/1e-6`, application max-abs `<=1e-3`, top-1 exact; immediate relin, exact diagnostics, zero N mutation; bare O2 maps to accepted profile | locked workload/statistical seeds | pinned host, accepted mock/OpenFHE manifests and security estimator | focused 5/30, full 1/5 paired, 20 min/3 h; no outlier deletion; retain signed release bundle for repository lifetime | release owner / architecture, crypto, runtime, all pass owners, independent reviewer; F0 through F1-ACCEPT and S1/S2 complete, every required gate passes |

**Exit:** only this milestone may declare `O2 complete` or `v0.9 O2 complete`,
and only if the accepted ADR/v0.9 text authorizes those exact words.

## 15. Acceptance and Measurement Contract

### 15.1 Compared profiles

Profile names below identify evidence rows, not yet accepted command-line syntax:

| Profile ID | Planner/transforms | Purpose |
| --- | --- | --- |
| `BASE-O0` | exact F1-ACCEPT bare-O0 end-to-end behavior | correctness and performance baseline; hard after F1-ACCEPT |
| `S1-MKR` | proposed MetaKernel plus a neutral/accepted non-ReSBM CKKS schedule | `F0-blocked`; single-factor layout ablation only if F0 proves separability or accepts the neutral schedule |
| `S1-RESBM` | proposed ReSBM plus a neutral/accepted non-MetaKernel layout | `F0-blocked`; single-factor scale/bootstrap ablation only if F0 proves separability or accepts the neutral layout |
| `S1-FULL` | MetaKernel plus ReSBM end-to-end | Stage 1 candidate and the only hard Stage 1 comparison to `BASE-O0` if Fhelipe is indivisible |
| `S2-CF`, `S2-MF`, `S2-SF` | one FHEFusion family at a time | Stage 2 rule ablation |
| `S2-HPAO-MU`, `MD`, `FM`, `LM` | one HPAO family at a time | backend ablation |
| `O2-ALL` | accepted bare-O2 profile | final candidate only |

Every comparison has two experiment families:

- **fixed resolved parameters:** identical `CKKSResolvedParameterIR`, provider,
  keys/profile domain, input, and transformed-constant source between profiles;
- **automatic resolution:** each profile resolves parameters from the same user
  constraints while holding the requested `N` fixed. Only active slots, depth,
  Q/P chain, scale/bootstrap profiles, keys, and achieved-security validation
  are resolved. Results are labeled auto and never substitute for a defined
  fixed-parameter comparison.

F0 records each profile as `required`, `informational`, or `blocked`. A blocked
single-factor profile is not a missing test and is not silently turned into a
hard gate. If F0 later approves component separability or a neutral baseline,
the exact profile becomes required through a reviewed protocol update.

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
12. the generated C compiles, links, loads the pinned adapter, executes, and
     destroys every object without sanitizer/lifetime failure;
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
- compiler/linker flags, OpenFHE and adapter hashes, capability manifest,
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
canonical.fhe.B
normalized-planner-input.json
resolved-parameters.json
fixed-n-capacity-security-diagnostics.json
provider-capabilities.json
planner-candidates.json
selected-plan.B
selected-plan.ir
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
materialized.o2.mid.ir
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
| `ARCH-1` | F0 ADR, Fhelipe source lock, v0.9 sync | `O2-F0-001` |
| `O0-PREP` | F1-PREP source inspection/delta/license/build, fixtures, goldens and independent-oracle design only | `O2-F1P-001`; no production planner/default |
| `CAN-1` | canonical record vocabulary and verifier | builder/unit/corruption tests |
| `CAN-2` | file-wide canonicalization transaction | `O2-P0-001` |
| `ABI-1` | P1a public provider C ABI and mock | `O2-P1A-001` |
| `REC-1` | accepted record builders/readers/dumpers | independent reopen and old-reader tests |
| `REC-2` | complete CKKS parameters/profiles/states/actions | `O2-P2-001` |
| `ABI-2` | P1b OpenFHE adapter against P2 parameters/profiles | `O2-P1B-001` |
| `O0-IMPL` | F1-IMPL complete F0-approved planner on P0/P1a/P2/P1b | `O2-F1I-001` |
| `O0-ACCEPT` | F1-ACCEPT reopen/materialize/generated-C/OpenFHE acceptance | `O2-F1A-001` |
| `S1-1` | source/support/diagnostic/cost locks | `O2-S10-001` |
| `S1-2` | atomic identity-plan transaction | `O2-S11-001` |
| `S1-3` | ACE virtual graph/effects/transfers | `O2-S12-001` |
| `S1-4` | MetaKernel artifact-equivalent `(Pb,Ps)` search plus paper differential | Figure 4, MVM1/MVM2 and `O2-S13-001` |
| `S1-5` | MVM transform and execution | `O2-S13-001` |
| `S1-6` | Ke2Col locked artifact/AE core | core rows of `O2-S14-001` |
| `S1-7+` | one Conv extension per commit | complete extension row of `O2-S14-001` |
| `S1-8` | fixed-N CKKS state/effect/security and WOPT non-interference | `O2-S15-001` |
| `S1-9` | ReSBM artifact-core region/SCC builder | `O2-S16-001` |
| `S1-10` | artifact ScaleMgr, cuts, endpoint DP with `q_w=q` | teaching cases and unit tests |
| `S1-11` | independent raw-graph oracle | `O2-S17-001` and dependency audit |
| `S1-12` | bounded call/control-flow extension | positive/negative extension tests |
| `S1-13` | key manifests and standard-call materializer | `O2-S18-001` |
| `S1-14` | acceptance bundle only | `O2-S19-001` and signed review |
| `S2-1` | full FHEFusion catalog | `O2-S20-001` |
| `S2-2` | CF/MF/SF semantic rewrites | `O2-S21-001` |
| `S2-3` | gap/minimum-required-N diagnostics with immutable requested N | `O2-S22-001` |
| `S2-4` | HPOLY contract | `O2-S23-001` |
| `S2-5` | one HPAO rule family per commit | applicable `O2-S24-001` rows |
| `S2-6` | cross-pass invalidation and ablations | `O2-S25-001` |
| `S2-7` | final acceptance bundle only | `O2-S26-001` and signed review |

## 17. Definition of Done

### 17.1 O2 Stage 1

Stage 1 is done only when:

- F0, F1-PREP, P0, P1a, P2, P1b, F1-IMPL, and F1-ACCEPT are accepted in that
  dependency order and v0.9/ADR/options agree;
- S1.0-S1.9 pass their exact commands;
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
- generated C executes through the stable provider ABI;
- all hard, numeric, model, and performance gates pass; F0-blocked
  `S1-MKR`/`S1-RESBM` cells are not fabricated or counted as failures; and
- the signed report uses `O2 Stage 1 accepted`, not a full-O2 claim.

### 17.2 Complete O2

Complete O2 additionally requires S2.0-S2.6, every F0-defined legal ablation
cell, actual post-pass replanning with fixed `N`, accepted bare-O2 default
mapping, and the release-lifetime evidence bundle. Stage 1 acceptance does not
waive any Stage 2 gate. Post-five-paper research in Section 19 is not required.

## 18. Decision Register, Risks, and Stop Rules

### 18.1 Explicit decision register

| Decision | Status | Owner | Evidence required | Blocks |
| --- | --- | --- | --- | --- |
| complete Fhelipe as bare O0 baseline | Proposed preferred direction | architecture owner | `FRZ-01` to `FRZ-03`, fixed source/build/oracle comparison | F0, F1-IMPL, F1-ACCEPT, S1.9, S2.6 |
| Fhelipe fixed revision (`6afbd1c...`, `a631dae...`, or another reviewed snapshot) | Unknown | Fhelipe/O0 owner | local source bundle SHA-256, commit delta, license and reproducibility | F0/F1-PREP |
| Fhelipe and MetaKernel+ReSBM are alternative end-to-end families | Project interpretation requiring confirmation | architecture owner | paper/source scope and ownership matrix | F0, physical top-level plan |
| bare O0/O2 and explicit option mapping | Unknown | driver/config owner | full option truth table including auto/on/manual/off/fallback | F0, UI, acceptance |
| normalized final-plan physical storage | Frozen | common-com owner | v0.9/ADR, image identity/version/compatibility review | P2/S1.1 production schema |
| Stage 1 `q_w=q`, integer scale-degree/logical-level, uniform one-level ReSBM restriction | Frozen artifact-backed plan contract | CKKS/ReSBM owner | fixed artifact, projection and rejection tests | S1.5-S1.9 |
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
| duplicate planner ownership | stop integrated work until F0; one final plan has exactly one planner-family owner |
| paper and artifact semantics silently conflated | source-locked production oracle plus separate paper-strict differential oracle; use `Artifact-backed behavior/extension`, never paper-exact language |
| temporary O0 planners become permanent | preferred Fhelipe baseline or equally complete F0 alternative; no throwaway acceptance baseline |
| freeze treated as absence of bootstrap semantics | preserve v0.9 `auto/on/manual/off`; F0 decides only Fhelipe preservation/amendment and ownership |
| F0-blocked ablations become hidden architecture decisions | only `BASE-O0` versus `S1-FULL` is hard for an indivisible Fhelipe baseline; record blocked cells and require F0 for separability/neutral baseline |
| F1 implemented before its inputs exist | enforce F1-PREP -> P0 -> P1a -> P2 -> P1b -> F1-IMPL -> F1-ACCEPT closure order |
| duplicate on-disk truth | enforce Section 8 crosswalk and one authoritative owner per field |
| WHIRL compatibility regression | common-com review, version/capability gates, old/new reader tests, binary reopen |
| canonicalization hides junk/gap bugs | nonzero sentinel negatives and per-slot verification |
| provider C++ ABI leaks | public-header audit and compile generated C without C++ provider headers |
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
- an accepted v0.9 statement conflicts with the intended implementation and no
  accepted ADR exists;
- a moving/unhashed source is needed for an algorithm claim;
- any stage changes requested `N`, silently accepts a larger-N rewrite, or
  materializes a non-adjacent Relin after `MulCC`;
- a provider cannot express the complete resolved parameter/bootstrap profile;
- a record cannot be reopened independently without producer memory;
- an extension lacks an exact oracle/rejection boundary;
- a cost or security calculation saturates or relies on unknown frequency;
- an unsupported private operation reaches the final WHIRL/C boundary; or
- any acceptance fixture requires relaxing a tolerance after observing its
  result without a fresh pre-registered review.

## 19. Post-Five-Paper Deferred Work / Research Backlog

Lazy relinearization is not part of O2 Stage 1, O2 Stage 2, `O2 complete`, or
the accepted five-paper capability. It may be reconsidered only after Fhelipe,
MetaKernel, ReSBM, FHEFusion, and HPAO are all implemented and accepted under
their current contracts.

A future lazy-relin proposal requires a new ADR; an explicit provider capability;
component-count, alias, state-transition, effect, lifetime, serialization, and
bootstrap-boundary proofs; an independent placement/state oracle; complete key
manifest changes; and pre-registered performance evidence against mandatory
immediate relin. This plan makes no commitment to implement or accept that work.

## 20. Immediate Work Queue Under the Freeze

Work may proceed in this order without pre-deciding the frozen architecture:

1. prepare and close F0: locally lock the chosen Fhelipe/baseline revision,
   decide ownership/options/profile separability, and accept the ADR plus v0.9
   synchronization;
2. perform F1-PREP source inspection, paper/code delta, license/build
   reproduction, fixtures, goldens, and independent-oracle design only; publish
   no production planner/default or O0 acceptance;
3. implement P0 canonicalization and sentinel/idempotence/reopen tests using the
   already fixed `N` from `FHECompilationConfigIR`;
4. implement P1a provider-neutral C ABI and mock, including immediate-relin
   ordering/effects;
5. close P2 semantic/physical records, fixed-N CKKS contract, serialization and
   independent reopen on P0/P1a identities;
6. implement P1b OpenFHE only against accepted P2 parameter/bootstrap profiles;
7. after the freeze is lifted, implement F1-IMPL on P0/P1a/P2/P1b, then close
   F1-ACCEPT through plan reopen, materialization, generated C and OpenFHE;
8. in parallel where ownership-neutral, lock S1.0 MetaKernel/ReSBM artifact
   fixtures, differentials, diagnostics and raw tables, and implement pure
   artifact-contract algorithms plus genuinely independent oracles; and
9. begin integrated Stage 1 materialization/acceptance only after F1-ACCEPT;
   treat `S1-MKR` and `S1-RESBM` as blocked unless F0 explicitly defines them.

This plan does not unfreeze SYNC-3 Commit 16 and does not modify either PDF in
`doc/` associated with that issue.
