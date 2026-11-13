# WHIRL optimization and GPU lowering architecture

Revision 1.0 | November 3 2026 | Normative design amendment

This amendment defines where AI compiler transformations occur and what each lowering boundary must preserve. It governs new implementation of the generated-kernel path and supersedes earlier passages that place algorithmic scheduling, tiling, shared-memory staging or prefetch-loop construction in Mid or Low WHIRL. Existing provider paths, numerical contracts, TensorEvolutionGraph lineage, complete-plan profitability gates and accepted implementation receipts remain in force. Architecture adoption does not certify that the proposed pipeline has been implemented.

## 1 Representation and ownership

| Level | Representation and work owned at this level | Required output |
| --- | --- | --- |
| Very High WHIRL | Domain operators and contracts for CNN, attention, normalization, MoE and stateful operations; domain gatekeeping; authorized algebraic alternatives; promotion to common algorithms | Verified common algorithmic operations with domain obligations discharged or explicitly retained |
| High WHIRL | Common algorithmic IR; canonical DO_LOOP and OPR_ARRAY tensor indexing; PREOPT canonicalization; LNO access, dependence and reuse analysis; Wolf-style unimodular and optional Cohen-style polyhedral scheduling; multi-level tiling; SMEM staging and prefetch DO_LOOP generation | Selected executable structured algorithm, explicit storage and movement obligations, and a verified High WHIRL exit contract |
| Mid WHIRL | Lower the selected structured memory operations: address calculations, copy issue, completion, visibility, synchronization and buffer reuse; preserve structured regions as needed until their obligations are discharged | Explicit target-compatible memory and control operations without a new algorithm or tile choice |
| Low WHIRL | GPU execution mapping and target realization: CTA, warp, lane, register/fragment assignment, address-space realization, target intrinsics and instruction constraints | Verified target kernel ready for CG and PTX emission |
| PTX backend | NVISA/CG instruction expansion and PTX emission; external assembler or driver produces target binaries | Inspectable PTX and separately certified cubin or JIT package |

Very Low WHIRL and machine-operation stages remain part of traditional Open64 continuous lowering where used. The five rows describe DSC responsibilities, not a replacement for every historical WHIRL level. Mid WHIRL preservation of selected structured memory operations is an extension contract to implement and verify; the historical specification already lowers ARRAY and high-level control flow on the High-to-Mid path. Do not claim that existing Mid WHIRL automatically provides a new structured-memory dialect.

High WHIRL has two explicit admission forms. H-A contains verified common algorithmic operators and descriptors. H-L contains canonical scalar/loop WHIRL on which the existing LNO services can operate. An algorithm-expansion adapter converts an eligible H-A region to H-L before LNO; the adapter must define numerical evaluation order, effects, rank, strides and bounds. Untaught canonical passes must never receive opaque DSL operators. Unsupported regions retain a certified provider/baseline path or receive a diagnostic. A high-level normalization operator may remain available for planning, but its ordered source arithmetic must be bound before expansion or schedule selection.

Common/com owns policy-free records, IDs, structural verifiers, printers and compatible atomic mutation mechanisms. VHO owns domain verification and common algorithm expansion in the active PU. PREOPT owns canonicalization. LNO owns canonical-loop access/dependence analysis, schedule legality and loop transformations. WOPT owns CFG/SSA analyses and applicable scalar optimizations. The driver owns PU/REGION lifetime, phase ordering, target entry registration and checkpoints. Cross-PU reasoning requires the IPA path and IPA-owned scope.

## 2 Tensor indexing and canonical High WHIRL

Use the actual Open64 operator OPR_ARRAY (printed as ARRAY), together with DO_LOOP, for multidimensional tensor subscripts. ARRAY_SUBSCRIPT is descriptive terminology, not a newly allocated opcode. ARRAY computes an element address; ILOAD and ISTORE or their valid equivalents perform the access. A tensor descriptor supplies element size, rank, extent, stride, layout, base identity and allocation/alias facts. The compiler must preserve those facts and selected-plan lineage through expansion.

For a rank-two row-major tensor, the semantic access A[i,k] is represented through ARRAY indexing before the lowerer forms base + element_size * (i * stride_i + k * stride_k). Retain logical dimensions even when an implementation uses a flat physical allocation. Noncontiguous views, negative or dynamic strides, broadcasts and overlapping slices require explicit descriptor and alias handling. ARRAY syntax alone proves neither injectivity nor disjointness.

The loop adapter must normalize induction variables, bounds and steps without changing overflow, zero-trip or exception behavior. Use LNO access vectors and the array dependence graph with existing PREOPT DU/alias services. An affine extractor admits a region only when its bounds, access functions, predicates and effects fit the supported model. Opaque calls, unproved aliases, indirect indexing, volatile/atomic effects and data-dependent bounds are barriers unless separately modeled or guarded. Unknown dependence is conservative; a dependence summary must never be converted into an independence proof by dropping an unknown edge.

## 3 Mathematical relationship of unimodular and polyhedral scheduling

The following notation fixes the proposed legality contract. Let statement S have integer iteration vector i, region-invariant parameters p and domain D_S(p) = {i in Z^n | A_S i + B_S p + c_S >= 0}. Each access a has an allocation identity and an affine access map F_a(i,p). A dependence relation R_ST contains ordered pairs (i,j) whose statement instances must retain source order, including observable effects and numerically ordered reductions.

A Wolf-style unimodular change of coordinates is i' = U i, where U is an integer n-by-n matrix and det(U) = +1 or -1. Its inverse is integer, so it is a bijection on the integer lattice. Loop interchange, reversal and skewing can be composed in this representation. For a uniform dependence distance d = j - i, a transformed lexicographic execution order is legal only when U d is lexicographically positive. Direction-vector approximations require the corresponding conservative test over every represented distance, not a test on one convenient representative. Nonuniform cases require relations or a sound overapproximation.

Example: U = [[1,1],[0,1]] has determinant 1 and maps (i,j) to (i+j,j). Dependences (1,0) and (0,1) become (1,0) and (1,1); both remain lexicographically positive. Interchange P = [[0,1],[1,0]] maps a legal source distance (1,-1) to (-1,1), which is negative in the first component and therefore illegal. Reversal requires transformed bounds and traversal direction; a negative determinant does not itself establish legality or illegality.

A general affine statement schedule is theta_S(i,p) = T_S i + P_S p + c_S in Z^m, ordered lexicographically. T_S need not be square or invertible, and different statements may have different schedules. Require theta_T(j,p) - theta_S(i,p) >lex 0 for every dependence (i,j) in R_ST, unless a separately certified parallel/reduction protocol discharges the edge. If the schedule is not injective, generated control must distinguish instances and provide deterministic tie-breaking for remaining ordered work. Coincident schedule dimensions permit parallel execution only after proving that no required dependence is carried by those dimensions.

The unimodular model embeds in this schedule model by taking theta_S(i) = U i, adding statement-order coordinates where necessary. It is a restricted family within the broader polyhedral view of integer domains, accesses and schedules. This relationship is conceptual and mathematical; it does not imply that historical implementations have identical capabilities or that every LNO rewrite is implemented by a polyhedral engine. Wolf-style treatment is especially economical for regular nests; statement-wise affine relations expose fusion/fission, imperfect nests, parametric domains and transformations beyond one common square coordinate change.

Tiling illustrates the additional machinery. For fixed positive tile size B, write i = B q + r with 0 <= r < B and preserve the original domain constraint. Scheduling by (q,r) introduces extra dimensions and integer quotient/remainder constraints; it is not a single same-dimensional unimodular transform of i. Repeating this construction yields hierarchical tiling. A runtime-variable B multiplies two variables and falls outside the simple affine/constant-divisor model; specialize a bounded set of constant tile sizes behind guards or use a separately justified nonlinear analysis. Do not advertise arbitrary dynamic tiling as ordinary affine scheduling.

For GEMM, choose the logical domain (i,j,k), reads A[i,k], B[k,j] and updates C[i,j]. Under a fixed ascending-k floating-point contract, the dependence from k to k+1 must remain ordered. Interchanging independent i/j axes or blocking k while visiting k in its original order can be legal. A tree reduction, split-K, reassociation, reciprocal substitution or Welford reformulation requires its own numerical authorization. Memory-dependence legality alone cannot grant that authorization. LayerNorm's centered statistics and RMSNorm's uncentered second moment are distinct algorithms; a scheduler cannot replace one with the other.

## 4 Historical connection and compiler comparisons

Michael E. Wolf and Monica S. Lam's 1991 transformation work and locality work establish the unimodular and reuse-oriented lineage relevant here. Distinguish Michael E. Wolf from Michael Wolfe, another loop-optimization researcher. Cohen, Girbal and Temam's Euro-Par 2004 paper and Cohen and colleagues' ICS 2005 paper describe compositional polyhedral transformation work on Open64/ORC. The latter identifies WRaP-IT as the WHIRL-to-polyhedral interface and URUK as the transformation component. The 2007 and 2008 iterative-optimization papers extend schedule-space exploration. This is a direct historical Open64 connection, not evidence that this checkout already contains a production polyhedral scheduler.

| System | Relevant connection | Boundary for this design |
| --- | --- | --- |
| Open64 LNO | Structured loops, ARRAY accesses, dependence/reuse services, SNL transformations and cache models | Reuse tested interfaces; adapt target costs and tensor semantics explicitly |
| Cohen and collaborators | Statement domains, access relations and schedule composition/search, including Open64/ORC integration | Optional adapter/search lane; historical prototypes do not establish current implementation |
| LLVM Polly and GCC Graphite | Other polyhedral compiler integrations | Compare extraction, legality, code generation and retained semantics; do not assume shared code or universal descent from Open64 |
| MLIR affine plus memref/scf | Explicit affine loop bounds/maps, array accesses and structured control with staged lowering | Architectural comparison to High WHIRL; memref/scf alone does not guarantee affine analyzability |
| Triton TTGIR | GPU distribution/layout and target-facing tensor execution representation | Functional comparison to the Low WHIRL GPU-mapping responsibility; TTGIR can retain structured control and is not identical to Low WHIRL |
| NVIDIA NVVM/PTX | GPU target IR and assembly path | Separate target comparison; nvcc is not established as an MLIR compiler by these comparisons |

## 5 Optional hybrid search

Retain a deterministic LNO-only baseline. First construct canonical loop, access and effect facts once. Generate a bounded family of inexpensive Wolf/LNO permutations, skews and legal hierarchical tiles. Optionally extract supported static-control regions into statement domains, accesses and dependence relations and explore affine schedules around that frontier. Canonicalize equivalent candidates so repeated representations do not consume the budget twice.

Both lanes must return the same plan contract: region/PU and IR generation, numerical-contract digest, access/dependence digest, schedule, tile hierarchy, layout, storage lifetimes, movement/events, guards, target requirements and full cost evidence. The shared verifier checks legality independently of the search strategy. The common selector compares only complete lowerable candidates against the unchanged baseline, including synchronization, unhidden movement, register/SMEM pressure, occupancy, launch and guard overhead. Search cost and code-size growth also have budgets. A smaller estimated score has no authority to repair missing legality.

Set explicit limits for candidate count, coefficient magnitude, dimensions, integer-set pieces, solver work, generated-code size and elapsed time. Pin seed, solver/version and iteration order; use deterministic tie-breaking. Wall-time limits are emergency bounds and must not make a partial candidate valid. On timeout, extraction failure, arithmetic overflow, unsupported semantics or unavailable solver, discard the optional result and retain the certified baseline/LNO winner with a reason. New libraries, including isl or an ILP solver, require the existing dependency and licensing review before linkage to be.so or its consumers. The architecture does not select a new dependency.

## 6 High WHIRL staging and prefetch generation

AI-P7 constructs candidate tile hierarchies, AI-P8 constructs movement/lifetime alternatives, and AI-P9 compares complete candidates and commits one. Keep these existing phase IDs and their delayed joint-plan semantics. For the generated path their executable algorithm, tile loops, staging loops and prefetch loops are materialized in High WHIRL before its exit gate. A High-level candidate can describe CTA/warp/thread tile extents and abstract ownership; Low WHIRL realizes those assignments as execution coordinates and instructions.

For each selected movement plan, emit DO_LOOP regions for prologue issue, steady-state movement/compute and epilogue drain as required. Preserve ARRAY accesses for both source tensors and SMEM buffers. Prefetch in this contract means an executable early load/copy or movement region with defined completion; a cache PREFETCH hint is a separate optional operation and cannot establish data readiness. The synchronous one-slot copy/compute/release form is the first safe materialization. Multiple slots and asynchronous issue require explicit generation and completion contracts before admission.

For logical tile t and stage count S > 0, bind slot(t) = t mod S and generation(t) = floor(t/S). Each staged tile requires valid source bounds, producer completion before issue, copy completion and visibility before its first consumer, and completion of every reader before a later generation overwrites the slot. For lookahead d, t+d must be within the logical tile domain or be masked/peeled without issuing an invalid access. Prologue and drain must cover zero, one and fewer-than-d tiles correctly. Barrier participation must be uniform over its scope even when edge loads are predicated. Track readiness and safe overwrite as separate proofs.

High WHIRL records transfer ranges, storage identity, issue/wait/release events, visibility scope, ownership and fallback. Mid WHIRL resolves structured memory operations into target-compatible issue/completion primitives and address expressions. Low WHIRL binds cooperative participants, barrier scopes and target instructions. A backend that lacks cp.async, TMA or a compute primitive must use an already certified synchronous/SIMT variant or reject the route; it must not infer a different algorithm during emission.

## 7 Analysis invalidation and transactional mutation

Every derived fact and selected plan is keyed by PU/REGION identity, IR mutation generation, canonical-form generation, descriptor/type identity, alias/effect generation, numerical contract, target capability and cost-model snapshot as applicable. Node addresses are not durable identities. Mapped-image reopen creates fresh analysis lifetimes and requires revalidation from the reopened image; persisted semantic identities remain distinct from ephemeral analysis pointers.

| Mutation | Invalidate or explicitly update and verify before reuse |
| --- | --- |
| Domain promotion or algorithm expansion | Operator census, shape refinements, DU/alias/effects, access/dependence, reduction recipe, loop identities and all dependent candidates |
| Loop interchange/skew/fusion/fission/tiling | Loop/parent maps, access vectors, dependence graph, DU/SSA/CFG where affected, ownership, locality, lifetimes, movement plans and costs |
| Layout, storage or allocation change | Access and alias regions, alignment/coalescing/bank facts, capacity, lifetime/reuse, issue/wait/visibility and resource costs |
| Staging or prefetch insertion | Memory effects, DU, dependence, buffer generations, event order, barrier convergence, live ranges, occupancy and full-plan cost |
| Mid memory or Low GPU lowering | Address-space/alias effects, target operation legality, synchronization and fragment ownership; high-level analysis is consumed evidence, not a live graph over deleted nodes |
| Target/profile/guard change | Capability, resource and numerical binding as affected, cost/confidence and variant admissibility; measurements never redefine semantic legality |

Use the owning Open64 service's supported update routines when their preconditions hold; otherwise invalidate and rebuild in the driver-established scope. A pass must declare its preservation set. An absent preservation claim means invalidation. Separate immutable source/numerical facts from IR-shape-dependent facts. The High exit verifier rejects a selected plan whose generation or any proof dependency is stale, including a plan selected before an unaccounted staging rewrite.

Preflight the complete selected rewrite, snapshot relevant WN/ST/TY/descriptor/maps and plan state through existing compatible transaction facilities, apply it, rebuild or update affected analyses, verify, then publish. Failure restores the prior certified state and analysis generation or aborts compilation without publishing a valid-looking partial image. Trace emission is observational and cannot determine decisions. No performance fallback is permitted after a possibly executed kernel unless the existing runtime failure/drain protocol explicitly makes it safe.

## 8 High WHIRL exit contract

All applicable items are mandatory before generated-kernel High-to-Mid lowering:

1. Domain gatekeeping is complete; each common algorithm has a bound numerical/effect contract and an executable expansion or certified provider boundary. No unsupported live DSL operation enters unaware LNO/WOPT/CG code.
2. Selected generated regions contain canonical DO_LOOP/IF and OPR_ARRAY accesses with explicit index/bounds/overflow/tail behavior. Source and plan lineage, symbol/type ownership and strict WN tree structure verify.
3. Dependences, aliases, reduction order and observable effects are proved or protected by executable guards with independently legal fallbacks. All proofs match the current IR generation.
4. The selected schedule, multi-level tiles, SMEM allocation/layout and prefetch DO_LOOP regions exist in the saved image. A plan row or CUDA template is insufficient evidence of a rewrite.
5. Issue, completion, visibility, consumer wait, final reader release and slot-generation reuse are explicit. Barrier convergence, zero-trip and boundary cases verify. Outstanding operations have a defined drain path.
6. Storage capacity, alignment and ownership have valid target envelopes. Abstract CTA/warp/thread/fragment assignments are consistent; target instruction binding is a later lowering obligation, recorded explicitly rather than claimed complete.
7. A complete cost and lowerability record identifies selected and baseline variants, guard obligations and target feature requirements. Immutable plan IDs and proofs are rebound after materialization.
8. The driver has registered any generated device PU and launch ABI with correct RID/maps/pools/source context. Per-PU optimizers have not traversed or mutated another PU.
9. An independently reopened binary WHIRL image and ir_b2a -st -src trace verify the boundary. Existing binary layout/ELF contracts remain unchanged unless a separately reviewed versioned extension is required.

Failure reports the PU, region, level, plan, stale/missing obligation and fallback disposition. A host provider call, selected-plan trace or detached unregistered device sidecar cannot satisfy generated-device integration. A provider path has its own explicit ABI and capability exit contract and must not be labeled compiler-generated tiling.

## 9 Driver implementation order

The backend orchestration commonly called be_driver is implemented here in osprey/be/be/driver.cxx. Existing anchors are Preprocess_PU, Backend_Processing, LNO_Processing, Perform_Preopt_Optimization, Do_WOPT_and_CG_with_Regions and WN_Lower(... LOWER_TO_CG ...). This is an insertion and refactoring plan, not a claim that a file named be_driver.cxx exists or that the new hooks are already implemented.

1. In driver program setup, establish option policy, target capability snapshot, input-image compatibility and program-owned generated-PU queue. Preserve existing FHE checkpoint routes and language pipelines.
2. In Preprocess_PU, run DSL/domain gatekeeping and required shape refinement on Very High WHIRL. Retain optional semantic optimization only within its declared optimization level. Expand selected common algorithms to the High H-A/H-L admission boundary without prematurely replacing eligible loops by opaque kernels.
3. Apply language VHO lowering only to the extent needed for High WHIRL admission. Run PREOPT in the existing driver-owned scope to canonicalize loops and establish DU/alias facts. Do not move the complete LOWER_TO_CG action ahead of LNO.
4. In the LNO portion of Backend_Processing, obtain access/dependence/reuse facts, form Wolf/LNO and optional polyhedral schedule candidates, and refine AI-P7/P8 complete tile/storage/movement alternatives. Preserve AI-P0 through AI-P11 logical report order even when a candidate is refined internally.
5. Select at AI-P9, materialize scheduling, multi-level tiling, staging and prefetch DO_LOOP regions in High WHIRL; rebuild affected analyses and run the High exit verifier. Driver-owned program code registers new device PUs and compiles each under its own context; the producer returns a request, not an unauthorized PU traversal.
6. Save the High exit checkpoint before ARRAY/control-flow destruction. Lower selected structured memory operations to Mid WHIRL and verify issue/wait/visibility, address and effect preservation. Run applicable WOPT on a supported canonical form; any transformation touching ordered arithmetic/events must preserve the contract and invalidate affected analyses.
7. Lower to Low WHIRL GPU execution mapping, including selected ownership realization, address spaces, warp/lane/fragment contracts and exact target operations. Treat LOWER_TO_CG as an audited bundle of actions: identify which existing actions realize High-to-Mid, Mid-to-Low and Very Low transitions; split/gate calls where necessary rather than attaching new names to an unexamined monolithic lowerer.
8. Run CG/NVISA PTX generation; validate target/toolkit requirements, assemble or JIT through the target finalizer, bind the host launch package and retain receipts. whirl2c -cuda remains a validation/emission consumer of the same selected WHIRL, with the documented legacy route clearly identified.

Mandatory baseline expansion works at -O0 without schedule search or profitability-driven tiling. Basic-block transforms require at least -O1, PU/CFG transforms -O2, and loop scheduling, tiling, memory-hierarchy optimization and parallelization -O3. A target-driven algebraic algorithm remains an -O2 semantic decision where its reviewed contract permits; -O3 cannot silently replace it. Cross-PU work requires -ipa. Pass-specific enable/disable/check-only controls must preserve fixed ordering, optimization-level limits and the driver's full-option propagation model. Any proposed new option spelling is subject to registry review before implementation.

## 10 Milestones and acceptance

All new gates below start open. Existing AIO, MMA, reduction and normalization completion claims remain governed by their retained evidence; this amendment neither revokes those receipts nor marks the broader pipeline complete. The previously deferred performance-framework queue remains deferred. Focused native correctness and target checks are acceptance requirements when their milestones are implemented, not authorization to resume that framework now.

| Gate | Deliverable and dependency | Acceptance evidence |
| --- | --- | --- |
| H0 Boundary inventory | Map current driver/lowerer actions, H-A/H-L constructs, options and producer/consumer ownership | Source-anchored phase table; baseline -O0/O1/O2/O3/IPA coverage; unsupported and legacy routes explicit |
| H1 Canonical loop adapter | H0; one fixed-shape F32 matmul and an ordered reduction expanded to DO_LOOP/ARRAY | PREOPT convergence, alias/stride/bounds negatives, mapped reopen and exact numerical-order checks |
| H2 Dependence and scheduling | H1; reuse LNO, implement bounded Wolf/LNO candidates and shared legality verification | Legal skew/interchange, illegal (1,-1) interchange, conservative unknowns, deterministic option-off baseline |
| H3 Optional polyhedral lane | H2; extraction/round-trip and reviewed dependency boundary before optional search | Statement ordering and imperfect nests, independent legality check, timeout/unavailable fallback, equivalent baseline execution |
| H4 Hierarchical tiling | H2; selected multi-level loop rewrite with lineage | Visible tile loops, exact/tail/zero-trip coverage, ascending reduction order, complete resource/cost record and before/after images |
| H5 SMEM and prefetch loops | H4; synchronous one-slot first, then separately admitted async/multi-slot | Prologue/steady/drain DO_LOOP, generation reuse proof, readiness/release negatives, barrier convergence and High exit verification |
| H6 Driver and Mid boundary | H5; driver registration and structured memory-operation lowering | Two independent PUs without scope leakage, host/device ABI, injected rollback, High/Mid checkpoints, no event or numerical loss |
| H7 Low GPU and PTX | H6; target ownership/fragment mapping and audited CG path | Minimal PTX entry first; target-feature rejection, compile/assemble, native correctness when hardware is available; no GPU claim from host simulation |
| H8 Comparative certification | H7; agreed workload/target matrix | Same-contract baseline and selected variant, pinned inputs/toolchain, separately reported correctness, code quality and native timings |

H3 is optional and does not block H4-H7. Generated provider-independent code remains a separate lane from cuBLASLt/CUTLASS or other library dispatch. Introduce one verified transformation boundary per implementation change; do not combine an IR format revision with schedule search.

## 11 Tests and risks

Use small literal iteration-domain and dependence oracles for schedule legality and enumeration. Include nonrectangular domains, imperfect nests, statement ties, dynamic shapes admitted behind constant-specialization guards, nonaffine rejection, aliasing slices, negative strides, integer overflow and size-one/zero-trip loops. Compare baseline and transformed execution with the actual floating-point contract: bit/order checks for strict reductions and only authorized tolerances for relaxed algorithms. Keep normalization and RMSNorm semantic negatives.

Movement tests cover S=1, S=2, d=0, short prologues, final drain, predicated tails, delayed completion, an early overwrite and a divergent barrier. Invalidation tests mutate an access, bound, layout, numerical binding and target snapshot after selection and require refusal until recomputation. Transaction fault injection covers preflight, partial tree construction, verification and publication; retain before/after images and an empty diff for no-op cases.

Driver tests cover phase order, level gating, independent controls, check-only/trace-off decision identity, language-only compatibility, provider fallback, current and legacy mapped-image inspection, and multiple PUs with no unauthorized cross-PU mutation. When source or dependencies change, rebuild affected products and audit be.so/lw_inline for link closure and frontend-only symbols. Documentation-only updates require hygiene, link/provenance checks and document rendering, not a new compiler/GPU certification run.

| Risk | Control |
| --- | --- |
| Common operators sent directly to unaware LNO | Explicit H-A to H-L adapter and admission verifier |
| Broad polyhedral claims hide aliases or nonaffine effects | Conservative extraction and independently verified dependence contract |
| Floating-point algorithm changes during scheduling | Source numerical binding and exact ordered-reduction edges |
| Search growth or unavailable new dependency | Optional bounded lane, deterministic fallback and dependency approval |
| Staging creates stale analyses or buffer races | Mutation generations, separate readiness/release proofs and High exit gate |
| Historical lowerer destroys structure too early | Audit action masks; checkpoint High exit before ARRAY/control-flow lowering |
| GPU ownership or target features inferred by emitter | Explicit Low mapping and target legality, one selected WHIRL source |
| Historical NVISA support overstated | Minimal target probe and per-feature compile/assembly/native gates |
| Old design passages or milestone labels misread as current | This dated normative amendment and an explicit implementation-status ledger |

Retain accepted compiler evidence under the primary Open64 artifacts/review hierarchy with immutable hashes and receipts. A new run must not delete prior accepted evidence. Before/after .B files, independently reopened matching .T files, full diffs, source mappings, raw traces and commands are mandatory for later transformation milestones. Performance and GPU claims require their own evidence and are not inferred from document adoption.

## 12 References and local anchors

The mathematical specification, phase split, hybrid strategy and gates above are DSC design decisions. The references provide historical or representation evidence, not proof of this implementation.

- Wolf and Lam, A Loop Transformation Theory and an Algorithm to Maximize Parallelism, IEEE TPDS 2(4), 452-471, 1991, DOI 10.1109/71.97902. Stanford author publication index: https://suif.stanford.edu/papers/papers.html
- Wolf and Lam, A Data Locality Optimizing Algorithm, PLDI 1991, 30-44, DOI 10.1145/113446.113449. https://doi.org/10.1145/113446.113449
- Cohen, Girbal and Temam, A Polyhedral Approach to Ease the Composition of Program Transformations, Euro-Par 2004, 292-303, DOI 10.1007/978-3-540-27866-5_38. Author bibliography: https://who.rocq.inria.fr/Albert.Cohen/publications/publications.html.en
- Cohen, Girbal, Parello, Sigler, Temam and Vasilache, Facilitating the Search for Compositions of Program Transformations, ICS 2005, 151-160. Author-hosted paper: https://pages.saclay.inria.fr/olivier.temam/files/eval/CGPSTV05.pdf
- Pouchet, Bastoul, Cohen and Vasilache, Iterative Optimization in the Polyhedral Model Part I One-Dimensional Time, CGO 2007; Pouchet, Bastoul, Cohen and Cavazos, Part II Multidimensional Time, PLDI 2008. Author bibliography above.
- MLIR affine dialect: https://mlir.llvm.org/docs/Dialects/Affine/ ; memref: https://mlir.llvm.org/docs/Dialects/MemRef/
- Triton layout source, moving main-branch reference inspected November 3 2026; pin an exact revision before implementation comparisons: https://github.com/triton-lang/triton/blob/main/include/triton/Tools/LinearLayout.h
- WHIRL Intermediate Language Specification, May 10 2000, Section 1.3 and Figure 1, pages 5-7. Local reference: /Users/shinmingliu/Documents/whirl.pdf. High preserves DO_LOOP and ARRAY; Mid introduces target characteristics. Concrete encodings and layouts remain governed by current source and compatibility contracts.
- Local source anchors: osprey/be/be/driver.cxx; osprey/be/lno/lnopt_main.cxx; osprey/be/lno/snl_trans.cxx; osprey/be/lno/cache_model.cxx; osprey/be/lno/lnoutils.cxx; osprey/common/com/wn.cxx; osprey/common/com/wn_lower.cxx; osprey/be/cg/NVISA/; osprey/be/whirl2c/wn2c.cxx.
