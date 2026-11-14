# M1 backend driver and LNO boundary audit v1.0

November 7, 2026 | M1 complete as a static source audit; integration remains open

Audited source baseline: commit `0de7c6ad7bce7142c6e66bcac3560e176cc5ab27`.
Authority: [search methodology](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.0.md).
Findings update the [production implementation plan v1.1](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.1.md)
and [consolidated plan v0.8](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.8.md).
The evidence packet records full source SHA256 values and line-numbered excerpts.
Line references below describe this source baseline, not a permanent API.

## 1. Snapshot decision

Place the proposed snapshot/estimation boundary **inside Lnoptimizer**, after
its first successful `Build_Array_Dependence_Graph(func_nd)` and after the
disabled-LNO/autopar-only early-return block, before `Hoist_Varying_Lower_Bounds`
and the subsequent array substitution, reversal, scalarization and scheduling.
The insertion gap is `osprey/be/lno/lnopt_main.cxx:1360`, after the enabled-path
checks and the non-IR `LNO_Tlog = ...` assignment; the first subsequent IR
transformation is at line 1361. No hook is implemented by this audit.

At that point the LNO parent/info/access maps, PREOPT DU and alias managers,
and the full LNO `DEPV_ARRAY_ARRAY_GRAPH` are simultaneously live. This is a
qualified canonical-High snapshot point for admitted loop nests, not a claim
that every input reaching it satisfies the proposed AI canonical contract.
M2 must verify that contract, analysis coverage and effective controls before
publishing a snapshot. Unsupported nests retain the normal compiler path.

The point is not pristine PREOPT output: short-loop unrolling, dead-SCF removal
and conditional hoisting can precede it. The access-only point at lines
1271-1280 precedes those changes but has no full dependence graph yet. M2 must
choose and test the explicit policy for managed loops: initially analyze only
surviving admitted nests, or separately defer interfering early transformations
and use the existing preparation/graph builders. Do not silently call an
earlier access-only observation a complete scheduling snapshot.

## 2. Actual call and scope chain

```text
driver.cxx: Preorder_Process_PUs
  Preprocess_PU
    DSL shape/PREOPT/domain work and current DSL runtime lowering
    VHO_Lower_Driver: Very High -> High language lowering
    Adjust_Opt_Level; REGION initialization
  Backend_Processing
    effective PU options; return/optional MP lowering; LNO feedback
    LNO_Processing: current iterator selects RID_TYPE_func_entry
      Perform_Loop_Nest_Optimization in lnodriver.cxx
        create DU/alias; PREOPT_LNO_PHASE; optional PREOPT_LNO1_PHASE
        Lnoptimizer in lnopt_main.cxx
          parent/info/access preparation; first full graph
          [proposed qualified High snapshot -> detached search]
          [future selected application/rebuild/High exit]
          ordinary LNO transformations; CG-graph export; cleanup
        delete DU/alias
    Post_LNO_Processing: .N / normal whirl2c source emission
    optional late MP lowering; Do_WOPT_and_CG_with_Regions
      MAINOPT entry lowering: ARRAY -> addresses
      optional feedback LOWER_SCF
      LOWER_TO_CG: structured control + arrays + other target lowering
      CG / NVISA
```

Driver anchors are `driver.cxx:2460` (per-PU traversal), `:2491` and `:2528`
(preprocess/backend calls), `:2379` and `:2386` (DSL/language VHO), `:2392`
(per-PU backoff), `:1258`/`:1296` (LNO wrapper), `:1874`/`:1886` (LNO/post-LNO),
and `:1963` (WOPT/CG). `lnodriver.cxx:265-303` creates analysis managers, runs
PREOPT_LNO and optional LNO1, then invokes Lnoptimizer. PREOPT is a real shared
canonicalization service here; calling ordinary MAINOPT is not a substitute.

The current driver LNO iterator uses `RID_TYPE_func_entry`, and `Mark_Code`
asserts `OPC_FUNC_ENTRY` (`can.cxx:168`). Wrapper comments allow future/smaller
regions, but they do not establish arbitrary REGION-root support in this
implementation. Initial M2 scope is the active FUNC_ENTRY/PU with admitted
loop subsets. A later REGION-root extension requires its own entry/analysis
tests. Do not switch PUs or expand to IPA scope from the search service.

## 3. Preparation, competing transformations and coverage

| Source boundary | Observed behavior | Consequence |
| --- | --- | --- |
| `lnodriver.cxx:273` | PREOPT_LNO constructs the form and DU consumed by LNO; optional LNO1 can transform again | Snapshot after the final applicable PREOPT invocation and LNO preparation |
| `lnopt_main.cxx:1185` | Parentization after map creation | Parent map is local analysis state, not a persisted tree identity |
| `lnopt_main.cxx:1240-1272` | Structure-split optimization, Mark_Code, optional call/Lego work and access construction | Preparation can mutate IR; charge it once before freezing the snapshot |
| `can.cxx:164-193` and `lnopt_main.h:494-505` | Mark_Code marks loops, can promote pointers to ARRAY, simplifies and can dismantle unsupported/deep loops | Do not label Mark_Code a read-only collector or promise preservation of every source DO_LOOP |
| `access_main.cxx:267-343` | Access build dispatches explicitly on DO_LOOP, OPR_ARRAY and IF and updates loop/access information | Reuse these facts; access-build work is outside per-candidate estimates |
| `lnopt_main.cxx:1274-1280` | Existing read-only partition discovery after access construction | Useful precedent for active-PU observations; insufficient alone for affine scheduling legality |
| `lnopt_main.cxx:1292-1316` | Optional full unrolling, reduction discovery, dead-SCF elimination and conditional hoisting | Initial graph describes the resulting tree; early-loop preservation needs an explicit M2 policy |
| `lnopt_main.cxx:1336-1359` | Disabled/skip path builds CG graph and returns; otherwise full graph must build successfully; autopar-only path can return | Proposed snapshot runs only on the enabled full-graph path |
| `lnopt_main.cxx:1361-1422` | Bounds hoist, array elimination/substitution, reversals, scalarization, optional parallel/vector/unswitch work and unsigned-loop canonicalization | These can invalidate a sealed snapshot or change available algorithm structure |
| `lnopt_main.cxx:1436-1505` | Lego tiling, Phase_123 and ordinary prefetch insertion | Selected AI schedules need explicit ownership/exclusion, not passive continuation through competing optimizers |
| `lnopt_main.cxx:2153` | Phase_123 calls LNO_Lower_Memlib | Some DO_LOOPs can become memory-library operations before LNO returns |

The unsigned-loop normalizer runs at `lnopt_main.cxx:1422`, after the proposed
snapshot. `cond.cxx:1732-1740` documents conversion to index <= expression and
guards for LT conversion. M2 must initially reject unsupported unsigned/overflow
forms or perform separately verified one-time normalization before rebuilding
the snapshot analyses. Snapshot construction itself must not change the tree.
No claim is made that all unsigned loops already have that later form at 1360.

Full graph build success is necessary but not sufficient for candidate legality.
`dep_graph.cxx:645-677` marks unsupported loops/regions as Has_Bad_Mem and can
erase vertices while traversal succeeds. Access vectors can be Too_Messy
(`access_main.cxx:420-433`). The snapshot must record coverage and refuse
unproved memory, calls, exits/barriers, alias relations and nonaffine accesses;
absence of an edge is not automatically independence. Preserve source FP and
reduction ordering permissions independently of the graph.

## 4. Lifetimes: why the driver return boundary is too late

| Object | Owner and lifetime | Snapshot obligation |
| --- | --- | --- |
| DU_MANAGER and local ALIAS_MANAGER | Created by lnodriver at 265-266; deleted at 324-325 | Consume during the LNO call; copy admitted facts, not manager pointers |
| Parent_Map, LNO_Info_Map, access vectors and auxiliary maps | Created by Lnoptimizer around 1162-1180; maps deleted at 1659-1666 | Copy bounds/access coefficients, symbolic identity and coverage before mutation/cleanup |
| Array_Dependence_Graph | Built at 1347 as DEPV_ARRAY_ARRAY_GRAPH; deleted/reset at 1653-1655 | Copy relevant dependence vectors/relations and completeness; no graph pointer or vertex-ID persistence |
| LNO_default/local/SNL pools | Pushed at 1154-1156; popped at 1669-1671 | Owned detached records must outlive pools if carried back to the driver |
| REDUCTION_MANAGER | Optional numerical-policy-dependent construction; deleted at 1652 | Treat absence as unavailable evidence; never dereference unconditionally or infer reassociation permission |
| Current_Dep_Graph | Exported for CG and attached to PU_Info by the driver | Different representation, not the full LNO graph needed for High scheduling |
| Existing partition observations | dsl_partition_axis.h:16-52 explicitly borrows source/maps until mutation | Deep-copy required fields or recollect; callback records are not durable snapshots |

`Build_CG_Dependence_Graph(Array_Dependence_Graph)` at
`lnopt_main.cxx:1542` converts DEPV_ARRAY_ARRAY_GRAPH to DEP_ARRAY_GRAPH.
`dep_graph.cxx:1928-1951` documents inner-loop vertex copying and omitted scalar
LDID/STID/CALL vertices. Exported CG dependence information is not an adequate
replacement for the original multidimensional LNO graph, even though
`PU_Info_depgraph_ptr` survives the LNO wrapper.

Generation evidence is incomplete for the new service. The audited VHO
shape-refinement API has its own explicit generation/invalidation protocol
(`dsl_shape_refine.h:16-60`, `dsl_shape_refine.cxx:117-171`). It is not a
general LNO tree/access/dependence epoch. The partition interface has an
expiry contract, not a shared epoch API. M2 must supply/reuse a reviewed
driver/LNO snapshot-generation protocol and stable owned joins. WN addresses,
debug Enum_loops numbers and local map/graph IDs cannot serve as persistent
source identities across mutation or mapped reopen.

## 5. Destructive lowering boundaries

Language VHO is described as VH->H (`vho_lower.cxx:9108`). Its final WN_Lower
calls use TREEHEIGHT/INLINE_INTRINSIC or FAST_EXP/FIELD_OFFSET masks
(`:9205-9213`), not a blanket LOWER_ARRAY/LOWER_SCF conversion. Nevertheless
VHO rewrites language expressions and control; validate the resulting common
algorithm form, not simply the phase label.

PREOPT_LNO uses Pre_Optimizer's non-MAINOPT branch (`opt_main.cxx:1613-1645`),
whose special masks cover NAMELIST IO, short-circuit and bits operations.
By contrast MAINOPT's entry mask explicitly contains LOWER_ARRAY, BASE_INDEX,
ALL_MAPS and LOWER_TO_MEMLIB (`:1526-1563`). Thus OPR_ARRAY can disappear at
WOPT entry, earlier than final CG lowering. `wn_lower.cxx:6074-6079` replaces
ARRAY with linearized addressing under LOWER_ARRAY.

In `Do_WOPT_and_CG_with_Regions`, feedback/instrumentation can invoke LOWER_SCF
at `driver.cxx:1565-1570`, and final CG requests LOWER_TO_CG at `:1592-1594`.
`wn_lower.cxx:16684-16708` expands it to LOWER_SCF, LOWER_ARRAY, LOWER_MP and
other actions. LOWER_SCF includes LOWER_DO_LOOP (`wn_lower.h:136`);
`wn_lower.cxx:14834-14849` spells the resulting labels/branches. Neither WOPT
entry nor the CG boundary is suitable for taking the required High snapshot.

Post_LNO_Processing emits .N/normal-whirl2c output at `driver.cxx:1377-1407`.
This is an output consumer after analysis cleanup and prior LNO transforms,
not a place to recover the original High analyses. Selected-source views must
be requested from the fixed plan at their own defined boundary.

## 6. AI producer and device-PU handoff gaps

The normal DSL path is not yet the proposed common-High MLA pipeline.
`VHO_DSL_Lower_Driver` performs domain/shape gates and existing VHO optimization,
then invokes verified runtime/generated-provider lowering at
`dsl_lower.cxx:4594-4602`. The ordinary lowerer explicitly describes direct
runtime WHIRL (`:2322-2332`). LNO cannot reconstruct a tensor algorithm hidden
behind such runtime calls. H-level producer/lineage preservation is an M2 entry
dependency under the existing H architecture gates. This audit does not claim
that the standalone MLA recipe currently reaches the proposed LNO point.

There are two useful but distinct device ownership precedents:

- Matmul's per-PU producer stages numeric selected tile/fetch records and owner
  ST in a pending registry (`dsl_lower.cxx:3461-3481`); the driver later calls
  VHO_DSL_CUDA_Take_Program_Kernel and constructs/publishes selected PUs at the
  program boundary (`driver.cxx:2697-2741`, `:3195-3204`). Preserve the detached
  handoff idea; this bounded matmul schema is not a generic MLA registry yet.
- The normalization checkpoint is an alternative driver branch before ordinary
  per-PU traversal (`driver.cxx:3156-3163`). Its driver-owned resident program,
  PREOPT callback and device construction are explicit in
  `normalization_driver.cxx:312-391`. It does not demonstrate ordinary LNO
  integration and cannot be used to bypass this audit's analysis requirements.

M2/M4 should select one driver-owned lifetime for the MLA handoff: synchronous
active-PU selected rewrite while LNO facts are live, or fully owned fixed facts
and a later driver rebind/revalidation. Initial recommendation is synchronous
search/application for existing loops, with detached numeric/device construction
requests carried to a driver-owned boundary. No LNO pointers may escape. Device
PU construction belongs to Phase 2, never candidate estimation.

## 7. Effective controls and selected-schedule ownership

`Adjust_Opt_Level` can reduce optimization and disable LNO/PREOPT/WOPT at
`driver.cxx:1165-1170`; it runs after VHO at `:2392`. Backend PU-option handling
also precedes LNO (`:1803-1816`). Gate search on the effective active-PU O3,
Run_lno, LNO_enabled, LNO_Opt, skip policy and admitted numerical permission
at the selected boundary, not only the original command line. The current
partition hook's O3/LNO checks are a precedent, not all required MLA controls.
Missing DU (`lnopt_main.cxx:1065`), no loops (`:1288`), graph-build failure,
skip/disabled and autopar-only paths must not publish a valid scheduling snapshot.

Initially refuse managed AI search on unsupported MP/Lego/autopar or REGION-root
forms and keep their existing lowering behavior. Broader admission requires
proof that those passes preserve selected ownership, scope and semantics.
M4 must prevent later reversals, fusion/fission/SNL, tiling, scalarization,
memlib substitution and prefetch from changing an accepted managed schedule.
Exempt the managed nests or use a reviewed selected-path continuation that
still performs required feedback, CG-graph export, cleanup and unrelated-loop
processing. Do not merely seal a plan and resume all ordinary optimizers.

After selected mutation, rebuild affected parent/access/DU/alias/dependence,
reduction and loop-summary facts using the owning services before their next
consumer. Define High exit before MAINOPT's destructive array lowering and
before Mid/Low memory/mapping realization. The audit identifies no already
implemented generic High exit verifier or shared snapshot epoch for this path.

## 8. M1 closure and implementation consequences

M1 closes source mapping, masks, lifetime, effective control, scope, snapshot
placement and device-handoff inspection. It does not close M2 or H0-H8 as a
whole. Production next work is M2a producer/admission, M2b owned snapshot and
epoch, M2c observer/isolation proof; M4 additionally needs the selected-path
ownership barrier. The updated plan lists the corresponding tests.

Validation is static: source hashes, line/excerpt consistency, ordering and
cross-reference checks plus documentation diff/link checks. No compiler code
was changed, no snapshot hook was installed, and no new B/T or GPU evidence was
produced. Existing v0.27 execution evidence remains historical and unchanged.
