# Owned High WHIRL snapshot contract v1.0

November 7, 2026 | M2b, bounded active-PU LNO implementation

The source implementation is `osprey/be/lno/dsl_high_snapshot.{h,cxx}` and
`dsl_high_snapshot_capture.cxx`, called from `Lnoptimizer` after M2a form
admission and the successful full array-dependence graph, before
`Hoist_Varying_Lower_Bounds`. This implements the owned-fact/lifetime portion
of M2. M2c search-observer isolation, M3 production search, M4 selected schedule
application and analysis rebuilding remain separate gates.

## Ownership and publication

The LNO collector borrows the active FUNC_ENTRY, current DU and alias managers,
LNO_Info_Map, and DEPV_ARRAY_ARRAY_GRAPH only during synchronous capture. It
first reruns M2a form/coverage admission, constructs a private result, and
publishes only on success. Refusal clears the output and continues ordinary
LNO. It neither switches PU context nor traverses PU_Info. The owned snapshot
contains strings, integers and standard-library vectors; it contains no WN,
ST, TY, graph, map, manager or compiler-pool pointers. Public data views are
const. Copying a snapshot copies its values, not an analysis lease.

The current opt-in remains `-DSL:partition_axis=on` at effective O3 with LNO
enabled. MP/Lego/autopar are excluded as in M2a. The trace control affects only
reporting; capture also runs with tracing disabled when admission is enabled.
There are no opcode, binary image, type-kind or be.so changes.

| Owned facts | Copied meaning and uncertainty |
| --- | --- |
| Source | Length-framed unit key, function/formal ABI order, executable semantic tree paths, opcodes, constant bit patterns, symbol/type signatures, offsets, fields, intrinsic IDs, flags and supported operation payloads |
| Loops | Semantic path identity, parent-loop identity, induction symbol/location, signed static lower/upper bounds, comparison and increasing unit step |
| Accesses | Stable operation/node IDs, enclosing loop, base-expression source join, read/write, element type/bytes, each affine dimension's constant, loop coefficients, linear symbols and nonconstant-loop count |
| Graph | Every full-graph vertex in the source subtree and every edge's source/sink, unused outer dimensions and complete DEPV rows; distance/direction and distance bound are retained |
| Scalar DU | Both LDID reaching definitions and STID uses; available/incomplete status, loop statement, sorted endpoints. External or unrepresented endpoints remain explicitly unknown |
| Alias | Effective PU alias context and all memory pairs including self pairs. Existing Aliased answers retain NOT_ALIASED=0, POSSIBLY_ALIASED=1, SAME_LOCATION=2 |
| Context | Effective IEEE and roundoff controls, native target ABI/ISA; source manifest, GPU resources and model bindings are explicitly unbound at this entry |

Nonlinear/delinearized accesses, unsupported aggregate/recursive types or
constant payloads, and graph vertices outside the represented subtree cause
snapshot refusal. Budgets bound source depth to 256, executable nodes to
200,000, memory operations to 512 and dependence edges to 100,000. The access
cap bounds pairwise alias queries to 131,328. These are collector safety limits,
not search budgets or profitability rules. Unknown DU/alias facts are copied as
unknown; they grant no schedule legality, floating-point reassociation or
parallel independence.

## Stable identity

The stable identity is the pair `(snapshot source namespace, local semantic
path)`. Paths use executable statement ordinals and kid positions, for example
`L@body.s3.4.s0` and `A@body.s3.4.s0.0`. COMMENT and nonexecutable PREAMBLE_END
markers do not consume executable ordinals. Source positions are excluded.
The parent/enclosing-loop paths are explicit joins into the same namespace.

The namespace is `high-v1/<FNV64 source digest>`. It is a compact review label,
not a collision-proof certificate. The complete length-framed source key is
owned and compared for equality, together with the full fact key and bindings.
No persistent identity uses a WN address, WN_map_id, graph vertex/edge number or
process-local ST/TY table index. Symbol first-occurrence ordinals and structural
scalar/pointer types replace table indices. PREG locations remain source-image
semantic operands; arbitrary PREG renumbering need not preserve the namespace.

The stability guarantee is the same qualified canonical source under the same
unit key, including repeated capture and independent mapped-image reopens in
separate backend processes. It is not equivalence checking across different
canonicalizations, source-path keys or compiler revisions. A source edit can
change the namespace; the local path alone is never a cross-version identity.
Numerical/target/model/manifest bindings are checked separately, so a model
update can retain source identities while expiring the old estimation context.

The caller supplies a stable unit key (the normal driver's Src_File_Name) and
owns its uniqueness across a build. Names/comments do not bind tensor shapes,
source numerical permission, disjointness or an external manifest. M2c/M3 must
supply and verify the complete required manifest/resource/model versions before
using this snapshot for production candidate admission or cost estimates.

## Mutation epochs and revalidation

`DSL_HIGH_EPOCH` belongs to one serial backend PU activation. Its value token is
`{owner, activation, mutation}`. A process-local monotonic activation separates
successive instances of even the same named PU, preventing ABA reuse. Mutation
advance is monotonic; saturation closes the epoch instead of wrapping. Scope
exit closes it, including early returns. Tokens are lifetime guards, not stable
source IDs and not portable tokens for a saved B file.

`Lnoptimizer` creates the epoch before any early exit. Once capture succeeds,
it advances the epoch immediately before Hoist_Varying_Lower_Bounds, the first
subsequent legacy mutation boundary. All remaining ordinary LNO work is one
conservatively expired region: it cannot reuse the captured facts as current.
This initial integration does not instrument each historical LNO routine with
a fresh generation or make the snapshot follow those transformations. New
snapshot consumers must call Before_mutation before any tree, symbol/type,
access, graph, DU, alias, numerical or context mutation in their live window.
They must rebuild affected native facts and capture a new snapshot afterward.

`Is_current` checks only owned context and epoch values and is safe after native
analysis destruction. `LNO_DSL_revalidate_high` performs this check first. On a
stale token/context it returns before examining any native argument. A current
token requires the caller still to be at the live admitted boundary; revalidation
recaptures source and analyses and compares full keys. It detects unannounced
changes to the covered facts as a defense in depth. Recapture is not a
substitute for notifying every mutation: it is not a general verifier of all
Open64 table state or of semantic equivalence.

A valid token alone grants no transformation. M4 still needs numerical and
schedule legality, fresh resource/model/space bindings, selected-plan preflight,
atomic application/rollback, analysis rebuild and the High exit verifier.

## Evidence and focused tests

`tests/run_high_snapshot_review.py` uses ordinary backend/PREOPT/LNO execution,
a production lno.so and a separately compiled test component. Only the latter
contains `LNO_DSL_HIGH_SNAPSHOT_TEST` perturbations. The production binary has
no test mutation callback or runtime test option.

The native tests deliberately change and restore a real loop bound, affine
access offset, dependence component, DU completeness, alias context and alias
answer, and effective IEEE controls. They test context changes, repeated full
content/identity equality, same-name activation and cross-PU rejection, stale
and closed epochs with NULL native arguments, and refusal clearing an earlier
output. Source-position changes preserve identity. An existing COMMENT can be
repositioned without renumbering; when PREOPT has removed comments, that check
is explicitly skipped, not counted as executed. No test adds strings or WNs to
the compiler tables to simulate an unchanged observer.

Both saved short/long absorbed MLA High images are tested; their graph edges
are empty, so a separate ordinary-C carried-dependence PU supplies a real
nonempty DEPV edge for invalidation. A nested affine PU tests independent
identities/activations; a dynamic-bound PU must refuse without publication. An extended-constant PU
passes form admission but deliberately fails snapshot payload admission; its
ordinary LNO output must also match baseline/on/off/default/trace-off output.
All native perturbations must restore byte-identical normal .N output.
Repeated production processes must produce identical full owned keys.

The production trace reports CAPTURE, a new live epoch with
snapshot_current=false before legacy mutation, EXPIRE_BEFORE_HOIST and
OWNED_AFTER_CLEANUP. The last report executes after the full graph, maps and LNO
pools have been deleted/popped. It uses only owned records. The snapshot does
not retain a DU/alias lease beyond wrapper cleanup.

The certified run passes 21 native check kinds across both MLA contexts and
four ordinary-C PUs, plus the standalone epoch lifecycle test. Both MLA
snapshots contain 59 loops, 87 accesses, 515 scalar DU records and 3,828 alias
pairs. Their graph has 87 represented vertices and no edges; the carried-C
fixture has a real edge and passes dependence invalidation. Test-build output
is byte-identical to production output. COMMENT checks are skipped at this
boundary when PREOPT has removed comments; source-position stability is tested.

The existing common-High runner additionally checks on/off/default/trace-off,
O0-O2 and LNO-opt=0 fallback, normal native output against the M2a LNO baseline,
and 21 host/reference executions. The 13-case native partition regression also passes (five proved, eight
refused), as an impact-selected compatibility check. The known baseline-identical long-context
post-LNO CG-dependence-image reopen failure remains an M6 blocker; it is not
counted as successful long post-LNO execution. No GPU execution, performance
result or globally optimal schedule is claimed by M2b.

## Next work and risks

M2c must add the read-only search observer and charge collection separately
from candidate exploration, prove zero candidate WN/ST/TY creation, and supply
complete external bindings. M3 integrates bounded portfolio controls. M4
revalidates the selected plan, owns the schedule and rebuilds analyses. M5
verifies High exit before Mid memory lowering, Low GPU mapping and PTX emission.

Risks include collector size/alias-query cost, incomplete external context,
missed mutation notification in a future consumer, identity churn after new
canonicalization, and the existing long post-LNO image defect. Fail closed on
missing bindings/coverage; preserve ordinary fallback and retain explicit
unknowns. A lifetime token is deliberately insufficient to authorize scheduling.
Owners, successor, independent reviewer and approving lead remain TBD.
