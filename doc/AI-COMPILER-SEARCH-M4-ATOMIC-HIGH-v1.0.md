# M4 atomic selected High-WHIRL MLA application v1.0

November 7, 2026 | Bounded native selected-only application

## Contract and scope

M4 consumes the actual M3 observer result only after the estimation isolation
guard succeeds. Search remains estimation-only and unchanged. A separate
-LNO:high_apply=on opt-in, together with explicit -LNO:high_search=on and a
complete v2 binding, enables one synchronous selected application in the active
FUNC_ENTRY at the canonical full-graph LNO boundary. The default and explicit
off retain the M3 observer and ordinary LNO. Invalid application controls,
stale selection, no incumbent or unsupported shape/capacity retain fallback.

This is the compiler-produced common-high-v1 absorbed MLA entry, its exact
48-operation closed DAG and disjoint storage-formal ABI. The producer recipe
witness is a compiler semantic provenance contract, not a cryptographic
signature or a general equivalence checker for externally modified WHIRL with
forged producer metadata. Role-1 formals are immutable inputs, role-2 formals
are observable results, and role-0 storage is compiler-private scratch, whose
contents may change. This does not authorize rewriting arbitrary C functions
that expose their intermediate buffers. Unbound or different providers retain
their existing paths.

Minimum scope is effective O3/LNO, with the existing admitted numerical
reassociation permission and exactly bound effective IEEE/roundoff/fast_math
context. MP/Lego/autopar, REGION-root/nested-region, nested-PU and feedback
contexts are unsupported. The driver owns the active PU; the service neither
traverses another PU nor switches its local scope. Primary owner, successor,
independent reviewer and approving lead remain TBD.

The architecture remains Very High domain operators -> High common algorithm
and LNO schedule construction -> Mid structured memory operations -> Low GPU
mapping -> PTX. M4 constructs executable scalar High semantics with DO_LOOP,
ARRAY, scalar expressions and COMMENT. Warp ownership, SMEM placement,
barriers and prefetch realization remain sealed obligations for M5's verified
High-exit/device bridge. No candidate CUDA/PTX or device PU is generated here.

## Fresh selected-plan legality

Preflight is read-only and precedes every selected native allocation:

1. Require a controlled M3 result with an eligible, evaluated incumbent.
2. Revalidate owner/activation/mutation stamp, complete source and fact keys
   against live native loop/access/DU/alias/full dependence facts.
3. Revalidate complete algorithm/ABI/numerical/GPU/model input and compare the
   report's full manifest, resource, model and ordered control identities.
4. Check both copies of the selected ordinal and descriptor; reject changed
   width/policy/scratch/cost or a non-best eligible explored incumbent. Stable
   ties use catalog ordinal. No external winner override is accepted.
5. Reuse the existing closed-DAG recognizer and schedule validator for the fixed
   descriptor, then independently recheck every stage's rounded occupancy,
   shared/register capacity and the full partial-tuple scratch bound.
6. Prove the scalar High tuple storage fits the existing private ABI. For
   C=ceil(L/width)>1, require C*R <= L for the numerator scratch; maxima and
   denominators each require C <= L. Direct attention uses the existing
   row-latent allocation. An otherwise estimated candidate can be refused by
   this bounded materializer; retain the accepted fallback, never search for a
   second candidate silently.
7. Require enough native analysis-map slots for the staged rebuild (48 free
   slots conservatively reserved), no unsupported scope and no feedback.

The mathematical legality is specific to this DAG. For a row and chunk c,
with scores s_t, mask mapping to negative infinity, define:

    raw_m_c = max_t s_t
    safe_m_c = finite(raw_m_c) ? raw_m_c : 0
    e_t = exp(s_t-safe_m_c)
    d_c = sum_t e_t
    safe_d_c = d_c > 0 ? d_c : 1
    z_c[r] = sum_t(e_t * cache[t,r]) / safe_d_c

For long contexts choose raw_M=max_c raw_m_c and its finite-safe M, then:

    mass_c = d_c == 0 ? 0 : d_c * exp(safe_m_c-M)
    D = sum_c mass_c
    safe_D = D > 0 ? D : 1
    z[r] = sum_c z_c[r] * (mass_c/safe_D)
    LSE = D > 0 ? M + log(safe_D) : -infinity

The implementation branches on zero chunk mass before exp. NaN-propagating
maximum and source finite/denominator guards remain explicit; masked token
weighting still observes the source's multiplication semantics. Chunk bounds
are a disjoint covering partition of [0,L); tail guards precede every cache/
mask access. Dot-product contributions remain ascending F32 folds. Changes to
the softmax/latent grouping require the admitted reassociation contract.

This is not an arbitrary unimodular/polyhedral dependence certificate. Generic
Wolf/Cohen schedule application must separately prove transformed dependence
vectors lexicographically nonnegative, handle reductions/numerics and provide
its own materializer. M4 does not mark those optional providers implemented.

## Selected High algorithm

The builder reuses tensor_high_lower.cxx's typed signed loops, affine indexing,
ordered projection generator and source positions. It emits the query
projection, fused chunk-local attention, optional stable merge, the value
projection and the two final result copies.

No full score or probability vector is produced. The scalar High view
recomputes each score in maximum and weight passes; it is a readable,
executable semantic implementation, not the eventual mapped GPU kernel.
Projection weights use the sealed static transpose permutation directly;
there is no read from an unproduced transposed scratch buffer. Short contexts
write the row latent and LSE directly. Long contexts reuse three existing
private buffers for normalized latent tuples, raw maxima and denominators,
within the proved capacities. Chunks write disjoint tuple slots, then the
merge consumes completed tuples. Physical parallelism is deferred.

The same High equations serve the selected policy-3/5/6 thread-ownership
families. Their differences remain fixed metadata for later GPU mapping;
M4 does not claim that its serial review source performs warp reductions or
shared-memory synchronization. The old cost model is not a timing prediction
for this host source or for its deliberate score recomputation.

The builder clones source F32 constant nodes instead of extending TCON/global
constant-ST interning caches. Existing scalar/pointer types and formal ABI are
reused. New locals and comments are created only for the selected body, under
the transaction. The source High producer retains its original unoptimized
behavior through the same shared construction helpers.

## Atomic transaction and rollback

The original tree and analysis bundle stay live until every recoverable check
on the staged replacement succeeds. A transaction journals all existing
global/local ST and TY row bytes, local append boundaries, PREG/label rows,
function/PU state, metadata/KV rows, string-table position, source-map contents,
native map allocator IDs/free lists and RID state.

The string savepoint reuses the already exported implementation whose
historical ABI names are DSL_CKKS_Strtab_Save/Restore. That implementation is
domain-neutral string-table save/restore in common/com/strtab.cxx. No CKKS
semantic, image, event or cryptographic operation is used; no new be.so
dependency/export is introduced. A later API-name cleanup can preserve that
compatibility ABI separately.

Before selected construction the owning epoch expires the old snapshot.
One staged body/root is built. An independent parent/loop/access/DU/alias/
dependence/reduction bundle is created for that tree. Partial map/manager
allocation unwinds only its own resources. Reuse PREOPT_DUONLY_PHASE, the
existing LNO reconstruction precedent in LNO_Check_Du, to rebuild scalar DU
and alias information. Reparent, mark loops, rebuild access arrays, full
dependence graph and reduction summaries using their existing owning services.
Capture and independently revalidate a fresh owned snapshot before commit.

PREOPT_DUONLY drops COMMENT nodes, so reattach the exact selected explanation
after its emitter, before verification. No executable scheduling policy is
introduced by that reattachment or by whirl2c.

On success, atomically hand the verified root and fresh DU/alias managers to
the LNO driver, update its debugger/PU root context, retire the old analysis
bundle, and continue through the selected ownership barrier and normal
cleanup. The internal Lnoptimizer manager parameters become references so
lnodriver owns and later destroys the correct returned managers. The public
Perform_Loop_Nest_Optimization backend entry ABI is unchanged.

On any recoverable construction, rebuild, verification or checkpoint failure,
destroy staged analyses; reinstall original root/RID/manager/map handles;
restore existing table and metadata row bytes, append boundaries and strings;
restore source map values and native map allocator state; recapture the
original live facts under the new generation and require full Same_content
equality. The old stamp remains expired after rollback. Ordinary LNO then
continues on restored analyses. No second descriptor is estimated,
materialized or tested as recovery.

Global type/TCON/constant-ST interning caches must not grow in this bounded
construction. An unexpected extension, or inability to prove rollback, is a
compiler invariant failure and stops compilation rather than pretending that
an unsafe truncation is recoverable. Process failure and out-of-memory are
not a transactional crash-recovery protocol. Staged unreachable WNs remain
owned by the ordinary PU pool until its normal cleanup; they are never
published as candidate artifacts.

## Driver ordering and selected ownership

1. Normal driver/PREOPT/common-High admission and first full LNO graph.
2. Owned snapshot and full M3 binding validation; detached exploration; native
   isolation guard and analysis revalidation, with existing separate counters.
3. Fresh M4 preflight and optional before checkpoint.
4. Expire old epoch, stage only the selected High algorithm, rebuild and
   verify analyses, bind fixed selected metadata, optional after checkpoint.
5. Commit root/managers together. The entire admitted closed MLA PU is
   managed; export its CG dependence graph and jump to ordinary LNO cleanup.
6. Bypass reversal, fusion/fission/SNL, scalarization, memlib/prefetch and
   competing locality transformations. Skip the final WN_Simplify_Tree on the
   managed path as well. No unrelated code is claimed to be preserved by
   blanket bypass in a general mixed PU; such admission is unsupported.
7. Normal driver RID/alias lifecycle, output, analysis-map/pool cleanup. The
   original owned snapshot remains readable but expired; SELECTED_REBUILT
   records identify the accepted generation.

M5 must verify High exit and consume the fixed plan before destructive
MAINOPT array lowering, then realize structured memory operations and GPU
mapping. Neither Mid/Low nor whirl2c may reopen search. M4 does not establish
the future full pipeline beyond this LNO ownership boundary.

## Fixed handoff and review artifacts

Existing ST tensor metadata carries lno-selected-mla-v1 with selected width,
policy and ordinal, full post-application source/fact keys, resource/model/
control/numerical identities and the private-scratch capacity contract.
No opcode, type kind, WHIRL image layout or reader/writer behavior changes.
This is a fixed decision/witness for later fresh admission, not a native
analysis pointer or an authorization to reuse expired analyses.

Optional -LNO:high_apply_checkpoint=/fresh/base publishes base.before.B and
base.after.B using the existing standard mapped-image writer in a child
process. The active PU alone is written; no scope switch or additional PU
traversal occurs. The child isolates the writer's consuming subsection state.
High checkpoints intentionally omit the later CG dependence section.
Publication uses fresh temporary files and no-replace link promotion. A
failed after checkpoint rolls application back; an existing destination is
not overwritten. This IO is outside candidate exploration and its budgets.

Each checkpoint is reopened as the matching .T with ir_b2a -st -src, and the
before/after diff is retained. Ordinary whirl2c emits executable .cu review
files directly from the selected images, preserving explanations. Their
syntax is host C with CUDA filename, not mapped device kernels. Host reference
tests compile with strict F32 flags and verify input immutability, allocation
guards and saved source/FP64 tolerances. Source, WHIRL, review CUDA and normal
post-LNO output must agree; no source-template substitute is accepted.

## Tests, limits and subsequent milestones

run_high_apply_review.py uses separate production and fault-injection LNO
components. Test-only hooks cannot be selected by options/JSON and are absent
from production behavior. It covers both default contexts; repeated outcomes;
all saved reference variants before/after/normal output; selected width-32/
64 policy families; construction, DU/alias/access, dependence and pre-commit
rollback; stale epoch/source/model/resource/controls; changed/non-best
incumbents; missing permission; no incumbent; lower optimization levels,
disabled LNO and application off/default/invalid modes. Reports retain every
raw image comparison and all exact invocations. Existing M3/legacy observer,
snapshot/lifecycle, common-High, partition and 38 tensor/search regressions
remain applicable. Exact accepted counts are recorded in the delivery receipt.

Strict fallback still requires byte identity. Reassociation output can expose
the existing trailing DEP_STRUCT padding defect; retain raw differences,
classify only native-ABI-proved padding offsets and compare every meaningful
byte exactly. No writer normalization is made. The older ordinary long
post-LNO graph reopen defect remains an M6 issue for that legacy path; selected
output execution is assessed independently and does not repair the writer.

M4 is bounded selected High application. M5 High exit/Mid-Low/PTX orchestration,
M6 general integration, M7 hardware correctness/performance, M8 equal-budget
strategy evaluation/optional hybrid providers and M9 independent transfer
remain open. No GPU execution, measured speedup, global GPU optimum or general
VHO routing is claimed. Role assignments remain TBD and block transfer
acceptance, not this scoped compiler milestone.
