# M5 verified High exit and selected device bridge v1.0

November 7, 2026. Scope: the compiler-produced, closed absorbed-MLA route accepted
by M4. This is the controlling contract for the native selected bridge. M1-M4
and the existing architecture amendment retain their contracts outside this
bounded route. The complete search remains Phase 1 estimation followed by one
selected Phase 2 implementation; this bridge never proposes or estimates a
candidate. AOT remains the baseline.

## Architecture and ownership

| Boundary | Executable meaning and owner |
| --- | --- |
| Very High | Domain/common tensor operators and descriptors; ingestion and VHO admission |
| Common High | Actual common algorithm, signed DO_LOOP/ARRAY accesses and LNO dependence/schedule analysis; M4 atomically applies one accepted algorithm and rebuilds analyses |
| Selected High exit | Fixed algorithm partition, abstract participant inputs, shared allocation/staging DO_LOOPs and explicit copy/compute/release synchronization; selected VHO realization under driver-owned fresh PU contexts |
| Mid | Native WN_Lower with LOWER_ARRAY and LOWER_SCF resolves ARRAY to byte-address expressions and structured control to labels/branches; synchronization protocol and abstract owners survive |
| Low | Fixed owner inputs bind to blockIdx.x/threadIdx.x and kernel entry ABIs; shared/global storage and collective scopes remain explicit |
| PTX | A bounded native Low-WHIRL emitter under be/cg/NVISA emits scalar PTX; ptxas assembles and nvlink links device math support |

This preserves WHIRL.pdf's distinction: traditional Mid has address expressions
and branch control, rather than retaining ARRAY/DO_LOOP and merely renaming a
checkpoint. The Low GPU ownership/resource role is comparable in purpose to a
GPU-specific lowering stage such as TTGIR; this implementation does not claim
TTGIR feature equivalence. No opcode, node-kid layout, type-kind encoding,
mapped-image layout or ELF writer contract changes.

The existing High Wolf/unimodular, Cohen/polyhedral and optional hybrid search
design remains applicable. Their legality and convergence decisions precede
this bridge. The implemented MLA provider is the finite descriptor family from
M3, not an implementation of unrestricted polyhedral schedule synthesis.

The backend driver owns the selected source PU. Its normal selected `.N` output
or immediate selected `.after.B` checkpoint is the handoff to the explicit
`tensor_selected_bridge` executable. That driver owns source admission in a child
process, the fresh device translation unit, PU creation/selection, backend
symbol-table observers, checkpoint publication and text publication. Each VHO/CG
service operates only on the active PU; none traverses PU_Info or switches
Current_pu. This is an explicit opt-in bridge, not default GPU routing for every
invocation of the traditional backend.

## Admission of the actual selected algorithm

M4 still requires its fresh source/epoch, numerical, manifest, resource, model,
control, incumbent, partition and capacity proofs. After its independent
PREOPT_DUONLY/LNO analysis rebuild and COMMENT reinsertion, it now records:

```
tensor.high.accepted.schema = mla-selected-native-v1
tensor.high.accepted.key = complete native tree + selected binding equality key
```

The key includes the actual native executable tree, F32 constant bits, referenced
declarations/types/prototypes, offsets/descriptors, flags, comments and all
existing `tensor.high.selected.*` obligations. Source positions, native map
identifiers and nonexecuting LOOP_INFO trip estimates are excluded. It is full equality evidence, not a lossy hash and not
a security signature or an arbitrary program-equivalence proof.

The bridge maps the source, recomputes this key before allocating device WN/ST/TY
state, compares the original semantic recipe key and closed-DAG/storage producer
witness, and reads the fixed width and physical policy. It requires the accepted
resource identity's architecture field to be sm_90. It does not infer SM count
from an architecture string or admit a different resource/model/control binding.
Admission verifies native symbol/type table counts are unchanged. Caller-owned
source bytes are never rewritten.

Older M4 products without this seal are refused. Re-run the current M4 producer;
do not manufacture a seal for an unrelated or subsequently optimized High tree.
Any intervening mutation that invalidates the key requires a fresh owning-phase
proof. This conservative boundary is intentional until broader High recognition
and proof transport exist.

The original accepted High tree is therefore a required input to translation.
The bridge is a verified closed-pattern translation using its producer witness,
not permission to regenerate a device algorithm from an unrelated JSON recipe.

## Fixed policy handoff and selected High exit contract

The driver JSON handoff adds `open64.tensor-kernel.v8` for a fixed register-latent
schedule with `fixed_chunk_policy` in 2..6. It retains the existing canonical
recipe seal. Existing v1-v7 schemas remain accepted unchanged. v8 has no automatic
chunk selector, portfolio or search certificate to replay. The policy participates
in the recipe fingerprint/kernel symbol, preventing an omitted policy from
making width-64 specializations share a name. The fingerprint is a convenience
name, not a semantic proof or collision-free artifact identity. Explicitly preserve width-32/32-thread,
width-64/64-thread, two-warp-broadcast and warp-owned-partial decisions.

Reuse the established native selected staging generator and its original
argument/prototype/resource/collective verifier and explanation generator. Before
publishing High, replace its temporary coordinate leaves with two explicit U4
formal inputs, `selected_block_owner` and `selected_thread_owner`, and an ordinary
device-function prototype. This adapter changes no arithmetic, token/column
ownership, chunk bound, staging loop or collective. The original transient
physical-coordinate form is not a published High exit. Extending owner loads
retain a U4 descriptor even when their result type is U8.

The High exit requires all of the following before destructive lowering:

1. Exact accepted source/recipe/resource/model/control/numerical lineage and one
   fixed, valid schedule; auto_chunks and search_selected are false.
2. Fully defined owner inputs and buffer ABI, with no hardware coordinate load.
3. Explicit staging/copy/compute/release loops, allocation extents and shared
   placement, plus typed CTA/warp synchronization and full-mask shuffle calls.
4. The selected direct or split attention algorithm, tuple layouts and stable
   merge; no missing scheduling choice may be inferred by an emitter.
5. Generated COMMENT explanations and strict native tree structure.
6. A complete native worker-level equality key and native symbol-table verifier.

The worker key additionally binds its level, fixed recipe, source witness,
launch sizes, mapping, scratch/argument bindings, shared byte budget, tuple/
accumulation/statistics policy, and the complete active shared-declaration
inventory and dormant GPU coordinate declarations. Unused added shared objects
also invalidate it, since emitters declare all shared objects. The legacy
kernel body hash is explicitly retired in favor of this complete level key.
The accepted M4 decision COMMENT is copied verbatim into each worker with a
provenance note; existing generated per-kernel explanations follow it.
Source lines/map IDs are not proof identities.

`tensor.bridge.schema=selected-mla-native-v1` versions this contract. Driver and
printer recognize partial schema/level/key markers and fail closed if required
fields are missing. The key distinguishes absent optional bindings from empty
ones and guards the provenance fields and absence of automatic/search metadata.

The bounded launch ABI requires nonzero U4-sized owner domains, at most 1024
threads per CTA, and at most 48 KiB static shared memory. Larger static shared
requirements need an explicit supported opt-in ABI; the bridge refuses them
rather than inventing one. The launch adapter must additionally check the
actual device's grid, block and memory limits.

The selected staging realization is the existing synchronous, one-slot
copy/compute/release baseline. There is no cp.async/TMA pipeline or separately
selected speculative look-ahead prefetch in these MLA descriptors. No missing
async completion or prefetch proof is synthesized in Mid/Low. Such descriptors
need a separate supported producer/verifier before admission.

## Algorithms, storage and numerical semantics

The short route launches query projection, one direct fused attention kernel,
value projection and the two output copies. The long route substitutes partial
attention and a tuple-merge kernel for direct attention. Exact launch sizes and
buffer joins are read from the verified Low image. The width-32 and width-64
physical policies retain their selected warp ownership, register broadcasts,
shared tuple slots and synchronization counts.

Within each chunk c, compute maximum m_c, denominator d_c and normalized latent
z_c. For the nonempty chunks, the stable merge uses

```
m = max_c m_c
rho_c = d_c * exp(m_c - m)
d = sum_c rho_c
latent = sum_c (rho_c * z_c) / safe(d)
lse = m + log(d), with the source's empty/nonfinite guards
```

The partial layout is `[maximum, denominator, normalized_latent...]`; it is not
an unnormalized numerator. Preserve existing guards for masked/empty chunks,
all-masked rows, infinities and tails, as well as the explicitly permitted
reduction reassociation. No global score/probability vector is introduced.

The source High function retains its original caller scratch ABI, as in M4.
Splitting it into device stages introduces the device allocation/launch ABI:
the verified Low argument records identify which logical allocations remain
materialized and the synthetic chunk-tuple scratch. This is not a claim that
the source caller's scratch interface disappeared or that both interfaces have
identical physical layouts. Launch adapters allocate/join these records and
preserve the original inputs and output shapes. M4's resource preflight already
checks the selected synthetic tuple bytes against its bound GPU scratch budget.

## Mid/Low lowering, invalidation and source emission

Use the exported native WN_Lower service from existing be.so in the standalone
bridge. Do not duplicate general ARRAY linearization or structured-control
lowering. The driver installs real PU-pool maps before construction; builder-only
malloc-sentinel maps are unsuitable for native simplifier reserved-map growth.
The driver initializes/registers backend symbol-table scopes around each active
PU. Lower_Init/Lowering_Finalize bracket the native lowering maps.

After High, no LNO DU/alias/access/dependence/reduction manager or snapshot pointer
is carried into the device translation unit. Mid owns its new native addresses
and labels. It must contain no ARRAY or structured IF/DO_LOOP. Low replaces only
the two owner inputs, restores the pointer-only kernel ABI, and retires the old
formal-only flags. Native symbol verification and exact level keys run at each
boundary; previous keys are replaced, never treated as current analyses.

For `whirl2c -cuda`, the ordinary backend driver recognizes bridge metadata
before VHO/goto/return/address canonicalization. It requires CUDA-only emission,
the Low level and the exact native key, then preserves that product through the
normal driver-owned read/context/trace/symbol lifetime and emission. The source
printer checks the key again. High/Mid or mutated Low products are rejected;
the printer cannot repair, recertify, select or reschedule them. Ordinary inputs
without bridge metadata retain their existing backend path.

Typed F32 collective return carriers remain native negative return pregs. The
CUDA printer uses the existing ST2C preg naming/type service for both the CALL
assignment and its immediate result load. It does not insert host-ABI return
lowering or put host physical return registers into GPU WHIRL.

The general PREOPT COMMENT-loss repair remains deferred. M4's exact reinsertion
after PREOPT stays intact. The bridge introduces no PREOPT round; COMMENT nodes
survive native memory/control lowering, GPU binding, whirl2c CUDA and native PTX
line comments. The existing schedule-source path remains an algorithm-review
view generated from the same fixed recipe, with explanation equality checks.

## Bounded native PTX backend

`be/cg/NVISA/tensor_selected_ptx.cxx` consumes only a verified active Low PU. It
emits PTX 7.8 for sm_90 with scalar integer/F32 registers, exact F32 constant bits,
explicit round-to-nearest add/sub/mul/div, predicate comparisons/selects, native
labels/branches, global/shared loads/stores, physical coordinate reads,
bar.sync/bar.warp.sync and full-mask width-32 shuffles. Unordered F32 inequality
preserves NaN tests. Integer bit operations preserve register width. No FMA or
approximate exp2/log2 substitutes are introduced.

exp/log use explicit external F32 math-helper calls. The generated `.math.cu`
contains only two noinline device wrappers around expf/logf and is compiled with
strict CUDA flags, then linked to the assembled native PTX object. It contains
no MLA algorithm, address calculation, launch decision or optimization template.
The CUDA emitted from Low is a separate parity lane, not the source of native
PTX. Unsupported effects, types, address spaces, masks or intrinsics refuse
emission before final text publication.

This restricted backend does not replace the historical general NVISA target
or establish support for arbitrary Low WHIRL/all GPU architectures. NVIDIA's
[PTX ISA](https://docs.nvidia.com/cuda/archive/12.3.0/parallel-thread-execution/index.html)
defines the emitted instruction and device-call contracts. The version-pinned
[CUDA 12.8.1 redistributable manifest](https://developer.download.nvidia.com/compute/cuda/redist/redistrib_12.8.1.json)
binds the validation assembler/linker/compiler packages by SHA256.

## Continuity review and bounded extension decision

The implementation reviewed native WN_Lower ARRAY/SCF expansion, the existing
LNO snapshot Source_key, the existing mapped-kernel fingerprint/auditor, and
NVISA expand/cgemit target services. WN_Lower and the selected staging/comment
generator are reused directly. The LNO snapshot depends on owning analysis
managers and intentionally omits comments; the legacy device fingerprint is
lossy, does not bind this Mid/Low/source contract and remains backward compatible.
The new common executable-key helper is shared by the owning phases, driver and
printer, rather than giving each consumer a different serializer.

The current native tools are built for n64/X8664 mapped WHIRL with the established
typed ordinary-CALL CUDA collective carrier. The historical NVISA target did
not expose a supported modern lowering contract for this carrier's full-mask
shuffle/register-broadcast/warp-fence protocol in that toolchain. Treating these
names as ordinary external device functions would leave CUDA compiler builtins
unresolved. Porting the full target and adding modern intrinsic/TOP/resource
contracts is broader M6 work. The explicitly reviewed M5 extension is therefore
an opt-in, bounded native Low consumer under CG/NVISA, with strict rejection and
assembler/link validation, while preserving the old target. It does not pretend
to have exercised the historical CG_Generate/TOP/TN pipeline or a general target
build. The design and acceptance boundary are explicit review records for this
extension, not permission to grow a second general optimizer in an emitter.

## Failure, publication, tests and remaining gates

Admission or lowering refusal never mutates the accepted source and never tries
another candidate. Unsupported source/architecture/level retains the existing
ordinary compiler output as fallback. Fresh worker state belongs to the bridge
process and is abandoned on failure. No Low/PTX success artifact is published
until all fixed workers pass native Low emission; valid High/Mid checkpoints and
failure diagnostics may remain for review. Each publication refuses an existing
destination and uses the established child-writer/no-replace protocol. Complete
receipt/manifest verification determines milestone success, not the presence of
an intermediate file. Existing review receipts are never overwritten.

The focused acceptance driver is
`osprey/be/vho/tests/run_selected_bridge_review.py`. It covers normal short/long
backend outputs and five narrow-policy selected variants; independent mapped
High/Mid/Low reopen and raw diffs; deterministic PTX regeneration from reopened
Low; ptxas assembly/device link; independent CUDA parity compilation; generated
explanation equality; host collective simulation with expanded FP64 oracles,
guarded buffers, input preservation and reversed launch order; native mutation
refusals; and the unchanged source/estimation/fallback contracts. M4's reference,
rollback/refusal suite is rerun for the new acceptance seal. Exact counts and
commands are evidence in the versioned receipt.

For comment/metadata-only finalization, the acceptance harness can reuse already
certified host execution only after validating the complete prior SHA256SUMS,
exact comment-free native executable keys, exact C/C++ lexical tokens and equal
launch/storage records. It rechecks every retained output against its FP64
oracle and reruns native assembly/link, CUDA compilation and refusal gates.
The separate `bridge-code-key` extraction mode is comparison data, not admission:
all production phase/printer keys continue to include COMMENTs. Receipts distinguish
fresh executions from revalidated retained evidence. Reviewers may omit the
reuse option to execute the full corpus again.

The new standalone executable links existing be.so and only the required
frontend-builder helper objects. No new library dependency or exported/undefined
symbol is added to be.so. Native driver and whirl2c plugin admission changes are
separate from that shared library's ABI. Retain source-matched symbol audits.

M6 still owns broader/general production routing, unsupported scopes and the
ordinary nonselected long post-LNO dependence-image reopen defect. M7 still
requires real GPU correctness, sanitizer/race checks, actual resources and paired
performance/calibration. Assembler/linker success and host collective simulation
are not GPU numerical or performance results. Actual ptxas resource reports must
inform later model calibration; the emitter never changes the selected plan to
fit a different estimate. There is no global GPU-optimum or speedup claim.

Primary owner, successor, independent reviewer and approving lead remain TBD per
the user's instruction. Independent transfer/production acceptance remains open.
The versioned source, native artifacts, readable CUDA, PTX, cubins, commands,
reports, risks and immutable receipt provide the technical transfer package.

Observed compiler/host acceptance for this milestone: seven fixed configurations
(41 kernel entries), seven native PTX assembly/device-link products and seven
separate CUDA parity compilations; 48 host collective reference executions with
reverse order, revalidated for final provenance-only changes under exact native
executable/CUDA-token/launch equality; 66 refusal checks (12 source, 43 worker,
3 staged boundary, 3 wrong-level, 2 premature CUDA and 3 other controls). The four
final short/long selected/estimate backend images are strictly byte-identical to
their certified M4 counterparts. M4's 52 High reference executions, 10 rollback
cases and 27 refusals also passed with the new acceptance seal. be.so is
byte-identical. These results do not close the real-GPU gate.
