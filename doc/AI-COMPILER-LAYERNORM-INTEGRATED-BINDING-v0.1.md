# LN-REF-1 Integrated Source, Target And Numerical Binding

Version v0.1 | 2026-10-28 | LN1-e bounded evidence implemented; no kernel lowering

## 1. Decision And Scope

Close the gap between the [native v2 contract](AI-COMPILER-LAYERNORM-NATIVE-REFERENCE-CONTRACT-v0.1.md),
[conditional target binding](AI-COMPILER-LAYERNORM-TARGET-ARITHMETIC-v0.1.md)
and [independent arithmetic oracle](AI-COMPILER-LAYERNORM-ARITHMETIC-ORACLE-v0.1.md).
A numerical witness is attached to an exact native source identity, domain,
operand mapping, epsilon, target request and environment, not merely labeled
"LayerNorm" or judged by output shape.

LN1-e is an evidence/verification integration, not a new lowering pass. It adds
no production numerical authorization, opcode, mapped-image field, option,
runtime provider, profitability decision or kernel. WHIRL remains the sole
executable truth. Test fixtures interpret the arithmetic contract admitted by
WHIRL; they do not execute the mapped image or claim CUDA/GPU behavior.
Unresolved version 1 stays unchanged and cannot enter the numerical lane.

## 2. Terms And Ownership

| Term / component | Definition and responsibility |
| --- | --- |
| Source identity | Image receipt/digest plus native PU, node and result IDs. IDs alone are not globally unique across images. |
| Source snapshot | Complete version-2 characterization: X/gamma/beta/result identities, static rows/width, exact epsilon/count bits, ordered statistics/scalar/environment contract and source position. |
| Target request | Exact `sm_90`, `sm_90a`, `sm_100` or `sm_100a`, pointer-scalar route, toolkit 12080/PTX 807, strict FP options and RNE/gradual/class-only environment. This is conditional ISA eligibility, not hardware conformance. |
| Invocation | Complete X[R,N], gamma[N] and beta[N] bit payloads bound by native value ID, plus source snapshot and test regime. Y is an expected result, not an input. |
| Row witness | One invocation row evaluated by exact rational arithmetic, independent MPFR intervals and native C0/C1/C2 storage witnesses. It preserves native N, epsilon, affine broadcasts and row order. |
| `dsl_normalization_evidence_test.cxx` | Reuses explicit source construction and existing native target queries; snapshots source images before/after read-only binding. No reduction service is used. |
| `normalization_binding_report.py` | Joins real PREOPT facts, target gates and numerical evidence; checks every node, tensor identity, byte digest, input transcript and state. |
| `run_normalization_binding_review.py` | Produces native/mapped/control evidence, binds numerical fixtures and independently regenerates conventional reports. |
| `test_normalization_binding_report.py` | Synthetic schema/provenance/refusal tests. Synthetic ELF-prefix fixtures test plumbing only and are never native mapped-reader evidence. |

The only existing test-source change is an entry-point guard allowing the new
fixture to reuse the LN1-c test helpers. Its normal entry point remains enabled.
An additional `ir_tools` test target links existing normalization/WOPT objects;
no concurrent reduction implementation, test, receipt or API is changed.

## 3. Native Evidence And Version Gates

Use seven explicit producer variants for each architecture: default, unit,
wide, epsilon2, subnormal, mixed v1/v2 and shared-affine. They cover R/N of 3/17,
1/1 and 7/65; exact epsilon `3727c5ac`, `3a83126f`, minimum subnormal and the
mixed variant's additional `3f800000`. Fixture names are not proof: the auditor
checks the actual contract count, domain, epsilon and affine identity for each
variant. Count-limit characterization remains preserved prior evidence, not an
attempt to allocate a huge numerical fixture.

For every native v2 assignment, query the existing target binder three times:

1. Exact current-source request: conditional binding succeeds.
2. Expected-source epsilon changed by one bit: stale-source refusal.
3. FTZ requested: unsafe-environment refusal.

Each request is repeated with trace disabled and compared to its traced
result. Existing helper snapshots check source/table immutability. Every v1
assignment receives an explicit `source_unbound` refusal and no row witness.
The source is checkpointed through the standard mapped-image writer before
and after the queries; the two images must be byte-identical.

Actual PREOPT runs separately with trace on, default trace, trace off and
PREOPT off. On/default semantic facts must agree; disabled controls emit no
normalization facts. V2 still reaches the explicit "no executable VHO lowering
route" refusal, and no object may be published. An incomplete backend image
is retained with a clearly incomplete suffix, never accepted as the source.
Independent `ir_b2a -st -src` processes print both valid source checkpoints;
the full dumps must agree, with source available under its DST path.

## 4. Integrated Binding Algorithm

```text
for architecture, source_variant in the exact required case census:
    source = independently audit actual PREOPT entry/exit facts
    target = independently audit native target queries joined to source
    require one baseline/stale/FTZ query for every v2 node
    require one source-unbound refusal for every v1 node
    validate actual source domain/epsilon/alias diversity against the variant

    for v2 node, regime in finite/cancellation/subnormal/exceptional:
        bind full input tensors by native value ID
        if gamma ID == beta ID: use one shared payload, not two arrays
        preserve all source rows, width, epsilon and affine broadcasts
        for r in ascending rows:
            bind X[r,:], gamma[:], beta[:] and native epsilon
            derive exact expected states from LN-REF-1
            evaluate native C0/C1/C2 and independent MPFR paths
            compare every state: non-NaN exact bits, NaN class only

    retain canonical input transcript and raw numerical trace
    require image/dump/control/target/input/trace and checker-source/tool hashes
    independently reconstruct the joins and full expected state census
    publish reports only after all checks succeed
```

Identical operand IDs in a source invocation mean identical payloads. Multiple
nodes sharing X/gamma/beta receive identical inputs in a regime even if their
epsilon differs. There is no implicit output alias or storage/event permission.
Manifest comparison distinguishes JSON booleans/floats from integer domains.
Canonical request order is independent of JSON key ordering, while each row's
ascending arithmetic order is unchanged. The retained transcript must equal
the reconstructed requests, not merely hash to a self-reported value.

The test resource bound is R<=32 and N<=1024; it limits this numerical harness,
not native LN-REF-1 legality. General native N remains in [1,2^24]. No claim of
complete domain exhaustiveness, universal real/LLM accuracy or floating-point
exception observability follows from sample agreement.

## 5. Reports, Trace And Retention

| Artifact | What it summarizes / proves |
| --- | --- |
| Raw native `target.t` | Actual native source-match, target/environment binding and refusal queries. Existing positional schema and source line last are unchanged. |
| `trace/*.t` | Actual backend/PREOPT on/default/off controls, retaining existing option behavior. |
| `numerical/arithmetic.t` | Test-only numerical-engine intermediate states; source line names the host checker, not Python input or WHIRL. |
| `binding.json` | Exact invocations/source snapshots and hashes of source images, mapped dumps, controls, target trace, requests and numerical trace, plus checker binary/source identity. |
| `binding.log` | Every numerical row's invocation, PU/node/result, architecture and original native source line last. Derived evidence, not a compiler transformation event. |
| `numerical.log` | Complete invocation/row/state census, exact acceptance policy, independent engines and explicit no-GPU/no-image-execution boundary. |
| `optimization.log` | Full collection of successful native semantic, target and joined numerical reports; earlier stage-local labels retain their historical scope. |
| `optimization-summary.log` | Bound/refused counts, v1 preservation, completed bounded LN1-e evidence and open LN2/LN3/LN4/GPU gates. COST/DECISION/TRANSFORM remain none. |
| Mapped `.B`/`.T` | Native source preservation and independently inspected declarations; not execution results. |

All `.log` reports start with `# What it summarizes:`. Reject missing, duplicate,
foreign or reordered target gate records; missing/extra invocations; changed
source identity/domain/epsilon/affine mapping; stale products; changed checker;
request/input drift; changed environment or numerical state; and incomplete
case coverage. Output directories must be fresh. Synthetic unit traces never
replace actual native trace or mapped-image evidence.

Retain exact commands/streams, source snapshots, native and host tools, test
dependencies, hashes, receipts and source-interleaved images under the existing
immutable review hierarchy. Reopen from final paths before updating indexes.
Keep raw `.t` and ASCII `.T` in different directories on case-insensitive hosts.

## 6. Validation And Next Work

The bounded suite passes 28 native cases, 36 version-2 source contracts, four v1
refusals, 112 target queries, 144 full-tensor invocations, 464 rows and 375,360
state comparisons. All 56 source checkpoints have equal paired full dumps.
The 13 new join tests, 14 independent oracle tests and 58 existing trace tests
pass. Every numerical case also passes the retained UBSan checker with an
identical raw trace. Four rebuilt legacy target fixtures reproduce the prior
images/dumps/traces, and four historical v1 images reopen unchanged.

The backend, WOPT and reader tool hashes match the authenticated prior receipt;
only the test producers are rebuilt. Final-path mapped and numerical replay is
a separate retained verification step, not a claim of a fresh compiler rebuild.

Next is LN2: derive legal C0/C1/C2 ownership, alias, storage, access-completion,
visibility, reuse-generation, tail and runtime-guard contracts from bound v2
source. LN3 must cost complete comparable plans before selection. LN4 must
materialize the selected operations/control/storage in WHIRL and obtain CUDA
only from that image. No emitter-invented arithmetic or storage decision is
permitted. GPU correctness/resources/timing remain a separate certification
gate and do not block continued compiler-side development.
