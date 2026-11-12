# Read-only High search observer contract v1.0

November 7, 2026 | M2c, bounded native common-High MLA observer

The LNO observer consumes the owned M2b snapshot immediately after capture and
before its mutation epoch expires at Hoist_Varying_Lower_Bounds. It validates a
complete external source/resource/model binding, invokes the existing detached
MLA portfolio, reports a scoped estimated winner, and continues ordinary LNO.
It never applies that winner. M3 production controls and M4 selected schedule
application, legality, rollback and analysis rebuild remain separate gates.

## Scope, ownership and controls

The existing partition_axis opt-in, effective O3 and enabled-LNO/opt-level gates
remain in Lnoptimizer. MP/Lego/autopar and unsupported scopes retain M2a/M2b
behavior. The scope is the driver's current FUNC_ENTRY; there is no PU traversal
or context switch. Valid trace-off inputs still execute the read-only observer.
An unset `OPEN64_HIGH_SEARCH_BINDINGS` skips without reading an external file.
The environment variable names an explicit offline binding document; it is a
bounded M2c input adapter, not a new compiler-option spelling or production
configuration service. M3 still owns production controls and portfolio budgets.

When a document is present, the observer requires the effective compiler
Roundoff_Level to admit reassociation and the manifest to explicitly allow it.
The exact IEEE, roundoff and fast_math settings are bound. This extension to
M2b's numerical context is transient and changes no binary WHIRL field. It
grants no schedule legality or permission beyond the recorded compiler scope.

The native wrapper reads lineage/ABI, checks epoch/context, collects physical
state, and invokes services receiving only owned records. Missing, malformed,
stale, unsupported or inconsistent bindings produce an explicit SKIP reason
and ordinary LNO continues. A failed isolation invariant is a compiler defect:
it aborts rather than pretending potentially corrupted state is a safe fallback.

## Complete bindings

The JSON schema is `open64.lno-high-search.v1`, with exactly these 12 fields:

| Field | Validation |
| --- | --- |
| schema | Exact supported version |
| unit, function | Exact active source unit/function identity |
| source_key, facts_key | Complete M2b canonical source and analysis keys; never just the FNV display namespace |
| numeric, host_target | Exact current native context strings; GPU capacities are supplied separately |
| allow_reassociate | Explicit boolean true plus effective compiler permission |
| algorithm | Complete `open64.high-algorithm.v1` semantic source recipe |
| buffers | Complete ordered canonical storage census, with no missing/extra/reordered storage |
| gpu | Existing `open64.mla-gpu-profile.v1` capacities/policy and explicit provenance |
| model | Existing `open64.mla-cost-profile.v1`, exact reference_cycles unit, provenance and all timing parameters |

The algorithm schema has source_pu, source_file, opt_level, threads, values and
operations, plus its schema identifier. Value rows preserve name/dtype/shape,
role, ordinal and storage. Operations preserve name/version/source-node/line,
inputs, output and all attributes. Derived launch/kernel spellings and the old
standalone JSON checksum are not authoritative source facts in this schema.
The existing typed source validator checks shapes, dataflow, schemas, aliases,
masks and numerical guards; the closed MLA pattern checker checks the full
absorbed graph before estimation. This remains a bounded MLA observer, not a
generic scheduler for arbitrary common-High programs.

The High producer now writes `tensor.high.recipe_key`, the complete deterministic
typed semantic recipe key, through the existing ST metadata service. The owning
materializer creates this witness from the same validated recipe used to build
the source algorithm. LNO compares the external recipe against that native
producer witness, then verifies the actual formal count, storage order, value
parameter convention, restrict pointer qualification and bool/F32 element types.
The exact source/fact keys additionally bind the current canonical algorithm
and analyses. Shapes are not inferred from names or COMMENT strings.

Older High images without this new witness remain valid compiler inputs; they
skip search. The witness and equality keys are compiler provenance/consistency
checks, not cryptographic authentication of arbitrary edited input images.
They do not substitute for M4's proof that a proposed schedule transforms the
actual source algorithm legally. No new WN opcode, type kind, metadata container,
ELF section, image layout or reader/writer behavior is introduced.

GPU admission reuses the existing architecture and rounded-occupancy validators.
SM count, register demand estimate, CTA target, scratch limit and every resource
field are explicit; no rate, SM count or instruction latency is inferred from
an architecture name. The model architecture must equal the GPU architecture;
all 12 bounded positive timing/rate inputs, unit and model provenance are
required. Complete resource/model keys include the field values, not just IDs.
Same-ID changes therefore recompute estimates rather than reusing stale costs.
The example envelope is synthetic and is not a measured GPU calibration.

The repository's header-only RapidJSON adapter rejects duplicate/unknown keys,
unsupported schemas/units, type coercions, invalid UTF-8, embedded NULs and extra
roots. File size is bounded to 16 MiB; iterative parsing avoids deep JSON parser
recursion, followed by depth/node and typed array/field limits. JSON, filesystem
and allocation services in this adapter operate only on ordinary owned data;
there is no JsonCpp linkage or other new library dependency in be.so.

## Detached planning and exploration

`tensor_schedule.cxx` separates the existing validator, attention recognition,
logical scheduling, grid and shared-memory accounting from native materialization
in tensor_kernel_lower.cxx. Both the standalone tool and LNO link this one
implementation. Native materialization uses the same boundary predicate. The
existing chunk selector, rounded occupancy, checked arithmetic, estimator,
proposal order and shared estimate cache are reused, not reimplemented in LNO.

For the detached admission seed, the observer sets width 128 so the shared
validator can admit long attention without treating it as an unsplit device
implementation. Every candidate evaluation overwrites this seed width; it is
neither a selected width nor a source transformation. The source High recipe
and its witness remain unoptimized.

The existing finite 12-entry portfolio is used with its default three strategies
and complete enumeration. Each input estimates each candidate once, with 24
cache hits across the remaining strategy visits. The observer reports the
candidate census, resource/model keys and `best_estimated_in_declared_space`.
It publishes no executable selected-plan handoff and reports applied=false and
schedule_legality=not_proved. This is no global GPU optimum claim.

## Isolation evidence and accounting

Five detached objects form the audited dependency boundary: the observer,
JSON adapter, tensor planner, chunk selector and attention search. They include
no native compiler context definitions. Their unresolved symbol audit rejects
WN/ST/TY construction, table/context access, native preparation/materialization,
image emission and process-launch entry points. The only existing external
hardware-description lookup is a read-only record-copy service. Reviewing the
actual dependency closure excludes transient unattached WN creation as well
as changes visible in a final tree; no fictitious allocation counter is used.

The native wrapper additionally copies the live physical tree bytes (including
node links, source positions and kid pointers), all active/global ST rows, and
all TY rows/counts around binding validation and exploration. It revalidates
source/access/dependence/DU/alias facts before collecting the final state.
An unchanged final artifact alone is not considered sufficient proof.
Candidate records contain no native handles. No candidate executes a backend,
regression test, native lowering, C/CUDA/PTX emission or binary-image writer.
The ordinary driver still publishes its normal LNO output once per invocation.

ACCOUNTING separates host snapshot collection, binding validation, exploration,
and isolation/revalidation time. Observer total excludes the separately charged
snapshot. Monotonic host microseconds are not GPU timing and are not mixed with
the model's estimated reference-cycle units.

## Focused acceptance and retained limitation

`tests/run_high_observer_review.py` drives two real absorbed MLA source images
through the normal backend. It compares every candidate and the winner with an
independent standalone entry to the same estimator. It verifies repeated
results, on/off/default/trace-off, lower levels, disabled LNO and explicit
numerical permission. Model-only and resource-only updates change complete
binding keys and estimates while preserving native state and executable meaning.

Twenty-eight refusal cases cover missing/stale/wrong-context/producer/ABI data,
invalid source shapes, resource and model errors, missing timings, wrong units,
unknown versions/fields, numeric coercions, duplicate fields, NULs, deep/multiple
roots and an unavailable file. Older images without the producer witness skip
without changing the ordinary compilation route. Native-state guards and zero
candidate artifact checks cover success and guarded refusal.

The strict common-High compatibility suite still compares output bytes exactly
against M2b and passes its 21 host/source-reference executions and eight form
cases. The existing 38 tensor/search tests, 21 snapshot check kinds/lifecycle
and 13 native partition cases remain impact-selected regression evidence.

With reassociation enabled, observer/trace/profile runs expose an existing
Depgraph_Write behavior: it writes the native DEP_STRUCT including trailing
padding. On the verified Linux x86_64 ABI, DEP is two bytes, Is_Must is one byte,
and the four-byte structure has one nonsemantic trailing byte. Some .N images
therefore differ in raw padding despite identical source/tables/analyses and
identical ir_b2a -st -src traces. The compiler writer is not changed in M2c.

`high_observer_image_parity.py` validates the producing component's IMAGE_ABI,
follows actual ELF and PU subsection headers and the existing graph writer,
and accepts differences only at those individually identified trailing padding
bytes. Every meaningful byte, including graph distances/directions and Is_Must,
is compared exactly. Raw differences/offsets/values and full trace diffs are
retained. A deliberate edit to Is_Must must fail the auditor. Unsupported ABI,
format, bounds or any other image difference fails closed. This is a test-side
classification of an existing defect, not rewritten or normalized WHIRL.

The pre-existing long post-LNO CG graph reopen failure also remains an M6
prerequisite, and is not counted as successful long post-LNO execution. No GPU
execution, resource measurement or performance improvement is claimed here.

## Reproduction and next gates

Prepare a fresh unoptimized common-High artifact with tensor_kernel_lower high.
Run its ordinary backend once with partition_axis/AI trace enabled at O3 and the
intended numerical settings; that trace supplies the complete snapshot keys.
Construct the versioned binding using its native producer manifest, full keys,
explicit GPU profile, explicit timing model and numerical permission. Set
OPEN64_HIGH_SEARCH_BINDINGS to that file and rerun the same source/context.
The focused runner and retained short/long binding files provide executable
examples without Python being a backend runtime dependency. Use fresh output
paths and retain both source images and all diagnostics.

M3 now follows: production option/profile loading, provider controls, versioned
global budgets and portfolio/convergence service. M4 must validate a selected
plan against fresh facts, prove legality, apply atomically, rebuild analyses and
protect the selected schedule from later competing LNO choices. M5 must verify
High exit before Mid memory lowering, Low GPU mapping and PTX emission. General
VHO routing, new schedule providers, native GPU acceptance and transfer acceptance
remain open. Owners, successor, independent reviewer and approving lead are TBD.
