# H1b immutable GPU resource and profile import v1.0

2026-11-12 | Native import and driver lifetime contract

H1b extends [H1a device admission](AI-COMPILER-H1A-GPU-DEVICE-SELECTION-v1.0.md)
under [H20 plan v1.1](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.1.md)
and [resource/memory design v0.3](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.3.md).
It resolves explicit h20_96gb/h20_141gb identities and optional pinned external
resource, execution and budget data into a driver-owned immutable context.
H1c planner/snapshot/cache bindings and H1d product/serving qualification remain
open. No collected H20 inventory, calibrated rate or GPU result is supplied
by this implementation or its synthetic test fixtures.

## Options and compatibility

The existing backend DSL option group adds:

~~~text
-DSL:gpu_profile=profile.json
-DSL:gpu_profile_sha256=<64-lowercase-hex-digest-of-exact-file-bytes>
-DSL:gpu_profile_toolchain=<declared-target-toolchain-id>
~~~

All three controls are required together for a file import, along with an
explicit gpu_device. The toolchain argument states the intended target binding;
it is compared exactly with the profile rather than discovered from the host.
The standard top-level backend forwarding prefix remains -Wb,.

Omitting both device and import options is a strict no-op. Device-only requests
create a sealed context with explicitly unknown resources, execution and budget,
while retaining H1a's exact identity-only report. Orphan companions, empty
inputs, incompatible devices/ISAs, wrong digest and mismatched toolchain refuse
before source loading, optimizer invocation, trace-file creation or checkpoint
publication. H1a's conservative sm_90-only device admission is unchanged;
architecture-only sm_90a remains available without an import.

## Versioned external schema

The root is an exact JSON object with these nine members; additional, missing
or duplicate members are rejected at every schema object boundary:

| Member | Contract |
| --- | --- |
| schema | Exact open64.gpu-device-profile.v1 |
| device | Exact requested h20_96gb or h20_141gb |
| architecture | Exact admitted sm_90 |
| device_revision | Exact H1a registry revision 1, encoded as an unsigned integer |
| contract | Exact open64.gpu-profile.sm90.v1; this import contract grants no additional instruction mechanism |
| toolchain | Nonempty declared target identity matching gpu_profile_toolchain |
| resources | Exact resource inventory object described below |
| execution | Explicit null when unbound, otherwise the execution object below |
| budget | Independent workload/serving allocation-budget object below |

Identifiers are bounded nonempty ASCII tokens using letters, digits and
underscore, dash, dot, colon, slash, plus or at-sign. They cannot hide a NUL or
control byte. Revision fields are positive uint32 generations. Optional
observation/condition metadata uses explicit null for unknown. Observation
strings use whole-second UTC spelling with bounded month/day/time fields;
the importer does not verify clock truth, chronology or freshness.

### Resource inventory

The resources object has exactly id, revision, device, architecture,
partition_kind, partition_id, inventory_id, observed_at and values.
partition_kind is full or mig. Device and ISA repeat the root binding;
partition and inventory identities are explicit rather than inferred from
memory size. The physical device capacity and visible partition capacity are
separate quantities. All fourteen fields below must appear in values, even
when their fact is explicitly unknown.

| Fields | Unit and scope |
| --- | --- |
| visible_sms, warp_lanes | count, visible device SMs and lanes per warp respectively |
| physical_hbm_bytes, visible_hbm_bytes, l2_bytes | bytes, physical board and visible partition/cache scopes as named |
| registers_per_sm, register_quantum | registers, per-SM capacity and allocation quantum |
| shared_bytes_per_sm, shared_bytes_per_cta_default, shared_bytes_per_cta_optin, shared_quantum | bytes, separate per-SM/per-CTA limits and allocation quantum |
| max_threads_per_sm, max_threads_per_cta, max_blocks_per_sm | count, scopes as named |

Known resource bounds must be positive uint64 values. Visible memory cannot
exceed physical memory; CTA default shared memory cannot exceed opt-in shared
memory, which cannot exceed per-SM capacity; CTA threads cannot exceed per-SM
threads. These relationships use conservative independent bounds. Correlated
uncertainty is not represented by this schema; refine the bounds or retain an
explicit unknown instead of assuming an unproved relationship.

### Execution evidence

An execution object has exactly id, revision, device, architecture, resource_id,
resource_revision, partition_id, toolchain, model, observed_at, conditions and
values. Device/ISA/toolchain must match the request, and resource generation
and partition must match the resource object. model is an evidence/model
identity; recognition by a particular optimizer remains an H1c obligation.
conditions has exactly clock, power, ecc, cache and workload, each an identifier
or null. All fourteen execution value fields must appear:

| Fields | Unit |
| --- | --- |
| hbm_read_rate, hbm_write_rate, hbm_mixed_rate, l2_rate | bytes/second at the visible device/partition scope |
| shared_rate_per_sm | bytes/second/SM |
| simt_rate_per_sm, dequant_rate_per_sm | operations/second/SM |
| memory_startup_ns, add_dependent_ns, mul_dependent_ns, exp_dependent_ns, div_dependent_ns, launch_ns, async_startup_ns | nanoseconds; startup and dependent latency remain distinct from byte/operation service |

Rates require finite strictly positive bounds. Latencies require finite
nonnegative bounds; known zero latency remains different from unknown.
No value is converted into the old synthetic reference_cycles unit. Declared
measured facts require all five operating-condition identities and an
observation string; estimates/synthetic experiments may explicitly leave those
conditions unknown. No measurement is inferred from an architecture spelling.

### Fact records and provenance

Every resource, execution or budget fact has exactly lower, value, upper, unit,
provenance and evidence. Unknown is represented by three null numeric members,
provenance=unknown and evidence=""; its unit is still explicit. A known fact
requires ordered bounds and a bounded nonempty evidence identity.

Resource/budget provenance accepts queried, documented, estimated or synthetic;
queried/documented bounds must be exact. Queried resources additionally require
an observation string. Execution provenance accepts measured, estimated or
synthetic. A known number cannot carry unknown provenance. Boolean/floating
resource capacities, overflowed integers, wrong units/scopes, nonfinite rates,
unknown provenance and contradictory bound declarations are rejected.

### Workload budget

The budget object has exactly id, revision, resource_id, resource_revision,
partition_id, limit_bytes, reserved_bytes and usable_bytes. The final three
members are byte facts; known zero is allowed. The resource generation and
partition join must match. Known budgets cannot exceed the conservative
visible capacity; reserved plus usable cannot overflow or exceed a known
limit. Budget_bounds_proved is true only when capacity and all three budget
facts establish these relationships. It establishes structural bounds, not that
a program's allocations fit or that a future allocation is guaranteed.

## Integrity and applicability

The importer reads at most 256 KiB once, checks exact file bytes against the
externally supplied SHA-256, and parses with the repository RapidJSON iterative,
UTF-8-validating parser. Raw NUL, trailing documents and malformed JSON refuse.
No free-form JSON members are silently retained or ignored.

The digest pin establishes content identity and integrity relative to the
request. It is not a signature, proof of physical device identity or attestation
that declared measurements are truthful. H0/H7 and the trusted inventory/test
producer establish that evidence separately. In particular, copying H200 values
and relabelling them cannot become qualified H20 evidence merely by hashing
the file. Explicit foreign device/resource bindings are rejected; physical
facts are never inferred from the 96gb/141gb labels or borrowed from other GPUs.

## Ownership and dependency keys

common/targ_info/access/dsl_device_profile.h owns policy-free typed facts and
the sealed DSL_GPU_DEVICE_CONTEXT. Its strings and fixed arrays are owned;
clients receive const access only. Contexts and owners are noncopyable. A
C++98-compatible DSL_GPU_DEVICE_CONTEXT_OWNER matches the production backend
language baseline. Failed imports preserve its previous owner and all its
facts; successful replacement expires previous borrowed views.

be/com/dsl_gpu_profile.h/.cxx owns bounded parsing, structural/binding checks
and private construction. It reuses the existing dependency-free byte-hash
utility under its legacy VHO_FHE_SHA256 name; it copies no hash algorithm and
introduces no encrypted runtime, new shared library or common-to-backend link.
The driver owns resolution before source/phase activity and releases the
context during ordinary cleanup. No WN/ST/TY or live JSON/file handle survives
inside the context.

| Key | Dependency boundary |
| --- | --- |
| Input_sha256 | Exact externally pinned file bytes, or none for device-only input |
| Resource_sha256 | Device/ISA/registry identity, toolchain and serialized resource section |
| Execution_sha256 | Resource key and execution section; none when execution is unbound |
| Budget_sha256 | Resource key and budget section |
| Context_sha256 | Complete pinned import and all section keys, or the identity-only context |

Section serialization retains member order. Reordering may conservatively
invalidate a key; these are content bindings, not semantic-equivalence claims.
Changing only execution data preserves resource/budget keys. Changing only
budget data preserves resource/execution keys. H1c will use these dependencies
for cache admission, selected-plan revalidation and cost/bound invalidation;
this milestone provides the keys without implementing planner policy.

## Two-phase search and fallback

No optimizer, loop transformation, native format, numerical/Q3 rule or COMMENT
generator is changed. Import occurs once before exploration. No candidate reads
a profile file, probes hardware, mutates WN/ST/TY, compiles or runs correctness/
timing tests. Architecture-only and H1a routes retain their old behavior.

Imported contexts are reported with their identities, section digests, known
fact counts and structural budget flag, alongside
planning=generic-architecture-fallback and consumer_bindings=pending. This is
the existing generic route, not a claim that it has been selected or qualified
against imported resources. H1c/H1d must check actual product footprints,
resource/model applicability and runtime obligations before device-specific
selection or execution acceptance. A malformed explicit import diagnoses
before publication rather than silently substituting a synthetic description.

## Focused validation

The native contract runner exercises both variants, resource-only and partial/
unknown snapshots, MIG bindings, declared-measured metadata and fractional
latencies. Negative cases cover schema/version/field/type/unit/bound errors,
duplicate members, wrong H200/variant/ISA/toolchain/resource/partition joins,
missing conditions/evidence, digest mismatch, input size/encoding errors and
budget arithmetic overflow. It proves independent ownership after file changes,
failure preservation and distinct resource/execution/budget dependency keys.

The native suite passes 97 cases (22 accepted, 75 refused), including host
ASan/UBSan validation. The actual backend review passes 16 accepted cases and
22 early refusals. Its runner reopens genuine O0/O3 mapped outputs with -st and -src
and compares them byte-for-byte with the generic route. Early CLI refusals use
absent source input and preserve an existing destination. Existing H1a admission
and estimation-only search tests, sanitizer runs, the backend rebuild and shared
consumer/dependency checks complete the focused validation. None is a GPU run
or an H20 calibration result.

~~~sh
python3 osprey/be/com/tests/run_gpu_profile_contract.py --output-dir /fresh/native-review
python3 osprey/be/vho/tests/run_gpu_profile_review.py \
  --build-dir /configured/build --output-dir /fresh/backend-review
~~~

The backend runner needs the existing native Python builder on PYTHONPATH and
the configured be, wopt and ir_b2a products with their library paths. Profiles
generated by gpu_profile_fixtures.py are deliberately synthetic sentinels;
they are not recommended production hardware settings.
