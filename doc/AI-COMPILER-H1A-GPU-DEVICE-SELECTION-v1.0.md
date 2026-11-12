# H1a GPU device selection v1.0

2026-11-12 | Native option and identity registry implementation

H1a implements the exact H20 device-selection and baseline ISA-admission
boundary from [H20 plan v1.1](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.1.md)
and [resource/memory design v0.3](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.3.md).
It supplies device identity only. H1b resource/profile import, H1c planner and
snapshot bindings, and H1d resource/serving qualification remain open. No
hardware capacity, calibrated service or GPU execution is established here.

## Options and admission

The existing backend DSL option group now accepts exact values:

~~~text
-DSL:gpu_device=h20_96gb
-DSL:gpu_device=h20_141gb
~~~

The established top-level backend forwarding spelling is
`-Wb,-DSL:gpu_device=h20_96gb` (or h20_141gb). This milestone's process tests
exercise the actual backend parser directly; the configured test build has no
top-level opencc executable. No new frontend option-processing path is added.

| Request | H1a behavior |
| --- | --- |
| No gpu_device | Backend admission is a strict no-op; old architecture-only defaults, diagnostics, selection and fallback remain unchanged |
| h20_96gb or h20_141gb, no gpu_arch | Resolve the generic baseline sm_90 before source loading and existing target-option checks |
| Either H20 key with explicit sm_90 | Admit exact device/ISA identity; continue the existing generic architecture route |
| Either H20 key with sm_90a | Refuse pending separate device-specific instruction qualification; no claim that the physical board lacks the mechanism |
| Either H20 key with sm_100/sm_100a | Refuse the non-admitted device/ISA combination |
| Unknown device, case/extra suffix/empty value, or m90/sm_90typo ISA | Diagnose before input loading, optimizer invocation or output publication |

The conservative sm_90-only device admission is deliberate. Omitted-device
sm_90a behavior remains available exactly as before. A future expansion of
device/ISA admission needs independent qualification; registering an option
does not authorize a new MMA or transfer mechanism.

For a successful explicit device request the backend emits one independent
identity report, outside the existing AI_OPT trace schema:

~~~text
DSL_GPU_DEVICE 1 device=h20_96gb device_revision=1 arch=sm_90 planning=generic-architecture-fallback device_resources=unbound device_calibration=unbound
~~~

This records why the existing generic route is used. It cannot be interpreted
as an H20-calibrated plan or a discovered resource envelope. The old synthetic
architecture table remains unchanged; no H100/H200 quantities or nominal HBM
byte counts are inserted into the device identity registry.

## Native records and ownership

- common/util/config_dsl.h/.cxx owns the optional process-level
  VHO_DSL_GPU_Device storage and shared option descriptor. NULL means omitted.
- common/targ_info/access/dsl_memory_hierarchy.h/.cxx owns two immutable
  DSL_HARDWARE_DEVICE_RECORD rows, revision 1, with distinct stable device IDs,
  exact names and baseline architecture identity. No existing enum values,
  description layouts, quantitative rows or mapped-image contracts change.
- DSL_hardware_device_find copies an exact record with borrowed static names.
  Invalid input preserves the caller's output.
- DSL_hardware_target_resolve returns caller-owned
  DSL_HARDWARE_TARGET_SELECTION identity data. It checks names and baseline
  admission atomically, publishing no partial result on failure. Its optional
  diagnostics explain unsupported device, unknown ISA and non-admitted pair
  failures. It has no options, WN, scope traversal, probe or profitability policy.
- The backend driver validates an explicit request immediately after normal
  command-line processing. For device-only input it fills the absent effective
  gpu_arch with the borrowed canonical baseline name before existing checks
  such as LN4's target requirement. Other paired-option, O-level, scope and
  checkpoint guards remain active.

Device selection is process-option validation at every optimization level,
not an optimization pass. It does not widen PU/REGION scope, change canonical
forms, alter tensor traits or mutate native analyses. The driver still owns
phase activation, native lifetimes and output publication. The registry is
policy-free and adds no library, GPU runtime or frontend dependency to be.so.

## Search and fallback

No cost or scheduling code is changed. Identity resolution precedes any search;
the selected generic algorithm, fallback and COMMENT contents are unchanged.
The two-phase explorer still uses detached immutable inputs and performs no
candidate WN/ST/TY construction, compilation, profiling or correctness trial.
H1b/H1c must bind real resources, profiles and budgets before claiming
device-specific selection. H1a must not be mistaken for complete H1.

## Focused evidence and reproduction

The production C++ registry contract test executes 72 assertions on macOS and
Linux: exact names, distinct identities, inferred/explicit baseline, invalid
values/ISA pairs, preserved output on refusal and unchanged architecture rows.

The Linux backend review covers seven accepted O3 cases, three O0 fallback
cases and fourteen early refusals, including both H20 variants, device-only
selection, explicit sm_90 and the old architecture-only sm_90a route. It retains
genuine selected B/T artifacts, independently reopened with -st and -src.
Device-only and explicit sm_90 outputs match the generic selected image and
dump byte for byte. O0 fallback images are byte-identical. Invalid requests
publish no checkpoint and preserve an existing destination in the tested case.

The rebuilt be/be.so passes shared-consumer closure checks without a change in
DT_NEEDED libraries. Two existing native MLA tests pass for estimation-only
deterministic portfolios and permission/optimization boundaries. A separate
pre-change backend comparison confirms byte-identical generic selected B/T.
These are compiler/host checks, not device execution or performance results.

With the configured native Python binding on PYTHONPATH, run:

~~~sh
python3 osprey/be/vho/tests/run_gpu_device_review.py \
  --build-dir /path/to/configured-build \
  --output-dir /path/to/fresh-review-directory
~~~

The script requires the configured be, wopt and ir_b2a tools and appropriate
shared-library paths. It compiles the actual registry implementation, creates
a genuine small matmul input through the existing native builder, and retains
commands, diagnostics, mapped outputs and checksums. It refuses an existing
review directory. The separate search regressions require the existing
PyTorch-enabled test environment.
