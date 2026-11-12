# Compiler user guide: bounded High-WHIRL search

This guide covers the experimental `-LNO:high_search` interface in PR #242.
It searches the supported MLA family/width space using estimates and reports
the best eligible estimate. It leaves native code unchanged and continues
ordinary LNO. It does not apply a winning schedule or measure GPU performance.
The fixed space currently contains 12 descriptors; arbitrary source programs
and additional search providers are outside this interface.

For the detailed semantic contract, see
[M3 search controls](AI-COMPILER-SEARCH-M3-CONTROLS-v1.0.md).

## Availability and prerequisites

Use a compiler containing both the new backend option table and LNO observer.
The explicit route needs effective `-O3`, enabled LNO, and an admitted common
High-WHIRL MLA procedure with its complete producer manifest. It uses normal
PREOPT/LNO analysis. A raw very-high-level DSL image is not yet an admitted
common-High input. O0/O1/O2 and disabled/excluded LNO retain their normal paths.

The current estimator requires permission to reassociate source arithmetic.
The reference commands below explicitly set `-OPT:IEEE_arith=3:roundoff=2` and
the generated binding declares `allow_reassociate=true`. Use them only when
that permission matches the source's numerical contract. Keep strict numerical
options for strict-FP input; search will refuse an incompatible request.

A binding must match the exact source, current analysis snapshot, numerical
options, producer ABI, GPU resources and cost model. There is no general frontend
switch that automatically creates such a binding for arbitrary WHIRL. The
reference recipe below uses the existing native producer manifest and snapshot
adapter. Regenerate a binding after changing the input or relevant options.

## Enable, disable and legacy behavior

Add these options to an otherwise admitted compiler invocation:

```text
-LNO:high_search=on:high_search_bindings=/absolute/path/search.v2.json
```

`high_search=off` explicitly disables both the production and legacy environment
routes. A binding path by itself does not opt in. When `high_search` is omitted,
the old `-DSL:partition_axis=on` plus `OPEN64_HIGH_SEARCH_BINDINGS` adapter remains
available; it accepts its existing v1 contract and can also receive v2.
Explicit `on` reads the command-line binding path and requires v2; it does not
use the environment binding. These options are file-scoped, not pragma controls.

## Defaults and budget accounting

Search is opt-in. The following values are the internal default policy and the
complete example template, not permission to omit required JSON fields.

| Setting | Default | Accepted values / meaning |
| --- | --- | --- |
| `global_proposal_limit` | 36 | 1..192 candidate visits across all runs |
| `global_unique_limit` | 12 | 1..12 first evaluations, including resource rejection |
| `global_time_limit_us` | 0 | 0 disables; otherwise up to 3,600,000,000 microseconds |
| `runs` | local, random, exhaustive | 1..16 ordered runs; supported strategies are those three |
| Each run's `seed` | 17 | 0..4,294,967,295 |
| Each run's `proposal_limit` | 12 | 1..12; also limited by the global caps |
| `stopping` | exhausted, budget | Hard caps and complete coverage always apply |

"Global" covers one bound search invocation for the active PU, shared across
its strategy runs. It does not cap a whole compilation, process or collection
of compiler jobs. Runs interleave one proposal at a time and have independent
seeds, visited sets and incumbents, with a shared cache. A first visit charges
one proposal and one unique evaluation. A later run's cached visit charges
one proposal and one cache hit. Rejected candidates still consume their first
evaluation. Reaching the unique cap stops the entire portfolio, including
future cached visits. Reaching all 12 descriptors completes the declared space;
36 is a maximum, not a required number of visits.

The deadline measures search-service time, including detached setup, proposals,
cache lookups and estimates. It excludes native snapshot collection, document
decoding/binding validation, isolation recapture and trace IO. It is cooperative:
one in-flight estimate plus bookkeeping can overrun it. It does not bound total
compilation time. Fixed seeds without a live deadline give reproducible event
trajectories; elapsed host time can vary.

The full 15-field template is
[default-controls.json](examples/high-search/default-controls.json):

```json
{
  "schema": "open64.mla-search-controls.v2",
  "space": "mla-family-width-space-v1",
  "mode": "estimate",
  "global_proposal_limit": 36,
  "global_unique_limit": 12,
  "global_time_limit_us": 0,
  "runs": [
    {"id": "local", "strategy": "local", "seed": 17, "proposal_limit": 12},
    {"id": "random", "strategy": "random", "seed": 17, "proposal_limit": 12},
    {"id": "exhaustive", "strategy": "exhaustive", "seed": 17, "proposal_limit": 12}
  ],
  "stopping": ["exhausted", "budget"],
  "patience": 12,
  "minimum_gain_cycles": 0,
  "target_cycles": 0,
  "gap_ppm": 10000,
  "remaining_invocations": 0,
  "evaluation_cost_cycles": 0,
  "economic_conversion": null
}
```

All fields must be present. Integers must be unsigned JSON integers, not strings,
fractions or booleans. Unknown/duplicate fields, unknown strategies and `apply`
mode are refused. The complete binding also requires `schema`, `unit`,
`function`, `source_key`, `facts_key`, `numeric`, `host_target`,
`allow_reassociate`, `algorithm`, `buffers`, `gpu`, `model` and `search_controls`.
The generator below writes all 13 root fields with actual snapshot identities;
the controls template alone is not a complete binding.

## Reproducible complete configuration

Run this recipe in Linux with a matching backend/LNO toolchain. Python 3 is
used only to prepare JSON; the compiler search itself has no Python dependency.
Set the following environment variables to existing producer/tool paths. Paths
must be absolute and visible in the same filesystem namespace as the compiler.

| Variable | Required input |
| --- | --- |
| `OPEN64_SRC` | Root of this source checkout |
| `OPEN64_TOOLS` | Tool tree containing `be/be`, `be/be.so`, `lno/lno.so`, `lno/ipl.so`, `wopt/wopt.so`, `ir_tools/ir_b2a` |
| `HIGH_INPUT` | Admitted native common-High binary WHIRL input |
| `HIGH_MANIFEST` | That input's producer `.high.json` manifest, including `recipe`, `function` and `buffers` |
| `HIGH_SOURCE` | Source pathname recorded by the producer, visible for source interleaving |
| `HIGH_OUT` | New output directory; the recipe retains all its evidence |

For the supplied short MLA fixture, use the authenticated M3 packet's
`controls/short.input.B` and `observer/short.high.json`. The original source path
is `/src/osprey/torch2whirl/python/tests/models/absorbed_mla.py`; mount the source
checkout at `/src` in Docker. The packet stores current backend/LNO tools and
compatible WOPT tools in separate trees; its `replay.py` shows how to construct
a matching tool view. See the review artifact index for the full immutable
receipt path; these artifacts are local review assets, not installed samples.

For new producer output, use that producer's matching `.B`, manifest and source.
Preserve the complete manifest. Do not hand-edit `source_key`, `facts_key`,
buffer ordinals or producer recipe data to bypass admission.

The bundled GPU profile and cycle model below are explicitly review/synthetic
inputs. Replace them with supported profiles carrying your actual provenance
when interpreting estimates for a target. The example does not calibrate them.

<!-- example: setup -->
```bash
set -euo pipefail
: "${OPEN64_SRC:?}" "${OPEN64_TOOLS:?}" "${HIGH_INPUT:?}"
: "${HIGH_MANIFEST:?}" "${HIGH_SOURCE:?}" "${HIGH_OUT:?}"
mkdir "$HIGH_OUT"
export LD_LIBRARY_PATH="$OPEN64_TOOLS/be:$OPEN64_TOOLS/lno:$OPEN64_TOOLS/wopt${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
unset OPEN64_HIGH_SEARCH_BINDINGS

# Run one native PU through LNO; keep each invocation's output and diagnostics.
run_high_search() {
    local label="$1"
    shift
    "$OPEN64_TOOLS/be/be" "-fB,$HIGH_INPUT" \
        "-fo,$HIGH_OUT/$label.o" "-ft,$HIGH_OUT/$label.phase.t" \
        "-PHASE:l=on:w=off:c=off:lpath=$OPEN64_TOOLS/lno:wpath=$OPEN64_TOOLS/wopt" \
        -O3 -OPT:IEEE_arith=3:roundoff=2 -DSL:ai_opt_trace=on -tr25 -tt25:1 \
        "$@" "$HIGH_SOURCE" \
        > "$HIGH_OUT/$label.stdout" 2> "$HIGH_OUT/$label.stderr"
}
```

First collect the current snapshot without running search. This is the legacy
snapshot observer opt-in with its environment binding unset. It records SOURCE,
FACTS and numerical/context joins for the same input and options used next.

<!-- example: snapshot -->
```bash
run_high_search snapshot -DSL:partition_axis=on
```

Generate the complete production binding. The documentation adapter reuses the
repository reference binding service; native parsing and admission still decide
whether the result is valid. It refuses to overwrite an existing output file.
Generation errors such as duplicate JSON members, missing snapshot records or
an existing output path stop the Python command; compiler fallback diagnostics
below apply once a generated binding is supplied to the compiler.

<!-- example: binding -->
```bash
python3 "$OPEN64_SRC/doc/examples/high-search/make_binding.py" \
    --manifest "$HIGH_MANIFEST" --snapshot "$HIGH_OUT/snapshot.phase.t" \
    --gpu "$OPEN64_SRC/osprey/be/vho/tests/mla_gpu_profile_sm90_review.json" \
    --model "$OPEN64_SRC/osprey/be/vho/tests/mla_cost_profile_sm90_synthetic.json" \
    --controls "$OPEN64_SRC/doc/examples/high-search/default-controls.json" \
    --output "$HIGH_OUT/search.v2.json"
run_high_search default \
    "-LNO:high_search=on:high_search_bindings=$HIGH_OUT/search.v2.json"
```

This inspection invocation stops after LNO (`w=off:c=off`); it retains a normal
`.N` WHIRL image, not an executable object. Inspect the input and output in
separate processes with matching source paths and preserve the raw text diff.
Copying `.N` to `.B` below changes only the filename of the binary WHIRL image.

<!-- example: inspect -->
```bash
cp "$HIGH_INPUT" "$HIGH_OUT/input.B"
cp "$HIGH_OUT/default.N" "$HIGH_OUT/default.B"
"$OPEN64_TOOLS/ir_tools/ir_b2a" -st -src "$HIGH_OUT/input.B" "$HIGH_OUT/input.T"
"$OPEN64_TOOLS/ir_tools/ir_b2a" -st -src "$HIGH_OUT/default.B" "$HIGH_OUT/default.T"
diff -u "$HIGH_OUT/input.T" "$HIGH_OUT/default.T" \
    > "$HIGH_OUT/input-default.diff" || test "$?" -eq 1
```

That diff includes ordinary LNO work, so it is not a search-application diff.
Compare search-enabled and explicitly disabled output from identical inputs to
check the observer's no-application contract. The existing review harness also
checks physical state and classifies any native dependence-image padding churn.

## Change the budgets or disable search

For example, cap the portfolio at 18 visits, six unique evaluations and 5,000
microseconds. This edits only policy inside a complete valid binding.

<!-- example: bounded -->
```bash
python3 - "$HIGH_OUT/search.v2.json" "$HIGH_OUT/bounded.v2.json" <<'PY'
import json
from pathlib import Path
import sys
value = json.loads(Path(sys.argv[1]).read_text())
value["search_controls"].update(global_proposal_limit=18,
                                global_unique_limit=6,
                                global_time_limit_us=5000)
with Path(sys.argv[2]).open("x") as output:
    json.dump(value, output, indent=2)
    output.write("\n")
PY
run_high_search bounded \
    "-LNO:high_search=on:high_search_bindings=$HIGH_OUT/bounded.v2.json"
```

The live deadline can alter the visited prefix. The smaller unique budget
allows an incomplete result; inspect `complete` and `conclusion` rather than
assuming all descriptors were covered.

<!-- example: off -->
```bash
run_high_search off -LNO:high_search=off
```

## Stopping policies and result interpretation

Hard global caps and complete coverage apply even if `budget` or `exhausted`
is omitted from `stopping`. Enabled semantic conditions compose with OR.

| Policy | Meaning and extra controls |
| --- | --- |
| `plateau` | Stop only that run after `patience` proposals without improvement exceeding `minimum_gain_cycles`; defaults 12 and 0; ranges 1..12 and 0..10^9 |
| `target` | Best eligible estimate <= positive `target_cycles` (up to 10^18) |
| `relative-gap` | Estimator gap from its internal lower bound <= `gap_ppm` (default 10000; 0..10^6) |
| `cost-aware` | Modeled remaining benefit bounded by explicit exploration cost; positive `remaining_invocations` (up to 10^12), `evaluation_cost_cycles` (up to 10^18) and a matching `economic_conversion` object required |

Cost-aware conversion requires schema `open64.search-cost-conversion.v1`,
positive `host_microseconds` <= 10^12, positive
`reference_cycles_per_microsecond` <= 10^9, and provenance `synthetic`,
`estimated` or `measured-input`. The checked product must equal
`evaluation_cost_cycles`; the compiler does not infer a host/device conversion.
The [contract](AI-COMPILER-SEARCH-M3-CONTROLS-v1.0.md#5-convergence-and-simultaneous-predicates)
defines the estimator lower bound and simultaneous-reason priority.

With tracing enabled, look for:

```text
AI_HIGH_SEARCH 2 OBSERVED ... complete=true ... conclusion=best_estimated_in_declared_space applied=false schedule_legality=not_proved
```

`proposals`, `estimates` and `cache_hits` describe actual work; normally
`proposals = estimates + cache_hits`. `EVENT`, `RUN`, `PREDICATE` and
`RUN_PREDICATE` lines explain visits, per-run state and all terminal conditions.
`complete=false` means the winner is only best among explored eligible entries.
`winner=none` is valid when no eligible candidate has been found; ordinary LNO
continues. A complete estimate proves coverage of this finite estimator space,
not schedule legality or measured optimality. `applied=false` is expected.

Trace visibility requires `-DSL:ai_opt_trace=on -tr25 -tt25:1`. Turning tracing
off suppresses reports without changing search decisions or native output;
absence of a report alone does not establish that search was disabled.

## Diagnostics and fallback

Refused bindings normally leave ordinary LNO available; a successful compiler
exit therefore does not establish search admission. Check the phase trace.
Look for both `AI_HIGH_SEARCH 1` and `2`: shared refusal/accounting records
retain version-one prefixes even on the production v2 route.
Explicit off can prevent observer entry entirely and be quiet; the
`search_explicitly_disabled` record is visible when the legacy partition
observer also reaches that boundary.

| Trace reason / symptom | Action |
| --- | --- |
| `search_explicitly_disabled` | Explicit off was honored; omit it or set on to request search |
| `invalid_search_mode` | Use exactly `on` or `off` |
| `no_binding_document` | Supply the command-line binding path with explicit on |
| `binding_document_unavailable` | Check absolute path, mount visibility and file permissions |
| `binding_document_invalid` | Check schema, required fields/types, duplicate/unknown keys and parser bounds |
| `production_controls_required` | Explicit on requires a v2 root and complete controls; legacy v1 is insufficient |
| `epoch_or_numerical_permission` | Use a current snapshot and source-authorized numerical options |
| `source_or_context_mismatch` | Regenerate for this exact input, source path, snapshot and numerical/target context |
| `native_producer_manifest_mismatch`, `formal_abi_mismatch`, `storage_manifest_mismatch` | Obtain the matching complete producer manifest and buffer/ABI evidence |
| `manifest_numerical_permission_required` | Source contract must explicitly authorize reassociation |
| `gpu_resources_invalid`, `model_binding_invalid`, `search_controls_invalid` | Correct the corresponding profile/control object; retain declared units and provenance |
| `controlled_search_failed` | Retain inputs and trace; investigate source/model admission, arithmetic bounds or clock failure |
| `search_without_eligible_candidate` | Valid bounded outcome; inspect rejected/unevaluated entries and resource budget |
| No report at O0/O1/O2, disabled/excluded LNO, or path-only invocation | Search may never have been requested/admitted at the owning phase boundary |

The JSON input must be valid UTF-8, nonempty, at most 16 MiB, without embedded
NULs, duplicate members, excessive nesting (limit 48) or excessive nodes
(limit 200000). A binding rejected at schema parsing can produce the generic
`binding_document_invalid` reason before a more specific control diagnostic.

Fallback does not repair malformed ordinary WHIRL or guarantee the rest of
compilation succeeds. An internal native-state/analysis isolation violation
is a compiler defect and asserts rather than silently continuing.

## Example validation and current limits

The guide's marked shell examples are intended to run in order in one Bash
session with the environment above. They generate a complete configuration,
exercise actual native parsing/observation, retain diagnostics and reopen real
binary WHIRL. Validation additionally covers malformed, missing and stale
bindings, smaller budgets and explicit off; see the PR's retained evidence.

The examples and native search controls have been validated with freshly built
Linux development backend, WOPT, LNO and inspection tools, with development
assertions enabled. Retained evidence distinguishes these fresh compiler runs
from authenticated producer inputs and comparison tools reused from the earlier
milestone. It includes complete short/long search coverage, bounded and refused
requests, native-state isolation, unchanged search-on/off mapped views, and a
file-by-file compliance review.

The known long post-LNO dependence-image failure in the downstream code-generation
reader reproduces on the comparison baseline. Short host execution and the long
pre-LNO host reference are validated separately; a long post-LNO executable
pipeline remains unverified. These checks certify bounded estimation and ordinary
LNO fallback, with no GPU execution, calibration or performance claim.
