# M6 selected-plan production route and dependence-image repair v1.0

November 8, 2026.

Scope: admitted absorbed MLA, active-PU M4 application and M5 native device lowering. AOT remains the baseline. This milestone integrates the bounded route into the backend driver; it does not extend the search space, synthesize arbitrary schedules, implement a generic NVISA target, or close M7 GPU acceptance.

## Production lifecycle

Opt in with `-LNO:high_search=on:high_apply=on:high_search_bindings=<v2.json>:high_device_output=<fresh-directory>:high_device_tool=<tensor_selected_bridge>`. The helper path is explicit; no shell command or PATH search is used. The ordinary compiler host output remains the fallback and is produced with its original options.

Immediately after LNO, the driver checks for a freshly accepted High seal before MAINOPT or source/ABI lowering can change the tree. It verifies the complete tree/COMMENT binding, captures that active PU in a forked writer, and invokes the selected native helper in a separate process. Neither writer nor helper can mutate the parent's WN/ST/TY/maps/phase managers. Device lowering uses fresh driver-owned contexts. It never invokes candidate exploration. Read-only search remains the existing M3 observer, followed by one M4 application.

The helper reuses the strict v2 binding reader and shared binding-key spelling. The actual High image and complete original source/facts/unit/function/target/resource/model/control/numerical/recipe/storage binding are required. A changed manifest, removed COMMENT, stale native key, unsupported architecture or unsupported selected scope refuses before publication. Legacy explicit M5 handoffs remain supported; production additionally requires the M6 input-binding seal.

Each compilation reserves a fresh output directory. Per-PU work is private until every native boundary and PTX emission succeeds. A per-PU completion manifest is published last using no-replace links. It names the native High/Mid/Low images, PTX, math helper, launch/storage metadata and readable optimized algorithm. No selected worker replaces a host PU or rewrites its callers. Consumers must require the completion manifest; partial work is retained only inside a `.pending-*` directory. Independent PUs receive ordinal directories. Reuse of an existing directory, missing helper, failed child or rejected binding leaves host compilation available and emits an explicit device refusal. No fallback candidate is searched or materialized.

COMMENTs use the existing M4 PREOPT reinsertion workaround. The M5 source seal, generated explanations, native lowering and CUDA-only Low verifier remain authoritative. General PREOPT COMMENT preservation is a separate effort.

The current v2 binding names one host PU per invocation; unrelated PUs retain ordinary routing. Ordinal publication and the two-PU isolation test do not expand that admission to multiple manifests or IPA. Host allocation/launch consumes the explicit device ABI; this milestone does not insert a CUDA launch or rewrite callers automatically.

## Dependence-image root cause and repair

The retained scalar long-context post-LNO image has 263 dependence vertices, one with WN offset zero. The tree writer's fresh offset map only assigns offsets to nodes actually written. LNO retained a non-null graph vertex whose WN was removed from the final tree; the old graph writer kept it and wrote the default zero. The reader consequently interpreted the tree header as a WN and failed in OPCODE_make_op from Add_Vertex.

Extend the graph writer's existing live-vertex compaction to require a nonzero offset in that fresh map. Remove incident edges through the same vertex remapping; preserve all surviving vertices and dependence tuples. The edge count remains the historical allocation-capacity bound. The reader applies the same compaction to legacy zero-offset entries before associating a vertex with a WN, and remaps incident edges. Header/vertex/edge bounds are checked before reads. Thus the immutable original images also reopen without interpreting their headers as nodes or dropping the entire surviving graph. This changes neither ELF sections nor native table/node layouts, offset widths, reader ABI or dependence encoding. Valid historical images retain identity vertex numbering and tuples. No repair-by-byte-edit is permitted.

Validation must reproduce the old-reader failure, reopen the exact immutable legacy images with the corrected reader, regenerate the exact scalar short/long cases, independently reopen with native ir_b2a `-st -src` and both backend readers, prove new images contain no zero vertex offsets, compare surviving graph nodes/tuples and unchanged decoded executable trees, and execute scalar host output against reference oracles. Before/after images, full traces, raw diffs and diagnostics are retained. The scalar long path can call Open64's strided vexpf runtime even with SIMD disabled: host tests compile/link the actual libm/vexpf.c and vexpftab.c with a declared ABI adapter for legacy whirl2c C casts/count types. Existing vector output is a separate compiler lane; unsupported whirl2c vector operators are not accepted host execution.

## Acceptance and remaining gates

Exercise selected short/long generation directly from be_driver without a manual checkpoint/bridge step; independent mapped High/Mid/Low reads; deterministic native PTX; whirl2c COMMENTs; assembly/device link and separate CUDA compilation. Test disabled/estimate-only/below-O3/unadmitted paths, stale or mismatched inputs, missing/failing helper, output collisions, multiple PU ownership, and host-output equality on device refusal. Reuse reference executions only under complete source/producer and exact executable/CUDA-token/launch equality receipts.

Audit be.so defined/undefined symbols and dependencies: existing implementation bodies may change, but no backend dependency or public API is added. Broader accepted forms, IPA, asynchronous pipelines and generic target lowering require separate admission. GPU numerical/sanitizer/resource/performance results remain M7. Owner, successor, independent reviewer and approving lead remain TBD.

Observed focused compiler/host acceptance: automatic selected short/long
production and one selected PU with an ordinary neighbor; 14 fallback/skip
cases, 10 changed-input refusals and two truncated graph refusals; 28 fresh
scalar host reference executions covering both immutable legacy and corrected
images with identical outputs. The long graph retains 262 live vertices after
one zero-offset entry is removed, with exact native surviving WN/tuple equality.
M4's final suite passes 52 fresh High host reference executions, 10 rollback
cases and 27 refusals. M5's seven configurations/41 entries pass native assembly,
device link, independent CUDA compilation and 66 refusals; 48 retained lowered
host results are revalidated only under exact executable/CUDA-token/launch
identity. Backend defined/undefined symbols and needed libraries are unchanged.
Complete commands, producer hashes, native traces/diffs and replay results live
in the versioned M6 receipt. These results make no GPU execution/performance
claim and do not assign the TBD personnel roles.
