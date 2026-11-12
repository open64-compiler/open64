# MLA Q7 automatic result return v1.1

November 10, 2026. Extends the [return collector v1.0](AI-COMPILER-MLA-Q7-RESULTS-RETURN-v1.0.md).
Friend distribution v1.2 introduces a tracked canonical `RUN_COMMANDS.sh` and
`run_mla_q7_workflow.py`. Earlier archives/documents remain unchanged.

The operator runs one command from the extracted packet:

```bash
bash RUN_COMMANDS.sh
```

The wrapper uses Python >=3.8, already required by Q7. It creates a fresh
`q7-run-*` sibling directory on every invocation, runs all Q7 control tests,
compiles the fixed generated CUDA/PTX, then invokes Q7 `--mode all` for
correctness, memcheck, racecheck, synccheck, initcheck and uninstrumented
performance. Q7 all already starts with correctness, so the wrapper does not
run a redundant standalone correctness pass. No algorithm selection, search,
IR transformation, tolerance relaxation or fallback launch is added.

Whether a phase succeeds, fails, times out or receives catchable SIGINT/SIGTERM,
finalization attempts to run the existing authenticated collector. It collects
workflow logs/status, compile outputs and partial or complete qualification
results, plus the existing GPU/Driver/CUDA/tool environment observations. The
console prints the exact paths of the two files to send back:

```text
q7-run-XXXXXXXX/Open64_MLA_Q7_results.tar.gz
q7-run-XXXXXXXX/Open64_MLA_Q7_results.tar.gz.sha256
```

Existing output directories/archives are never reused. Options such as
`bash RUN_COMMANDS.sh --device 1 --iterations 10 --results-parent /data/results`
are explicit operator choices. `--timeout` controls Q7's per-GPU/tool child
budget; `--step-timeout` bounds each full wrapper phase (default 14,400 seconds)
and `--collection-timeout` bounds final packaging (default 900 seconds).
`CUDA_HOME` defaults to `/usr/local/cuda`; `OPEN64_Q7_PYTHON` can name the Python
executable. Results must be outside the immutable packet.

Each phase owns a new process group. On interruption/timeout the wrapper
terminates and reaps its child before collecting; it also stops leftover
descendants in that group. No unrelated process group is targeted. Terminating
a failed qualification process does not certify device drain/correctness or
permit an in-process source fallback. Actual Q3/CUDA ownership stays within
the unchanged Q6d process/runtime contract.

The first nonzero workflow exit is retained, including 77 for unavailable
hardware, 78 for unavailable tools, 124 for a wrapper step timeout and 130/143
for SIGINT/SIGTERM. Successful collection cannot turn that result into exit 0.
If testing succeeds but collection fails, the collector's nonzero status is
returned. Both statuses and final artifact paths are recorded in
`q7-run-*/completion.json`; the pre-collection `workflow/status.json` is inside
the tar. Collector stdout/stderr remain beside the tar rather than inside its
live input tree, avoiding self-changing snapshots. Every original phase log
and report remains intact. Collection still means packaging, not qualification.

Collection is best effort when Python, packet authentication, storage or
permissions fail. SIGKILL, power loss and a second termination during collection
cannot guarantee an archive. In those cases the wrapper reports the retained
log directory and a nonzero status; it never invents a successful return file.

Focused process-level tests exercise successful collection, unit/compile/GPU-
unavailable failures, timeout, SIGINT/SIGTERM, collector failure, exit precedence
and fresh outputs on repeat runs. These are control-flow tests, not simulated
GPU numerical/performance evidence. Extracted-packet testing retains the actual
no-Driver report and packaging behavior. Fixed source, cubins, WHIRL, COMMENT,
Q3 publication, two-phase search and source fallback remain unchanged. AOT
selection remains deferred; hardware acceptance still requires NVIDIA results.
