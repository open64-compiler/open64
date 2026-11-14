# Open64 MLA Q7 profiling release notes

November 11, 2026. NVIDIA execution package v1.3; returned-results validator v1.1.

Adds independent compute-wait, data-path and data-origin diagnostic groups with
explicit measured/derived/proxy/estimate/unknown classification. A separate NCU
lane queries supported metrics and preserves raw base-unit CSV and native
reports. Optional Nsight Systems captures a separate clean-request timeline.
Topology and host-interface traffic observations remain explicitly scoped.

Receiver summaries recompute derived rates, issue inactivity and evidence
classification from raw counters, authenticate the three stage entry names and
verify child correctness plus profiling isolation. Missing tools are diagnostic
gaps; contradictory evidence fails validation. Existing qualification timing
and diagnostic statuses remain separate. Earlier Q7 returns are supported.

No local/peer/remote exposed wait percentage or time-at-saturation percentage is
invented from aggregate counters. Such values remain unknown until evidence
supports attribution. Host-interface counters are not remote-GPU counters.

Generated CUDA/PTX/WHIRL/cubins, Q3 runtime and all inputs/references are preserved
byte-for-byte from v1.2. No lowering, compiler search, cost model, dispatch or JIT
changes. AOT selection stays deferred. Existing archives remain immutable.

Run `bash RUN_COMMANDS.sh`; optionally add `--profile-timeline` and repeated
`--profile-case CASE` arguments. Send back the automatically created results tar
and checksum. Use the v1.1 receiver and the exact v1.3 source packet to inspect
the returned evidence. See the included profiling specification for scope and
interpretation. No NVIDIA correctness, race or performance result is claimed
from package preparation or CPU tests.
