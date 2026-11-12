# MLA Q7 result return specification v1.0

November 10, 2026. Addendum to Q7 qualification v1.0. Friend distribution
v1.1 adds `package_mla_q7_results.py`; it also works as a standalone script
beside an already extracted v1.0 packet. Python >=3.8, standard library only.
Original distribution archives and qualification documents remain intact.

After the runs finish, invoke from the extracted source packet directory:

```bash
python3 package_mla_q7_results.py \
  --packet . --expected-sha256 "$(cat PACKET_ID)" \
  --results ../q7-correctness ../q7-all ../q7-compile-generated \
  --output ../Open64_MLA_Q7_results.tar.gz
```

Send back **both** `Open64_MLA_Q7_results.tar.gz` and its
`Open64_MLA_Q7_results.tar.gz.sha256` sidecar. Adjust `--results` to the actual
output directories. Add optional host-rebuild or Nsight output directories to
the same list. If only a partial/failed run exists, collect it anyway. Missing
directories and incomplete JSON remain diagnostic notes; packaging can succeed
without hardware acceptance. Choose a new output filename on each collection.

The collector authenticates the exact source packet before collecting. It
copies original reports, commands, stdout/stderr, raw performance samples,
Nsight files and other regular files under only the explicitly named result
directories. It records missing/unreadable/changing files and skipped links.
It does not follow result symlinks or recursively package its own output.
Output must be outside the source packet and all selected result directories.
The full original source packet is not duplicated.

The archive has `Open64_MLA_Q7_results/` as its single root:

| Location | Content |
| --- | --- |
| `bundle.json` | Packet identity, collection time, explicit input roots, report inventory, packet-binding match/mismatch/missing, collection notes; qualification remains NOT_ASSESSED_BY_COLLECTOR |
| `results/NN-name/` | Original bytes of every collected regular result file; duplicate input basenames are separated by ordinal |
| `environment/` | Collection-time nvidia-smi GPU UUID/Driver/settings observations; nvcc, ptxas, nvlink, Compute Sanitizer, Nsight Compute/Systems and host C++ versions; OS/kernel/Python facts; missing/error/timeout diagnostics |
| `packet/` | Original packet ID, source manifest, case metadata and available source/build identity records |
| `SHA256SUMS` | Hash of every archived regular file except this checksum manifest |

Environment capture reads settings and version strings only. It launches no
GPU workloads, compiler optimization, search, lowering, profiler recording or
fallback. Each command has a bounded timeout (15 seconds by default;
`--command-timeout` accepts 1..120). It records CUDA_HOME/CUDA_PATH, visible-device,
module-loading, NVIDIA capability and executable/library path variables only;
it never dumps the entire environment. Collection-time observations supplement
original run-time nvidia-smi snapshots and device/timing facts, not replace them.
nvidia-smi's CUDA version is Driver capability; nvcc identifies the installed
toolkit. Unavailable tools are recorded without preventing return packaging.

The collector does not rewrite an original status, relax numerical tolerances,
interpret a clean sanitizer summary, combine incompatible packet identities,
or label hardware qualified. Its only success is creating the return archive.
Different-packet reports are retained with an explicit MISMATCH note. This
allows an independent reviewer to inspect failed, unavailable, incomplete or
successful evidence without confusing collection with qualification.

Archive members are relative paths without symlinks; a private staging snapshot
is checksummed before tar creation. Complete tar/checksum files are published
without replacement. Existing outputs are refused. A file changing during its
copy is retained with a note, so stop active runs before collecting final
evidence. Exit 0 means packaging succeeded; exit 1 means packet authentication,
output, or collection publication failed. It does not reuse Q7 exit 77/78 as
qualification outcomes.

Focused tests cover report/sample byte preservation, manifest verification,
failed/partial/missing runs, identity mismatches, symlink exclusion, output
collision/recursion, tampered packet refusal and unavailable environment tools.
Integration tests package the producer's actual unavailable-GPU and offline
compile reports. No NVIDIA result is invented. The v1.1 source packet preserves
all compiler-generated CUDA/PTX/cubins, WHIRL, COMMENT and Q3 implementation
bytes from v1.0; the new collector changes no execution path or kernel.

Two-phase search, source fallback and COMMENT policy remain unchanged. AOT
selection remains deferred. Returned evidence still needs independent Q7
hardware review; personnel roles remain TBD.
