#!/usr/bin/env bash
# Manual recovery uses the same read-only collector, including diagnostics.
set -euo pipefail
packet_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
run_dir=${1:?Usage: bash RETURN_RESULTS.sh /path/to/q7-run-XXXXXXXX /path/to/new-return.tar.gz}
output=${2:?Choose a fresh return archive filename}
results=("$run_dir/workflow" "$run_dir/compile-generated" "$run_dir/qualification")
if [[ -d "$run_dir/profiling" ]]; then
    results+=("$run_dir/profiling")
fi
exec "${OPEN64_Q7_PYTHON:-python3}" "$packet_dir/package_mla_q7_results.py" \
    --packet "$packet_dir" --expected-sha256 "$(cat "$packet_dir/PACKET_ID")" \
    --results "${results[@]}" --output "$output"
