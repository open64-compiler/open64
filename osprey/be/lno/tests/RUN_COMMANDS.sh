#!/usr/bin/env bash
# Q7 one-command workflow. Python owns finalization after failures/signals;
# every run gets a fresh result directory outside the immutable source packet.
# Contract: doc/AI-COMPILER-MLA-Q7-RESULTS-RETURN-v1.1.md.
# v1.3 adds a separate optional counter lane; see Q7-PROFILING-v1.0.md.
set -euo pipefail
packet_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
: "${CUDA_HOME:=/usr/local/cuda}"
export CUDA_HOME
export PATH="$CUDA_HOME/bin:$PATH"
export PYTHONDONTWRITEBYTECODE=1
exec "${OPEN64_Q7_PYTHON:-python3}" "$packet_dir/run_mla_q7_workflow.py" --packet "$packet_dir" "$@"
