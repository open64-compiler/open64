#!/usr/bin/env bash
# Combined source bundle wrapper; all finalization belongs to the Python runner.
set -euo pipefail
packet_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
: "${CUDA_HOME:=/usr/local/cuda}"
export CUDA_HOME
export PATH="$CUDA_HOME/bin:$PATH"
export PYTHONDONTWRITEBYTECODE=1
exec "${OPEN64_GPU_PYTHON:-python3}" "$packet_dir/tools/run_gpu_test_suite.py" --packet "$packet_dir" "$@"
