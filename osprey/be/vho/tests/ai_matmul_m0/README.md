# Direct F32 Matmul M0 Runner

This fixture emits one `common.matmul` PU with two 128 x 128 F32 splat
constants. `run.py` produces binary WHIRL, a separate-process
`ir_b2a -st -src` trace, an `-O0` Open64 object, a linked executable,
correctness checks, 30 raw full-PU timing samples, command logs, a receipt,
and checksums. The runtime implements only the direct host F32 ABI needed by
this fixture. It is not a production tensor runtime or a GPU implementation.

Run on a native Linux/x86-64 machine with a matching Open64 build:

```sh
python3 osprey/be/vho/tests/ai_matmul_m0/run.py \
  --build-dir /path/to/open64/build \
  --output-dir /path/to/new/m0-run
```

The output directory must not exist. A failed run retains a staging directory
for diagnosis and does not publish the requested output directory. Run
`shasum -a 256 -c SHA256SUMS` inside a successful output directory to verify
the bundle. The original Open64 object is retained; `matmul.link.o` is a
link-only copy whose local PU symbol was made global with `objcopy`. The
timed scope includes the two constant allocations and the direct matmul, not
just the innermost matrix multiply.

On x86-64 emulation, the runner fails closed unless
`--allow-emulated-smoke` is supplied. That mode checks the compiled path and
records samples, but labels them `not_for_profitability_emulated`. It cannot
close the target-specific `m0-baseline` gate. The runner also does not yet
publish into the immutable AI review namespace or update its index; its
output is a run bundle pending acceptance review.

The compiled `emulated-smoke-001/` bundle can be transferred without
rebuilding Open64 on the target. `prepare_native.py` verifies its checksum
manifest and creates a non-overwriting package containing the same executable,
source, binary WHIRL, and text WHIRL. The prepared
`m0-baseline/native-ready-001/` package has no native receipt yet.

```sh
python3 osprey/be/vho/tests/ai_matmul_m0/prepare_native.py \
  --smoke-artifact /Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/m0-baseline/emulated-smoke-001 \
  --output-dir /path/to/new/native-ready-run
python3 osprey/be/vho/tests/ai_matmul_m0/remote_native.py \
  --artifact-dir /path/to/new/native-ready-run \
  --host USER@NATIVE_X86_HOST --remote-dir /absolute/new/remote/path
```

The remote runner rejects QEMU/VirtualApple emulation, rechecks the compiled
binary and WHIRL hashes, repeats all 30 correctness-checked timings, retains
CPU/GPU identity when available, and retrieves a checksum-verified receipt.
That receipt remains `review_accepted=false` until target and measurement
comparability are reviewed; it does not by itself validate the CUDA tile.

`sh osprey/be/vho/tests/ai_matmul_m0/test_runtime.sh` is a host-only test
of the minimal ABI runtime and timing harness. It uses a stand-in entry
function and is not Open64 performance evidence.
