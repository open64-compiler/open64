# Torch2whirl Llama 2 Operator Census

This file records the observable-discovery evidence for the tiny Llama 2
prefill fixture. It is a frontend discovery artifact only: it does not allocate
DSL opcodes, define WHIRL encodings, or authorize native lowering.

## Progress Dashboard

Progress is earned only when the checkpoint exit test passes.

| Checkpoint | Weight | Status | Review evidence |
| --- | ---: | --- | --- |
| D0: Record baseline and exact tiny configuration | 5% | complete | Baseline command and tiny profile below |
| D1: Implement deterministic tiny Llama 2 fixture | 20% | complete | `python/tests/models/llama2_model.py` |
| D2: Pass eager shape/dtype/repeatability tests | 15% | complete | Docker `python_torch_test` ran 32 tests OK |
| D3: Capture and normalize the real PyTorch graph | 20% | pending | Preserved normalized graph artifact |
| D4: Complete semantic operator census | 25% | pending | One row per captured FX node |
| D5: Add and pass graph/census drift tests | 10% | pending | Golden comparison and drift test |
| D6: Publish main-infrastructure handoff | 5% | pending | Contract requests and blockers |

Current earned progress: 40%.

## D0 Baseline

Branch at kickoff: `codex/torch2whirl-python-fe`.

Commit at kickoff: `e07f0ea5`.

Required bootstrap commit: `cf291910 Plan observable Llama 2 ingestion`.
The bootstrap cherry-pick landed on this branch as `3113cfb9` after resolving
documentation-only conflicts in `AGENTS.md` and keeping the added infrastructure
reference document.

Baseline command:

```sh
make -C osprey/torch2whirl -f Makefile.gbase python_test
```

Baseline result:

```text
Ran 103 tests in 0.010s

OK (skipped=41)
```

## Tiny Profile

The first discovery fixture uses this static, dependency-light configuration:

| Property | Value |
| --- | --- |
| Vocabulary | 128 tokens |
| Hidden dimension | 32 |
| Decoder layers | 2 |
| Attention heads | 4 |
| KV heads | 4 |
| Head dimension | 8 |
| Batch size | 1 |
| Sequence length | 8 |
| Activation dtype | `float32` |
| Weight dtype | `float32` |
| Token dtype | `int64` |
| KV cache | Disabled |
| Dropout | None |
| Initialization | Deterministic local arithmetic pattern |
| Input | `input_ids` tensor with shape `[1,8]` |
| Output | Per-token logits with shape `[1,8,128]` |

Intentional differences from Meta's runtime implementation:

1. The fixture uses ordinary `torch.nn` modules only.
2. FairScale, CUDA-only state, distributed checkpointing, tokenizer logic, and
   licensed weights are excluded.
3. KV-cache allocation and mutation are excluded from the first prefill
   profile.
4. Shapes are static so graph drift can be reviewed deterministically before
   frontend lowering or native DSL contracts are added.

## Retained Review Artifacts

The D0 retained artifact is this census file. D1 adds the reviewable source
fixture at `python/tests/models/llama2_model.py`. D3-D5 will add the normalized
FX graph golden under `python/tests/golden`.

## D2 Eager Test Evidence

Local syntax and optional-skip checks:

```sh
PYTHONPYCACHEPREFIX=/private/tmp/open64-pycache python3 -m compileall -q \
  osprey/torch2whirl/python/tests
make -C osprey/torch2whirl -f Makefile.gbase python_test
```

Local result:

```text
Ran 106 tests in 0.008s

OK (skipped=44)
```

Torch Docker command:

```sh
OPEN64_TORCH2WHIRL_BUILD_DIR=/private/tmp/open64-torch2whirl-llama2 \
  sh osprey/torch2whirl/scripts/run_torch_docker_test.sh
```

Torch Docker result:

```text
Ran 32 tests in 2.128s

OK
```

The D2 tests verify the selected integer input dtype and shape, output logits
shape and dtype, repeatable inference, eval mode, absence of cache state, and
state-dict discoverability for embedding, attention, feed-forward, final norm,
and output tensors.

## Discovered Graph Forms

D0 has not captured a graph yet. Expected forms to inspect in D3-D5 include
`placeholder`, `get_attr`, `call_module`, `call_function`, `call_method`, and
`output`.

## Semantic Ambiguities

D0 has not captured semantic ambiguities yet. D3-D5 must determine how PyTorch
exposes token embedding, RMSNorm, RoPE, causal masking, attention, SwiGLU,
residual paths, decoder-layer boundaries, and logits.

## Infrastructure Contracts Needed

D0 requests no new native infrastructure. L0-L1 discovery remains independent
of common/com implementation.
