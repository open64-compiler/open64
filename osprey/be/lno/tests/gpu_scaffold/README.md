# GEMM and BF16 MLA testing scaffold

This layer supplies server-owned workload ABIs, deterministic input/oracle data,
example baseline/candidate kernels and the unprofiled CUDA harness/runner.
SPEC.md controls acceptance. PM diagnostics are added in the next review layer.

From this directory, use `python3 server/run.py submissions/gemm --check-only`
or `python3 server/run.py submissions/mla --check-only` for CPU/corpus checks.
Without --check-only a compatible CUDA host is required. Same-session baseline
and candidate correctness precede separate wall/device timing; candidate/baseline
wall ratio below one means improvement. Check-only is not GPU qualification.

Run `python3 -m unittest discover -s tests -v`. Actual NVIDIA execution and
performance qualification remain pending. Preserve original source packets and
returned evidence; generated data/cache/run directories are ignored by Git.
