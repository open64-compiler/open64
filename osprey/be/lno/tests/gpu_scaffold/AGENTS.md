# Working on this scaffold

Read SPEC.md before making changes. It records the user's converged GEMM/MLA
performance-testing contract and is the basis for future improvements.
Preserve server-owned test/golden data, same-GPU baseline measurement for every
dataset, and candidate/baseline wall-clock ratio direction. Keep the common ABI
minimal. PROFILING.md describes the optional single-pass PM diagnostic path.
It is implemented but not yet validated on real CUDA/Nsight Compute hardware.
Do not mix profiled timings into benchmark results.
The v2 distribution writes the same result schema and uses the same offline
validator as Open64 MLA Q7. See SHARED_RESULTS.md. Keep the two shared Python
modules byte-identical to their canonical parent sources when packaging.
README.md contains execution and validation instructions. Record actual validation
and limitations; host checks do not establish CUDA correctness or GPU performance.
